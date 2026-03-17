;;; wikipedia-test.el --- Tests for wikipedia.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Comprehensive test suite for the wikipedia.el package.
;; Covers: parsing, formatting, caching, draft encoding, watchlist logic,
;; history annotation, XTools helpers, database operations, and URL building.
;;
;; Run with: emacs -batch -L .. -L . -l wikipedia-test.el -f ert-run-tests-batch-and-exit
;; Or via the project Makefile: make test

;;; Code:

(require 'ert)
(require 'cl-lib)

;;;; Provide mediawiki stubs for batch testing
;; The real mediawiki package is an external dependency; these stubs
;; provide just enough interface to load the wikipedia modules.

(unless (featurep 'mediawiki)
  (defvar mediawiki-site-alist
    '(("en" :url "https://en.wikipedia.org/w/index.php")
      ("es" :url "https://es.wikipedia.org/w/index.php"))
    "Stub site configuration for tests.")
  (defvar mediawiki-site nil "Stub current site.")
  (defvar mediawiki-page-title nil "Stub page title.")
  (defun mediawiki-site (&optional _site) nil)
  (defun mediawiki-api-call (&rest _args) nil)
  (defun mediawiki-open (&rest _args) nil)
  (defun mediawiki-save (&rest _args) nil)
  (defun mediawiki-mode (&rest _args) nil)
  (provide 'mediawiki))

(unless (featurep 'mediawiki-api)
  (defun mediawiki-make-api-url (_site)
    "https://en.wikipedia.org/w/api.php")
  (provide 'mediawiki-api))

(unless (featurep 'mediawiki-http)
  (unless (fboundp 'url-http-post)
    (defun url-http-post (&rest _args) nil))
  (provide 'mediawiki-http))

;; Add project root to load path
(let ((dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (add-to-list 'load-path (expand-file-name ".." dir)))

(require 'wikipedia-adapter)
(require 'wikipedia-cache)
(require 'wikipedia-common)
(require 'wikipedia-diff)
(require 'wikipedia-draft)
(require 'wikipedia-watchlist)
(require 'wikipedia-history)
(require 'wikipedia-xtools)
(require 'wikipedia-user)
(require 'wikipedia-mirror)

;; Conditionally load database module (requires SQLite support)
(defvar wikipedia-test--sqlite-available
  (condition-case nil
      (progn (require 'wikipedia-db) t)
    (error nil))
  "Non-nil if SQLite is available for database tests.")

;;;; Test helpers

(defmacro wikipedia-test--with-site (&rest body)
  "Execute BODY with a mock wiki site configured."
  (declare (indent 0))
  `(let ((wp--current-site "en")
         (mediawiki-site-alist
          '(("en" :url "https://en.wikipedia.org/w/index.php")
            ("es" :url "https://es.wikipedia.org/w/index.php"))))
     ,@body))

(defmacro wikipedia-test--with-cache (&rest body)
  "Execute BODY with a fresh revision cache (max 5 entries)."
  (declare (indent 0))
  `(let ((wikipedia--revision-cache (make-hash-table :test 'eql))
         (wikipedia--revision-cache-keys nil)
         (wikipedia--revision-cache-max-size 5)
         (wikipedia--prefetch-in-flight (make-hash-table :test 'eql)))
     ,@body))

(defmacro wikipedia-test--with-draft-dir (&rest body)
  "Execute BODY with a temporary draft directory, cleaned up afterward."
  (declare (indent 0))
  `(let* ((tmpdir (make-temp-file "wp-draft-test-" t))
          (wikipedia-draft-directory tmpdir))
     (unwind-protect
         (progn ,@body)
       (delete-directory tmpdir t))))


;;;; ================================================================
;;;; 1. CRITICAL: Adapter parsing tests
;;;; ================================================================

;;; wp--parse-number

(ert-deftest wp-parse-number/string ()
  "Parse a numeric string."
  (should (= (wp--parse-number "42") 42)))

(ert-deftest wp-parse-number/number ()
  "Return numbers unchanged."
  (should (= (wp--parse-number 42) 42)))

(ert-deftest wp-parse-number/nil ()
  "Return nil for nil input."
  (should (null (wp--parse-number nil))))

(ert-deftest wp-parse-number/empty-string ()
  "Return nil for empty string."
  (should (null (wp--parse-number ""))))

(ert-deftest wp-parse-number/zero ()
  "Parse \"0\" as 0, not nil."
  (should (eql (wp--parse-number "0") 0)))

(ert-deftest wp-parse-number/negative ()
  "Parse negative number string."
  (should (= (wp--parse-number "-100") -100)))

;;; wp--parse-revision

(ert-deftest wp-parse-revision/basic ()
  "Parse a typical revision XML element."
  (let* ((rev '(rev ((revid . "12345")
                     (parentid . "12344")
                     (timestamp . "2024-01-15T10:30:00Z")
                     (user . "ExampleUser")
                     (comment . "Fixed typo")
                     (size . "5432"))))
         (parsed (wp--parse-revision rev)))
    (should (= (alist-get 'revid parsed) 12345))
    (should (= (alist-get 'parentid parsed) 12344))
    (should (equal (alist-get 'timestamp parsed) "2024-01-15T10:30:00Z"))
    (should (equal (alist-get 'user parsed) "ExampleUser"))
    (should (equal (alist-get 'comment parsed) "Fixed typo"))
    (should (= (alist-get 'size parsed) 5432))))

(ert-deftest wp-parse-revision/missing-comment ()
  "Missing comment defaults to empty string."
  (let* ((rev '(rev ((revid . "1") (timestamp . "2024-01-01T00:00:00Z")
                     (user . "Bot") (size . "100"))))
         (parsed (wp--parse-revision rev)))
    (should (equal (alist-get 'comment parsed) ""))))

(ert-deftest wp-parse-revision/minor-edit ()
  "Minor flag is truthy when present."
  (let* ((rev '(rev ((revid . "1") (minor . "")
                     (timestamp . "2024-01-01T00:00:00Z")
                     (user . "User") (size . "100"))))
         (parsed (wp--parse-revision rev)))
    (should (alist-get 'minor parsed))))

(ert-deftest wp-parse-revision/no-parent ()
  "First revision of a page has nil parentid."
  (let* ((rev '(rev ((revid . "1") (timestamp . "2024-01-01T00:00:00Z")
                     (user . "Creator") (size . "500"))))
         (parsed (wp--parse-revision rev)))
    (should (null (alist-get 'parentid parsed)))))

;;; wp--parse-watchlist-entry

(ert-deftest wp-parse-watchlist-entry/basic ()
  "Parse a typical watchlist entry."
  (let* ((entry '(item ((title . "Example page")
                        (revid . "67890") (old_revid . "67889")
                        (timestamp . "2024-03-01T14:00:00Z")
                        (user . "Editor") (comment . "Updated section")
                        (oldlen . "1000") (newlen . "1050"))))
         (parsed (wp--parse-watchlist-entry entry)))
    (should (equal (alist-get 'title parsed) "Example page"))
    (should (= (alist-get 'revid parsed) 67890))
    (should (= (alist-get 'old_revid parsed) 67889))
    (should (= (alist-get 'oldlen parsed) 1000))
    (should (= (alist-get 'newlen parsed) 1050))))

;;; wp--parse-contrib-entry

(ert-deftest wp-parse-contrib-entry/basic ()
  "Parse a typical contribution entry."
  (let* ((entry '(item ((revid . "111") (parentid . "110")
                        (title . "Some Article")
                        (timestamp . "2024-02-20T08:15:00Z")
                        (comment . "Added references")
                        (sizediff . "250"))))
         (parsed (wp--parse-contrib-entry entry)))
    (should (= (alist-get 'revid parsed) 111))
    (should (= (alist-get 'parentid parsed) 110))
    (should (equal (alist-get 'title parsed) "Some Article"))
    (should (= (alist-get 'sizediff parsed) 250))))

(ert-deftest wp-parse-contrib-entry/negative-sizediff ()
  "Parse a contribution with negative size diff."
  (let* ((entry '(item ((revid . "222") (title . "Article")
                        (sizediff . "-100"))))
         (parsed (wp--parse-contrib-entry entry)))
    (should (= (alist-get 'sizediff parsed) -100))))

;;; wp--extract-revision-content

(ert-deftest wp-extract-revision-content/basic ()
  "Extract wikitext from a typical revision element."
  (let ((rev '(rev ((revid . "123"))
               (slots nil
                 (slot ((contentmodel . "wikitext")
                        (contentformat . "text/x-wiki")
                        (slot . "main"))
                   "== Hello ==\nThis is wikitext.")))))
    (should (equal (wp--extract-revision-content rev)
                   "== Hello ==\nThis is wikitext."))))

(ert-deftest wp-extract-revision-content/nil ()
  "Return nil for nil input."
  (should (null (wp--extract-revision-content nil))))

(ert-deftest wp-extract-revision-content/no-slots ()
  "Return nil when slots element is missing."
  (should (null (wp--extract-revision-content '(rev ((revid . "123")))))))

;;; wp--extract-preview-html

(ert-deftest wp-extract-preview-html/string-child ()
  "Extract HTML from text element with string child."
  (let ((text '(text ((xml:space . "preserve")) "<p>Hello</p>")))
    (should (equal (wp--extract-preview-html text) "<p>Hello</p>"))))

(ert-deftest wp-extract-preview-html/nil ()
  "Return nil for nil input."
  (should (null (wp--extract-preview-html nil))))

;;; wp--extract-diff-content

(ert-deftest wp-extract-diff-content/with-body ()
  "Extract diff content string from body element children."
  (let ((diff-body '((body nil "<tr>diff rows</tr>"))))
    (should (equal (wp--extract-diff-content diff-body)
                   "<tr>diff rows</tr>"))))

(ert-deftest wp-extract-diff-content/fallback ()
  "Fall back to last element when no body element."
  (should (equal (wp--extract-diff-content '("the diff content"))
                 "the diff content")))

(ert-deftest wp-extract-diff-content/nil ()
  "Return nil for nil input."
  (should (null (wp--extract-diff-content nil))))

;;; wp--find-token-in-tree

(ert-deftest wp-find-token-in-tree/direct ()
  "Find token at top level."
  (should (equal (wp--find-token-in-tree '(tokens ((csrftoken . "abc+\\"))))
                 "abc+\\")))

(ert-deftest wp-find-token-in-tree/nested ()
  "Find token nested in XML tree."
  (let ((tree '(api nil (query nil (tokens ((csrftoken . "mytoken+\\")))))))
    (should (equal (wp--find-token-in-tree tree) "mytoken+\\"))))

(ert-deftest wp-find-token-in-tree/not-found ()
  "Return nil when token is absent."
  (should (null (wp--find-token-in-tree '(api nil (query nil))))))

;;; wp--batch-titles

(ert-deftest wp-batch-titles/empty ()
  (should (null (wp--batch-titles nil 50))))

(ert-deftest wp-batch-titles/single-batch ()
  (let ((result (wp--batch-titles '("A" "B" "C") 50)))
    (should (= (length result) 1))
    (should (equal (car result) '("A" "B" "C")))))

(ert-deftest wp-batch-titles/exact-fit ()
  (let ((result (wp--batch-titles '("A" "B" "C" "D") 2)))
    (should (= (length result) 2))
    (should (equal (car result) '("A" "B")))
    (should (equal (cadr result) '("C" "D")))))

(ert-deftest wp-batch-titles/overflow ()
  (let ((result (wp--batch-titles '("A" "B" "C" "D" "E") 2)))
    (should (= (length result) 3))
    (should (equal (nth 2 result) '("E")))))

;;; wp--format-timestamp-for-api

(ert-deftest wp-format-timestamp-for-api/known-value ()
  "Format epoch 1704067200 -> 2024-01-01T00:00:00Z."
  (should (equal (wp--format-timestamp-for-api 1704067200)
                 "2024-01-01T00:00:00Z")))


;;;; ================================================================
;;;; 2. CRITICAL: Display & formatting utilities
;;;; ================================================================

;;; wikipedia--format-timestamp

(ert-deftest format-timestamp/default-length ()
  "Default format truncates to 16 chars (YYYY-MM-DD HH:MM)."
  (should (equal (wikipedia--format-timestamp "2024-01-15T10:30:00Z")
                 "2024-01-15 10:30")))

(ert-deftest format-timestamp/with-seconds ()
  "Length 19 includes seconds."
  (should (equal (wikipedia--format-timestamp "2024-01-15T10:30:45Z" 19)
                 "2024-01-15 10:30:45")))

(ert-deftest format-timestamp/nil ()
  "Return empty string for nil."
  (should (equal (wikipedia--format-timestamp nil) "")))

(ert-deftest format-timestamp/short-string ()
  "Handle timestamps shorter than requested length."
  (should (equal (wikipedia--format-timestamp "2024-01" 16) "2024-01")))

(ert-deftest format-timestamp/replaces-T-separator ()
  "Replace T with space in ISO 8601 format."
  (should-not (string-match-p "T"
               (wikipedia--format-timestamp "2024-01-15T10:30:00Z"))))

;;; wikipedia--size-change-face

(ert-deftest size-change-face/positive ()
  (should (eq (wikipedia--size-change-face 100) 'success)))

(ert-deftest size-change-face/negative ()
  (should (eq (wikipedia--size-change-face -50) 'error)))

(ert-deftest size-change-face/zero ()
  (should (eq (wikipedia--size-change-face 0) 'default)))

;;; wikipedia--format-size-change

(ert-deftest format-size-change/positive ()
  (should (string-prefix-p "+" (wikipedia--format-size-change 100))))

(ert-deftest format-size-change/negative ()
  (should (string-prefix-p "-" (wikipedia--format-size-change -50))))

(ert-deftest format-size-change/zero ()
  (should (equal (substring-no-properties (wikipedia--format-size-change 0))
                 "+0")))

(ert-deftest format-size-change/nil ()
  (should (equal (wikipedia--format-size-change nil) "")))

(ert-deftest format-size-change/has-face-property ()
  "Formatted string carries a face property."
  (should (get-text-property 0 'face (wikipedia--format-size-change 100))))


;;;; ================================================================
;;;; 3. CRITICAL: URL utilities
;;;; ================================================================

(ert-deftest page-url/basic ()
  (wikipedia-test--with-site
    (let ((url (wikipedia--page-url "Test Article")))
      (should (string-match-p "en\\.wikipedia\\.org" url))
      (should (string-match-p "title=" url)))))

(ert-deftest page-url/encodes-spaces ()
  (wikipedia-test--with-site
    (should-not (string-match-p " " (wikipedia--page-url "Two Words")))))

(ert-deftest page-url/encodes-special-chars ()
  (wikipedia-test--with-site
    (let ((url (wikipedia--page-url "Test/Article&More")))
      (should-not (string-match-p "&More" url)))))

(ert-deftest revision-url/includes-oldid ()
  (wikipedia-test--with-site
    (should (string-match-p "oldid=12345"
             (wikipedia--revision-url "Test" 12345)))))

(ert-deftest user-page-url/includes-user-prefix ()
  (wikipedia-test--with-site
    (should (string-match-p "User:" (wikipedia--user-page-url "TestUser")))))

(ert-deftest build-revision-api-url/parameters ()
  "API URL includes required query parameters."
  (wikipedia-test--with-site
    (let ((url (wikipedia--build-revision-api-url "en" "Test Page" 12345)))
      (should (string-match-p "api\\.php" url))
      (should (string-match-p "action=query" url))
      (should (string-match-p "format=xml" url))
      (should (string-match-p "rvstartid=12345" url))
      (should (string-match-p "rvendid=12345" url)))))


;;;; ================================================================
;;;; 4. IMPORTANT: Cache tests
;;;; ================================================================

(ert-deftest cache/put-and-get ()
  (wikipedia-test--with-cache
    (wikipedia--cache-put 100 "content A")
    (should (equal (wikipedia--cache-get 100) "content A"))))

(ert-deftest cache/get-missing ()
  (wikipedia-test--with-cache
    (should (null (wikipedia--cache-get 999)))))

(ert-deftest cache/update-existing-no-duplicate-key ()
  "Updating an existing entry doesn't grow the key list."
  (wikipedia-test--with-cache
    (wikipedia--cache-put 100 "v1")
    (wikipedia--cache-put 100 "v2")
    (should (equal (wikipedia--cache-get 100) "v2"))
    (should (= (hash-table-count wikipedia--revision-cache) 1))
    (should (= (length wikipedia--revision-cache-keys) 1))))

(ert-deftest cache/eviction-triggers ()
  "Cache evicts when exceeding max size."
  (wikipedia-test--with-cache
    ;; max-size is 5 in fixture; insert 7 entries
    (dotimes (i 7)
      (wikipedia--cache-put (1+ i) (format "c%d" i)))
    ;; After eviction, count should be well below 7
    (should (< (hash-table-count wikipedia--revision-cache) 7))))

(ert-deftest cache/eviction-removes-oldest ()
  "Oldest entries (lowest revids) are evicted first."
  (wikipedia-test--with-cache
    (dotimes (i 7)
      (wikipedia--cache-put (1+ i) (format "c%d" i)))
    ;; Newest should survive
    (should (wikipedia--cache-get 7))
    (should (wikipedia--cache-get 6))))

(ert-deftest cache/keys-and-table-stay-in-sync ()
  "Key list length matches hash table count."
  (wikipedia-test--with-cache
    (dotimes (i 4)
      (wikipedia--cache-put (1+ i) (format "c%d" i)))
    (should (= (length wikipedia--revision-cache-keys)
               (hash-table-count wikipedia--revision-cache)))))


;;;; ================================================================
;;;; 5. CRITICAL: Draft encode/decode tests
;;;; ================================================================

(ert-deftest draft-encode/ascii ()
  "Safe ASCII passes through unchanged."
  (should (equal (wikipedia-draft--encode-title "Simple Title")
                 "Simple Title")))

(ert-deftest draft-encode/preserves-safe-chars ()
  "Dots, hyphens, spaces pass through."
  (should (equal (wikipedia-draft--encode-title "a-b.c d") "a-b.c d")))

(ert-deftest draft-encode/escapes-slash ()
  "Slash is escaped."
  (let ((encoded (wikipedia-draft--encode-title "Test/Article")))
    (should-not (string-match-p "/" encoded))
    (should (string-match-p "_" encoded))))

(ert-deftest draft-encode/escapes-underscore ()
  "Underscore itself is escaped (ensures _XX is unambiguous)."
  (should-not (equal (wikipedia-draft--encode-title "foo_bar") "foo_bar")))

(ert-deftest draft-encode/unicode ()
  "Unicode characters are escaped as UTF-8 byte sequences."
  (let ((encoded (wikipedia-draft--encode-title "日本語")))
    (should (string-match-p "_" encoded))
    (should-not (string-match-p "日" encoded))))

(ert-deftest draft-roundtrip/special-chars ()
  "Round-trip preserves special characters."
  (let ((title "Test/Article:With&Special<Chars>"))
    (should (equal (wikipedia-draft--decode-title
                    (wikipedia-draft--encode-title title))
                   title))))

(ert-deftest draft-roundtrip/unicode ()
  "Round-trip preserves Unicode."
  (let ((title "日本語の記事"))
    (should (equal (wikipedia-draft--decode-title
                    (wikipedia-draft--encode-title title))
                   title))))

(ert-deftest draft-roundtrip/mixed ()
  "Round-trip preserves mixed ASCII and Unicode."
  (let ((title "Article about café culture"))
    (should (equal (wikipedia-draft--decode-title
                    (wikipedia-draft--encode-title title))
                   title))))

(ert-deftest draft-roundtrip/empty ()
  "Round-trip preserves empty string."
  (should (equal (wikipedia-draft--decode-title
                  (wikipedia-draft--encode-title ""))
                 "")))

(ert-deftest draft-roundtrip/underscore ()
  "Round-trip preserves underscore."
  (should (equal (wikipedia-draft--decode-title
                  (wikipedia-draft--encode-title "foo_bar"))
                 "foo_bar")))

(ert-deftest draft-roundtrip/hex-lookalike ()
  "Title resembling _XX escape sequences round-trips correctly."
  (let ((title "item_FF_00"))
    (should (equal (wikipedia-draft--decode-title
                    (wikipedia-draft--encode-title title))
                   title))))

(ert-deftest draft-decode/trailing-underscore ()
  "Trailing underscore without hex digits is treated as literal."
  (should (equal (wikipedia-draft--decode-title "test_") "test_")))

(ert-deftest draft-decode/underscore-then-non-hex ()
  "Underscore followed by non-hex is treated as literal."
  (should (equal (wikipedia-draft--decode-title "a_GZ") "a_GZ")))

;;; Draft file path

(ert-deftest draft-file-for-title/structure ()
  "File path ends with .wiki inside draft directory."
  (let ((wikipedia-draft-directory "/tmp/test-drafts"))
    (let ((path (wikipedia-draft--file-for-title "Simple")))
      (should (string-suffix-p ".wiki" path))
      (should (string-prefix-p "/tmp/test-drafts/" path)))))

;;; Draft I/O

(ert-deftest draft-write-and-list ()
  "Written draft appears in the list."
  (wikipedia-test--with-draft-dir
    (wikipedia-draft--write "Test Page" "content")
    (should (member "Test Page" (wikipedia-draft--list)))))

(ert-deftest draft-write-read-content ()
  "Written content can be read back verbatim."
  (wikipedia-test--with-draft-dir
    (let ((content "== Section ==\n''italic'' and '''bold'''.\n"))
      (wikipedia-draft--write "Article" content)
      (let ((read-back (with-temp-buffer
                         (insert-file-contents
                          (wikipedia-draft--file-for-title "Article"))
                         (buffer-string))))
        (should (equal read-back content))))))

(ert-deftest draft-write-unicode-content ()
  "Draft preserves Unicode content."
  (wikipedia-test--with-draft-dir
    (let ((content "日本語 with emojis and accents"))
      (wikipedia-draft--write "日本語" content)
      (let ((coding-system-for-read 'utf-8)
            (read-back (with-temp-buffer
                         (insert-file-contents
                          (wikipedia-draft--file-for-title "日本語"))
                         (buffer-string))))
        (should (equal read-back content))))))

(ert-deftest draft-list/empty-directory ()
  (wikipedia-test--with-draft-dir
    (should (null (wikipedia-draft--list)))))

(ert-deftest draft-list/nonexistent-directory ()
  (let ((wikipedia-draft-directory "/tmp/nonexistent-wp-draft-xyz"))
    (should (null (wikipedia-draft--list)))))

(ert-deftest draft-list/multiple-drafts ()
  "Multiple drafts are all listed."
  (wikipedia-test--with-draft-dir
    (wikipedia-draft--write "Alpha" "a")
    (wikipedia-draft--write "Beta" "b")
    (wikipedia-draft--write "Gamma" "c")
    (let ((drafts (wikipedia-draft--list)))
      (should (= (length drafts) 3))
      (should (member "Alpha" drafts))
      (should (member "Beta" drafts))
      (should (member "Gamma" drafts)))))


;;;; ================================================================
;;;; 6. IMPORTANT: Watchlist logic tests
;;;; ================================================================

;;; wikipedia-watchlist--group-entries

(ert-deftest watchlist-group/basic ()
  "Group entries by title."
  (let* ((entries '(((title . "A") (revid . 1))
                    ((title . "B") (revid . 2))
                    ((title . "A") (revid . 3))))
         (groups (wikipedia-watchlist--group-entries entries)))
    (should (= (length groups) 2))
    (should (equal (caar groups) "A"))
    (should (= (length (cdar groups)) 2))
    (should (equal (caadr groups) "B"))
    (should (= (length (cdadr groups)) 1))))

(ert-deftest watchlist-group/empty ()
  (should (null (wikipedia-watchlist--group-entries nil))))

(ert-deftest watchlist-group/preserves-first-appearance-order ()
  (let* ((entries '(((title . "C") (revid . 1))
                    ((title . "A") (revid . 2))
                    ((title . "B") (revid . 3))
                    ((title . "A") (revid . 4))))
         (titles (mapcar #'car (wikipedia-watchlist--group-entries entries))))
    (should (equal titles '("C" "A" "B")))))

(ert-deftest watchlist-group/single-entry-per-title ()
  (let* ((entries '(((title . "X") (revid . 1))
                    ((title . "Y") (revid . 2))
                    ((title . "Z") (revid . 3))))
         (groups (wikipedia-watchlist--group-entries entries)))
    (should (= (length groups) 3))
    (dolist (g groups)
      (should (= (length (cdr g)) 1)))))

;;; wikipedia-watchlist--summarize-users

(ert-deftest watchlist-summarize-users/single ()
  (should (equal (wikipedia-watchlist--summarize-users '(((user . "Alice"))))
                 "Alice")))

(ert-deftest watchlist-summarize-users/repeated ()
  "Repeated user shows count."
  (let ((result (wikipedia-watchlist--summarize-users
                 '(((user . "Alice")) ((user . "Alice")) ((user . "Alice"))))))
    (should (string-match-p "Alice" result))
    (should (string-match-p "3×" result))))

(ert-deftest watchlist-summarize-users/multiple ()
  "Multiple users comma-separated."
  (let ((result (wikipedia-watchlist--summarize-users
                 '(((user . "Alice")) ((user . "Bob"))))))
    (should (string-match-p "Alice" result))
    (should (string-match-p "Bob" result))
    (should (string-match-p ", " result))))

(ert-deftest watchlist-summarize-users/nil-user ()
  "Entry with nil user is skipped."
  (let ((result (wikipedia-watchlist--summarize-users
                 '(((user . "Alice")) ((user . nil))))))
    (should (equal result "Alice"))))

;;; wikipedia-watchlist--total-size-change

(ert-deftest watchlist-total-size-change/basic ()
  (should (= (wikipedia-watchlist--total-size-change
              '(((oldlen . 100) (newlen . 150))
                ((oldlen . 150) (newlen . 120))))
             20)))

(ert-deftest watchlist-total-size-change/skips-missing ()
  (should (= (wikipedia-watchlist--total-size-change
              '(((oldlen . 100) (newlen . 200))
                ((oldlen . nil) (newlen . nil))))
             100)))

(ert-deftest watchlist-total-size-change/empty ()
  (should (= (wikipedia-watchlist--total-size-change nil) 0)))

(ert-deftest watchlist-total-size-change/negative ()
  "Deletions produce negative total."
  (should (= (wikipedia-watchlist--total-size-change
              '(((oldlen . 500) (newlen . 200))))
             -300)))

;;; wikipedia-watchlist--format-seconds-ago

(ert-deftest watchlist-format-seconds-ago/just-now ()
  (should (equal (wikipedia-watchlist--format-seconds-ago 0) "just now"))
  (should (equal (wikipedia-watchlist--format-seconds-ago 30) "just now"))
  (should (equal (wikipedia-watchlist--format-seconds-ago 59) "just now")))

(ert-deftest watchlist-format-seconds-ago/one-minute ()
  (should (equal (wikipedia-watchlist--format-seconds-ago 60) "1 minute ago"))
  (should (equal (wikipedia-watchlist--format-seconds-ago 119) "1 minute ago")))

(ert-deftest watchlist-format-seconds-ago/minutes-plural ()
  (should (equal (wikipedia-watchlist--format-seconds-ago 120) "2 minutes ago"))
  (should (equal (wikipedia-watchlist--format-seconds-ago 3599) "59 minutes ago")))

(ert-deftest watchlist-format-seconds-ago/one-hour ()
  (should (equal (wikipedia-watchlist--format-seconds-ago 3600) "1 hour ago")))

(ert-deftest watchlist-format-seconds-ago/hours-plural ()
  (should (equal (wikipedia-watchlist--format-seconds-ago 7200) "2 hours ago")))

(ert-deftest watchlist-format-seconds-ago/one-day ()
  (should (equal (wikipedia-watchlist--format-seconds-ago 86400) "1 day ago")))

(ert-deftest watchlist-format-seconds-ago/days-plural ()
  (should (equal (wikipedia-watchlist--format-seconds-ago 172800) "2 days ago")))

(ert-deftest watchlist-format-seconds-ago/one-week ()
  (should (equal (wikipedia-watchlist--format-seconds-ago 604800) "1 week ago")))

(ert-deftest watchlist-format-seconds-ago/weeks-plural ()
  (should (equal (wikipedia-watchlist--format-seconds-ago 1209600) "2 weeks ago")))

(ert-deftest watchlist-format-seconds-ago/months ()
  (should (string-match-p "month"
           (wikipedia-watchlist--format-seconds-ago 2592000))))

;;; Column width computation

(ert-deftest watchlist-compute-column-widths/basic ()
  "Compute max width of each column."
  (let ((entries `((id1 ,(vector ">" "3" "Short" "12:00" "User" "+10" "" "comment"))
                   (id2 ,(vector " " "" "A Longer Title" "13:00" "U" "-5" "" "")))))
    (let ((widths (wikipedia-watchlist--compute-column-widths entries)))
      ;; Column 2 (Page): "A Longer Title" = 14
      (should (= (nth 2 widths) 14))
      ;; Column 4 (User): "User" = 4
      (should (= (nth 4 widths) 4)))))

(ert-deftest watchlist-compute-column-widths/uses-string-width ()
  "Column widths use display width, not character count."
  ;; CJK character "中" has string-width 2 but length 1
  (let* ((entries (list (list 'id1 (vector "" "" "中" "" "" "" "" "")))))
    (let ((widths (wikipedia-watchlist--compute-column-widths entries)))
      (should (= (nth 2 widths) 2)))))


;;;; ================================================================
;;;; 7. IMPORTANT: History annotation tests
;;;; ================================================================

(ert-deftest history-annotate-diffs/basic ()
  "Compute size diffs between consecutive revisions."
  (let ((revisions '(((revid . 3) (size . 300))
                     ((revid . 2) (size . 250))
                     ((revid . 1) (size . 100)))))
    (let ((annotated (wikipedia-history--annotate-diffs revisions)))
      ;; Oldest (revid 1) has no previous -> nil diff
      (should (null (alist-get 'sizediff (nth 2 annotated))))
      ;; revid 2: 250 - 100 = 150
      (should (= (alist-get 'sizediff (nth 1 annotated)) 150))
      ;; revid 3: 300 - 250 = 50
      (should (= (alist-get 'sizediff (nth 0 annotated)) 50)))))

(ert-deftest history-annotate-diffs/empty ()
  (should (null (wikipedia-history--annotate-diffs nil))))

(ert-deftest history-annotate-diffs/single ()
  "Single revision has nil diff."
  (let ((annotated (wikipedia-history--annotate-diffs
                    '(((revid . 1) (size . 500))))))
    (should (= (length annotated) 1))
    (should (null (alist-get 'sizediff (car annotated))))))

(ert-deftest history-annotate-diffs/missing-size ()
  "Missing size fields produce nil diffs."
  (let ((annotated (wikipedia-history--annotate-diffs
                    '(((revid . 2) (size . nil))
                      ((revid . 1) (size . 100))))))
    ;; revid 1: no prev -> nil
    (should (null (alist-get 'sizediff (nth 1 annotated))))
    ;; revid 2: size is nil -> nil diff
    (should (null (alist-get 'sizediff (nth 0 annotated))))))

(ert-deftest history-annotate-diffs/preserves-original-fields ()
  "Annotation preserves all original revision fields."
  (let ((annotated (wikipedia-history--annotate-diffs
                    '(((revid . 1) (user . "Alice") (size . 200))))))
    (should (equal (alist-get 'user (car annotated)) "Alice"))
    (should (= (alist-get 'revid (car annotated)) 1))))

(ert-deftest history-annotate-diffs/negative-growth ()
  "Deletions produce negative diffs."
  (let ((annotated (wikipedia-history--annotate-diffs
                    '(((revid . 2) (size . 100))
                      ((revid . 1) (size . 500))))))
    (should (= (alist-get 'sizediff (nth 0 annotated)) -400))))


;;;; ================================================================
;;;; 8. IMPORTANT: XTools helper tests
;;;; ================================================================

(ert-deftest xtools-format-groups/vector ()
  (should (equal (wikipedia-xtools--format-groups ["sysop" "bureaucrat"])
                 "sysop, bureaucrat")))

(ert-deftest xtools-format-groups/list ()
  (should (equal (wikipedia-xtools--format-groups '("autoconfirmed" "editor"))
                 "autoconfirmed, editor")))

(ert-deftest xtools-format-groups/string ()
  (should (equal (wikipedia-xtools--format-groups "sysop") "sysop")))

(ert-deftest xtools-format-groups/symbols ()
  (should (string-match-p "sysop"
           (wikipedia-xtools--format-groups '[sysop editor]))))

(ert-deftest xtools-is-admin/sysop ()
  (should (wikipedia-xtools--is-admin ["sysop" "autoconfirmed"])))

(ert-deftest xtools-is-admin/bureaucrat ()
  (should (wikipedia-xtools--is-admin '("bureaucrat"))))

(ert-deftest xtools-is-admin/not ()
  (should-not (wikipedia-xtools--is-admin ["autoconfirmed" "editor"])))

(ert-deftest xtools-is-admin/nil ()
  (should-not (wikipedia-xtools--is-admin nil)))

(ert-deftest xtools-is-admin/empty-vector ()
  (should-not (wikipedia-xtools--is-admin [])))


;;;; ================================================================
;;;; 9. IMPORTANT: Mirror formatting tests
;;;; ================================================================

(ert-deftest mirror-format-entry/basic ()
  "Format a watched page with revisions."
  (let* ((row '(1 "Test Article" 1704067200 1 5))
         (entry (wikipedia-mirror--format-entry row))
         (cols (cadr entry)))
    (should (= (car entry) 1))
    (should (equal (aref cols 0) "Test Article"))
    (should (equal (aref cols 1) "5"))
    (should (equal (aref cols 3) "Yes"))))

(ert-deftest mirror-format-entry/unwatched ()
  (let* ((row '(1 "Article" nil 0 3))
         (cols (cadr (wikipedia-mirror--format-entry row))))
    (should (equal (aref cols 2) "Never"))
    (should (equal (aref cols 3) "No"))))

(ert-deftest mirror-format-entry/zero-revisions ()
  (let* ((row '(1 "Empty" nil 0 0))
         (cols (cadr (wikipedia-mirror--format-entry row))))
    (should (equal (aref cols 1) "0"))))


;;;; ================================================================
;;;; 10. USEFUL: Database tests (conditional on SQLite support)
;;;; ================================================================

(when wikipedia-test--sqlite-available
  (defmacro wikipedia-test--with-db (&rest body)
    "Execute BODY with a temporary database."
    (declare (indent 0))
    `(let* ((tmpfile (make-temp-file "wp-test-db-" nil ".db"))
            (wikipedia-db-file tmpfile)
            (wikipedia-db--connection nil))
       (unwind-protect
           (progn ,@body)
         (when wikipedia-db--connection
           (wikipedia-db-close))
         (delete-file tmpfile))))

  (ert-deftest db/insert-and-get-page ()
    (wikipedia-test--with-db
      (let ((page-id (wikipedia-db-insert-page "Test Article")))
        (should page-id)
        (let ((page (wikipedia-db-get-page "Test Article")))
          (should page)
          (should (equal (nth 1 page) "Test Article"))))))

  (ert-deftest db/insert-page-idempotent ()
    (wikipedia-test--with-db
      (should (= (wikipedia-db-insert-page "Article")
                 (wikipedia-db-insert-page "Article")))))

  (ert-deftest db/get-page-not-found ()
    (wikipedia-test--with-db
      (should (null (wikipedia-db-get-page "Nonexistent")))))

  (ert-deftest db/insert-and-get-revision ()
    (wikipedia-test--with-db
      (let* ((pid (wikipedia-db-insert-page "Test"))
             (rid (wikipedia-db-insert-revision
                   pid 12345 12344 "User" "2024-01-01T00:00:00Z" "comment" 500)))
        (should rid)
        (let ((revisions (wikipedia-db-get-revisions pid)))
          (should (= (length revisions) 1))
          (should (= (nth 1 (car revisions)) 12345))))))

  (ert-deftest db/insert-revision-idempotent ()
    (wikipedia-test--with-db
      (let ((pid (wikipedia-db-insert-page "Test")))
        (wikipedia-db-insert-revision pid 100 nil "User" nil nil nil)
        (wikipedia-db-insert-revision pid 100 nil "User" nil nil nil)
        (should (= (length (wikipedia-db-get-revisions pid)) 1)))))

  (ert-deftest db/insert-and-get-content ()
    (wikipedia-test--with-db
      (let* ((pid (wikipedia-db-insert-page "Test"))
             (rid (wikipedia-db-insert-revision pid 100 nil nil nil nil nil)))
        (wikipedia-db-insert-content rid "== Hello ==\nWorld")
        (should (equal (wikipedia-db-get-content rid) "== Hello ==\nWorld")))))

  (ert-deftest db/has-content-p ()
    (wikipedia-test--with-db
      (let* ((pid (wikipedia-db-insert-page "Test"))
             (rid (wikipedia-db-insert-revision pid 100 nil nil nil nil nil)))
        (should-not (wikipedia-db-has-content-p rid))
        (wikipedia-db-insert-content rid "content")
        (should (wikipedia-db-has-content-p rid)))))

  (ert-deftest db/set-and-get-watched ()
    (wikipedia-test--with-db
      (wikipedia-db-set-watched "A" t)
      (wikipedia-db-set-watched "B" nil)
      (wikipedia-db-set-watched "C" t)
      (let* ((watched (wikipedia-db-get-watched-pages))
             (titles (mapcar #'cadr watched)))
        (should (= (length watched) 2))
        (should (member "A" titles))
        (should (member "C" titles)))))

  (ert-deftest db/get-latest-revision ()
    (wikipedia-test--with-db
      (let ((pid (wikipedia-db-insert-page "Test")))
        (wikipedia-db-insert-revision pid 100 nil "A" nil nil nil)
        (wikipedia-db-insert-revision pid 200 100 "B" nil nil nil)
        (wikipedia-db-insert-revision pid 150 100 "C" nil nil nil)
        (should (= (nth 1 (wikipedia-db-get-latest-revision pid)) 200)))))

  (ert-deftest db/get-revision-by-revid ()
    (wikipedia-test--with-db
      (let ((pid (wikipedia-db-insert-page "My Page")))
        (wikipedia-db-insert-revision pid 999 nil "Author" "2024-01-01" "test" 100)
        (let ((row (wikipedia-db-get-revision-by-revid 999)))
          (should row)
          (should (= (nth 2 row) 999))
          (should (equal (nth 8 row) "My Page"))))))

  (ert-deftest db/update-page-synced ()
    (wikipedia-test--with-db
      (let ((pid (wikipedia-db-insert-page "Test")))
        (wikipedia-db-update-page-synced pid)
        (should (numberp (nth 3 (wikipedia-db-get-page "Test")))))))

  (ert-deftest db/multiple-sites ()
    "Same title from different sites creates separate records."
    (wikipedia-test--with-db
      (wikipedia-db-insert-page "Test" "en.wikipedia.org")
      (wikipedia-db-insert-page "Test" "es.wikipedia.org")
      (should (wikipedia-db-get-page "Test" "en.wikipedia.org"))
      (should (wikipedia-db-get-page "Test" "es.wikipedia.org"))
      (should-not (= (car (wikipedia-db-get-page "Test" "en.wikipedia.org"))
                     (car (wikipedia-db-get-page "Test" "es.wikipedia.org"))))))

  (ert-deftest db/stats ()
    (wikipedia-test--with-db
      (let* ((pid (wikipedia-db-insert-page "Test"))
             (rid (wikipedia-db-insert-revision pid 100 nil nil nil nil nil)))
        (wikipedia-db-set-watched "Test" t)
        (wikipedia-db-insert-content rid "hello")
        (let ((stats (wikipedia-db-stats)))
          (should (= (plist-get stats :pages) 1))
          (should (= (plist-get stats :watched) 1))
          (should (= (plist-get stats :revisions) 1))
          (should (= (plist-get stats :content) 1))))))

  (ert-deftest db/revisions-ordered-by-revid-desc ()
    "Revisions are returned newest first."
    (wikipedia-test--with-db
      (let ((pid (wikipedia-db-insert-page "Test")))
        (wikipedia-db-insert-revision pid 10 nil nil nil nil nil)
        (wikipedia-db-insert-revision pid 30 nil nil nil nil nil)
        (wikipedia-db-insert-revision pid 20 nil nil nil nil nil)
        (let ((revs (wikipedia-db-get-revisions pid)))
          (should (= (nth 1 (nth 0 revs)) 30))
          (should (= (nth 1 (nth 1 revs)) 20))
          (should (= (nth 1 (nth 2 revs)) 10))))))

  (ert-deftest db/content-upsert ()
    "Inserting content for same revision replaces it."
    (wikipedia-test--with-db
      (let* ((pid (wikipedia-db-insert-page "Test"))
             (rid (wikipedia-db-insert-revision pid 100 nil nil nil nil nil)))
        (wikipedia-db-insert-content rid "version 1")
        (wikipedia-db-insert-content rid "version 2")
        (should (equal (wikipedia-db-get-content rid) "version 2")))))

  (ert-deftest db/insert-content-nil-is-noop ()
    "Inserting nil content does nothing."
    (wikipedia-test--with-db
      (let* ((pid (wikipedia-db-insert-page "Test"))
             (rid (wikipedia-db-insert-revision pid 100 nil nil nil nil nil)))
        (wikipedia-db-insert-content rid nil)
        (should-not (wikipedia-db-has-content-p rid))))))


;;;; ================================================================
;;;; 11. USEFUL: Diff utility tests
;;;; ================================================================

;;; wikipedia--write-temp-file

(ert-deftest write-temp-file/basic ()
  "Write content to temp file and verify."
  (let ((file (wikipedia--write-temp-file "hello world" 42)))
    (unwind-protect
        (progn
          (should (file-exists-p file))
          (should (string-match-p "wp-rev-42" file))
          (should (equal (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         "hello world")))
      (delete-file file))))

(ert-deftest write-temp-file/nil-content ()
  "Nil content signals an error."
  (should-error (wikipedia--write-temp-file nil 1) :type 'error))

(ert-deftest write-temp-file/unicode ()
  "Unicode content is preserved."
  (let ((file (wikipedia--write-temp-file "日本語テスト" 99)))
    (unwind-protect
        (let ((coding-system-for-read 'utf-8))
          (should (equal (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         "日本語テスト")))
      (delete-file file))))

;;; wikipedia--generate-unified-diff

(ert-deftest generate-unified-diff/identical-files ()
  "Identical content produces empty diff."
  (let ((f1 (wikipedia--write-temp-file "same content" 1))
        (f2 (wikipedia--write-temp-file "same content" 2)))
    (unwind-protect
        (should (equal (wikipedia--generate-unified-diff f1 f2 1 2) ""))
      (delete-file f1)
      (delete-file f2))))

(ert-deftest generate-unified-diff/different-files ()
  "Different content produces non-empty diff with revision labels."
  (let ((f1 (wikipedia--write-temp-file "line one\n" 10))
        (f2 (wikipedia--write-temp-file "line one\nline two\n" 11)))
    (unwind-protect
        (let ((diff (wikipedia--generate-unified-diff f1 f2 10 11)))
          (should (> (length diff) 0))
          (should (string-match-p "Revision 10" diff))
          (should (string-match-p "Revision 11" diff))
          (should (string-match-p "\\+line two" diff)))
      (delete-file f1)
      (delete-file f2))))

(ert-deftest generate-unified-diff/deletion ()
  "Removing lines shows - prefix."
  (let ((f1 (wikipedia--write-temp-file "keep\nremove\n" 1))
        (f2 (wikipedia--write-temp-file "keep\n" 2)))
    (unwind-protect
        (let ((diff (wikipedia--generate-unified-diff f1 f2 1 2)))
          (should (string-match-p "-remove" diff)))
      (delete-file f1)
      (delete-file f2))))


;;;; ================================================================
;;;; 12. USEFUL: Buffer creation tests
;;;; ================================================================

(ert-deftest create-revision-buffer/basic ()
  "Create a read-only buffer with content."
  (let ((buf (wikipedia--create-revision-buffer "Test" 42 "== Section ==")))
    (unwind-protect
        (with-current-buffer buf
          (should (string-match-p "WP Rev 42" (buffer-name)))
          (should buffer-read-only)
          (should (equal (buffer-substring-no-properties (point-min) (point-max))
                         "== Section ==")))
      (kill-buffer buf))))

(ert-deftest create-revision-buffer/nil-content ()
  "Nil content produces empty buffer."
  (let ((buf (wikipedia--create-revision-buffer "Test" 1 nil)))
    (unwind-protect
        (with-current-buffer buf
          (should (equal (buffer-substring-no-properties (point-min) (point-max))
                         "")))
      (kill-buffer buf))))

(ert-deftest create-revision-buffer/point-at-beginning ()
  "Point is at beginning of buffer."
  (let ((buf (wikipedia--create-revision-buffer "T" 1 "content")))
    (unwind-protect
        (with-current-buffer buf
          (should (= (point) (point-min))))
      (kill-buffer buf))))


;;;; ================================================================
;;;; 13. USEFUL: Tabulated list entry formatting
;;;; ================================================================

;;; wikipedia-history--make-entry

(ert-deftest history-make-entry/basic ()
  "Format a history entry with all fields."
  (let* ((rev '((revid . 12345)
                (timestamp . "2024-06-15T08:30:00Z")
                (user . "Editor")
                (sizediff . 42)
                (comment . "Added intro")
                (minor . nil)))
         (entry (wikipedia-history--make-entry rev))
         (cols (cadr entry)))
    (should (= (car entry) 12345))
    (should (equal (aref cols 0) "12345"))
    (should (equal (aref cols 2) "Editor"))
    ;; Comment should not have "m " prefix since minor is nil
    (should (equal (aref cols 4) "Added intro"))))

(ert-deftest history-make-entry/minor-edit ()
  "Minor edit comment has 'm ' prefix."
  (let* ((rev '((revid . 1) (timestamp . "2024-01-01T00:00:00Z")
                (user . "U") (sizediff . 0) (comment . "Fix")
                (minor . (minor))))
         (entry (wikipedia-history--make-entry rev))
         (cols (cadr entry)))
    (should (string-prefix-p "m " (aref cols 4)))))

(ert-deftest history-make-entry/nil-user ()
  "Nil user shows empty string."
  (let* ((rev '((revid . 1) (timestamp . "2024-01-01T00:00:00Z")
                (user . nil) (sizediff . nil) (comment . nil) (minor . nil)))
         (cols (cadr (wikipedia-history--make-entry rev))))
    (should (equal (aref cols 2) ""))))

;;; wikipedia-user--make-contrib-entry

(ert-deftest user-make-contrib-entry/basic ()
  "Format a user contribution entry."
  (let* ((contrib '((revid . 555)
                    (title . "Some Article")
                    (timestamp . "2024-03-15T12:00:00Z")
                    (comment . "Expanded")
                    (sizediff . 200)))
         (entry (wikipedia-user--make-contrib-entry contrib))
         (cols (cadr entry)))
    (should (= (car entry) 555))
    (should (equal (aref cols 1) "Some Article"))
    (should (equal (aref cols 3) "Expanded"))))

(ert-deftest user-make-contrib-entry/nil-fields ()
  "Nil fields produce empty strings."
  (let* ((contrib '((revid . 1) (title . nil) (timestamp . nil)
                    (comment . nil) (sizediff . nil)))
         (cols (cadr (wikipedia-user--make-contrib-entry contrib))))
    (should (equal (aref cols 1) ""))
    (should (equal (aref cols 3) ""))))

;;; wikipedia-mirror-history--format-entry

(ert-deftest mirror-history-format-entry/basic ()
  "Format a local history revision row."
  (let* ((row '(1 42 41 "Editor" "2024-01-15T10:30:00Z" "Fixed typo" 5000))
         (entry (wikipedia-mirror-history--format-entry row))
         (cols (cadr entry)))
    (should (= (car entry) 1))
    (should (equal (aref cols 0) "42"))
    (should (equal (aref cols 2) "Editor"))
    (should (equal (aref cols 3) "5000"))
    (should (equal (aref cols 4) "Fixed typo"))))

(ert-deftest mirror-history-format-entry/nil-fields ()
  "Nil fields produce empty strings."
  (let* ((row '(1 100 nil nil nil nil nil))
         (cols (cadr (wikipedia-mirror-history--format-entry row))))
    (should (equal (aref cols 2) ""))
    (should (equal (aref cols 3) ""))
    (should (equal (aref cols 4) ""))))


;;;; ================================================================
;;;; 14. USEFUL: Watchlist display helper tests
;;;; ================================================================

;;; wikipedia-watchlist--apply-unread-face

(ert-deftest watchlist-apply-unread-face/unread ()
  "Unread rows get bold face applied to string cells."
  (let* ((row (vector ">" "Title" "12:00" "User" "+10" "comment"))
         (result (wikipedia-watchlist--apply-unread-face row t)))
    ;; Each string cell should have the unread face
    (should (eq (get-text-property 0 'face (aref result 1))
                'wikipedia-watchlist-unread))))

(ert-deftest watchlist-apply-unread-face/read ()
  "Read rows are returned unchanged."
  (let* ((row (vector ">" "Title" "12:00" "User" "+10" "comment"))
         (result (wikipedia-watchlist--apply-unread-face row nil)))
    (should (eq result row))))

;;; wikipedia-watchlist--build-format

(ert-deftest watchlist-build-format/basic ()
  "Build column format vector."
  (let ((result (wikipedia-watchlist--build-format
                 '(2 3 40 20 20 8 6) '(1 1 30 15 10 6 4))))
    (should (vectorp result))
    (should (= (length result) 8))
    ;; Each column width should be min of max and actual
    (should (= (nth 1 (aref result 2)) 30))   ; Page: min(40,30)=30
    (should (= (nth 1 (aref result 4)) 10))   ; User: min(20,10)=10
    ;; Last column (Summary) always has width 0
    (should (= (nth 1 (aref result 7)) 0))))

(ert-deftest watchlist-build-format/actual-exceeds-max ()
  "When actual width exceeds max, max is used."
  (let ((result (wikipedia-watchlist--build-format
                 '(2 3 40 20 20 8 6) '(3 2 50 25 25 12 8))))
    (should (= (nth 1 (aref result 2)) 40))   ; min(40,50)=40
    (should (= (nth 1 (aref result 5)) 8))))  ; min(8,12)=8

;;; wikipedia-watchlist--group-has-unread-p

(ert-deftest watchlist-group-has-unread-p/all-unread ()
  "Group with no read entries is unread."
  (let ((wikipedia-watchlist--read (make-hash-table :test 'eql))
        (entries '(((revid . 1)) ((revid . 2)))))
    (should (wikipedia-watchlist--group-has-unread-p entries))))

(ert-deftest watchlist-group-has-unread-p/all-read ()
  "Group with all read entries is read."
  (let ((wikipedia-watchlist--read (make-hash-table :test 'eql))
        (entries '(((revid . 1)) ((revid . 2)))))
    (puthash 1 t wikipedia-watchlist--read)
    (puthash 2 t wikipedia-watchlist--read)
    (should-not (wikipedia-watchlist--group-has-unread-p entries))))

(ert-deftest watchlist-group-has-unread-p/partial ()
  "Group with some unread entries is unread."
  (let ((wikipedia-watchlist--read (make-hash-table :test 'eql))
        (entries '(((revid . 1)) ((revid . 2)))))
    (puthash 1 t wikipedia-watchlist--read)
    (should (wikipedia-watchlist--group-has-unread-p entries))))


;;;; ================================================================
;;;; 15. USEFUL: Render unified diff (integration test)
;;;; ================================================================

(ert-deftest render-unified-diff/produces-diff-mode-buffer ()
  "Render diff creates a read-only diff-mode buffer."
  (let ((buf (wikipedia--render-unified-diff
              "old content\n" "new content\n"
              100 101 "*test-diff*")))
    (unwind-protect
        (with-current-buffer buf
          (should buffer-read-only)
          (should (derived-mode-p 'diff-mode))
          (should (string-match-p "Revision 100" (buffer-string)))
          (should (string-match-p "Revision 101" (buffer-string))))
      (kill-buffer buf))))

(ert-deftest render-unified-diff/identical-content ()
  "Identical content produces an empty-ish diff buffer."
  (let ((buf (wikipedia--render-unified-diff
              "same\n" "same\n" 1 2 "*test-diff-empty*")))
    (unwind-protect
        (with-current-buffer buf
          ;; No +/- lines since content is the same
          (should-not (string-match-p "^[-+]same" (buffer-string))))
      (kill-buffer buf))))


;;;; ================================================================
;;;; 16. USEFUL: Prefetch queue logic tests
;;;; ================================================================

(ert-deftest prefetch-watchlist-diffs/builds-queue ()
  "Prefetch populates queue with uncached revisions."
  (wikipedia-test--with-cache
    ;; Mock the drain function to not actually fetch
    (cl-letf (((symbol-function 'wikipedia--prefetch-drain-queue) #'ignore))
      (let ((entries '(((title . "A") (revid . 10) (old_revid . 9))
                       ((title . "B") (revid . 20) (old_revid . 19)))))
        (wikipedia--prefetch-watchlist-diffs entries)
        ;; Should have 4 items: revid 9, 10, 19, 20
        (should (= (length wikipedia--prefetch-queue) 4))))))

(ert-deftest prefetch-watchlist-diffs/skips-cached ()
  "Already-cached revisions are not queued."
  (wikipedia-test--with-cache
    (cl-letf (((symbol-function 'wikipedia--prefetch-drain-queue) #'ignore))
      ;; Pre-cache revision 10
      (wikipedia--cache-put 10 "cached content")
      (let ((entries '(((title . "A") (revid . 10) (old_revid . 9)))))
        (wikipedia--prefetch-watchlist-diffs entries)
        ;; Should only queue revision 9 (10 is cached)
        (should (= (length wikipedia--prefetch-queue) 1))
        (should (= (cdr (car wikipedia--prefetch-queue)) 9))))))

(ert-deftest prefetch-watchlist-diffs/skips-nil-revids ()
  "Entries with nil revids are skipped entirely."
  (wikipedia-test--with-cache
    (cl-letf (((symbol-function 'wikipedia--prefetch-drain-queue) #'ignore))
      (let ((entries '(((title . "A") (revid . nil) (old_revid . nil)))))
        (wikipedia--prefetch-watchlist-diffs entries)
        (should (= (length wikipedia--prefetch-queue) 0))))))


(provide 'wikipedia-test)

;;; wikipedia-test.el ends here
