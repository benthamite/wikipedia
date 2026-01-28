;;; wikipedia-adapter.el --- Adapter layer for mediawiki.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This module provides an adapter layer that isolates the dependency on
;; mediawiki.el.  All interactions with mediawiki.el should go through
;; this module, allowing the underlying implementation to be replaced
;; if needed.
;;
;; The adapter provides:
;;   `wp--login' - Establish a session with a wiki site
;;   `wp--api-call' - Make API requests to the wiki
;;   `wp--open-page-buffer' - Open a page in a buffer for editing
;;   `wp--save-page-buffer' - Save the current buffer to the wiki

;;; Code:

(require 'mediawiki)

(defvar wp--current-site nil
  "The currently active wiki site name.")

(defun wp--login (site)
  "Establish a session with SITE.
SITE should be a site name configured in `mediawiki-site-alist'."
  (mediawiki-site site)
  (setq wp--current-site site))

(defun wp--api-call (action params)
  "Make an API call with ACTION and PARAMS.
ACTION is the MediaWiki API action (e.g., \"query\").
PARAMS is an alist of additional parameters.
Returns the parsed response.  Note that mediawiki.el uses XML format
internally, so the response is an XML-derived structure, not JSON."
  (let ((site (wp--get-site)))
    (mediawiki-api-call site action params)))

(defun wp--get-site ()
  "Return the current site, signaling an error if none is active."
  (or wp--current-site
      (bound-and-true-p mediawiki-site)
      (error "No active wiki session; use `wikipedia-login' first")))

(defun wp--open-page-buffer (title)
  "Open TITLE for editing in a buffer.
Returns the buffer containing the page content."
  (let ((mediawiki-site (wp--get-site)))
    (mediawiki-open title)))

(defun wp--save-page-buffer (&optional summary)
  "Save the current buffer to the wiki.
SUMMARY is the edit summary.  If nil, the user will be prompted."
  (mediawiki-save summary))

(defun wp--current-page-title ()
  "Return the title of the page in the current buffer, or nil."
  (when (bound-and-true-p mediawiki-page-title)
    mediawiki-page-title))

(defun wp--preview (title wikitext)
  "Preview WIKITEXT as if it were the content of TITLE.
Returns the parsed HTML as a string."
  (let* ((site (wp--get-site))
         (result (mediawiki-api-call
                  site "parse"
                  (list (cons "title" title)
                        (cons "text" wikitext)
                        (cons "prop" "text")
                        (cons "disableeditsection" "1")
                        (cons "preview" "1"))))
         (text-element (assq 'text (cddr result))))
    (wp--extract-preview-html text-element)))

(defun wp--extract-preview-html (text-element)
  "Extract HTML string from TEXT-ELEMENT returned by parse API."
  (when text-element
    (let ((content (cddr text-element)))
      (if (stringp (car content))
          (car content)
        (car (last text-element))))))

(defun wp--ensure-logged-in ()
  "Ensure we have an active session, prompting for login if needed."
  (unless (or wp--current-site (bound-and-true-p mediawiki-site))
    (call-interactively #'mediawiki-site))
  (when (bound-and-true-p mediawiki-site)
    (setq wp--current-site mediawiki-site)))

(defun wp--get-page-history (title &optional limit)
  "Fetch revision history for TITLE.
LIMIT is the maximum number of revisions to fetch (default 50).
Returns a list of revision alists with keys: revid, parentid,
timestamp, user, comment, size, minor."
  (let* ((site (wp--get-site))
         (result (mediawiki-api-call
                  site "query"
                  (list (cons "titles" title)
                        (cons "prop" "revisions")
                        (cons "rvprop" "ids|timestamp|user|comment|size|flags")
                        (cons "rvlimit" (number-to-string (or limit 50))))))
         (pages (cddr (assq 'pages (cddr result))))
         (page (car pages))
         (revisions (cddr (assq 'revisions (cddr page)))))
    (mapcar #'wp--parse-revision revisions)))

(defun wp--parse-revision (rev)
  "Parse a revision element REV into an alist."
  (let ((attrs (cadr rev)))
    `((revid . ,(wp--parse-number (cdr (assq 'revid attrs))))
      (parentid . ,(wp--parse-number (cdr (assq 'parentid attrs))))
      (timestamp . ,(cdr (assq 'timestamp attrs)))
      (user . ,(cdr (assq 'user attrs)))
      (comment . ,(or (cdr (assq 'comment attrs)) ""))
      (size . ,(wp--parse-number (cdr (assq 'size attrs))))
      (minor . ,(assq 'minor attrs)))))

(defun wp--parse-number (value)
  "Parse VALUE as a number.
If VALUE is already a number, return it.  If it's a string, convert it.
If it's nil or empty, return nil."
  (cond
   ((numberp value) value)
   ((and (stringp value) (not (string-empty-p value)))
    (string-to-number value))
   (t nil)))

(defun wp--get-revision-content (title revid)
  "Fetch the wikitext content of TITLE at revision REVID."
  (let* ((site (wp--get-site))
         (result (mediawiki-api-call
                  site "query"
                  (list (cons "titles" title)
                        (cons "prop" "revisions")
                        (cons "rvprop" "content")
                        (cons "rvslots" "main")
                        (cons "rvstartid" (number-to-string revid))
                        (cons "rvendid" (number-to-string revid)))))
         (pages (cddr (assq 'pages (cddr result))))
         (page (car pages))
         (revisions (cddr (assq 'revisions (cddr page))))
         (rev (car revisions)))
    (wp--extract-revision-content rev)))

(defun wp--extract-revision-content (rev)
  "Extract wikitext content from revision element REV."
  (when rev
    (let* ((slots (assq 'slots (cddr rev)))
           (slot (assq 'slot (cddr slots))))
      (when slot
        (let ((content (car (last slot))))
          (if (stringp content)
              content
            nil))))))

(defun wp--compare-revisions (from-rev to-rev)
  "Get diff between FROM-REV and TO-REV revision IDs.
Returns the diff HTML as a string."
  (let* ((site (wp--get-site))
         (result (mediawiki-api-call
                  site "compare"
                  (list (cons "fromrev" (number-to-string from-rev))
                        (cons "torev" (number-to-string to-rev)))))
         (diff-body (cddr result)))
    (wp--extract-diff-content diff-body)))

(defun wp--extract-diff-content (diff-body)
  "Extract diff content from DIFF-BODY."
  (when diff-body
    (let ((body-element (assq 'body diff-body)))
      (if body-element
          (or (cdr body-element)
              (car (last body-element)))
        (car (last diff-body))))))

(defun wp--get-watchlist (&optional limit days)
  "Fetch the user's watchlist.
LIMIT is the maximum number of entries to fetch (default 50).
DAYS is how many days back to fetch (default 30).
Returns a list of watchlist entry alists.
By default, shows only unseen changes, excludes bot edits and minor edits,
and only includes page edits and page creations (matching the Wikipedia UI defaults)."
  (let* ((site (wp--get-site))
         (end-time (wp--format-timestamp-for-api (- (float-time) (* (or days 30) 86400))))
         (result (mediawiki-api-call
                  site "query"
                  (list (cons "list" "watchlist")
                        (cons "wlprop" "ids|title|timestamp|user|comment|sizes")
                        (cons "wlallrev" "1")
                        (cons "wlshow" "unread|!bot|!minor")
                        (cons "wltype" "edit|new")
                        (cons "wlend" end-time)
                        (cons "wllimit" (number-to-string (or limit 50))))))
         (watchlist (cddr (assq 'watchlist (cddr result)))))
    (mapcar #'wp--parse-watchlist-entry watchlist)))

(defun wp--parse-watchlist-entry (entry)
  "Parse a watchlist ENTRY element into an alist."
  (let ((attrs (cadr entry)))
    `((title . ,(cdr (assq 'title attrs)))
      (revid . ,(wp--parse-number (cdr (assq 'revid attrs))))
      (old_revid . ,(wp--parse-number (cdr (assq 'old_revid attrs))))
      (timestamp . ,(cdr (assq 'timestamp attrs)))
      (user . ,(cdr (assq 'user attrs)))
      (comment . ,(or (cdr (assq 'comment attrs)) ""))
      (oldlen . ,(wp--parse-number (cdr (assq 'oldlen attrs))))
      (newlen . ,(wp--parse-number (cdr (assq 'newlen attrs)))))))

(defun wp--thank-revision (revid)
  "Send a thank notification for revision REVID.
Returns non-nil on success."
  (let* ((site (wp--get-site))
         (token (wp--get-csrf-token site)))
    (mediawiki-api-call
     site "thank"
     (list (cons "rev" (number-to-string revid))
           (cons "token" token)))
    t))

(defun wp--get-csrf-token (site)
  "Get a CSRF token for SITE."
  (let* ((result (mediawiki-api-call
                  site "query"
                  (list (cons "meta" "tokens")
                        (cons "type" "csrf"))))
         (tokens (assq 'tokens (cddr (assq 'query (cddr result))))))
    (cdr (assq 'csrftoken (cadr tokens)))))

(defun wp--unwatch-page (title)
  "Remove TITLE from the user's watchlist.
Returns non-nil on success."
  (let* ((site (wp--get-site))
         (token (wp--get-csrf-token site)))
    (mediawiki-api-call
     site "watch"
     (list (cons "titles" title)
           (cons "unwatch" "1")
           (cons "token" token)))
    t))

(defun wp--get-user-contributions (username &optional limit)
  "Fetch contributions for USERNAME.
LIMIT is the maximum number of contributions to fetch (default 50).
Returns a list of contribution alists."
  (let* ((site (wp--get-site))
         (result (mediawiki-api-call
                  site "query"
                  (list (cons "list" "usercontribs")
                        (cons "ucuser" username)
                        (cons "ucprop" "ids|title|timestamp|comment|sizediff|flags")
                        (cons "uclimit" (number-to-string (or limit 50))))))
         (contribs (cddr (assq 'usercontribs (cddr result)))))
    (mapcar #'wp--parse-contrib-entry contribs)))

(defun wp--format-timestamp-for-api (time)
  "Format TIME (as seconds since epoch) for the MediaWiki API."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" (seconds-to-time time) t))

(defun wp--parse-contrib-entry (entry)
  "Parse a user contribution ENTRY element into an alist."
  (let ((attrs (cadr entry)))
    `((revid . ,(wp--parse-number (cdr (assq 'revid attrs))))
      (parentid . ,(wp--parse-number (cdr (assq 'parentid attrs))))
      (title . ,(cdr (assq 'title attrs)))
      (timestamp . ,(cdr (assq 'timestamp attrs)))
      (comment . ,(or (cdr (assq 'comment attrs)) ""))
      (sizediff . ,(wp--parse-number (cdr (assq 'sizediff attrs))))
      (minor . ,(assq 'minor attrs)))))

(provide 'wikipedia-adapter)

;;; wikipedia-adapter.el ends here
