;;; wikipedia-cache.el --- Revision cache and prefetch for wikipedia.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Pablo Stafforini

;;; Commentary:

;; In-memory revision content cache with oldest-first eviction and an
;; async prefetch queue.  This complements the persistent SQLite store
;; in wikipedia-db.el: the in-memory cache serves the diff-follow mode
;; and prefetch pipeline where low-latency access is needed, while the
;; database provides long-term storage for synced pages.

;;; Code:

(require 'wikipedia-adapter)

(declare-function mediawiki-make-api-url "mediawiki-api")

;;;; Revision content cache

(defvar wikipedia--revision-cache (make-hash-table :test 'eql)
  "Cache of revision content, keyed by revision ID.")

(defvar wikipedia--revision-cache-keys nil
  "List of revision IDs in cache, newest first (oldest evicted first).")

(defvar wikipedia--revision-cache-max-size 200
  "Maximum number of revisions to keep in the cache.
200 keeps memory reasonable (~10 MB at ~50 KB per revision) while
covering typical watchlist + prefetch working sets.")

(defvar wikipedia--prefetch-in-flight (make-hash-table :test 'eql)
  "Revision IDs currently being fetched.")

(defvar wikipedia--prefetch-pending-callbacks (make-hash-table :test 'eql)
  "Callbacks waiting for in-flight revision fetches.
Maps revision ID to a list of callbacks to invoke when the fetch
completes.  This allows multiple callers (e.g. prefetch and AI review)
to request the same revision concurrently without dropping callbacks.")

(defun wikipedia--cache-get (revid)
  "Get cached content for REVID, or nil if not cached."
  (gethash revid wikipedia--revision-cache))

(defun wikipedia--cache-put (revid content)
  "Store CONTENT in cache for REVID, evicting old entries if needed."
  (unless (gethash revid wikipedia--revision-cache)
    (push revid wikipedia--revision-cache-keys)
    (when (>= (hash-table-count wikipedia--revision-cache)
              wikipedia--revision-cache-max-size)
      (wikipedia--cache-evict)))
  (puthash revid content wikipedia--revision-cache))

(defun wikipedia--cache-evict ()
  "Evict oldest entries from the revision cache.
Shrinks to half capacity to amortize eviction cost."
  (let ((target (/ wikipedia--revision-cache-max-size 2)))
    (while (and wikipedia--revision-cache-keys
                (> (hash-table-count wikipedia--revision-cache) target))
      (let ((old-key (car (last wikipedia--revision-cache-keys))))
        (remhash old-key wikipedia--revision-cache)
        (setq wikipedia--revision-cache-keys
              (nbutlast wikipedia--revision-cache-keys))))))

(defun wikipedia--get-revision-content-cached (title revid)
  "Get revision content for TITLE at REVID, using cache if available."
  (or (wikipedia--cache-get revid)
      (let ((content (wp--get-revision-content title revid)))
        (when content
          (wikipedia--cache-put revid content))
        content)))

;;;; Async prefetching

(defun wikipedia--build-revision-api-url (site title revid)
  "Build API URL for fetching revision REVID of TITLE from SITE."
  (let ((api-url (mediawiki-make-api-url site)))
    (format "%s?%s" api-url
            (url-build-query-string
             `(("action" "query")
               ("format" "xml")
               ("titles" ,title)
               ("prop" "revisions")
               ("rvprop" "content")
               ("rvslots" "main")
               ("rvstartid" ,(number-to-string revid))
               ("rvendid" ,(number-to-string revid)))))))

(defun wikipedia--parse-async-revision-response ()
  "Parse revision content from async response in current buffer.
Returns the wikitext content or nil."
  (goto-char (point-min))
  (when (re-search-forward "\n\n" nil t)
    (let* ((xml (xml-parse-region (point) (point-max)))
           (api (car xml))
           (query (assq 'query (cddr api)))
           (pages (assq 'pages (cddr query)))
           (page (car (cddr pages)))
           (revisions (assq 'revisions (cddr page)))
           (rev (car (cddr revisions)))
           (slots (assq 'slots (cddr rev)))
           (slot (assq 'slot (cddr slots))))
      (when slot
        (let ((content (car (last slot))))
          (when (stringp content)
            content))))))

(defun wikipedia--fetch-revision-async (title revid &optional callback)
  "Fetch revision REVID for TITLE asynchronously.
CALLBACK is called with the content when done (optional).
The content is always stored in the cache.  If the revision is
already cached, CALLBACK fires immediately.  If a fetch is already
in-flight, CALLBACK is queued and fires when that fetch completes."
  (when revid
    (let ((cached (wikipedia--cache-get revid)))
      (cond
       ;; Already cached — fire callback immediately.
       (cached
        (when callback (funcall callback cached)))
       ;; Already in-flight — register callback for when it completes.
       ((gethash revid wikipedia--prefetch-in-flight)
        (when callback
          (puthash revid
                   (cons callback
                         (gethash revid wikipedia--prefetch-pending-callbacks))
                   wikipedia--prefetch-pending-callbacks)))
       ;; New fetch.
       (t
        (puthash revid t wikipedia--prefetch-in-flight)
        (when callback
          (puthash revid (list callback)
                   wikipedia--prefetch-pending-callbacks))
        (let* ((site (wp--get-site))
               (url (wikipedia--build-revision-api-url site title revid)))
          (url-retrieve
           url
           (lambda (status revid-arg)
             (unwind-protect
                 (let ((content nil))
                   (remhash revid-arg wikipedia--prefetch-in-flight)
                   (unless (plist-get status :error)
                     (condition-case err
                         (setq content (wikipedia--parse-async-revision-response))
                       (error
                        (message "wikipedia: prefetch parse error for rev %d: %s"
                                 revid-arg (error-message-string err))))
                     (when content
                       (wikipedia--cache-put revid-arg content)))
                   (dolist (cb (gethash revid-arg
                                        wikipedia--prefetch-pending-callbacks))
                     (funcall cb content))
                   (remhash revid-arg wikipedia--prefetch-pending-callbacks))
               (when (buffer-live-p (current-buffer))
                 (kill-buffer (current-buffer)))))
           (list revid)
           t t)))))))

(defvar wikipedia--prefetch-queue nil
  "Queue of (TITLE . REVID) pairs pending prefetch.")

(defvar wikipedia--prefetch-max-concurrent 10
  "Maximum number of concurrent prefetch requests.
Kept conservative to avoid triggering MediaWiki API rate limits.")

(defun wikipedia--prefetch-watchlist-diffs (entries)
  "Prefetch revision content for all watchlist ENTRIES asynchronously.
This fetches in the background without blocking Emacs, throttled to
`wikipedia--prefetch-max-concurrent' concurrent requests."
  (setq wikipedia--prefetch-queue nil)
  (dolist (entry entries)
    (let ((title (alist-get 'title entry))
          (revid (alist-get 'revid entry))
          (old-revid (alist-get 'old_revid entry)))
      (when (and title revid old-revid)
        (unless (or (wikipedia--cache-get old-revid)
                    (gethash old-revid wikipedia--prefetch-in-flight))
          (push (cons title old-revid) wikipedia--prefetch-queue))
        (unless (or (wikipedia--cache-get revid)
                    (gethash revid wikipedia--prefetch-in-flight))
          (push (cons title revid) wikipedia--prefetch-queue)))))
  (setq wikipedia--prefetch-queue (nreverse wikipedia--prefetch-queue))
  (wikipedia--prefetch-drain-queue))

(defun wikipedia--prefetch-drain-queue ()
  "Start prefetch requests up to the concurrency limit."
  (while (and wikipedia--prefetch-queue
              (< (hash-table-count wikipedia--prefetch-in-flight)
                 wikipedia--prefetch-max-concurrent))
    (let* ((item (pop wikipedia--prefetch-queue))
           (title (car item))
           (revid (cdr item)))
      (wikipedia--fetch-revision-async
       title revid
       (lambda (_) (wikipedia--prefetch-drain-queue))))))

(provide 'wikipedia-cache)

;;; wikipedia-cache.el ends here
