;;; wikipedia-cache.el --- Revision cache and prefetch for wikipedia.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This module provides a revision content cache with FIFO eviction
;; and an async prefetch queue for wikipedia.el.

;;; Code:

(require 'wikipedia-adapter)

(declare-function mediawiki-make-api-url "mediawiki-api")

;;;; Revision content cache

(defvar wikipedia--revision-cache (make-hash-table :test 'eql)
  "Cache of revision content, keyed by revision ID.")

(defvar wikipedia--revision-cache-keys nil
  "List of revision IDs in cache, newest first (FIFO eviction).")

(defvar wikipedia--revision-cache-max-size 200
  "Maximum number of revisions to keep in the cache.")

(defvar wikipedia--prefetch-in-flight (make-hash-table :test 'eql)
  "Revision IDs currently being fetched.")

(defun wikipedia--cache-get (revid)
  "Get cached content for REVID, or nil if not cached."
  (gethash revid wikipedia--revision-cache))

(defun wikipedia--cache-put (revid content)
  "Store CONTENT in cache for REVID, evicting old entries if needed."
  (unless (gethash revid wikipedia--revision-cache)
    (push revid wikipedia--revision-cache-keys)
    (when (> (hash-table-count wikipedia--revision-cache)
             wikipedia--revision-cache-max-size)
      (wikipedia--cache-evict)))
  (puthash revid content wikipedia--revision-cache))

(defun wikipedia--cache-evict ()
  "Evict oldest entries from the revision cache."
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
        (wikipedia--cache-put revid content)
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
The content is always stored in the cache."
  (when (and revid (not (wikipedia--cache-get revid))
             (not (gethash revid wikipedia--prefetch-in-flight)))
    (puthash revid t wikipedia--prefetch-in-flight)
    (let* ((site (wp--get-site))
           (url (wikipedia--build-revision-api-url site title revid)))
      (url-retrieve
       url
       (lambda (status revid-arg callback-arg)
         (unwind-protect
             (progn
               (remhash revid-arg wikipedia--prefetch-in-flight)
               (unless (plist-get status :error)
                 (condition-case nil
                     (let ((content (wikipedia--parse-async-revision-response)))
                       (when content
                         (wikipedia--cache-put revid-arg content))
                       (when callback-arg
                         (funcall callback-arg content)))
                   (error nil))))
           (when (buffer-live-p (current-buffer))
             (kill-buffer (current-buffer)))))
       (list revid callback)
       t t))))

(defvar wikipedia--prefetch-queue nil
  "Queue of (TITLE . REVID) pairs pending prefetch.")

(defvar wikipedia--prefetch-max-concurrent 10
  "Maximum number of concurrent prefetch requests.")

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
