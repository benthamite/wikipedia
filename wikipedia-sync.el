;;; wikipedia-sync.el --- Sync Wikipedia pages to local database -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 Pablo Stafforini

;;; Commentary:

;; Synchronize watched Wikipedia pages to local SQLite storage.

;;; Code:

(require 'wikipedia-db)
(require 'wikipedia-adapter)
(require 'wikipedia-common)

(defgroup wikipedia-sync nil
  "Synchronization settings for Wikipedia local mirror."
  :group 'wikipedia)

(defcustom wikipedia-sync-revision-limit 20
  "Maximum number of revisions to sync per page."
  :type 'integer
  :group 'wikipedia-sync)

(defcustom wikipedia-sync-fetch-content t
  "Whether to fetch full content for revisions during sync."
  :type 'boolean
  :group 'wikipedia-sync)

;;;###autoload
(defun wikipedia-sync-page (title)
  "Sync page TITLE to local database."
  (interactive (list (wikipedia--read-page-title)))
  (wp--ensure-logged-in)
  (let ((page-id (wikipedia-db-insert-page title)))
    (message "Syncing %s..." title)
    (let ((revisions (wp--get-page-history title wikipedia-sync-revision-limit)))
      (wikipedia-sync--store-revisions page-id title revisions)
      (wikipedia-db-update-page-synced page-id)
      (message "Synced %s: %d revisions" title (length revisions)))))

(defun wikipedia-sync--store-revisions (page-id title revisions)
  "Store REVISIONS for PAGE-ID (TITLE) in database."
  (dolist (rev revisions)
    (let* ((revid (alist-get 'revid rev))
           (parentid (alist-get 'parentid rev))
           (user (alist-get 'user rev))
           (timestamp (alist-get 'timestamp rev))
           (comment (alist-get 'comment rev))
           (size (alist-get 'size rev))
           (rev-row-id (wikipedia-db-insert-revision
                        page-id revid parentid user timestamp comment size)))
      ;; Only fetch content for the latest revision to save bandwidth
      (when (and wikipedia-sync-fetch-content
                 (eql revid (alist-get 'revid (car revisions)))
                 (not (wikipedia-db-has-content-p rev-row-id)))
        (condition-case err
            (wikipedia-sync--fetch-content title revid rev-row-id)
          (error
           (message "Failed to fetch content for %s rev %d: %s"
                    title revid (error-message-string err))))))))

(defun wikipedia-sync--fetch-content (title revid rev-row-id)
  "Fetch content for TITLE at REVID and store with REV-ROW-ID."
  (let ((content (wp--get-revision-content title revid)))
    (when content
      (wikipedia-db-insert-content rev-row-id content))))

(defun wikipedia-sync--sync-titles (titles operation)
  "Sync each page in TITLES, reporting progress as OPERATION.
Returns the list of titles that failed to sync."
  (message "%s %d pages..." operation (length titles))
  (let ((failed nil))
    (dolist (title titles)
      (condition-case err
          (wikipedia-sync-page title)
        (error
         (push title failed)
         (message "Failed to sync %s: %s" title (error-message-string err))))
      ;; Pause between pages to stay well under the MediaWiki API rate
      ;; limit of ~200 req/s for authenticated users.  Each sync page
      ;; fires multiple API calls (history + content), so 0.5 s keeps
      ;; bursts modest without noticeably slowing a typical sync.
      (sleep-for 0.5))
    (nreverse failed)))

;;;###autoload
(defun wikipedia-sync-watchlist ()
  "Sync all pages from watchlist to local database."
  (interactive)
  (wp--ensure-logged-in)
  (message "Fetching watchlist...")
  ;; Fetch up to 500 entries (MediaWiki API maximum for list queries)
  (let* ((entries (wp--get-watchlist 500))
         (titles (delete-dups (mapcar (lambda (e) (alist-get 'title e)) entries))))
    (dolist (title titles)
      (wikipedia-db-set-watched title t))
    (let ((failed (wikipedia-sync--sync-titles titles "Syncing")))
      (if failed
          (message "Sync complete with %d failures: %s"
                   (length failed) (string-join failed ", "))
        (message "Sync complete!")))))

;;;###autoload
(defun wikipedia-sync-update ()
  "Update all watched pages in the database."
  (interactive)
  (wp--ensure-logged-in)
  (let ((watched (wikipedia-db-get-watched-pages)))
    (if (null watched)
        (message "No watched pages in database. Run M-x wikipedia-sync-watchlist first.")
      (let* ((titles (mapcar #'cadr watched))
             (failed (wikipedia-sync--sync-titles titles "Updating")))
        (if failed
            (message "Update complete with %d failures" (length failed))
          (message "Update complete!"))))))

(provide 'wikipedia-sync)

;;; wikipedia-sync.el ends here
