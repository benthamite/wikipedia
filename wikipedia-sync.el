;;; wikipedia-sync.el --- Sync Wikipedia pages to local database -*- lexical-binding: t -*-

;; Copyright (C) 2025

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
      (when (and wikipedia-sync-fetch-content
                 (eq rev (car revisions))
                 (not (wikipedia-db-has-content-p rev-row-id)))
        (wikipedia-sync--fetch-content title revid rev-row-id)))))

(defun wikipedia-sync--fetch-content (title revid rev-row-id)
  "Fetch content for TITLE at REVID and store with REV-ROW-ID."
  (let ((content (wp--get-revision-content title revid)))
    (when content
      (wikipedia-db-insert-content rev-row-id content))))

;;;###autoload
(defun wikipedia-sync-watchlist ()
  "Sync all pages from watchlist to local database."
  (interactive)
  (wp--ensure-logged-in)
  (message "Fetching watchlist...")
  (let* ((entries (wp--get-watchlist 500))
         (titles (delete-dups (mapcar (lambda (e) (alist-get 'title e)) entries))))
    (dolist (title titles)
      (wikipedia-db-set-watched title t))
    (message "Syncing %d watched pages..." (length titles))
    (dolist (title titles)
      (wikipedia-sync-page title)
      (sit-for 0.5))
    (message "Sync complete!")))

;;;###autoload
(defun wikipedia-sync-update ()
  "Update all watched pages in the database."
  (interactive)
  (wp--ensure-logged-in)
  (let ((watched (wikipedia-db-get-watched-pages)))
    (if (null watched)
        (message "No watched pages in database. Run M-x wikipedia-sync-watchlist first.")
      (message "Updating %d watched pages..." (length watched))
      (dolist (row watched)
        (let ((title (nth 1 row)))
          (wikipedia-sync-page title)
          (sit-for 0.5)))
      (message "Update complete!"))))

(provide 'wikipedia-sync)

;;; wikipedia-sync.el ends here
