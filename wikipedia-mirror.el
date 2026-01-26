;;; wikipedia-mirror.el --- Browse local Wikipedia mirror -*- lexical-binding: t -*-

;; Copyright (C) 2025

;;; Commentary:

;; Browse locally stored Wikipedia pages and revisions.

;;; Code:

(require 'wikipedia-db)
(require 'tabulated-list)

(defvar wikipedia-mirror-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'wikipedia-mirror-open-page)
    (define-key map "o" #'wikipedia-mirror-open-page)
    (define-key map "h" #'wikipedia-mirror-show-history)
    (define-key map "s" #'wikipedia-mirror-sync-page)
    (define-key map "S" #'wikipedia-mirror-sync-all)
    (define-key map "g" #'wikipedia-mirror-refresh)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `wikipedia-mirror-mode'.")

(define-derived-mode wikipedia-mirror-mode tabulated-list-mode "WP-Mirror"
  "Major mode for browsing local Wikipedia mirror.
\\{wikipedia-mirror-mode-map}"
  (setq tabulated-list-format
        [("Title" 40 t)
         ("Revisions" 10 t)
         ("Last Synced" 20 t)
         ("Watched" 8 t)])
  (setq tabulated-list-sort-key '("Title"))
  (tabulated-list-init-header))

;;;###autoload
(defun wikipedia-mirror ()
  "Browse the local Wikipedia mirror."
  (interactive)
  (let ((buf (get-buffer-create "*Wikipedia Mirror*")))
    (with-current-buffer buf
      (wikipedia-mirror-mode)
      (wikipedia-mirror-refresh))
    (pop-to-buffer buf)))

(defun wikipedia-mirror-refresh ()
  "Refresh the mirror page list."
  (interactive)
  (let ((db (wikipedia-db--ensure-connection)))
    (let ((pages (sqlite-select db "
      SELECT p.id, p.title, p.last_synced, p.watched,
             (SELECT COUNT(*) FROM revisions WHERE page_id = p.id) as rev_count
      FROM pages p ORDER BY p.title")))
      (setq tabulated-list-entries
            (mapcar #'wikipedia-mirror--format-entry pages))
      (tabulated-list-print t))))

(defun wikipedia-mirror--format-entry (row)
  "Format ROW as a tabulated list entry."
  (let ((id (nth 0 row))
        (title (nth 1 row))
        (last-synced (nth 2 row))
        (watched (nth 3 row))
        (rev-count (nth 4 row)))
    (list id
          (vector
           title
           (number-to-string (or rev-count 0))
           (if last-synced
               (format-time-string "%Y-%m-%d %H:%M" last-synced)
             "Never")
           (if (= watched 1) "Yes" "No")))))

(defun wikipedia-mirror--page-id-at-point ()
  "Get page ID at point."
  (tabulated-list-get-id))

(defun wikipedia-mirror--title-at-point ()
  "Get title of page at point."
  (let ((entry (tabulated-list-get-entry)))
    (when entry (aref entry 0))))

(defun wikipedia-mirror-open-page ()
  "Open the page at point for editing online."
  (interactive)
  (when-let ((title (wikipedia-mirror--title-at-point)))
    (wp--ensure-logged-in)
    (wp--open-page-buffer title)))

(defun wikipedia-mirror-show-history ()
  "Show local history for page at point."
  (interactive)
  (when-let ((title (wikipedia-mirror--title-at-point)))
    (wikipedia-mirror-history title)))

(defun wikipedia-mirror-sync-page ()
  "Sync the page at point."
  (interactive)
  (when-let ((title (wikipedia-mirror--title-at-point)))
    (require 'wikipedia-sync)
    (wikipedia-sync-page title)
    (wikipedia-mirror-refresh)))

(defun wikipedia-mirror-sync-all ()
  "Sync all watched pages."
  (interactive)
  (require 'wikipedia-sync)
  (wikipedia-sync-update)
  (wikipedia-mirror-refresh))

;;; Local history browser

(defvar wikipedia-mirror-history-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'wikipedia-mirror-history-view)
    (define-key map "v" #'wikipedia-mirror-history-view)
    (define-key map "d" #'wikipedia-mirror-history-diff)
    (define-key map "g" #'wikipedia-mirror-history-refresh)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `wikipedia-mirror-history-mode'.")

(defvar-local wikipedia-mirror-history--page-id nil
  "Page ID for current history buffer.")

(defvar-local wikipedia-mirror-history--page-title nil
  "Page title for current history buffer.")

(define-derived-mode wikipedia-mirror-history-mode tabulated-list-mode "WP-LocalHist"
  "Major mode for browsing local revision history.
\\{wikipedia-mirror-history-mode-map}"
  (setq tabulated-list-format
        [("Rev ID" 12 t)
         ("Date" 20 t)
         ("User" 20 t)
         ("Size" 8 t)
         ("Comment" 0 nil)])
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header))

(defun wikipedia-mirror-history (title)
  "Show local history for TITLE."
  (let* ((page (wikipedia-db-get-page title))
         (page-id (car page)))
    (unless page-id
      (user-error "Page '%s' not in local database" title))
    (let ((buf (get-buffer-create (format "*WP Local History: %s*" title))))
      (with-current-buffer buf
        (wikipedia-mirror-history-mode)
        (setq wikipedia-mirror-history--page-id page-id)
        (setq wikipedia-mirror-history--page-title title)
        (wikipedia-mirror-history-refresh))
      (pop-to-buffer buf))))

(defun wikipedia-mirror-history-refresh ()
  "Refresh the local history list."
  (interactive)
  (let ((revisions (wikipedia-db-get-revisions wikipedia-mirror-history--page-id 100)))
    (setq tabulated-list-entries
          (mapcar #'wikipedia-mirror-history--format-entry revisions))
    (tabulated-list-print t)))

(defun wikipedia-mirror-history--format-entry (row)
  "Format revision ROW as a tabulated list entry."
  (let ((row-id (nth 0 row))
        (revid (nth 1 row))
        (user (nth 3 row))
        (timestamp (nth 4 row))
        (comment (nth 5 row))
        (size (nth 6 row)))
    (list row-id
          (vector
           (number-to-string revid)
           (or timestamp "")
           (or user "")
           (if size (number-to-string size) "")
           (or comment "")))))

(defun wikipedia-mirror-history--row-id-at-point ()
  "Get revision row ID at point."
  (tabulated-list-get-id))

(defun wikipedia-mirror-history--revid-at-point ()
  "Get revision ID at point."
  (let ((entry (tabulated-list-get-entry)))
    (when entry
      (string-to-number (aref entry 0)))))

(defun wikipedia-mirror-history-view ()
  "View content of revision at point."
  (interactive)
  (let* ((row-id (wikipedia-mirror-history--row-id-at-point))
         (revid (wikipedia-mirror-history--revid-at-point))
         (content (wikipedia-db-get-content row-id)))
    (if content
        (let ((buf (get-buffer-create
                    (format "*WP Local: %s (r%d)*"
                            wikipedia-mirror-history--page-title revid))))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert content)
              (goto-char (point-min)))
            (special-mode))
          (pop-to-buffer buf))
      (user-error "No content stored for revision %d" revid))))

(defun wikipedia-mirror-history-diff ()
  "Show diff between revision at point and its parent."
  (interactive)
  (let* ((row-id (wikipedia-mirror-history--row-id-at-point))
         (revid (wikipedia-mirror-history--revid-at-point))
         (rev-data (wikipedia-db-get-revision-by-revid revid))
         (parentid (nth 3 rev-data))
         (new-content (wikipedia-db-get-content row-id)))
    (unless new-content
      (user-error "No content stored for revision %d" revid))
    (unless parentid
      (user-error "This is the first revision (no parent)"))
    (let* ((parent-data (wikipedia-db-get-revision-by-revid parentid))
           (parent-row-id (nth 0 parent-data))
           (old-content (wikipedia-db-get-content parent-row-id)))
      (unless old-content
        (user-error "No content stored for parent revision %d" parentid))
      (wikipedia-mirror--show-local-diff old-content new-content parentid revid))))

(defun wikipedia-mirror--show-local-diff (old-content new-content old-rev new-rev)
  "Show diff between OLD-CONTENT and NEW-CONTENT (revisions OLD-REV and NEW-REV)."
  (let ((old-buf (generate-new-buffer (format "*WP r%d*" old-rev)))
        (new-buf (generate-new-buffer (format "*WP r%d*" new-rev))))
    (with-current-buffer old-buf
      (insert old-content)
      (setq buffer-read-only t))
    (with-current-buffer new-buf
      (insert new-content)
      (setq buffer-read-only t))
    (ediff-buffers old-buf new-buf)))

(provide 'wikipedia-mirror)

;;; wikipedia-mirror.el ends here
