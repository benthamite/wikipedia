;;; wikipedia-watchlist.el --- Watchlist for wikipedia.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This module provides watchlist browsing for Wikipedia.

;;; Code:

(require 'wikipedia-adapter)
(require 'tabulated-list)

(defvar-local wikipedia-watchlist--entries nil
  "The list of watchlist entries displayed in this buffer.")

(defvar-local wikipedia-watchlist--grouped-entries nil
  "The grouped watchlist entries, keyed by page title.")

(defvar-local wikipedia-watchlist--expanded nil
  "Hash table tracking which page titles are expanded.")

(defvar wikipedia-watchlist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'wikipedia-watchlist-open-page)
    (define-key map (kbd "TAB") #'wikipedia-watchlist-toggle-expand)
    (define-key map "o" #'wikipedia-watchlist-open-page)
    (define-key map "d" #'wikipedia-watchlist-show-diff)
    (define-key map "h" #'wikipedia-watchlist-show-history)
    (define-key map "b" #'wikipedia-watchlist-browse)
    (define-key map "g" #'wikipedia-watchlist-refresh)
    (define-key map "e" #'wikipedia-watchlist-expand-all)
    (define-key map "c" #'wikipedia-watchlist-collapse-all)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `wikipedia-watchlist-mode'.")

(define-derived-mode wikipedia-watchlist-mode tabulated-list-mode "WP-Watchlist"
  "Major mode for browsing Wikipedia watchlist.
\\{wikipedia-watchlist-mode-map}"
  (setq tabulated-list-format
        [("" 2 nil)
         ("Page" 40 t)
         ("Time" 20 t)
         ("User" 20 t)
         ("Change" 8 t)
         ("Summary" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (setq wikipedia-watchlist--expanded (make-hash-table :test 'equal))
  (tabulated-list-init-header))

;;;###autoload
(defun wikipedia-watchlist ()
  "Display the user's watchlist."
  (interactive)
  (wp--ensure-logged-in)
  (let ((buffer (get-buffer-create "*Wikipedia Watchlist*")))
    (with-current-buffer buffer
      (wikipedia-watchlist-mode)
      (wikipedia-watchlist-refresh))
    (pop-to-buffer buffer)))

(defun wikipedia-watchlist-refresh ()
  "Refresh the watchlist."
  (interactive)
  (let ((entries (wp--get-watchlist 500)))
    (setq wikipedia-watchlist--entries entries)
    (setq wikipedia-watchlist--grouped-entries
          (wikipedia-watchlist--group-entries entries))
    (wikipedia-watchlist--rebuild-list)
    (tabulated-list-print t)))

(defun wikipedia-watchlist--group-entries (entries)
  "Group ENTRIES by page title, preserving order of first appearance."
  (let ((groups (make-hash-table :test 'equal))
        (order nil))
    (dolist (entry entries)
      (let ((title (alist-get 'title entry)))
        (unless (gethash title groups)
          (push title order))
        (puthash title
                 (append (gethash title groups) (list entry))
                 groups)))
    (mapcar (lambda (title)
              (cons title (gethash title groups)))
            (nreverse order))))

(defun wikipedia-watchlist--rebuild-list ()
  "Rebuild `tabulated-list-entries' from grouped entries."
  (setq tabulated-list-entries
        (wikipedia-watchlist--build-display-entries)))

(defun wikipedia-watchlist--build-display-entries ()
  "Build the list of display entries from grouped data."
  (let ((result nil))
    (dolist (group wikipedia-watchlist--grouped-entries)
      (let* ((title (car group))
             (entries (cdr group))
             (expanded-p (gethash title wikipedia-watchlist--expanded)))
        (push (wikipedia-watchlist--make-group-entry title entries) result)
        (when expanded-p
          (dolist (entry entries)
            (push (wikipedia-watchlist--make-child-entry entry) result)))))
    (nreverse result)))

(defun wikipedia-watchlist--make-group-entry (title entries)
  "Create a group header entry for TITLE with ENTRIES."
  (let* ((count (length entries))
         (latest (car entries))
         (timestamp (alist-get 'timestamp latest))
         (users (wikipedia-watchlist--summarize-users entries))
         (total-change (wikipedia-watchlist--total-size-change entries))
         (expandable (> count 1))
         (expanded-p (gethash title wikipedia-watchlist--expanded))
         (indicator (if expandable
                        (if expanded-p "▼" "▶")
                      " ")))
    (list (cons 'group title)
          (vector
           indicator
           (wikipedia-watchlist--format-title-with-count title count)
           (wikipedia-watchlist--format-timestamp timestamp)
           users
           (wikipedia-watchlist--format-size-change-value total-change)
           ""))))

(defun wikipedia-watchlist--make-child-entry (entry)
  "Create a child entry for individual ENTRY."
  (let ((title (alist-get 'title entry))
        (timestamp (alist-get 'timestamp entry))
        (user (alist-get 'user entry))
        (comment (alist-get 'comment entry))
        (revid (alist-get 'revid entry))
        (oldlen (alist-get 'oldlen entry))
        (newlen (alist-get 'newlen entry)))
    (list (cons 'child (cons title revid))
          (vector
           ""
           (concat "  " (wikipedia-watchlist--format-timestamp timestamp))
           ""
           (or user "")
           (wikipedia-watchlist--format-size-change oldlen newlen)
           (or comment "")))))

(defun wikipedia-watchlist--format-title-with-count (title count)
  "Format TITLE with change COUNT if more than 1."
  (if (> count 1)
      (format "%s (%d changes)" title count)
    title))

(defun wikipedia-watchlist--summarize-users (entries)
  "Summarize the users who made changes in ENTRIES."
  (let ((users (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (let ((user (alist-get 'user entry)))
        (when user
          (puthash user (1+ (gethash user users 0)) users))))
    (let ((user-list nil))
      (maphash (lambda (user count)
                 (push (if (> count 1)
                           (format "%s (%d×)" user count)
                         user)
                       user-list))
               users)
      (string-join (nreverse user-list) ", "))))

(defun wikipedia-watchlist--total-size-change (entries)
  "Calculate the total size change across all ENTRIES."
  (let ((total 0))
    (dolist (entry entries)
      (let ((oldlen (alist-get 'oldlen entry))
            (newlen (alist-get 'newlen entry)))
        (when (and oldlen newlen)
          (setq total (+ total (- newlen oldlen))))))
    total))

(defun wikipedia-watchlist--format-size-change-value (diff)
  "Format DIFF as a size change string with face."
  (propertize (format "%+d" diff)
              'face (wikipedia-watchlist--size-change-face diff)))

(defun wikipedia-watchlist--format-size-change (oldlen newlen)
  "Format the size change from OLDLEN to NEWLEN."
  (if (and oldlen newlen)
      (let ((diff (- newlen oldlen)))
        (propertize (format "%+d" diff)
                    'face (wikipedia-watchlist--size-change-face diff)))
    ""))

(defun wikipedia-watchlist--size-change-face (diff)
  "Return the face for a size change of DIFF characters."
  (cond
   ((> diff 0) 'success)
   ((< diff 0) 'error)
   (t 'default)))

(defun wikipedia-watchlist--format-timestamp (timestamp)
  "Format TIMESTAMP for display."
  (if timestamp
      (replace-regexp-in-string "T" " " (substring timestamp 0 16))
    ""))

(defun wikipedia-watchlist--entry-at-point ()
  "Return the watchlist entry at point."
  (let ((id (tabulated-list-get-id)))
    (cond
     ((and (consp id) (eq (car id) 'group))
      (let ((title (cdr id)))
        (car (alist-get title wikipedia-watchlist--grouped-entries nil nil #'equal))))
     ((and (consp id) (eq (car id) 'child))
      (let ((revid (cddr id)))
        (seq-find (lambda (e) (eq (alist-get 'revid e) revid))
                  wikipedia-watchlist--entries)))
     (t nil))))

(defun wikipedia-watchlist--title-at-point ()
  "Return the page title at point."
  (let ((id (tabulated-list-get-id)))
    (cond
     ((and (consp id) (eq (car id) 'group))
      (cdr id))
     ((and (consp id) (eq (car id) 'child))
      (cadr id))
     (t nil))))

(defun wikipedia-watchlist-toggle-expand ()
  "Toggle expansion of the group at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when (and (consp id) (eq (car id) 'group))
      (let* ((title (cdr id))
             (entries (alist-get title wikipedia-watchlist--grouped-entries nil nil #'equal)))
        (when (> (length entries) 1)
          (if (gethash title wikipedia-watchlist--expanded)
              (remhash title wikipedia-watchlist--expanded)
            (puthash title t wikipedia-watchlist--expanded))
          (wikipedia-watchlist--rebuild-list)
          (tabulated-list-print t)
          (wikipedia-watchlist--goto-title title))))))

(defun wikipedia-watchlist--goto-title (title)
  "Move point to the row for TITLE."
  (goto-char (point-min))
  (while (and (not (eobp))
              (let ((id (tabulated-list-get-id)))
                (not (and (consp id)
                          (eq (car id) 'group)
                          (equal (cdr id) title)))))
    (forward-line 1)))

(defun wikipedia-watchlist-expand-all ()
  "Expand all groups."
  (interactive)
  (dolist (group wikipedia-watchlist--grouped-entries)
    (let ((title (car group))
          (entries (cdr group)))
      (when (> (length entries) 1)
        (puthash title t wikipedia-watchlist--expanded))))
  (wikipedia-watchlist--rebuild-list)
  (tabulated-list-print t))

(defun wikipedia-watchlist-collapse-all ()
  "Collapse all groups."
  (interactive)
  (clrhash wikipedia-watchlist--expanded)
  (wikipedia-watchlist--rebuild-list)
  (tabulated-list-print t))

(defun wikipedia-watchlist-open-page ()
  "Open the page at point for editing."
  (interactive)
  (let ((title (wikipedia-watchlist--title-at-point)))
    (unless title
      (error "No entry at point"))
    (wp--open-page-buffer title)))

(declare-function wikipedia--show-ediff "wikipedia-history")
(defun wikipedia-watchlist-show-diff ()
  "Show the diff for the change at point."
  (interactive)
  (let* ((entry (wikipedia-watchlist--entry-at-point))
         (title (alist-get 'title entry))
         (revid (alist-get 'revid entry))
         (old-revid (alist-get 'old_revid entry)))
    (unless title
      (error "No entry at point"))
    (unless (and revid old-revid)
      (error "Cannot determine revisions for diff"))
    (wikipedia--show-ediff old-revid revid title)))

(declare-function wikipedia-history "wikipedia-history")
(defun wikipedia-watchlist-show-history ()
  "Show the history for the page at point."
  (interactive)
  (let ((title (wikipedia-watchlist--title-at-point)))
    (unless title
      (error "No entry at point"))
    (wikipedia-history title)))

(defun wikipedia-watchlist-browse ()
  "Open the page at point in an external browser."
  (interactive)
  (let ((title (wikipedia-watchlist--title-at-point)))
    (unless title
      (error "No entry at point"))
    (let ((url (wikipedia--page-url title)))
      (browse-url url))))

(declare-function wikipedia--get-site-url "wikipedia-history")
(defun wikipedia--page-url (title)
  "Return the URL for page TITLE."
  (let ((site-url (wikipedia--get-site-url)))
    (format "%s?title=%s"
            site-url
            (url-hexify-string title))))

(provide 'wikipedia-watchlist)

;;; wikipedia-watchlist.el ends here
