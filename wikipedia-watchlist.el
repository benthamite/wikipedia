;;; wikipedia-watchlist.el --- Watchlist for wikipedia.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This module provides watchlist browsing for Wikipedia.

;;; Code:

(require 'wikipedia-adapter)
(require 'tabulated-list)

(defvar-local wikipedia-watchlist--entries nil
  "The list of watchlist entries displayed in this buffer.")

(defvar wikipedia-watchlist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'wikipedia-watchlist-open-page)
    (define-key map "o" #'wikipedia-watchlist-open-page)
    (define-key map "d" #'wikipedia-watchlist-show-diff)
    (define-key map "h" #'wikipedia-watchlist-show-history)
    (define-key map "b" #'wikipedia-watchlist-browse)
    (define-key map "g" #'wikipedia-watchlist-refresh)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `wikipedia-watchlist-mode'.")

(define-derived-mode wikipedia-watchlist-mode tabulated-list-mode "WP-Watchlist"
  "Major mode for browsing Wikipedia watchlist.
\\{wikipedia-watchlist-mode-map}"
  (setq tabulated-list-format
        [("Page" 40 t)
         ("Time" 20 t)
         ("User" 20 t)
         ("Summary" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Time" . t))
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
  (let ((entries (wp--get-watchlist)))
    (setq wikipedia-watchlist--entries entries)
    (setq tabulated-list-entries
          (mapcar #'wikipedia-watchlist--make-entry entries))
    (tabulated-list-print t)))

(defun wikipedia-watchlist--make-entry (entry)
  "Create a tabulated list entry from watchlist ENTRY."
  (let ((title (alist-get 'title entry))
        (timestamp (alist-get 'timestamp entry))
        (user (alist-get 'user entry))
        (comment (alist-get 'comment entry))
        (revid (alist-get 'revid entry)))
    (list revid
          (vector
           (or title "")
           (wikipedia-watchlist--format-timestamp timestamp)
           (or user "")
           (or comment "")))))

(defun wikipedia-watchlist--format-timestamp (timestamp)
  "Format TIMESTAMP for display."
  (if timestamp
      (replace-regexp-in-string "T" " " (substring timestamp 0 16))
    ""))

(defun wikipedia-watchlist--entry-at-point ()
  "Return the watchlist entry at point."
  (let ((revid (tabulated-list-get-id)))
    (when revid
      (seq-find (lambda (e) (eq (alist-get 'revid e) revid))
                wikipedia-watchlist--entries))))

(defun wikipedia-watchlist-open-page ()
  "Open the page at point for editing."
  (interactive)
  (let* ((entry (wikipedia-watchlist--entry-at-point))
         (title (alist-get 'title entry)))
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
  (let* ((entry (wikipedia-watchlist--entry-at-point))
         (title (alist-get 'title entry)))
    (unless title
      (error "No entry at point"))
    (wikipedia-history title)))

(defun wikipedia-watchlist-browse ()
  "Open the page at point in an external browser."
  (interactive)
  (let* ((entry (wikipedia-watchlist--entry-at-point))
         (title (alist-get 'title entry)))
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
