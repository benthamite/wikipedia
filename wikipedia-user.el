;;; wikipedia-user.el --- User inspection for wikipedia.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This module provides user inspection functionality for Wikipedia,
;; including viewing user contributions, statistics, and profile pages.

;;; Code:

(require 'wikipedia-adapter)
(require 'wikipedia-xtools)
(require 'tabulated-list)

(defvar-local wikipedia-user--username nil
  "The username displayed in this buffer.")

(defvar-local wikipedia-user--contributions nil
  "The list of contributions displayed in this buffer.")

;;; User contributions mode

(defvar wikipedia-user-contributions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'wikipedia-user-contributions-view-diff)
    (define-key map "d" #'wikipedia-user-contributions-view-diff)
    (define-key map "v" #'wikipedia-user-contributions-view-revision)
    (define-key map "o" #'wikipedia-user-contributions-open-page)
    (define-key map "b" #'wikipedia-user-contributions-browse)
    (define-key map "h" #'wikipedia-user-contributions-show-history)
    (define-key map "t" #'wikipedia-user-contributions-thank)
    (define-key map "s" #'wikipedia-user-stats)
    (define-key map "g" #'wikipedia-user-contributions-refresh)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `wikipedia-user-contributions-mode'.")

(define-derived-mode wikipedia-user-contributions-mode tabulated-list-mode "WP-Contribs"
  "Major mode for browsing Wikipedia user contributions.
\\{wikipedia-user-contributions-mode-map}"
  (setq tabulated-list-format
        [("Time" 16 t)
         ("Page" 35 t)
         ("Change" 8 t)
         ("Summary" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header))

;;;###autoload
(defun wikipedia-user-contributions (username)
  "Display contributions for USERNAME."
  (interactive "sUsername: ")
  (wp--ensure-logged-in)
  (let ((buffer (get-buffer-create (format "*Wikipedia Contributions: %s*" username))))
    (with-current-buffer buffer
      (wikipedia-user-contributions-mode)
      (setq wikipedia-user--username username)
      (wikipedia-user-contributions-refresh))
    (pop-to-buffer buffer)))

(defun wikipedia-user-contributions-refresh ()
  "Refresh the contributions list."
  (interactive)
  (let ((contribs (wp--get-user-contributions wikipedia-user--username)))
    (setq wikipedia-user--contributions contribs)
    (setq tabulated-list-entries
          (mapcar #'wikipedia-user--make-contrib-entry contribs))
    (tabulated-list-print t)))

(defun wikipedia-user--make-contrib-entry (contrib)
  "Create a tabulated list entry from CONTRIB."
  (let ((revid (alist-get 'revid contrib))
        (title (alist-get 'title contrib))
        (timestamp (alist-get 'timestamp contrib))
        (comment (alist-get 'comment contrib))
        (sizediff (alist-get 'sizediff contrib)))
    (list revid
          (vector
           (wikipedia-user--format-timestamp timestamp)
           (or title "")
           (wikipedia-user--format-size-change sizediff)
           (or comment "")))))

(defun wikipedia-user--format-timestamp (timestamp)
  "Format TIMESTAMP for display."
  (if timestamp
      (replace-regexp-in-string "T" " " (substring timestamp 0 16))
    ""))

(defun wikipedia-user--format-size-change (sizediff)
  "Format SIZEDIFF as a size change string with face."
  (if sizediff
      (propertize (format "%+d" sizediff)
                  'face (wikipedia-user--size-change-face sizediff))
    ""))

(defun wikipedia-user--size-change-face (diff)
  "Return the face for a size change of DIFF characters."
  (cond
   ((> diff 0) 'success)
   ((< diff 0) 'error)
   (t 'default)))

(defun wikipedia-user--contrib-at-point ()
  "Return the contribution at point."
  (let ((revid (tabulated-list-get-id)))
    (when revid
      (seq-find (lambda (c) (eq (alist-get 'revid c) revid))
                wikipedia-user--contributions))))

(defun wikipedia-user-contributions-view-diff ()
  "Show diff for the contribution at point."
  (interactive)
  (let* ((contrib (wikipedia-user--contrib-at-point))
         (revid (alist-get 'revid contrib))
         (parentid (alist-get 'parentid contrib))
         (title (alist-get 'title contrib)))
    (unless revid
      (error "No contribution at point"))
    (unless parentid
      (error "This revision has no parent"))
    (wikipedia--show-ediff parentid revid title)))

(defun wikipedia-user-contributions-view-revision ()
  "View the wikitext of the revision at point."
  (interactive)
  (let* ((contrib (wikipedia-user--contrib-at-point))
         (revid (alist-get 'revid contrib))
         (title (alist-get 'title contrib)))
    (unless revid
      (error "No contribution at point"))
    (let* ((content (wp--get-revision-content title revid))
           (buffer (get-buffer-create (format "*Wikipedia Rev %d: %s*" revid title))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (or content "(empty)"))
          (goto-char (point-min)))
        (special-mode)
        (setq-local header-line-format
                    (format "Revision %d of %s" revid title)))
      (pop-to-buffer buffer))))

(defun wikipedia-user-contributions-open-page ()
  "Open the page at point for editing."
  (interactive)
  (let* ((contrib (wikipedia-user--contrib-at-point))
         (title (alist-get 'title contrib)))
    (unless title
      (error "No contribution at point"))
    (wp--open-page-buffer title)))

(defun wikipedia-user-contributions-browse ()
  "Open the revision at point in an external browser."
  (interactive)
  (let* ((contrib (wikipedia-user--contrib-at-point))
         (revid (alist-get 'revid contrib))
         (title (alist-get 'title contrib)))
    (unless revid
      (error "No contribution at point"))
    (let ((url (wikipedia--revision-url title revid)))
      (browse-url url))))

(defun wikipedia-user-contributions-show-history ()
  "Show history for the page at point."
  (interactive)
  (let* ((contrib (wikipedia-user--contrib-at-point))
         (title (alist-get 'title contrib)))
    (unless title
      (error "No contribution at point"))
    (wikipedia-history title)))

(defun wikipedia-user-contributions-thank ()
  "Thank the user for the contribution at point."
  (interactive)
  (let* ((contrib (wikipedia-user--contrib-at-point))
         (revid (alist-get 'revid contrib)))
    (unless revid
      (error "No contribution at point"))
    (wikipedia-thank revid wikipedia-user--username)))

(declare-function wikipedia--show-ediff "wikipedia-history")
(declare-function wikipedia--revision-url "wikipedia-history")
(declare-function wikipedia-history "wikipedia-history")
(declare-function wikipedia-thank "wikipedia")

;;; User stats

(defun wikipedia-user-stats ()
  "Display statistics for the current user."
  (interactive)
  (let ((username (or wikipedia-user--username
                      (read-string "Username: "))))
    (wikipedia-xtools-user-stats username)))

;;; User page commands

;;;###autoload
(defun wikipedia-user-page (username)
  "Open the user page for USERNAME."
  (interactive "sUsername: ")
  (wp--open-page-buffer (format "User:%s" username)))

;;;###autoload
(defun wikipedia-user-talk (username)
  "Open the talk page for USERNAME."
  (interactive "sUsername: ")
  (wp--open-page-buffer (format "User talk:%s" username)))

;;;###autoload
(defun wikipedia-user-browse (username)
  "Open USERNAME's user page in an external browser."
  (interactive "sUsername: ")
  (let ((url (wikipedia--user-page-url username)))
    (browse-url url)))

(defun wikipedia--user-page-url (username)
  "Return the URL for USERNAME's user page."
  (let ((site-url (wikipedia--get-site-url)))
    (format "%s?title=User:%s"
            site-url
            (url-hexify-string username))))

(declare-function wikipedia--get-site-url "wikipedia-history")

;;; Inspect user at point

;;;###autoload
(defun wikipedia-user-at-point ()
  "Inspect the user at point.
Works in watchlist and history modes."
  (interactive)
  (let ((username (wikipedia--user-at-point)))
    (unless username
      (error "No user at point"))
    (wikipedia-user-contributions username)))

(declare-function wikipedia--user-at-point "wikipedia")

(provide 'wikipedia-user)

;;; wikipedia-user.el ends here
