;;; wikipedia-user.el --- User inspection for wikipedia.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Pablo Stafforini

;;; Commentary:

;; This module provides user inspection functionality for Wikipedia,
;; including viewing user contributions, statistics, and profile pages.

;;; Code:

(require 'wikipedia-adapter)
(require 'wikipedia-common)
(require 'wikipedia-xtools)
(require 'tabulated-list)

(defvar-local wikipedia-user--username nil
  "The username displayed in this buffer.")

(defvar-local wikipedia-user--contributions nil
  "The list of contributions displayed in this buffer.")

;;; User contributions mode

(declare-function wikipedia--show-diff "wikipedia-diff")
(declare-function wikipedia-history "wikipedia-history")
(declare-function wikipedia-browse "wikipedia-page")

(defvar wikipedia-user-contributions-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map wikipedia-list-mode-map)
    (define-key map (kbd "RET") #'wikipedia-user-contributions-view-diff)
    (define-key map "o" #'wikipedia-user-contributions-open-page)
    (define-key map "v" #'wikipedia-user-contributions-view-revision)
    (define-key map "d" #'wikipedia-user-contributions-view-diff)
    (define-key map "h" #'wikipedia-user-contributions-show-history)
    (define-key map "b" #'wikipedia-user-contributions-browse)
    (define-key map "g" #'wikipedia-user-contributions-refresh)
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
  (interactive (list (wikipedia--read-username)))
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
           (wikipedia--format-timestamp timestamp)
           (or title "")
           (wikipedia--format-size-change sizediff)
           (or comment "")))))

(defun wikipedia-user--contrib-at-point ()
  "Return the contribution at point."
  (let ((revid (tabulated-list-get-id)))
    (when revid
      (seq-find (lambda (c) (eql (alist-get 'revid c) revid))
                wikipedia-user--contributions))))

(defun wikipedia-user--require-contrib-at-point ()
  "Return the contribution at point, or signal an error."
  (or (wikipedia-user--contrib-at-point)
      (error "No contribution at point")))

(defun wikipedia-user-contributions-view-diff ()
  "Show diff for the contribution at point."
  (interactive)
  (let* ((contrib (wikipedia-user--require-contrib-at-point))
         (revid (alist-get 'revid contrib))
         (parentid (alist-get 'parentid contrib))
         (title (alist-get 'title contrib)))
    (unless (and parentid (not (zerop parentid)))
      (error "This revision has no parent (first revision of page)"))
    (wikipedia--show-diff parentid revid title)))

(defun wikipedia-user-contributions-view-revision ()
  "View the wikitext of the revision at point."
  (interactive)
  (let* ((contrib (wikipedia-user--require-contrib-at-point))
         (revid (alist-get 'revid contrib))
         (title (alist-get 'title contrib))
         (content (wp--get-revision-content title revid)))
    (wikipedia--display-revision-buffer title revid content)))

(defun wikipedia-user-contributions-open-page ()
  "Open the page at point for editing."
  (interactive)
  (let* ((contrib (wikipedia-user--require-contrib-at-point))
         (title (alist-get 'title contrib)))
    (wp--open-page-buffer title)))

(defun wikipedia-user-contributions-browse ()
  "Open the revision at point in an external browser."
  (interactive)
  (let* ((contrib (wikipedia-user--require-contrib-at-point))
         (revid (alist-get 'revid contrib))
         (title (alist-get 'title contrib))
         (url (wikipedia--revision-url title revid)))
    (browse-url url)))

(defun wikipedia-user-contributions-show-history ()
  "Show history for the page at point."
  (interactive)
  (let* ((contrib (wikipedia-user--require-contrib-at-point))
         (title (alist-get 'title contrib)))
    (wikipedia-history title)))

;;; User page commands

;;;###autoload
(defun wikipedia-user-page (username)
  "Open the user page for USERNAME."
  (interactive (list (wikipedia--read-username)))
  (wp--open-page-buffer (format "User:%s" username)))

;;;###autoload
(defun wikipedia-user-talk (username)
  "Open the talk page for USERNAME."
  (interactive (list (wikipedia--read-username)))
  (wp--open-page-buffer (format "User talk:%s" username)))

;;;###autoload
(defun wikipedia-user-browse (username)
  "Open USERNAME's user page in an external browser."
  (interactive (list (wikipedia--read-username)))
  (let ((url (wikipedia--user-page-url username)))
    (browse-url url)))

;;; Thank

;;;###autoload
(defun wikipedia-thank (revid &optional user)
  "Thank the author of revision REVID.
If USER is provided, it is used in the confirmation message."
  (interactive (list (wikipedia--revid-at-point)
                     (wikipedia--user-at-point)))
  (unless revid
    (error "No revision at point"))
  (when (yes-or-no-p (format "Thank %s for this edit? "
                             (or user "the user")))
    (condition-case err
        (progn
          (wp--thank-revision revid)
          (message "Thanks sent for revision %s" revid))
      (error
       (message "Failed to send thanks: %s" (error-message-string err))))))

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

(provide 'wikipedia-user)

;;; wikipedia-user.el ends here
