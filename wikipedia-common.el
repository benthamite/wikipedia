;;; wikipedia-common.el --- Shared utilities for wikipedia.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This module provides shared utilities and cross-mode commands for
;; wikipedia.el.  Functions here are used across multiple modules
;; (watchlist, history, user contributions, etc.) and don't belong
;; to any single feature module.

;;; Code:

(require 'wikipedia-adapter)

;;;; URL utilities

(defun wikipedia--get-site-url ()
  "Return the base URL for the current wiki site."
  (let* ((site (wp--get-site))
         (site-info (cdr (assoc site mediawiki-site-alist))))
    (or (plist-get site-info :url)
        (car site-info)
        (error "Cannot determine URL for site %s" site))))

(defun wikipedia--page-url (title)
  "Return the URL for page TITLE."
  (let ((site-url (wikipedia--get-site-url)))
    (format "%s?title=%s"
            site-url
            (url-hexify-string title))))

(defun wikipedia--revision-url (title revid)
  "Return the URL for TITLE at REVID."
  (let ((site-url (wikipedia--get-site-url)))
    (format "%s?title=%s&oldid=%d"
            site-url
            (url-hexify-string title)
            revid)))

(defun wikipedia--user-page-url (username)
  "Return the URL for USERNAME's user page."
  (let ((site-url (wikipedia--get-site-url)))
    (format "%s?title=User:%s"
            site-url
            (url-hexify-string username))))

;;;; Diff utilities

(defun wikipedia--show-ediff (from-rev to-rev title)
  "Display ediff between FROM-REV and TO-REV for TITLE."
  (let* ((from-content (wp--get-revision-content title from-rev))
         (to-content (wp--get-revision-content title to-rev))
         (from-buffer (wikipedia--create-revision-buffer title from-rev from-content))
         (to-buffer (wikipedia--create-revision-buffer title to-rev to-content)))
    (ediff-buffers from-buffer to-buffer)))

(defun wikipedia--create-revision-buffer (title revid content)
  "Create a buffer for TITLE at REVID with CONTENT."
  (let ((buffer (get-buffer-create (format "*WP Rev %d: %s*" revid title))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or content ""))
        (goto-char (point-min)))
      (setq buffer-read-only t))
    buffer))

;;;; Context detection

(declare-function wikipedia-watchlist--revid-at-point "wikipedia-watchlist")
(declare-function wikipedia-watchlist--user-at-point "wikipedia-watchlist")
(declare-function wikipedia-watchlist--title-at-point "wikipedia-watchlist")
(declare-function wikipedia-history--revid-at-point "wikipedia-history")
(declare-function wikipedia-history--user-at-point "wikipedia-history")
(declare-function wikipedia-user--contrib-at-point "wikipedia-user")

(defun wikipedia--revid-at-point ()
  "Return the revision ID at point, or nil.
This function checks various contexts to find a revision ID."
  (cond
   ((derived-mode-p 'wikipedia-watchlist-mode)
    (wikipedia-watchlist--revid-at-point))
   ((derived-mode-p 'wikipedia-history-mode)
    (wikipedia-history--revid-at-point))
   ((derived-mode-p 'wikipedia-user-contributions-mode)
    (tabulated-list-get-id))
   (t nil)))

(defun wikipedia--user-at-point ()
  "Return the username at point, or nil.
This function checks various contexts to find a username."
  (cond
   ((derived-mode-p 'wikipedia-watchlist-mode)
    (wikipedia-watchlist--user-at-point))
   ((derived-mode-p 'wikipedia-history-mode)
    (wikipedia-history--user-at-point))
   ((derived-mode-p 'wikipedia-user-contributions-mode)
    (bound-and-true-p wikipedia-user--username))
   (t nil)))

(defun wikipedia--page-title-at-point ()
  "Return the page title at point, or nil.
This function checks various contexts to find a page title."
  (cond
   ((derived-mode-p 'wikipedia-watchlist-mode)
    (wikipedia-watchlist--title-at-point))
   ((derived-mode-p 'wikipedia-history-mode)
    (bound-and-true-p wikipedia-history--page-title))
   ((derived-mode-p 'wikipedia-user-contributions-mode)
    (let ((contrib (wikipedia-user--contrib-at-point)))
      (when contrib (alist-get 'title contrib))))
   ((bound-and-true-p mediawiki-page-title)
    mediawiki-page-title)
   (t nil)))

;;;; Reading with defaults

(defun wikipedia--read-page-title ()
  "Read a page title, defaulting to the title at point."
  (let ((default (wikipedia--page-title-at-point)))
    (if default
        (read-string (format "Page title (default %s): " default) nil nil default)
      (read-string "Page title: "))))

(defun wikipedia--read-username ()
  "Read a username, defaulting to the user at point."
  (let ((default (wikipedia--user-at-point)))
    (if default
        (read-string (format "Username (default %s): " default) nil nil default)
      (read-string "Username: "))))

;;;; Cross-mode commands

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

(provide 'wikipedia-common)

;;; wikipedia-common.el ends here
