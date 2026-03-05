;;; wikipedia-common.el --- Shared utilities for wikipedia.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Pablo Stafforini

;;; Commentary:

;; This module provides shared utilities and cross-mode commands for
;; wikipedia.el.  Functions here are used across multiple modules
;; (watchlist, history, user contributions, etc.) and don't belong
;; to any single feature module.

;;; Code:

(require 'wikipedia-adapter)
(require 'tabulated-list)

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

;;;; Display utilities

(defun wikipedia--format-timestamp (timestamp &optional length)
  "Format ISO 8601 TIMESTAMP for display.
Truncate to LENGTH characters (default 16, giving \"YYYY-MM-DD HH:MM\";
use 19 to include seconds: \"YYYY-MM-DD HH:MM:SS\")."
  (if timestamp
      (let ((len (or length 16)))
        (replace-regexp-in-string
         "T" " " (substring timestamp 0 (min len (length timestamp)))))
    ""))

(defun wikipedia--size-change-face (diff)
  "Return the face for a size change of DIFF bytes."
  (cond
   ((> diff 0) 'success)
   ((< diff 0) 'error)
   (t 'default)))

(defun wikipedia--format-size-change (diff)
  "Format DIFF as a size change string with face.
If DIFF is nil, return an empty string."
  (if diff
      (propertize (format "%+d" diff)
                  'face (wikipedia--size-change-face diff))
    ""))

;;;; Shared display

(defun wikipedia--display-revision-buffer (title revid content)
  "Display CONTENT for TITLE at REVID in a read-only buffer."
  (let ((buffer (get-buffer-create (format "*Wikipedia Rev %d: %s*" revid title))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or content "(empty)"))
        (goto-char (point-min)))
      (special-mode)
      (setq-local header-line-format
                  (format "Revision %d of %s" revid title)))
    (pop-to-buffer buffer)))

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

;;;; Shared keymap for list modes

(declare-function wikipedia-diff-follow-mode "wikipedia-diff")
(declare-function wikipedia-thank "wikipedia-user")
(declare-function wikipedia-user-at-point "wikipedia-user")
(declare-function wikipedia-xtools-user-stats "wikipedia-xtools")
(declare-function wikipedia-browse "wikipedia-page")
(declare-function wikipedia-watchlist-watch "wikipedia-watchlist")
(declare-function wikipedia-watchlist-unwatch "wikipedia-watchlist")

(defvar wikipedia-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "f" #'wikipedia-diff-follow-mode)
    (define-key map "t" #'wikipedia-thank)
    (define-key map "u" #'wikipedia-user-at-point)
    (define-key map "s" #'wikipedia-xtools-user-stats)
    (define-key map "B" #'wikipedia-browse)
    (define-key map "w" #'wikipedia-watchlist-watch)
    (define-key map "x" #'wikipedia-watchlist-unwatch)
    (define-key map "q" #'quit-window)
    map)
  "Base keymap for Wikipedia list modes.
Provides common bindings shared across watchlist, history, and
user contributions modes.")

(provide 'wikipedia-common)

;;; wikipedia-common.el ends here
