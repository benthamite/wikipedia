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

(defvar wikipedia-diff-function)

;;;; Diff follow mode

(defvar-local wikipedia-diff-follow--last-revid nil
  "The last revision ID for which a diff was shown in follow mode.")

(defvar wikipedia-diff-follow--buffer-name "*WP Diff*"
  "Buffer name for diff follow mode output.")

(defun wikipedia-diff-follow--show ()
  "Show diff for the entry at point if it changed."
  (when (and wikipedia-diff-follow-mode
             (not (eq wikipedia-diff-function 'ediff)))
    (let ((revid (wikipedia--revid-at-point)))
      (when (and revid (not (equal revid wikipedia-diff-follow--last-revid)))
        (setq wikipedia-diff-follow--last-revid revid)
        (wikipedia-diff-follow--display-diff)))))

(defun wikipedia-diff-follow--display-diff ()
  "Display the diff for the current entry in the follow buffer."
  (condition-case err
      (let ((diff-info (wikipedia-diff-follow--get-diff-info)))
        (when diff-info
          (let ((from-rev (plist-get diff-info :from-rev))
                (to-rev (plist-get diff-info :to-rev))
                (title (plist-get diff-info :title)))
            (when (and from-rev to-rev title)
              (wikipedia-diff-follow--fetch-and-display
               from-rev to-rev title)))))
    (error (message "Diff follow: %s" (error-message-string err)))))

(declare-function wikipedia-history--revision-at-point "wikipedia-history")
(declare-function wikipedia-watchlist--entry-at-point "wikipedia-watchlist")

(defun wikipedia-diff-follow--get-diff-info ()
  "Get diff info for the entry at point.
Returns a plist with :from-rev, :to-rev, and :title, or nil."
  (cond
   ((derived-mode-p 'wikipedia-history-mode)
    (wikipedia-diff-follow--history-info))
   ((derived-mode-p 'wikipedia-watchlist-mode)
    (wikipedia-diff-follow--watchlist-info))
   ((derived-mode-p 'wikipedia-user-contributions-mode)
    (wikipedia-diff-follow--contributions-info))
   (t nil)))

(defun wikipedia-diff-follow--history-info ()
  "Get diff info for history mode."
  (let* ((rev (wikipedia-history--revision-at-point))
         (revid (alist-get 'revid rev))
         (parentid (alist-get 'parentid rev)))
    (when (and revid parentid)
      (list :from-rev parentid
            :to-rev revid
            :title wikipedia-history--page-title))))

(defun wikipedia-diff-follow--watchlist-info ()
  "Get diff info for watchlist mode."
  (let* ((entry (wikipedia-watchlist--entry-at-point))
         (title (alist-get 'title entry))
         (revid (alist-get 'revid entry))
         (old-revid (alist-get 'old_revid entry)))
    (when (and title revid old-revid)
      (list :from-rev old-revid
            :to-rev revid
            :title title))))

(defun wikipedia-diff-follow--contributions-info ()
  "Get diff info for user contributions mode."
  (let* ((contrib (wikipedia-user--contrib-at-point))
         (revid (alist-get 'revid contrib))
         (parentid (alist-get 'parentid contrib))
         (title (alist-get 'title contrib)))
    (when (and revid parentid title)
      (list :from-rev parentid
            :to-rev revid
            :title title))))

(defun wikipedia-diff-follow--fetch-and-display (from-rev to-rev title)
  "Fetch and display diff between FROM-REV and TO-REV for TITLE."
  (let* ((from-content (wp--get-revision-content title from-rev))
         (to-content (wp--get-revision-content title to-rev))
         (from-file (wikipedia--write-temp-file from-content from-rev))
         (to-file (wikipedia--write-temp-file to-content to-rev))
         (diff-output (wikipedia--generate-unified-diff from-file to-file from-rev to-rev))
         (buf (get-buffer-create wikipedia-diff-follow--buffer-name))
         (source-window (selected-window)))
    (delete-file from-file)
    (delete-file to-file)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert diff-output)
        (goto-char (point-min)))
      (diff-mode)
      (setq buffer-read-only t)
      (setq header-line-format
            (format " %s: %d → %d" title from-rev to-rev)))
    (wikipedia-diff-follow--display-buffer buf)
    (select-window source-window)))

(defun wikipedia-diff-follow--display-buffer (buffer)
  "Display BUFFER in a side window for diff follow mode."
  (display-buffer buffer
                  '((display-buffer-reuse-window
                     display-buffer-in-side-window)
                    (side . bottom)
                    (slot . 0)
                    (window-height . 0.4)
                    (preserve-size . (nil . t)))))

(defun wikipedia-diff-follow--cleanup ()
  "Clean up diff follow mode state."
  (setq wikipedia-diff-follow--last-revid nil)
  (when-let ((buf (get-buffer wikipedia-diff-follow--buffer-name)))
    (when-let ((win (get-buffer-window buf)))
      (delete-window win))))

;;;###autoload
(define-minor-mode wikipedia-diff-follow-mode
  "Minor mode to automatically show diffs when navigating entries.
When enabled, moving to a different entry in history, watchlist, or
contributions buffers will automatically display the diff in a side window.
Only works with unified diff mode, not ediff."
  :lighter " Follow"
  (if wikipedia-diff-follow-mode
      (progn
        (when (eq wikipedia-diff-function 'ediff)
          (message "Diff follow mode requires unified diff, not ediff")
          (setq wikipedia-diff-follow-mode nil))
        (add-hook 'post-command-hook #'wikipedia-diff-follow--show nil t))
    (remove-hook 'post-command-hook #'wikipedia-diff-follow--show t)
    (wikipedia-diff-follow--cleanup)))

(defun wikipedia--show-diff (from-rev to-rev title)
  "Display diff between FROM-REV and TO-REV for TITLE.
Uses `wikipedia-diff-function' to determine the diff style."
  (let ((from-content (wp--get-revision-content title from-rev))
        (to-content (wp--get-revision-content title to-rev)))
    (wikipedia--show-diff-contents from-content to-content from-rev to-rev title)))

(defun wikipedia--show-diff-contents (from-content to-content from-rev to-rev title)
  "Display diff between FROM-CONTENT and TO-CONTENT.
FROM-REV and TO-REV are the revision IDs, TITLE is the page title.
Uses `wikipedia-diff-function' to determine the diff style."
  (pcase wikipedia-diff-function
    ('unified (wikipedia--show-diff-unified
               from-content to-content from-rev to-rev title))
    ('ediff (wikipedia--show-diff-ediff
             from-content to-content from-rev to-rev title))
    (_ (error "Unknown diff function: %s" wikipedia-diff-function))))

(defun wikipedia--show-diff-unified (from-content to-content from-rev to-rev title)
  "Display unified diff between FROM-CONTENT and TO-CONTENT.
FROM-REV and TO-REV are the revision IDs, TITLE is the page title."
  (let* ((from-file (wikipedia--write-temp-file from-content from-rev))
         (to-file (wikipedia--write-temp-file to-content to-rev))
         (diff-output (wikipedia--generate-unified-diff from-file to-file from-rev to-rev))
         (buf (get-buffer-create (format "*WP Diff: %s (%d → %d)*" title from-rev to-rev))))
    (delete-file from-file)
    (delete-file to-file)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert diff-output)
        (goto-char (point-min)))
      (diff-mode)
      (setq buffer-read-only t))
    (pop-to-buffer buf)))

(defun wikipedia--write-temp-file (content revid)
  "Write CONTENT to a temporary file named after REVID."
  (let ((file (make-temp-file (format "wp-rev-%d-" revid))))
    (with-temp-file file
      (insert (or content "")))
    file))

(defun wikipedia--generate-unified-diff (from-file to-file from-rev to-rev)
  "Generate unified diff output between FROM-FILE and TO-FILE.
FROM-REV and TO-REV are used for the diff header labels."
  (with-temp-buffer
    (let ((exit-code (call-process "diff" nil t nil
                                   "-u"
                                   (format "--label=Revision %d" from-rev)
                                   (format "--label=Revision %d" to-rev)
                                   from-file to-file)))
      (if (> exit-code 1)
          (error "Diff command failed with exit code %d" exit-code)
        (buffer-string)))))

(defun wikipedia--show-diff-ediff (from-content to-content from-rev to-rev title)
  "Display ediff between FROM-CONTENT and TO-CONTENT.
FROM-REV and TO-REV are the revision IDs, TITLE is the page title."
  (let ((from-buffer (wikipedia--create-revision-buffer title from-rev from-content))
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

(provide 'wikipedia-common)

;;; wikipedia-common.el ends here
