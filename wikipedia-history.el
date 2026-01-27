;;; wikipedia-history.el --- Revision history  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This module provides revision history browsing for Wikipedia pages.

;;; Code:

(require 'wikipedia-adapter)
(require 'tabulated-list)

(defvar-local wikipedia-history--page-title nil
  "The page title for this history buffer.")

(defvar-local wikipedia-history--revisions nil
  "The list of revisions displayed in this buffer.")

(defvar wikipedia-history-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'wikipedia-history-view-revision)
    (define-key map "v" #'wikipedia-history-view-revision)
    (define-key map "d" #'wikipedia-history-diff-to-previous)
    (define-key map "c" #'wikipedia-history-diff-to-current)
    (define-key map "D" #'wikipedia-history-diff-revisions)
    (define-key map "b" #'wikipedia-history-browse-revision)
    (define-key map "q" #'quit-window)
    (define-key map "g" #'wikipedia-history-refresh)
    map)
  "Keymap for `wikipedia-history-mode'.")

(define-derived-mode wikipedia-history-mode tabulated-list-mode "WP-History"
  "Major mode for browsing Wikipedia page revision history.
\\{wikipedia-history-mode-map}"
  (setq tabulated-list-format
        [("Rev" 10 t)
         ("Date" 20 t)
         ("User" 20 t)
         ("Size" 8 t :right-align t)
         ("Summary" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Date" . t))
  (tabulated-list-init-header))

;;;###autoload
(defun wikipedia-history (title)
  "Display revision history for page TITLE."
  (interactive
   (list (or (wp--current-page-title)
             (read-string "Page title: "))))
  (wp--ensure-logged-in)
  (let ((buffer (get-buffer-create (format "*Wikipedia History: %s*" title))))
    (with-current-buffer buffer
      (wikipedia-history-mode)
      (setq wikipedia-history--page-title title)
      (wikipedia-history-refresh))
    (pop-to-buffer buffer)))

(defun wikipedia-history-refresh ()
  "Refresh the history list."
  (interactive)
  (let ((revisions (wp--get-page-history wikipedia-history--page-title)))
    (setq wikipedia-history--revisions revisions)
    (setq tabulated-list-entries
          (mapcar #'wikipedia-history--make-entry revisions))
    (tabulated-list-print t)))

(defun wikipedia-history--make-entry (rev)
  "Create a tabulated list entry from revision REV."
  (let ((revid (alist-get 'revid rev))
        (timestamp (alist-get 'timestamp rev))
        (user (alist-get 'user rev))
        (size (alist-get 'size rev))
        (comment (alist-get 'comment rev))
        (minor (alist-get 'minor rev)))
    (list revid
          (vector
           (number-to-string revid)
           (wikipedia-history--format-timestamp timestamp)
           (or user "")
           (if size (number-to-string size) "")
           (concat (if minor "m " "") (or comment ""))))))

(defun wikipedia-history--format-timestamp (timestamp)
  "Format TIMESTAMP for display."
  (if timestamp
      (replace-regexp-in-string "T" " " (substring timestamp 0 16))
    ""))

(defun wikipedia-history--revision-at-point ()
  "Return the revision alist at point."
  (let ((revid (tabulated-list-get-id)))
    (when revid
      (seq-find (lambda (r) (eq (alist-get 'revid r) revid))
                wikipedia-history--revisions))))

(defun wikipedia-history-view-revision ()
  "View the wikitext of the revision at point."
  (interactive)
  (let* ((rev (wikipedia-history--revision-at-point))
         (revid (alist-get 'revid rev)))
    (unless revid
      (error "No revision at point"))
    (let* ((content (wp--get-revision-content
                     wikipedia-history--page-title revid))
           (buffer (get-buffer-create
                    (format "*Wikipedia Rev %d: %s*"
                            revid wikipedia-history--page-title))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (or content "(empty)"))
          (goto-char (point-min)))
        (special-mode)
        (setq-local header-line-format
                    (format "Revision %d of %s"
                            revid wikipedia-history--page-title)))
      (pop-to-buffer buffer))))

(defun wikipedia-history-diff-to-previous ()
  "Show diff between revision at point and its parent.
This is equivalent to Wikipedia \"prev\"."
  (interactive)
  (let* ((rev (wikipedia-history--revision-at-point))
         (revid (alist-get 'revid rev))
         (parentid (alist-get 'parentid rev)))
    (unless revid
      (error "No revision at point"))
    (unless parentid
      (error "This revision has no parent"))
    (wikipedia--show-ediff parentid revid wikipedia-history--page-title)))

(defun wikipedia-history-diff-to-current ()
  "Show diff between revision at point and the current revision.
This is equivalent to Wikipedia's \"cur\"."
  (interactive)
  (let* ((rev (wikipedia-history--revision-at-point))
         (revid (alist-get 'revid rev))
         (current-rev (car wikipedia-history--revisions))
         (current-revid (alist-get 'revid current-rev)))
    (unless revid
      (error "No revision at point"))
    (when (= revid current-revid)
      (error "Already at the current revision"))
    (wikipedia--show-ediff revid current-revid wikipedia-history--page-title)))

(defun wikipedia-history-browse-revision ()
  "Open the revision at point in an external browser."
  (interactive)
  (let* ((rev (wikipedia-history--revision-at-point))
         (revid (alist-get 'revid rev)))
    (unless revid
      (error "No revision at point"))
    (let ((url (wikipedia--revision-url wikipedia-history--page-title revid)))
      (browse-url url))))

(defun wikipedia-history-diff-revisions ()
  "Diff two revisions, prompting for revision IDs."
  (interactive)
  (let* ((from (read-number "From revision: "))
         (to (read-number "To revision: ")))
    (wikipedia--show-ediff from to wikipedia-history--page-title)))

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

(defun wikipedia--revision-url (title revid)
  "Return the URL for TITLE at REVID."
  (let ((site-url (wikipedia--get-site-url)))
    (format "%s?title=%s&oldid=%d"
            site-url
            (url-hexify-string title)
            revid)))

(defun wikipedia--get-site-url ()
  "Return the base URL for the current wiki site."
  (let* ((site (wp--get-site))
         (site-info (cdr (assoc site mediawiki-site-alist))))
    (or (plist-get site-info :url)
        (car site-info)
        (error "Cannot determine URL for site %s" site))))

(provide 'wikipedia-history)

;;; wikipedia-history.el ends here
