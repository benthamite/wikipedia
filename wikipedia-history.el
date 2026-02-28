;;; wikipedia-history.el --- Revision history  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This module provides revision history browsing for Wikipedia pages.

;;; Code:

(require 'wikipedia-adapter)
(require 'wikipedia-common)
(require 'tabulated-list)

(declare-function wikipedia-thank "wikipedia-user")
(declare-function wikipedia-user-at-point "wikipedia-user")
(declare-function wikipedia-open "wikipedia-page")
(declare-function wikipedia-browse "wikipedia-page")
(declare-function wikipedia-xtools-user-stats "wikipedia-xtools")
(declare-function wikipedia-watchlist-watch "wikipedia-watchlist")
(declare-function wikipedia-watchlist-unwatch "wikipedia-watchlist")

(defvar-local wikipedia-history--page-title nil
  "The page title for this history buffer.")

(defvar-local wikipedia-history--revisions nil
  "The list of revisions displayed in this buffer.")

(defvar wikipedia-history-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'wikipedia-history-view-revision)
    (define-key map "o" #'wikipedia-open)
    (define-key map "v" #'wikipedia-history-view-revision)
    (define-key map "d" #'wikipedia-history-diff-to-previous)
    (define-key map "c" #'wikipedia-history-diff-to-current)
    (define-key map "D" #'wikipedia-history-diff-revisions)
    (define-key map "f" #'wikipedia-diff-follow-mode)
    (define-key map "b" #'wikipedia-history-browse-revision)
    (define-key map "B" #'wikipedia-browse)
    (define-key map "g" #'wikipedia-history-refresh)
    (define-key map "t" #'wikipedia-thank)
    (define-key map "u" #'wikipedia-user-at-point)
    (define-key map "s" #'wikipedia-xtools-user-stats)
    (define-key map "w" #'wikipedia-watchlist-watch)
    (define-key map "x" #'wikipedia-watchlist-unwatch)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `wikipedia-history-mode'.")

(define-derived-mode wikipedia-history-mode tabulated-list-mode "WP-History"
  "Major mode for browsing Wikipedia page revision history.
\\{wikipedia-history-mode-map}"
  (setq tabulated-list-format
        [("Rev" 10 t)
         ("Date" 20 t)
         ("User" 20 t)
         ("Change" 8 t)
         ("Summary" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Date" . t))
  (tabulated-list-init-header))

;;;###autoload
(defun wikipedia-history (title)
  "Display revision history for page TITLE."
  (interactive (list (wikipedia--read-page-title)))
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
    (setq wikipedia-history--revisions
          (wikipedia-history--annotate-diffs revisions))
    (setq tabulated-list-entries
          (mapcar #'wikipedia-history--make-entry wikipedia-history--revisions))
    (tabulated-list-print t)))

(defun wikipedia-history--annotate-diffs (revisions)
  "Add sizediff to each revision by comparing consecutive REVISIONS.
REVISIONS are in reverse chronological order (newest first)."
  (let ((result nil)
        (prev-size nil))
    (dolist (rev (reverse revisions))
      (let* ((size (alist-get 'size rev))
             (diff (when (and size prev-size) (- size prev-size))))
        (push (cons (cons 'sizediff diff) rev) result)
        (setq prev-size size)))
    result))

(defun wikipedia-history--make-entry (rev)
  "Create a tabulated list entry from revision REV."
  (let ((revid (alist-get 'revid rev))
        (timestamp (alist-get 'timestamp rev))
        (user (alist-get 'user rev))
        (sizediff (alist-get 'sizediff rev))
        (comment (alist-get 'comment rev))
        (minor (alist-get 'minor rev)))
    (list revid
          (vector
           (number-to-string revid)
           (wikipedia-history--format-timestamp timestamp)
           (or user "")
           (wikipedia--format-size-change sizediff)
           (concat (if minor "m " "") (or comment ""))))))

(defun wikipedia-history--format-timestamp (timestamp)
  "Format TIMESTAMP for display."
  (wikipedia--format-timestamp timestamp))

(defun wikipedia-history--revision-at-point ()
  "Return the revision alist at point."
  (let ((revid (tabulated-list-get-id)))
    (when revid
      (seq-find (lambda (r) (eql (alist-get 'revid r) revid))
                wikipedia-history--revisions))))

(defun wikipedia-history--revid-at-point ()
  "Return the revision ID at point."
  (tabulated-list-get-id))

(defun wikipedia-history--user-at-point ()
  "Return the username at point."
  (let ((rev (wikipedia-history--revision-at-point)))
    (when rev
      (alist-get 'user rev))))

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
    (wikipedia--show-diff parentid revid wikipedia-history--page-title)))

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
    (wikipedia--show-diff revid current-revid wikipedia-history--page-title)))

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
    (wikipedia--show-diff from to wikipedia-history--page-title)))

(provide 'wikipedia-history)

;;; wikipedia-history.el ends here
