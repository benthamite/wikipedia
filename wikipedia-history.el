;;; wikipedia-history.el --- Revision history for wikipedia.el -*- lexical-binding: t; -*-

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
    (define-key map "D" #'wikipedia-history-diff-revisions)
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
  "Show diff between revision at point and its parent."
  (interactive)
  (let* ((rev (wikipedia-history--revision-at-point))
         (revid (alist-get 'revid rev))
         (parentid (alist-get 'parentid rev)))
    (unless revid
      (error "No revision at point"))
    (unless parentid
      (error "This revision has no parent"))
    (wikipedia--show-diff parentid revid wikipedia-history--page-title)))

(defun wikipedia-history-diff-revisions ()
  "Diff two marked revisions, or prompt for revision IDs."
  (interactive)
  (let* ((from (read-number "From revision: "))
         (to (read-number "To revision: ")))
    (wikipedia--show-diff from to wikipedia-history--page-title)))

(defun wikipedia--show-diff (from-rev to-rev title)
  "Display diff between FROM-REV and TO-REV for TITLE."
  (let* ((diff-html (wp--compare-revisions from-rev to-rev))
         (buffer (get-buffer-create
                  (format "*Wikipedia Diff: %s (%d→%d)*" title from-rev to-rev))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if diff-html
            (progn
              (insert diff-html)
              (goto-char (point-min))
              (wikipedia-diff--render-html))
          (insert "(No differences)")))
      (wikipedia-diff-mode)
      (setq-local header-line-format
                  (format "Diff: %s (rev %d → %d)" title from-rev to-rev)))
    (pop-to-buffer buffer)))

(defun wikipedia-diff--render-html ()
  "Render the HTML diff in the current buffer as text."
  (let ((dom (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (wikipedia-diff--render-table dom)))

(defun wikipedia-diff--render-table (dom)
  "Render diff table from DOM."
  (let ((rows (dom-by-tag dom 'tr)))
    (dolist (row rows)
      (let ((cells (dom-by-tag row 'td)))
        (when (>= (length cells) 4)
          (let* ((left-line (dom-text (nth 0 cells)))
                 (left-content (wikipedia-diff--cell-text (nth 1 cells)))
                 (right-line (dom-text (nth 2 cells)))
                 (right-content (wikipedia-diff--cell-text (nth 3 cells)))
                 (left-class (dom-attr (nth 1 cells) 'class))
                 (right-class (dom-attr (nth 3 cells) 'class)))
            (cond
             ((and left-content (string-match "diff-deletedline" (or left-class "")))
              (insert (format "-%s\n" left-content)))
             ((and right-content (string-match "diff-addedline" (or right-class "")))
              (insert (format "+%s\n" right-content)))
             ((and left-content (not (string-empty-p left-content)))
              (insert (format " %s\n" left-content))))))))))

(defun wikipedia-diff--cell-text (cell)
  "Extract text content from diff CELL."
  (when cell
    (let ((div (car (dom-by-tag cell 'div))))
      (if div
          (string-trim (dom-texts div ""))
        (string-trim (dom-texts cell ""))))))

(defvar wikipedia-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `wikipedia-diff-mode'.")

(define-derived-mode wikipedia-diff-mode special-mode "WP-Diff"
  "Major mode for viewing Wikipedia diffs.
\\{wikipedia-diff-mode-map}")

(provide 'wikipedia-history)

;;; wikipedia-history.el ends here
