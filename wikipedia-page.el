;;; wikipedia-page.el --- Page operations for wikipedia.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This module provides page-level operations: opening pages for editing
;; and saving edits back to the wiki.

;;; Code:

(require 'wikipedia-adapter)
(require 'wikipedia-common)
(require 'shr)

;;;###autoload
(defun wikipedia-login (site)
  "Log in to SITE.
SITE should be a site name configured in `mediawiki-site-alist'.
When called interactively, prompts for the site name."
  (interactive
   (list (completing-read "Wiki site: "
                          (mapcar #'car mediawiki-site-alist)
                          nil t)))
  (wp--login site)
  (message "Logged in to %s" site))

;;;###autoload
(defun wikipedia-open (title)
  "Open the Wikipedia page TITLE for editing.
When called interactively, prompts for the page title."
  (interactive "sPage title: ")
  (wp--ensure-logged-in)
  (wp--open-page-buffer title))

;;;###autoload
(defun wikipedia-save (&optional summary)
  "Save the current buffer to Wikipedia.
SUMMARY is the edit summary.  When called interactively, prompts
for the summary if the current value is empty."
  (interactive)
  (wp--ensure-logged-in)
  (wp--save-page-buffer summary)
  (message "Saved %s" (or (wp--current-page-title) "page")))

;;;###autoload
(defun wikipedia-browse (title)
  "Open Wikipedia page TITLE in an external browser.
If called interactively and point is on a page (in watchlist, history,
user contributions, or editing buffer), use that page. Otherwise, prompt."
  (interactive
   (list (or (wikipedia--page-title-at-point)
             (read-string "Page title to browse: "))))
  (let ((url (wikipedia--page-url title)))
    (browse-url url)))

;;;###autoload
(defun wikipedia-preview ()
  "Preview the current buffer's wikitext.
Opens a preview buffer showing the rendered HTML."
  (interactive)
  (wp--ensure-logged-in)
  (let* ((title (or (wp--current-page-title) "Preview"))
         (wikitext (buffer-substring-no-properties (point-min) (point-max)))
         (html (wp--preview title wikitext))
         (source-buffer (current-buffer)))
    (wikipedia--display-preview html title source-buffer)))

(defun wikipedia--display-preview (html title source-buffer)
  "Display HTML preview for TITLE in a dedicated buffer.
SOURCE-BUFFER is the buffer containing the wikitext being previewed."
  (let ((buffer (get-buffer-create (format "*Wikipedia Preview: %s*" title))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert html)
        (goto-char (point-min))
        (shr-render-region (point-min) (point-max))
        (goto-char (point-min)))
      (wikipedia-preview-mode)
      (setq-local wikipedia--preview-source-buffer source-buffer))
    (display-buffer buffer)))

(defvar-local wikipedia--preview-source-buffer nil
  "The source buffer for this preview.")

(defvar wikipedia-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'quit-window)
    (define-key map "e" #'wikipedia-preview-edit)
    (define-key map "g" #'wikipedia-preview-refresh)
    map)
  "Keymap for `wikipedia-preview-mode'.")

(define-derived-mode wikipedia-preview-mode special-mode "WP-Preview"
  "Major mode for Wikipedia preview buffers.
\\{wikipedia-preview-mode-map}"
  (setq-local revert-buffer-function #'wikipedia-preview--revert))

(defun wikipedia-preview-edit ()
  "Switch to the source buffer for editing."
  (interactive)
  (if (buffer-live-p wikipedia--preview-source-buffer)
      (pop-to-buffer wikipedia--preview-source-buffer)
    (error "Source buffer no longer exists")))

(defun wikipedia-preview-refresh ()
  "Refresh the preview from the source buffer."
  (interactive)
  (if (buffer-live-p wikipedia--preview-source-buffer)
      (with-current-buffer wikipedia--preview-source-buffer
        (wikipedia-preview))
    (error "Source buffer no longer exists")))

(defun wikipedia-preview--revert (_ignore-auto _noconfirm)
  "Revert function for preview buffers."
  (wikipedia-preview-refresh))

(provide 'wikipedia-page)

;;; wikipedia-page.el ends here
