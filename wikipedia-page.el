;;; wikipedia-page.el --- Page operations for wikipedia.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Pablo Stafforini

;;; Commentary:

;; This module provides page-level operations: opening pages for editing
;; and saving edits back to the wiki.

;;; Code:

(require 'wikipedia-adapter)
(require 'wikipedia-common)
(require 'shr)

(declare-function wikipedia-ai--request-summary "wikipedia-ai")
(declare-function wikipedia--diff-to-live-text "wikipedia-diff")
(defvar wikipedia-ai-summarize-auto)
(defvar wikipedia-ai--pending-summary)

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
  (interactive (list (wikipedia--read-page-title)))
  (wp--ensure-logged-in)
  (wp--open-page-buffer title))

;;;###autoload
(defun wikipedia-publish (&optional summary)
  "Publish the current buffer to Wikipedia.
SUMMARY is the edit summary.  When called interactively, prompts
for the summary.  When `wikipedia-ai-summarize-auto' is non-nil and
no SUMMARY is provided, an AI-generated summary pre-fills the prompt."
  (interactive)
  (wp--ensure-logged-in)
  (if (and (not summary)
           (bound-and-true-p wikipedia-ai-summarize-auto)
           (not (bound-and-true-p wikipedia-ai--pending-summary))
           (require 'gptel nil t))
      (wikipedia--publish-with-ai-summary)
    (wikipedia--publish-now summary)))

(defun wikipedia--publish-now (&optional summary)
  "Publish the current buffer with SUMMARY and show confirmation."
  (wp--publish-page-buffer summary)
  (message "Published %s" (or (wp--current-page-title)
                              (when buffer-file-name
                                (file-name-base buffer-file-name))
                              "page")))

(defun wikipedia--publish-with-ai-summary ()
  "Generate an AI edit summary, then publish.
Falls back to normal publishing if the diff or AI request fails."
  (let ((buf (current-buffer)))
    (condition-case err
        (let ((diff-text (wikipedia--diff-to-live-text)))
          (if diff-text
              (wikipedia-ai--request-summary
               diff-text buf
               (lambda (_)
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (wikipedia--publish-now)))))
            (wikipedia--publish-now)))
      (error
       (message "AI summary skipped: %s" (error-message-string err))
       (wikipedia--publish-now)))))

;;;###autoload
(defalias 'wikipedia-save #'wikipedia-publish)

;;;###autoload
(defun wikipedia-browse (title)
  "Open Wikipedia page TITLE in an external browser."
  (interactive (list (wikipedia--read-page-title)))
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
         (html (or (wp--preview title wikitext)
                   (error "Preview failed: no HTML returned")))
         (source-buffer (current-buffer)))
    (wikipedia--display-preview html title source-buffer)))

(defun wikipedia--display-preview (html title source-buffer)
  "Display HTML preview for TITLE in a dedicated buffer.
SOURCE-BUFFER is the buffer containing the wikitext being previewed."
  (let ((buffer (get-buffer-create (format "*Wikipedia Preview: %s*" title))))
    (with-current-buffer buffer
      (wikipedia-preview-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert html)
        (goto-char (point-min))
        (shr-render-region (point-min) (point-max))
        (goto-char (point-min)))
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
