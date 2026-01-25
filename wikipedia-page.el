;;; wikipedia-page.el --- Page operations for wikipedia.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This module provides page-level operations: opening pages for editing
;; and saving edits back to the wiki.

;;; Code:

(require 'wikipedia-adapter)

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

(provide 'wikipedia-page)

;;; wikipedia-page.el ends here
