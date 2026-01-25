;;; wikipedia-adapter.el --- Adapter layer for mediawiki.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This module provides an adapter layer that isolates the dependency on
;; mediawiki.el.  All interactions with mediawiki.el should go through
;; this module, allowing the underlying implementation to be replaced
;; if needed.
;;
;; The adapter provides:
;;   `wp--login' - Establish a session with a wiki site
;;   `wp--api-call' - Make API requests to the wiki
;;   `wp--open-page-buffer' - Open a page in a buffer for editing
;;   `wp--save-page-buffer' - Save the current buffer to the wiki

;;; Code:

(require 'mediawiki)

(defun wp--login (site)
  "Establish a session with SITE.
SITE should be a site name configured in `mediawiki-site-alist'."
  (mediawiki-site site))

(defun wp--api-call (action params)
  "Make an API call with ACTION and PARAMS.
ACTION is the MediaWiki API action (e.g., \"query\").
PARAMS is an alist of additional parameters.
Returns the parsed JSON response as an alist."
  (let ((mediawiki-api-params (append `((action . ,action)
                                        (format . "json"))
                                      params)))
    (mediawiki-api-call mediawiki-api-params)))

(defun wp--open-page-buffer (title)
  "Open TITLE for editing in a buffer.
Returns the buffer containing the page content."
  (mediawiki-open title))

(defun wp--save-page-buffer (&optional summary)
  "Save the current buffer to the wiki.
SUMMARY is the edit summary.  If nil, the user will be prompted."
  (if summary
      (let ((mediawiki-edit-summary summary))
        (mediawiki-save))
    (mediawiki-save)))

(defun wp--current-page-title ()
  "Return the title of the page in the current buffer, or nil."
  (when (bound-and-true-p mediawiki-page-title)
    mediawiki-page-title))

(defun wp--ensure-logged-in ()
  "Ensure we have an active session, signaling an error if not."
  (unless (bound-and-true-p mediawiki-site)
    (error "No active wiki session; use `wikipedia-login' first")))

(provide 'wikipedia-adapter)

;;; wikipedia-adapter.el ends here
