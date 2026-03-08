;;; wikipedia-ai.el --- AI-powered commands for wikipedia.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Pablo Stafforini

;;; Commentary:

;; This module provides AI-powered commands using gptel as the LLM
;; backend.  Currently supports generating Wikipedia-formatted
;; citations from a URL or free-text description.

;;; Code:

(require 'wikipedia-common)

(declare-function gptel-request "gptel-request")

(defgroup wikipedia-ai nil
  "AI-powered Wikipedia editing commands."
  :group 'wikipedia
  :prefix "wikipedia-ai-")

(defcustom wikipedia-ai-citation-system-prompt
  "You are a Wikipedia citation expert.  Your task is to generate a \
properly formatted Wikipedia citation template from the information provided.

Rules:
- Use the appropriate citation template: {{cite web}}, {{cite book}}, \
{{cite journal}}, {{cite news}}, {{cite conference}}, etc.
- Include all fields you can determine from the input.
- For URLs, include |url=, |title=, |website= or |publisher=, and \
|access-date= (use today's date in YYYY-MM-DD format).
- For books, include |title=, |last=, |first=, |year=, |publisher=, \
|isbn= when available.
- For journal articles, include |title=, |journal=, |volume=, |issue=, \
|pages=, |doi= when available.
- Output ONLY the citation template, with no surrounding text, \
explanation, or markup fences.
- Use a single line (no line breaks within the template)."
  "System prompt for the citation generation command."
  :type 'string
  :group 'wikipedia-ai)

;;;###autoload
(defun wikipedia-ai-cite (input)
  "Generate a Wikipedia citation from INPUT and insert it at point.
INPUT can be a URL or a free-text description of a source (e.g.,
\"The Wealth of Nations by Adam Smith, 1776\").

Requires the `gptel' package."
  (interactive "sURL or source description: ")
  (unless (require 'gptel nil t)
    (user-error "This command requires the `gptel' package"))
  (when (string-empty-p (string-trim input))
    (user-error "Input cannot be empty"))
  (let ((buf (current-buffer))
        (pos (point-marker)))
    (message "Generating citation...")
    (gptel-request
     (format "Generate a Wikipedia citation for: %s\n\nToday's date is %s."
             input
             (format-time-string "%Y-%m-%d"))
     :system wikipedia-ai-citation-system-prompt
     :stream nil
     :callback
     (lambda (response info)
       (if (not (stringp response))
           (message "Citation generation failed: %s"
                    (plist-get info :status))
         (let ((citation (string-trim response)))
           (if (buffer-live-p buf)
               (with-current-buffer buf
                 (save-excursion
                   (goto-char pos)
                   (insert citation))
                 (message "Citation inserted."))
             (message "Buffer no longer exists.  Citation: %s" citation))))))))

(provide 'wikipedia-ai)

;;; wikipedia-ai.el ends here
