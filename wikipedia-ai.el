;;; wikipedia-ai.el --- AI-powered commands for wikipedia.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Pablo Stafforini

;;; Commentary:

;; This module provides AI-powered commands using gptel as the LLM
;; backend.  Currently supports generating Wikipedia-formatted
;; citations from a URL or free-text description.

;;; Code:

(require 'cl-lib)
(require 'wikipedia-common)

(declare-function gptel-request "gptel-request")
(declare-function gptel-get-backend "gptel")
(declare-function gptel-backend-models "gptel-openai")

(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-use-tools)
(defvar gptel-use-context)
(defvar gptel--known-backends)

(defgroup wikipedia-ai nil
  "AI-powered Wikipedia editing commands."
  :group 'wikipedia
  :prefix "wikipedia-ai-")

(defcustom wikipedia-ai-backend nil
  "The gptel backend name for AI commands, e.g. \"Gemini\" or \"Claude\".
When nil, the backend is inferred from `wikipedia-ai-model', falling
back to `gptel-backend'."
  :type '(choice (const :tag "Infer from model or use gptel default" nil)
                 (string :tag "Backend name"))
  :group 'wikipedia-ai)

(defcustom wikipedia-ai-model nil
  "The gptel model for AI commands, e.g. `claude-sonnet-4-5-20250514'.
When nil, defaults to `gptel-model'."
  :type '(choice (const :tag "Use gptel default" nil)
                 (symbol :tag "Model name"))
  :group 'wikipedia-ai)

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

(defun wikipedia-ai--find-backend-for-model (model)
  "Return the gptel backend that provides MODEL, or nil."
  (cl-loop for (_name . backend) in gptel--known-backends
           when (member model (gptel-backend-models backend))
           return backend))

(defun wikipedia-ai--resolve-backend-and-model ()
  "Return (backend . model) for AI commands.
Resolves `wikipedia-ai-backend' and `wikipedia-ai-model', inferring the
backend from the model when needed."
  (let* ((model (or wikipedia-ai-model gptel-model))
         (backend (cond
                   (wikipedia-ai-backend
                    (gptel-get-backend wikipedia-ai-backend))
                   (wikipedia-ai-model
                    (or (wikipedia-ai--find-backend-for-model wikipedia-ai-model)
                        gptel-backend))
                   (t gptel-backend))))
    (cons backend model)))

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
  (let* ((resolved (wikipedia-ai--resolve-backend-and-model))
         (gptel-backend (car resolved))
         (gptel-model (cdr resolved))
         (gptel-use-tools nil)
         (gptel-use-context nil)
         (buf (current-buffer))
         (pos (point-marker)))
    (message "Generating citation...")
    (gptel-request
     (format "Generate a Wikipedia citation for: %s\n\nToday's date is %s."
             input
             (format-time-string "%Y-%m-%d"))
     :system wikipedia-ai-citation-system-prompt
     :transforms nil
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
