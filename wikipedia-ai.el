;;; wikipedia-ai.el --- AI-powered commands for wikipedia.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Pablo Stafforini

;;; Commentary:

;; This module provides AI-powered commands using gptel as the LLM
;; backend.  Currently supports generating Wikipedia-formatted
;; citations and AI-powered edit summaries.

;;; Code:

(require 'cl-lib)
(require 'wikipedia-common)

(declare-function gptel-request "gptel-request")
(declare-function gptel-get-backend "gptel")
(declare-function gptel-backend-models "gptel-openai")
(declare-function wp--ensure-logged-in "wikipedia-adapter")
(declare-function wikipedia--diff-to-live-text "wikipedia-diff")

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
The value can be:
- nil: infer from the model or use `gptel-backend'
- A string: use this backend for all AI commands
- An alist mapping command keys to backend name strings, e.g.
  \\='((cite . \"Gemini\")
    (review . \"Claude\")
    (t . \"Gemini\"))

  Valid keys: `cite', `summarize', `review', and `t' (default).
  Commands not listed fall back to the `t' entry, then inference."
  :type '(choice (const :tag "Infer from model or use gptel default" nil)
                 (string :tag "Backend for all commands")
                 (alist :tag "Per-command backend mapping"
                        :key-type (choice (const :tag "Default" t)
                                          (const :tag "Citation" cite)
                                          (const :tag "Edit summary" summarize)
                                          (const :tag "Watchlist review" review))
                        :value-type (string :tag "Backend name")))
  :group 'wikipedia-ai)

(defcustom wikipedia-ai-model nil
  "The gptel model for AI commands.
The value can be:
- nil: use `gptel-model'
- A symbol: use this model for all AI commands
- An alist mapping command keys to model symbols, e.g.
  \\='((cite . gemini-2.0-flash)
    (review . claude-sonnet-4-5-20250514)
    (t . gemini-2.0-flash-lite))

  Valid keys: `cite', `summarize', `review', and `t' (default).
  Commands not listed fall back to the `t' entry, then `gptel-model'."
  :type '(choice (const :tag "Use gptel default" nil)
                 (symbol :tag "Model for all commands")
                 (alist :tag "Per-command model mapping"
                        :key-type (choice (const :tag "Default" t)
                                          (const :tag "Citation" cite)
                                          (const :tag "Edit summary" summarize)
                                          (const :tag "Watchlist review" review))
                        :value-type (symbol :tag "Model name")))
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

(defun wikipedia-ai--resolve (backend-name model-name)
  "Return (BACKEND . MODEL) given BACKEND-NAME and MODEL-NAME overrides.
When BACKEND-NAME is non-nil, look it up via `gptel-get-backend'.
When MODEL-NAME is non-nil, infer the backend from the model.
Falls back to `gptel-backend' and `gptel-model'."
  (let* ((model (or model-name gptel-model))
         (backend (cond
                   (backend-name
                    (gptel-get-backend backend-name))
                   (model-name
                    (or (wikipedia-ai--find-backend-for-model model-name)
                        gptel-backend))
                   (t gptel-backend))))
    (cons backend model)))

(defun wikipedia-ai--lookup-model (command)
  "Look up the model for COMMAND from `wikipedia-ai-model'.
COMMAND is a key symbol (e.g. `cite', `summarize', `review').
Returns a model symbol or nil."
  (cond
   ((null wikipedia-ai-model) nil)
   ((consp wikipedia-ai-model)
    (or (alist-get command wikipedia-ai-model)
        (alist-get t wikipedia-ai-model)))
   (t wikipedia-ai-model)))

(defun wikipedia-ai--lookup-backend (command)
  "Look up the backend for COMMAND from `wikipedia-ai-backend'.
COMMAND is a key symbol (e.g. `cite', `summarize', `review').
Returns a backend name string or nil."
  (cond
   ((null wikipedia-ai-backend) nil)
   ((consp wikipedia-ai-backend)
    (or (alist-get command wikipedia-ai-backend)
        (alist-get t wikipedia-ai-backend)))
   (t wikipedia-ai-backend)))

(defun wikipedia-ai--resolve-backend-and-model (&optional command)
  "Return (backend . model) for AI COMMAND.
COMMAND is a key symbol (e.g. `cite', `summarize', `review')
used to look up per-command overrides in `wikipedia-ai-model'
and `wikipedia-ai-backend'."
  (wikipedia-ai--resolve (wikipedia-ai--lookup-backend command)
                         (wikipedia-ai--lookup-model command)))

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
  (let* ((resolved (wikipedia-ai--resolve-backend-and-model 'cite))
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

;;;; Edit summary generation

(defcustom wikipedia-ai-summarize-system-prompt
  "You are a Wikipedia edit summary generator.  Given a unified diff of \
changes to a Wikipedia article, produce a concise edit summary following \
Wikipedia conventions (see [[Help:Edit summary]]).

Rules:
- Maximum 500 characters, but aim for brevity (one concise sentence when \
possible).
- Summarize WHAT changed and, if inferable from context, WHY.
- Be specific: name the section, fact, or nature of the change.
- Use standard Wikipedia abbreviations when appropriate: ce (copy edit), \
sp (typo/spelling), fmt (formatting), ref/refs (references/citations), \
+ (added), - (removed), rv (revert), cl (cleanup), gr (grammar), \
lk/wl (wikilink), cat (category), dab (disambiguation).
- For multiple changes, mention the most significant and append \"; misc\" \
or similar.
- Never be vague (e.g. \"I made some changes\"); always be specific.
- Do not use section markers like /* Section */; the system adds those \
automatically when editing a section.
- Output ONLY the edit summary text.  No surrounding explanation, quotes, \
or markup fences."
  "System prompt for the edit summary generation command."
  :type 'string
  :group 'wikipedia-ai)

(defcustom wikipedia-ai-summarize-auto nil
  "Whether to auto-generate an AI edit summary when publishing.
When non-nil, `wikipedia-publish' generates an AI-powered edit
summary before prompting, pre-filling the minibuffer with the
suggestion.  The user can edit the suggestion before confirming."
  :type 'boolean
  :group 'wikipedia-ai)

(defvar-local wikipedia-ai--pending-summary nil
  "AI-generated edit summary awaiting use by `wikipedia-publish'.")

;;;###autoload
(defun wikipedia-ai-summarize ()
  "Generate an AI edit summary for the current buffer's changes.
Compares the buffer against the live version on Wikipedia and sends
the diff to an LLM to produce a concise edit summary.  The result
is stored for use by `wikipedia-publish'.

Requires the `gptel' package."
  (interactive)
  (unless (require 'gptel nil t)
    (user-error "This command requires the `gptel' package"))
  (wp--ensure-logged-in)
  (let ((diff-text (wikipedia--diff-to-live-text)))
    (unless diff-text
      (user-error "No changes to summarize (buffer matches live version)"))
    (wikipedia-ai--request-summary diff-text (current-buffer))))

(defun wikipedia-ai--request-summary (diff-text buffer &optional callback)
  "Request an AI edit summary for DIFF-TEXT.
BUFFER is the editing buffer where the summary will be stored.
Optional CALLBACK is called with the generated summary string,
or nil on failure."
  (let* ((resolved (wikipedia-ai--resolve-backend-and-model 'summarize))
         (gptel-backend (car resolved))
         (gptel-model (cdr resolved))
         (gptel-use-tools nil)
         (gptel-use-context nil))
    (message "Generating edit summary...")
    (gptel-request
     (format "Generate an edit summary for the following diff:\n\n%s" diff-text)
     :system wikipedia-ai-summarize-system-prompt
     :transforms nil
     :callback
     (lambda (response info)
       (if (not (stringp response))
           (progn
             (message "Edit summary generation failed: %s"
                      (plist-get info :status))
             (when callback
               (funcall callback nil)))
         (let ((summary (string-trim response)))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (setq wikipedia-ai--pending-summary summary)))
           (message "Edit summary: %s" summary)
           (when callback
             (funcall callback summary))))))))

(provide 'wikipedia-ai)

;;; wikipedia-ai.el ends here
