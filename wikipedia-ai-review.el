;;; wikipedia-ai-review.el --- AI-powered watchlist review for wikipedia.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Pablo Stafforini

;;; Commentary:

;; This module uses AI to review watchlist changes and highlight edits
;; that may warrant the user's attention.  It analyzes the unified diff
;; of each watchlist group and uses an LLM to determine which edits are
;; noteworthy based on user-configurable criteria.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'tabulated-list)
(require 'wikipedia-ai)
(require 'wikipedia-cache)
(require 'wikipedia-common)
(require 'wikipedia-diff)

(declare-function gptel-request "gptel-request")
(declare-function gptel-get-backend "gptel")

(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-use-tools)
(defvar gptel-use-context)
(defvar wikipedia-watchlist-mode-map)
(defvar wikipedia-watchlist--grouped-entries)

;;;; User options

(defcustom wikipedia-ai-review-system-prompt
  "You are a Wikipedia edit reviewer.  You will be shown a unified diff \
of changes to a Wikipedia article.  Analyze the diff and determine whether \
the edit is noteworthy and warrants manual review.

Respond with a JSON object containing two fields:
- \"noteworthy\": a boolean indicating whether the edit warrants review
- \"reason\": a brief (one sentence) explanation of your assessment

Output ONLY the JSON object, with no surrounding text or markup fences."
  "System prompt for the AI watchlist review command."
  :type 'string
  :group 'wikipedia-ai)

(defcustom wikipedia-ai-review-prompt
  "Identify edits that make substantive changes to the article, such as \
adding or removing sentences, changing factual claims, or altering the \
article's structure.  Ignore minor edits like typo fixes, whitespace \
changes, or formatting adjustments."
  "User prompt describing which edits to flag during AI review.
This prompt tells the AI what kinds of edits you consider noteworthy."
  :type 'string
  :group 'wikipedia-ai)

(defcustom wikipedia-ai-review-backend nil
  "The gptel backend name for AI review, e.g. \"Gemini\" or \"Claude\".
When nil, the backend is inferred from `wikipedia-ai-review-model',
falling back to `gptel-backend'."
  :type '(choice (const :tag "Infer from model or use gptel default" nil)
                 (string :tag "Backend name"))
  :group 'wikipedia-ai)

(defcustom wikipedia-ai-review-model nil
  "The gptel model for AI review.
When nil, defaults to `gptel-model'."
  :type '(choice (const :tag "Use gptel default" nil)
                 (symbol :tag "Model name"))
  :group 'wikipedia-ai)

;;;; Faces

(defface wikipedia-ai-review-noteworthy
  '((t :inherit warning))
  "Face for noteworthy entries in AI review results.")

(defface wikipedia-ai-review-minor
  '((t :inherit shadow))
  "Face for minor entries in AI review results.")

;;;; Internal state

(defvar wikipedia-ai-review--queue nil
  "Queue of groups to process: list of (TITLE OLD-REVID REVID).")

(defvar wikipedia-ai-review--results nil
  "Accumulated review results during processing.")

(defvar wikipedia-ai-review--total 0
  "Total number of groups being reviewed.")

(defvar wikipedia-ai-review--generation 0
  "Generation counter to discard stale callbacks.")

;;;; Backend resolution

(defun wikipedia-ai-review--resolve-backend-and-model ()
  "Return (BACKEND . MODEL) for review commands.
Resolves `wikipedia-ai-review-backend' and `wikipedia-ai-review-model',
inferring the backend from the model when needed."
  (let* ((model (or wikipedia-ai-review-model gptel-model))
         (backend (cond
                   (wikipedia-ai-review-backend
                    (gptel-get-backend wikipedia-ai-review-backend))
                   (wikipedia-ai-review-model
                    (or (wikipedia-ai--find-backend-for-model
                         wikipedia-ai-review-model)
                        gptel-backend))
                   (t gptel-backend))))
    (cons backend model)))

;;;; Diff text generation

(defun wikipedia-ai-review--get-diff-text (title old-revid revid)
  "Return the unified diff text between OLD-REVID and REVID for TITLE.
Returns nil if revisions cannot be fetched."
  (condition-case nil
      (let* ((from-content (or (wikipedia--cache-get old-revid)
                               (wp--get-revision-content title old-revid)))
             (to-content (or (wikipedia--cache-get revid)
                             (wp--get-revision-content title revid))))
        (when (and from-content to-content)
          (let* ((from-file (wikipedia--write-temp-file from-content old-revid))
                 (to-file (wikipedia--write-temp-file to-content revid)))
            (unwind-protect
                (wikipedia--generate-unified-diff
                 from-file to-file old-revid revid)
              (delete-file from-file)
              (delete-file to-file)))))
    (error nil)))

;;;; Response parsing

(defun wikipedia-ai-review--parse-response (response)
  "Parse RESPONSE JSON from the LLM.
Returns (NOTEWORTHY-P REASON)."
  (condition-case nil
      (let* ((cleaned (replace-regexp-in-string
                       "\\`[ \t\n]*```\\(?:json\\)?[ \t\n]*" ""
                       (replace-regexp-in-string
                        "[ \t\n]*```[ \t\n]*\\'" ""
                        (string-trim response))))
             (json (json-parse-string cleaned
                                      :object-type 'alist
                                      :false-object nil))
             (noteworthy (alist-get 'noteworthy json))
             (reason (or (alist-get 'reason json) "")))
        (list (eq noteworthy t) reason))
    (error (list nil (format "Could not parse response: %s"
                             (truncate-string-to-width response 100))))))

;;;; Async processing

(defun wikipedia-ai-review--process-next ()
  "Process the next group in the review queue."
  (if (null wikipedia-ai-review--queue)
      (wikipedia-ai-review--show-results)
    (let* ((group (pop wikipedia-ai-review--queue))
           (title (nth 0 group))
           (old-revid (nth 1 group))
           (revid (nth 2 group))
           (processed (- wikipedia-ai-review--total
                         (length wikipedia-ai-review--queue) 1)))
      (message "Reviewing %d/%d: %s..."
               (1+ processed) wikipedia-ai-review--total title)
      (let ((diff-text (wikipedia-ai-review--get-diff-text
                        title old-revid revid)))
        (if (or (null diff-text) (string-empty-p (string-trim diff-text)))
            (progn
              (push (list title nil "No changes detected" old-revid revid)
                    wikipedia-ai-review--results)
              (wikipedia-ai-review--process-next))
          (wikipedia-ai-review--send-to-llm
           title old-revid revid diff-text))))))

(defun wikipedia-ai-review--send-to-llm (title old-revid revid diff-text)
  "Send DIFF-TEXT for TITLE to the LLM for review.
OLD-REVID and REVID identify the revision range."
  (let* ((resolved (wikipedia-ai-review--resolve-backend-and-model))
         (gptel-backend (car resolved))
         (gptel-model (cdr resolved))
         (gptel-use-tools nil)
         (gptel-use-context nil)
         (gen wikipedia-ai-review--generation)
         (prompt (format "%s\n\nArticle: %s\n\nDiff:\n%s"
                         wikipedia-ai-review-prompt title diff-text)))
    (gptel-request prompt
     :system wikipedia-ai-review-system-prompt
     :transforms nil
     :callback
     (lambda (response info)
       (when (= gen wikipedia-ai-review--generation)
         (let ((result (if (stringp response)
                           (wikipedia-ai-review--parse-response response)
                         (list nil (format "LLM error: %s"
                                           (plist-get info :status))))))
           (push (list title (nth 0 result) (nth 1 result) old-revid revid)
                 wikipedia-ai-review--results)
           (wikipedia-ai-review--process-next)))))))

;;;; Gathering watchlist data

(defun wikipedia-ai-review--gather-groups ()
  "Gather watchlist groups for review.
Returns a list of (TITLE OLD-REVID REVID) for each group."
  (let ((buffer (get-buffer "*Wikipedia Watchlist*")))
    (unless buffer
      (user-error "No watchlist buffer found; run `wikipedia-watchlist' first"))
    (with-current-buffer buffer
      (unless wikipedia-watchlist--grouped-entries
        (user-error "Watchlist has no entries"))
      (mapcar
       (lambda (group)
         (let* ((title (car group))
                (entries (cdr group))
                (revids (mapcar (lambda (e) (alist-get 'revid e)) entries))
                (old-revids (mapcar (lambda (e) (alist-get 'old_revid e))
                                    entries)))
           (list title
                 (apply #'min old-revids)
                 (apply #'max revids))))
       wikipedia-watchlist--grouped-entries))))

;;;; Results display

(defvar wikipedia-ai-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'wikipedia-ai-review-show-diff)
    (define-key map "d" #'wikipedia-ai-review-show-diff)
    (define-key map "v" #'wikipedia-ai-review-show-diff)
    (define-key map "o" #'wikipedia-ai-review-open-page)
    (define-key map "b" #'wikipedia-ai-review-browse)
    (define-key map "g" #'wikipedia-ai-review-refresh)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `wikipedia-ai-review-mode'.")

(define-derived-mode wikipedia-ai-review-mode tabulated-list-mode "WP-Review"
  "Major mode for browsing AI review results.
\\{wikipedia-ai-review-mode-map}"
  (setq tabulated-list-format
        [("" 1 nil)
         ("Page" 35 t)
         ("Reason" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header))

(defun wikipedia-ai-review--make-entry (result)
  "Create a tabulated-list entry from RESULT.
RESULT is (TITLE NOTEWORTHY REASON OLD-REVID REVID)."
  (let* ((title (nth 0 result))
         (noteworthy (nth 1 result))
         (reason (nth 2 result))
         (old-revid (nth 3 result))
         (revid (nth 4 result))
         (face (if noteworthy
                   'wikipedia-ai-review-noteworthy
                 'wikipedia-ai-review-minor))
         (indicator (if noteworthy
                        (propertize "*" 'face face)
                      " ")))
    (list (list title old-revid revid)
          (vector
           indicator
           (propertize title 'face face)
           (propertize (or reason "") 'face face)))))

(defun wikipedia-ai-review--show-results ()
  "Display the accumulated review results."
  (let* ((results (nreverse wikipedia-ai-review--results))
         (noteworthy-count (cl-count-if (lambda (r) (nth 1 r)) results))
         (buffer (get-buffer-create "*Wikipedia AI Review*")))
    (with-current-buffer buffer
      (wikipedia-ai-review-mode)
      (setq tabulated-list-entries
            (mapcar #'wikipedia-ai-review--make-entry results))
      (tabulated-list-print t)
      (goto-char (point-min)))
    (pop-to-buffer buffer)
    (message "Review complete: %d/%d noteworthy"
             noteworthy-count (length results))))

;;;; Interactive commands

(defun wikipedia-ai-review-show-diff ()
  "Show the diff for the entry at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id
      (user-error "No entry at point"))
    (wikipedia--show-diff (nth 1 id) (nth 2 id) (nth 0 id))))

(defun wikipedia-ai-review-open-page ()
  "Open the page at point for editing."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id
      (user-error "No entry at point"))
    (wp--open-page-buffer (nth 0 id))))

(defun wikipedia-ai-review-browse ()
  "Browse the page at point in a web browser."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id
      (user-error "No entry at point"))
    (browse-url (wikipedia--page-url (nth 0 id)))))

(defun wikipedia-ai-review-refresh ()
  "Re-run the AI review on the current watchlist."
  (interactive)
  (wikipedia-ai-review-watchlist))

;;;###autoload
(defun wikipedia-ai-review-watchlist ()
  "Review watchlist changes using AI to identify noteworthy edits.
Analyzes the unified diff of each watchlist group and uses an LLM to
determine which edits match the criteria in `wikipedia-ai-review-prompt'.

Results are displayed in a dedicated buffer where you can view diffs,
open pages, or browse them in a web browser.

Requires the `gptel' package."
  (interactive)
  (unless (require 'gptel nil t)
    (user-error "This command requires the `gptel' package"))
  (let ((groups (wikipedia-ai-review--gather-groups)))
    (unless groups
      (user-error "No watchlist entries to review"))
    (cl-incf wikipedia-ai-review--generation)
    (setq wikipedia-ai-review--queue groups
          wikipedia-ai-review--results nil
          wikipedia-ai-review--total (length groups))
    (message "Starting AI review of %d watchlist entries..." (length groups))
    (wikipedia-ai-review--process-next)))

;;;; Keybinding integration

(with-eval-after-load 'wikipedia-watchlist
  (define-key wikipedia-watchlist-mode-map "R"
              #'wikipedia-ai-review-watchlist))

(provide 'wikipedia-ai-review)

;;; wikipedia-ai-review.el ends here
