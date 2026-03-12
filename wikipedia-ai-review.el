;;; wikipedia-ai-review.el --- AI-powered watchlist review for wikipedia.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Pablo Stafforini

;;; Commentary:

;; This module uses AI to score watchlist changes by review priority.
;; Each watchlist group is assigned a score between 0.0 and 1.0 based
;; on how much the edit warrants manual review.  Scores are displayed
;; in the watchlist buffer and can be used to sort entries.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'wikipedia-ai)
(require 'wikipedia-common)
(require 'wikipedia-db)
(require 'wikipedia-diff)

(declare-function gptel-request "gptel-request")
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-use-tools)
(defvar gptel-use-context)
(defvar wikipedia-watchlist-mode-map)
(defvar wikipedia-watchlist--grouped-entries)
(defvar wikipedia-watchlist--scores)

(declare-function wikipedia-watchlist--rebuild-list "wikipedia-watchlist")

;;;; User options

(defcustom wikipedia-ai-review-system-prompt
  "You are a Wikipedia edit reviewer.  You will be shown a unified diff \
of one or more edits to a Wikipedia article, along with metadata about \
the individual edits (authors and edit summaries).  Evaluate how much \
the change warrants manual review, considering all edits as a whole.

Rate on a scale from 0.0 to 1.0, where 0.0 means the change is \
trivial and needs no review, and 1.0 means it is highly significant \
and should definitely be reviewed.

Respond with a JSON object containing two fields:
- \"score\": a float between 0.0 and 1.0
- \"reason\": a brief (one sentence) explanation of your assessment, \
with the edit(s) as the tacit subject (e.g. \"adds a new section\" not \
\"the edit adds a new section\")

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

(defcustom wikipedia-ai-review-auto nil
  "When non-nil, automatically score watchlist entries after refresh.
Runs `wikipedia-ai-review-watchlist' each time the watchlist is
refreshed, so entries are always scored without pressing \\`R'."
  :type 'boolean
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

;;;; Internal state

(defvar wikipedia-ai-review--queue nil
  "Queue of groups to process: list of (TITLE OLD-REVID REVID ENTRIES).")

(defvar wikipedia-ai-review--total 0
  "Total number of groups being reviewed.")

(defvar wikipedia-ai-review--scored 0
  "Number of groups scored so far in the current review.")

(defvar wikipedia-ai-review--generation 0
  "Generation counter to discard stale callbacks.")

(defvar wikipedia-ai-review--watchlist-buffer nil
  "The watchlist buffer being scored.")

(defvar wikipedia-ai-review--scored-revisions
  (make-hash-table :test 'equal)
  "Map from TITLE to (OLD-REVID . REVID) for the last scored revision range.")

;;;; Backend resolution

(defun wikipedia-ai-review--resolve-backend-and-model ()
  "Return (BACKEND . MODEL) for review commands."
  (wikipedia-ai--resolve wikipedia-ai-review-backend
                         wikipedia-ai-review-model))

;;;; Diff text generation
;; Reuses `wikipedia--get-diff-text' from wikipedia-diff.el, which
;; fetches revision content via the same path as the interactive diff
;; commands (pressing `d' in the watchlist).

;;;; Response parsing

(defun wikipedia-ai-review--parse-response (response)
  "Parse RESPONSE JSON from the LLM.
Returns (SCORE . REASON) where SCORE is a float 0.0-1.0, or nil on failure."
  (condition-case nil
      (let* ((cleaned (replace-regexp-in-string
                       "\\`[ \t\n]*```\\(?:json\\)?[ \t\n]*" ""
                       (replace-regexp-in-string
                        "[ \t\n]*```[ \t\n]*\\'" ""
                        (string-trim response))))
             (json (json-parse-string cleaned
                                      :object-type 'alist
                                      :false-object nil))
             (score (alist-get 'score json))
             (reason (or (alist-get 'reason json) "")))
        (if (and score (numberp score) (<= 0.0 score) (<= score 1.0))
            (cons score reason)
          (cons nil (format "Invalid score: %s" score))))
    (error (cons nil (format "Could not parse: %s"
                             (truncate-string-to-width response 100))))))

;;;; Storing results

(defun wikipedia-ai-review--store-score (title score-data old-revid revid)
  "Store SCORE-DATA for TITLE and refresh the watchlist display.
OLD-REVID and REVID record which revision range was scored."
  (wikipedia-db-set-ai-score title (car score-data) (cdr score-data)
                             old-revid revid)
  (puthash title (cons old-revid revid) wikipedia-ai-review--scored-revisions)
  (let ((buffer wikipedia-ai-review--watchlist-buffer))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (puthash title score-data wikipedia-watchlist--scores)
        (wikipedia-watchlist--rebuild-list)
        (tabulated-list-print t)))))

;;;; Async processing

(defun wikipedia-ai-review--process-next ()
  "Process the next group in the review queue."
  (if (null wikipedia-ai-review--queue)
      (wikipedia-ai-review--done)
    (let* ((group (pop wikipedia-ai-review--queue))
           (title (nth 0 group))
           (old-revid (nth 1 group))
           (revid (nth 2 group))
           (entries (nth 3 group)))
      (cl-incf wikipedia-ai-review--scored)
      (message "Scoring %d/%d: %s..."
               wikipedia-ai-review--scored wikipedia-ai-review--total title)
      (let ((diff-text (wikipedia--get-diff-text
                        old-revid revid title)))
        (cond
         ((null diff-text)
          (wikipedia-ai-review--process-next))
         ((string-empty-p (string-trim diff-text))
          (wikipedia-ai-review--store-score
           title (cons 0.0 "No net change") old-revid revid)
          (wikipedia-ai-review--process-next))
         (t
          (wikipedia-ai-review--send-to-llm
           title old-revid revid diff-text entries)))))))

(defun wikipedia-ai-review--format-edit-metadata (entries)
  "Format ENTRIES into a metadata summary for the AI prompt.
Lists the number of edits and each edit's author and summary,
in chronological order (oldest first)."
  (let* ((sorted (sort (copy-sequence entries)
                       (lambda (a b)
                         (string< (alist-get 'timestamp a)
                                  (alist-get 'timestamp b)))))
         (count (length sorted))
         (lines (mapcar
                 (lambda (e)
                   (format "- %s: %s"
                           (or (alist-get 'user e) "?")
                           (let ((c (alist-get 'comment e)))
                             (if (or (null c) (string-empty-p c))
                                 "(no summary)"
                               c))))
                 sorted)))
    (format "%d edit%s:\n%s"
            count (if (= count 1) "" "s")
            (string-join lines "\n"))))

(defun wikipedia-ai-review--send-to-llm (title old-revid revid diff-text
                                                &optional entries)
  "Send DIFF-TEXT for TITLE to the LLM for scoring.
OLD-REVID and REVID identify the revision range.  ENTRIES, when
provided, are the individual watchlist entries in the group."
  (let* ((resolved (wikipedia-ai-review--resolve-backend-and-model))
         (gptel-backend (car resolved))
         (gptel-model (cdr resolved))
         (gptel-use-tools nil)
         (gptel-use-context nil)
         (gen wikipedia-ai-review--generation)
         (metadata (when entries
                     (wikipedia-ai-review--format-edit-metadata entries)))
         (prompt (if metadata
                     (format "%s\n\nArticle: %s\n\n%s\n\nDiff:\n%s"
                             wikipedia-ai-review-prompt title metadata
                             diff-text)
                   (format "%s\n\nArticle: %s\n\nDiff:\n%s"
                           wikipedia-ai-review-prompt title diff-text))))
    (gptel-request prompt
     :system wikipedia-ai-review-system-prompt
     :transforms nil
     :callback
     (lambda (response info)
       (when (= gen wikipedia-ai-review--generation)
         (if (stringp response)
             (let ((result (wikipedia-ai-review--parse-response response)))
               (when (car result)
                 (wikipedia-ai-review--store-score
                  title result old-revid revid)))
           (message "Scoring failed for %s: %s"
                    title (plist-get info :status)))
         (wikipedia-ai-review--process-next))))))

(defun wikipedia-ai-review--done ()
  "Called when all groups have been scored."
  (message "AI review complete (%d entries scored). Press S to sort by score."
           wikipedia-ai-review--scored))

;;;; Gathering watchlist data

(defun wikipedia-ai-review--gather-groups ()
  "Gather watchlist groups that need scoring.
Returns a list of (TITLE OLD-REVID REVID ENTRIES) for each unscored or
stale group, skipping entries already scored for the same revision range."
  (let ((buffer (get-buffer "*Wikipedia Watchlist*")))
    (unless buffer
      (user-error "No watchlist buffer found; run `wikipedia-watchlist' first"))
    (with-current-buffer buffer
      (unless wikipedia-watchlist--grouped-entries
        (user-error "Watchlist has no entries"))
      (cl-loop for group in wikipedia-watchlist--grouped-entries
               for title = (car group)
               for entries = (cdr group)
               for revids = (mapcar (lambda (e) (alist-get 'revid e)) entries)
               for old-revids = (mapcar (lambda (e) (alist-get 'old_revid e))
                                        entries)
               for old-revid = (apply #'min old-revids)
               for revid = (apply #'max revids)
               for cached = (gethash title
                                     wikipedia-ai-review--scored-revisions)
               unless (and cached
                          (equal (car cached) old-revid)
                          (equal (cdr cached) revid))
               collect (list title old-revid revid entries)))))

;;;###autoload
(defun wikipedia-ai-review-watchlist ()
  "Score watchlist changes using AI to prioritize review.
Analyzes the unified diff of each watchlist group and assigns a score
between 0.0 and 1.0 based on the criteria in `wikipedia-ai-review-prompt'.

Scores appear in the watchlist buffer's Score column.  Press S to sort
by score (highest first), or I to see the AI's reasoning.

Requires the `gptel' package."
  (interactive)
  (unless (require 'gptel nil t)
    (user-error "This command requires the `gptel' package"))
  (let ((groups (wikipedia-ai-review--gather-groups)))
    (if (null groups)
        (when (called-interactively-p 'any)
          (message "All watchlist entries already scored for current revisions."))
      (setq wikipedia-ai-review--watchlist-buffer
            (get-buffer "*Wikipedia Watchlist*"))
      (cl-incf wikipedia-ai-review--generation)
      (setq wikipedia-ai-review--queue groups
            wikipedia-ai-review--scored 0
            wikipedia-ai-review--total (length groups))
      (message "Starting AI review of %d watchlist entries..." (length groups))
      (wikipedia-ai-review--process-next))))

;;;; Score restoration and keybinding integration

(defun wikipedia-ai-review--restore-cached-scores ()
  "Restore scores from the database to the watchlist buffer."
  (dolist (row (wikipedia-db-get-ai-scores))
    (let ((title (nth 0 row))
          (score (nth 1 row))
          (reason (nth 2 row))
          (old-revid (nth 3 row))
          (revid (nth 4 row)))
      (puthash title (cons score reason) wikipedia-watchlist--scores)
      (puthash title (cons old-revid revid)
               wikipedia-ai-review--scored-revisions))))

(add-hook 'wikipedia-watchlist-mode-hook
          #'wikipedia-ai-review--restore-cached-scores)

(defun wikipedia-ai-review--maybe-auto-score (&rest _)
  "Run AI review after watchlist refresh if `wikipedia-ai-review-auto' is set."
  (when (and wikipedia-ai-review-auto
             (require 'gptel nil t))
    (wikipedia-ai-review-watchlist)))

(with-eval-after-load 'wikipedia-watchlist
  (define-key wikipedia-watchlist-mode-map "R"
              #'wikipedia-ai-review-watchlist)
  (advice-add 'wikipedia-watchlist-refresh :after
              #'wikipedia-ai-review--maybe-auto-score))

(provide 'wikipedia-ai-review)

;;; wikipedia-ai-review.el ends here
