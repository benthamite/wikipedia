;;; wikipedia-diff.el --- Diff engine and follow mode for wikipedia.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Pablo Stafforini

;;; Commentary:

;; This module provides the diff engine (unified and ediff) and the
;; diff-follow minor mode for wikipedia.el.

;;; Code:

(require 'wikipedia-adapter)
(require 'wikipedia-cache)

(defcustom wikipedia-diff-function 'unified
  "Function to use for displaying diffs.
When set to `unified', show diffs in a single buffer using `diff-mode'.
When set to `ediff', use `ediff-buffers' with side-by-side comparison."
  :type '(choice (const :tag "Unified diff (single buffer)" unified)
                 (const :tag "Ediff (side-by-side)" ediff))
  :group 'wikipedia)

;;;; Diff follow mode

(defvar-local wikipedia-diff-follow-mode nil
  "Non-nil if diff follow mode is enabled in this buffer.")

(defvar-local wikipedia-diff-follow--last-revid nil
  "The last revision ID for which a diff was shown in follow mode.")

(defvar wikipedia-diff-follow--buffer-name "*WP Diff*"
  "Buffer name for diff follow mode output.")

(defvar wikipedia-history--page-title)
(defvar wikipedia--buffer-page-title)

(declare-function wikipedia--revid-at-point "wikipedia-common")
(declare-function wikipedia-history--revision-at-point "wikipedia-history")
(declare-function wikipedia-watchlist--entry-at-point "wikipedia-watchlist")
(declare-function wikipedia-watchlist--mark-at-point-read "wikipedia-watchlist")
(declare-function wikipedia-user--contrib-at-point "wikipedia-user")

(defun wikipedia-diff-follow--show ()
  "Show diff for the entry at point if it changed."
  (when (and wikipedia-diff-follow-mode
             (not (eq wikipedia-diff-function 'ediff)))
    (let ((revid (wikipedia--revid-at-point)))
      (when (and revid (not (equal revid wikipedia-diff-follow--last-revid)))
        (setq wikipedia-diff-follow--last-revid revid)
        (wikipedia-diff-follow--display-diff)
        (wikipedia-diff-follow--mark-read)))))

(defun wikipedia-diff-follow--display-diff ()
  "Display the diff for the current entry in the follow buffer."
  (condition-case err
      (let ((diff-info (wikipedia-diff-follow--get-diff-info)))
        (when diff-info
          (let ((from-rev (plist-get diff-info :from-rev))
                (to-rev (plist-get diff-info :to-rev))
                (title (plist-get diff-info :title)))
            (when (and from-rev to-rev title)
              (wikipedia-diff-follow--fetch-and-display
               from-rev to-rev title)))))
    (error (message "Diff follow: %s" (error-message-string err)))))

(defun wikipedia-diff-follow--get-diff-info ()
  "Get diff info for the entry at point.
Returns a plist with :from-rev, :to-rev, and :title, or nil."
  (cond
   ((derived-mode-p 'wikipedia-history-mode)
    (wikipedia-diff-follow--history-info))
   ((derived-mode-p 'wikipedia-watchlist-mode)
    (wikipedia-diff-follow--watchlist-info))
   ((derived-mode-p 'wikipedia-user-contributions-mode)
    (wikipedia-diff-follow--contributions-info))
   (t nil)))

(defun wikipedia-diff-follow--history-info ()
  "Get diff info for history mode."
  (let* ((rev (wikipedia-history--revision-at-point))
         (revid (alist-get 'revid rev))
         (parentid (alist-get 'parentid rev)))
    (when (and revid parentid (not (zerop parentid)))
      (list :from-rev parentid
            :to-rev revid
            :title wikipedia-history--page-title))))

(defun wikipedia-diff-follow--watchlist-info ()
  "Get diff info for watchlist mode."
  (let* ((entry (wikipedia-watchlist--entry-at-point))
         (title (alist-get 'title entry))
         (revid (alist-get 'revid entry))
         (old-revid (alist-get 'old_revid entry)))
    (when (and title revid old-revid)
      (list :from-rev old-revid
            :to-rev revid
            :title title))))

(defun wikipedia-diff-follow--contributions-info ()
  "Get diff info for user contributions mode."
  (let* ((contrib (wikipedia-user--contrib-at-point))
         (revid (alist-get 'revid contrib))
         (parentid (alist-get 'parentid contrib))
         (title (alist-get 'title contrib)))
    (when (and revid parentid (not (zerop parentid)) title)
      (list :from-rev parentid
            :to-rev revid
            :title title))))

(defun wikipedia-diff-follow--fetch-and-display (from-rev to-rev title)
  "Fetch and display diff between FROM-REV and TO-REV for TITLE."
  (let ((from-content (wikipedia--cache-get from-rev))
        (to-content (wikipedia--cache-get to-rev))
        (source-window (selected-window)))
    (if (and from-content to-content)
        (wikipedia-diff-follow--render-diff from-content to-content
                                            from-rev to-rev title source-window)
      (wikipedia-diff-follow--show-loading title from-rev to-rev source-window)
      (wikipedia-diff-follow--fetch-both-async
       from-rev to-rev title source-window))))

(defun wikipedia-diff-follow--show-loading (title from-rev to-rev source-window)
  "Show loading message in diff buffer for TITLE (FROM-REV to TO-REV).
SOURCE-WINDOW is preserved for later use."
  (let ((buf (get-buffer-create wikipedia-diff-follow--buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Loading diff..."))
      (setq buffer-read-only t)
      (setq header-line-format
            (format " %s: %d → %d (loading...)" title from-rev to-rev)))
    (wikipedia-diff-follow--display-buffer buf source-window)))

(defun wikipedia-diff-follow--fetch-both-async (from-rev to-rev title source-window)
  "Fetch both revisions async and display diff when both are ready.
FROM-REV and TO-REV are revision IDs, TITLE is the page title.
SOURCE-WINDOW is preserved for display."
  (let ((expected-revid wikipedia-diff-follow--last-revid)
        (source-buffer (current-buffer))
        (pending 2)
        (from-content (wikipedia--cache-get from-rev))
        (to-content (wikipedia--cache-get to-rev)))
    (cl-flet ((maybe-render
                ()
                (cl-decf pending)
                (when (zerop pending)
                  (setq from-content (or from-content (wikipedia--cache-get from-rev)))
                  (setq to-content (or to-content (wikipedia--cache-get to-rev)))
                  (when (and from-content to-content
                             (buffer-live-p source-buffer)
                             (with-current-buffer source-buffer
                               (eq expected-revid wikipedia-diff-follow--last-revid)))
                    (wikipedia-diff-follow--render-diff
                     from-content to-content from-rev to-rev title source-window)))))
      (if from-content
          (maybe-render)
        (wikipedia--fetch-revision-async
         title from-rev (lambda (_) (maybe-render))))
      (if to-content
          (maybe-render)
        (wikipedia--fetch-revision-async
         title to-rev (lambda (_) (maybe-render)))))))

(defun wikipedia-diff-follow--render-diff (from-content to-content from-rev to-rev title source-window)
  "Render diff between FROM-CONTENT and TO-CONTENT in the diff buffer.
FROM-REV and TO-REV are revision IDs, TITLE is the page title.
SOURCE-WINDOW is the window to avoid when displaying the diff."
  (let ((buf (wikipedia--render-unified-diff
              from-content to-content from-rev to-rev
              wikipedia-diff-follow--buffer-name)))
    (with-current-buffer buf
      (setq-local wikipedia--buffer-page-title title)
      (setq header-line-format
            (format " %s: %d → %d" title from-rev to-rev)))
    (wikipedia-diff-follow--display-buffer buf source-window)))

(defun wikipedia-diff-follow--display-buffer (buffer source-window)
  "Display BUFFER for diff follow mode, reusing other window if available.
SOURCE-WINDOW is the window to avoid displaying in."
  (let ((other-window (wikipedia-diff-follow--find-other-window source-window)))
    (if other-window
        (set-window-buffer other-window buffer)
      (display-buffer buffer
                      '((display-buffer-reuse-window
                         display-buffer-use-some-window)
                        (inhibit-same-window . t))))))

(defun wikipedia-diff-follow--find-other-window (source-window)
  "Find another window to display the diff, excluding SOURCE-WINDOW.
Returns nil if no suitable window is found."
  (let ((windows (window-list nil 'no-minibuf)))
    (when (> (length windows) 1)
      (seq-find (lambda (w) (not (eq w source-window))) windows))))

(defun wikipedia-diff-follow--mark-read ()
  "Mark the current entry as read if in watchlist mode."
  (when (derived-mode-p 'wikipedia-watchlist-mode)
    (wikipedia-watchlist--mark-at-point-read)))

(defun wikipedia-diff-follow--cleanup ()
  "Clean up diff follow mode state."
  (setq wikipedia-diff-follow--last-revid nil))

;;;###autoload
(define-minor-mode wikipedia-diff-follow-mode
  "Minor mode to automatically show diffs when navigating entries.
When enabled, moving to a different entry in history, watchlist, or
contributions buffers will automatically display the diff in another window.
Only works with unified diff mode, not ediff."
  :lighter " Follow"
  :variable wikipedia-diff-follow-mode
  (if wikipedia-diff-follow-mode
      (if (eq wikipedia-diff-function 'ediff)
          (progn
            (message "Diff follow mode requires unified diff, not ediff")
            (setq wikipedia-diff-follow-mode nil))
        (add-hook 'post-command-hook #'wikipedia-diff-follow--show nil t))
    (remove-hook 'post-command-hook #'wikipedia-diff-follow--show t)
    (wikipedia-diff-follow--cleanup)))

;;;; Diff engine

(defun wikipedia--show-diff (from-rev to-rev title)
  "Display diff between FROM-REV and TO-REV for TITLE.
Uses `wikipedia-diff-function' to determine the diff style."
  (let ((from-content (wp--get-revision-content title from-rev))
        (to-content (wp--get-revision-content title to-rev)))
    (wikipedia--show-diff-contents from-content to-content from-rev to-rev title)))

(defun wikipedia--get-diff-text (from-rev to-rev title)
  "Return unified diff text between FROM-REV and TO-REV for TITLE.
Uses the same content-fetching path as `wikipedia--show-diff'.
Returns the diff string, or nil if revisions cannot be fetched."
  (condition-case nil
      (let ((from-content (wp--get-revision-content title from-rev))
            (to-content (wp--get-revision-content title to-rev)))
        (when (and from-content to-content)
          (let* ((from-file (wikipedia--write-temp-file from-content from-rev))
                 (to-file (wikipedia--write-temp-file to-content to-rev)))
            (unwind-protect
                (wikipedia--generate-unified-diff
                 from-file to-file from-rev to-rev)
              (delete-file from-file)
              (delete-file to-file)))))
    (error nil)))

(defun wikipedia--show-diff-contents (from-content to-content from-rev to-rev title)
  "Display diff between FROM-CONTENT and TO-CONTENT.
FROM-REV and TO-REV are the revision IDs, TITLE is the page title.
Uses `wikipedia-diff-function' to determine the diff style."
  (pcase wikipedia-diff-function
    ('unified (wikipedia--show-diff-unified
               from-content to-content from-rev to-rev title))
    ('ediff (wikipedia--show-diff-ediff
             from-content to-content from-rev to-rev title))
    (_ (error "Unknown diff function: %s" wikipedia-diff-function))))

(defun wikipedia--render-unified-diff (from-content to-content from-rev to-rev buffer-name)
  "Render unified diff between FROM-CONTENT and TO-CONTENT into BUFFER-NAME.
FROM-REV and TO-REV are revision IDs for diff labels.
Returns the diff buffer."
  (let* ((from-file (wikipedia--write-temp-file from-content from-rev))
         (to-file (wikipedia--write-temp-file to-content to-rev))
         (diff-output (unwind-protect
                          (wikipedia--generate-unified-diff from-file to-file from-rev to-rev)
                        (delete-file from-file)
                        (delete-file to-file)))
         (buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert diff-output)
        (goto-char (point-min)))
      (diff-mode)
      (setq buffer-read-only t))
    buf))

(defun wikipedia--show-diff-unified (from-content to-content from-rev to-rev title)
  "Display unified diff between FROM-CONTENT and TO-CONTENT.
FROM-REV and TO-REV are the revision IDs, TITLE is the page title."
  (let ((buf (wikipedia--render-unified-diff
              from-content to-content from-rev to-rev
              (format "*WP Diff: %s (%d → %d)*" title from-rev to-rev))))
    (with-current-buffer buf
      (setq-local wikipedia--buffer-page-title title))
    (pop-to-buffer buf)))

(defun wikipedia--write-temp-file (content revid)
  "Write CONTENT to a temporary file named after REVID.
Signals an error if CONTENT is nil."
  (unless content
    (error "Cannot write temp file for revision %d: nil content" revid))
  (let ((file (make-temp-file (format "wp-rev-%d-" revid)))
        (coding-system-for-write 'utf-8))
    (with-temp-file file
      (insert content))
    file))

(defun wikipedia--generate-unified-diff (from-file to-file from-rev to-rev
                                                  &optional context-lines)
  "Generate unified diff output between FROM-FILE and TO-FILE.
FROM-REV and TO-REV are used for the diff header labels.
Optional CONTEXT-LINES overrides the default context (3 lines)."
  (wikipedia--generate-labeled-diff
   from-file to-file
   (format "Revision %d" from-rev)
   (format "Revision %d" to-rev)
   context-lines))

(defun wikipedia--generate-word-diff (from-file to-file from-rev to-rev)
  "Generate a word-level diff between FROM-FILE and TO-FILE for AI review.
Uses `git diff --word-diff=porcelain' which places each changed word
on its own line prefixed with - or +, making it trivial to separate
actual changes from unchanged context.
FROM-REV and TO-REV label the revisions in the header."
  (with-temp-buffer
    (let ((exit-code (call-process "git" nil t nil
                                   "diff" "--no-index"
                                   "--word-diff=porcelain"
                                   "--no-color"
                                   "-U0"
                                   from-file to-file)))
      (cond
       ((zerop exit-code) "")            ; files are identical
       ((= exit-code 1)
        ;; Strip git-specific header, keep only hunks.
        (goto-char (point-min))
        (when (looking-at "diff --git[^\n]*\n\\(?:index [^\n]*\n\\)?")
          (replace-match ""))
        (goto-char (point-min))
        (when (re-search-forward "^--- " nil t)
          (delete-region (point) (line-end-position))
          (insert (format "Revision %d" from-rev)))
        (when (re-search-forward "^\\+\\+\\+ " nil t)
          (delete-region (point) (line-end-position))
          (insert (format "Revision %d" to-rev)))
        (buffer-string))
       (t (error "git diff failed with exit code %d" exit-code))))))

(defun wikipedia--generate-labeled-diff (from-file to-file from-label to-label
                                                   &optional context-lines)
  "Generate unified diff between FROM-FILE and TO-FILE.
FROM-LABEL and TO-LABEL are strings used for the diff header labels.
Optional CONTEXT-LINES overrides the default context (3 lines)."
  (with-temp-buffer
    (let ((exit-code (call-process "diff" nil t nil
                                   (if context-lines
                                       (format "-U%d" context-lines)
                                     "-u")
                                   (format "--label=%s" from-label)
                                   (format "--label=%s" to-label)
                                   from-file to-file)))
      (if (> exit-code 1)
          (error "Diff command failed with exit code %d" exit-code)
        (buffer-string)))))

(defvar ediff-buffer-A)
(defvar ediff-buffer-B)

(defun wikipedia--ediff-with-cleanup (buffer-a buffer-b)
  "Run ediff on BUFFER-A and BUFFER-B, killing both when ediff quits."
  (ediff-buffers buffer-a buffer-b
                 (list (lambda ()
                         (let ((a ediff-buffer-A)
                               (b ediff-buffer-B))
                           (add-hook 'ediff-quit-hook
                                     (lambda ()
                                       (when (buffer-live-p a) (kill-buffer a))
                                       (when (buffer-live-p b) (kill-buffer b)))
                                     nil t))))))

(defun wikipedia--show-diff-ediff (from-content to-content from-rev to-rev title)
  "Display ediff between FROM-CONTENT and TO-CONTENT.
FROM-REV and TO-REV are the revision IDs, TITLE is the page title."
  (let ((from-buffer (wikipedia--create-revision-buffer title from-rev from-content))
        (to-buffer (wikipedia--create-revision-buffer title to-rev to-content)))
    (wikipedia--ediff-with-cleanup from-buffer to-buffer)))

(defun wikipedia--create-revision-buffer (title revid content)
  "Create a buffer for TITLE at REVID with CONTENT."
  (let ((buffer (get-buffer-create (format "*WP Rev %d: %s*" revid title))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or content ""))
        (goto-char (point-min)))
      (setq-local wikipedia--buffer-page-title title)
      (setq buffer-read-only t))
    buffer))

;;;; Diff to live

;;;###autoload
(defun wikipedia-diff-to-live ()
  "Show diff between the current buffer and the live version on Wikipedia.
Compares the latest published revision against the local editing buffer,
so additions show new content and deletions show removed content."
  (interactive)
  (wp--ensure-logged-in)
  (let* ((title (or (wp--current-page-title)
                    (when buffer-file-name
                      (file-name-base buffer-file-name))
                    (error "No page title for this buffer")))
         (latest (or (wp--get-latest-revision-content title)
                     (error "Could not fetch latest revision for %s" title)))
         (live-revid (car latest))
         (live-content (cdr latest))
         (local-content (buffer-substring-no-properties (point-min) (point-max))))
    (if (string= live-content local-content)
        (message "No changes (buffer matches live revision %d)" live-revid)
      (wikipedia--diff-buffer-to-revision
       local-content live-content live-revid title))))

(defun wikipedia--diff-buffer-to-revision (local-content live-content revid title)
  "Display diff between LOCAL-CONTENT and LIVE-CONTENT at REVID for TITLE.
Uses `wikipedia-diff-function' to determine the diff style."
  (pcase wikipedia-diff-function
    ('unified
     (wikipedia--diff-buffer-to-revision-unified
      local-content live-content revid title))
    ('ediff
     (wikipedia--diff-buffer-to-revision-ediff
      local-content live-content revid title))
    (_ (error "Unknown diff function: %s" wikipedia-diff-function))))

(defun wikipedia--diff-buffer-to-revision-unified (local-content live-content revid title)
  "Show unified diff between LOCAL-CONTENT and LIVE-CONTENT for TITLE.
REVID is the live revision ID."
  (let* ((live-file (wikipedia--write-temp-file live-content revid))
         (local-file (wikipedia--write-temp-file local-content 0))
         (diff-output (unwind-protect
                          (wikipedia--generate-labeled-diff
                           live-file local-file
                           (format "Revision %d (live)" revid)
                           "Local buffer")
                        (delete-file live-file)
                        (delete-file local-file)))
         (buf-name (format "*WP Diff: %s (live → local)*" title))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert diff-output)
        (goto-char (point-min)))
      (diff-mode)
      (setq buffer-read-only t)
      (setq-local wikipedia--buffer-page-title title))
    (pop-to-buffer buf)))

(defun wikipedia--diff-buffer-to-revision-ediff (local-content live-content revid title)
  "Show ediff between LOCAL-CONTENT and LIVE-CONTENT for TITLE.
REVID is the live revision ID."
  (let ((live-buffer (wikipedia--create-revision-buffer title revid live-content))
        (local-buffer (get-buffer-create (format "*WP Local: %s*" title))))
    (with-current-buffer local-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or local-content ""))
        (goto-char (point-min)))
      (setq-local wikipedia--buffer-page-title title)
      (setq buffer-read-only t))
    (wikipedia--ediff-with-cleanup live-buffer local-buffer)))

(defun wikipedia--diff-to-live-text ()
  "Return unified diff text between the current buffer and the live version.
Returns the diff string, or nil if there are no changes."
  (let* ((title (or (wp--current-page-title)
                    (when buffer-file-name
                      (file-name-base buffer-file-name))
                    (error "No page title for this buffer")))
         (latest (or (wp--get-latest-revision-content title)
                     (error "Could not fetch latest revision for %s" title)))
         (live-content (cdr latest))
         (local-content (buffer-substring-no-properties (point-min) (point-max))))
    (if (string= live-content local-content)
        nil
      (let* ((live-file (wikipedia--write-temp-file live-content 0))
             (local-file (wikipedia--write-temp-file local-content 1)))
        (unwind-protect
            (wikipedia--generate-labeled-diff
             live-file local-file "Live" "Local")
          (delete-file live-file)
          (delete-file local-file))))))

(provide 'wikipedia-diff)

;;; wikipedia-diff.el ends here
