;;; wikipedia-watchlist.el --- Watchlist for wikipedia.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Pablo Stafforini

;;; Commentary:

;; This module provides watchlist browsing for Wikipedia.

;;; Code:

(require 'cl-lib)
(require 'wikipedia-adapter)
(require 'wikipedia-cache)
(require 'wikipedia-common)
(require 'wikipedia-diff)
(require 'wikipedia-history)
(require 'tabulated-list)
(require 'iso8601)

(declare-function wikipedia-browse "wikipedia-page")

(defvar-local wikipedia-watchlist--entries nil
  "The list of watchlist entries displayed in this buffer.")

(defvar-local wikipedia-watchlist--grouped-entries nil
  "The grouped watchlist entries, keyed by page title.")

(defvar-local wikipedia-watchlist--expanded nil
  "Hash table tracking which page titles are expanded.")

(defvar-local wikipedia-watchlist--read nil
  "Hash table tracking which revision IDs have been read.")

(defvar-local wikipedia-watchlist--scores nil
  "Hash table mapping page titles to (SCORE . REASON) cons cells.
Populated by `wikipedia-ai-review-watchlist'.")

(defvar-local wikipedia-watchlist--original-group-order nil
  "Saved group order before sorting by score, for toggling back.")

(defface wikipedia-watchlist-unread
  '((t :weight bold))
  "Face for unread watchlist entries.")

(defface wikipedia-watchlist-score-high
  '((t :inherit warning :weight bold))
  "Face for high AI review scores.")

(defface wikipedia-watchlist-score-low
  '((t :inherit shadow))
  "Face for low AI review scores.")

(defconst wikipedia-watchlist-score-high-threshold 0.7
  "Scores at or above this value are highlighted with `wikipedia-watchlist-score-high'.
The AI review scores range from 0.0 (trivial, no review needed) to
1.0 (highly significant, definitely review).  0.7 marks the boundary
where edits are substantial enough to warrant prominent visual emphasis.")

(defconst wikipedia-watchlist-score-low-threshold 0.3
  "Scores at or below this value are dimmed with `wikipedia-watchlist-score-low'.
Edits scoring this low are minor (typo fixes, whitespace, formatting)
and can typically be skipped during review.")

(defvar wikipedia-watchlist-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map wikipedia-list-mode-map)
    (define-key map (kbd "RET") #'wikipedia-watchlist-open-page)
    (define-key map (kbd "TAB") #'wikipedia-watchlist-toggle-expand)
    (define-key map (kbd "SPC") #'wikipedia-watchlist-toggle-expand)
    (define-key map "o" #'wikipedia-watchlist-open-page)
    (define-key map "v" #'wikipedia-watchlist-show-diff)
    (define-key map "d" #'wikipedia-watchlist-show-diff)
    (define-key map "h" #'wikipedia-watchlist-show-history)
    (define-key map "b" #'wikipedia-watchlist-browse)
    (define-key map "g" #'wikipedia-watchlist-refresh)
    (define-key map "e" #'wikipedia-watchlist-expand-all)
    (define-key map "c" #'wikipedia-watchlist-collapse-all)
    (define-key map "m" #'wikipedia-watchlist-mark-all-read)
    (define-key map "!" #'wikipedia-watchlist-mark-all-read)
    (define-key map "S" #'wikipedia-watchlist-sort-by-score)
    (define-key map "I" #'wikipedia-watchlist-show-score-reason)
    map)
  "Keymap for `wikipedia-watchlist-mode'.")

(define-derived-mode wikipedia-watchlist-mode tabulated-list-mode "WP-Watchlist"
  "Major mode for browsing Wikipedia watchlist.
\\{wikipedia-watchlist-mode-map}"
  (setq tabulated-list-format
        [("" 2 nil)
         ("#" 3 t)
         ("Page" 40 t)
         ("Time" 20 t)
         ("User" 20 t)
         ("Change" 8 t)
         ("Score" 6 t)
         ("Summary" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (setq wikipedia-watchlist--expanded (make-hash-table :test 'equal))
  (setq wikipedia-watchlist--read (make-hash-table :test 'eql))
  (setq wikipedia-watchlist--scores (make-hash-table :test 'equal))
  (tabulated-list-init-header))

;;;###autoload
(defun wikipedia-watchlist ()
  "Display the user's watchlist."
  (interactive)
  (wp--ensure-logged-in)
  (let ((buffer (get-buffer-create "*Wikipedia Watchlist*")))
    (with-current-buffer buffer
      (wikipedia-watchlist-mode)
      (wikipedia-watchlist-refresh))
    (pop-to-buffer buffer)))

(defun wikipedia-watchlist-refresh ()
  "Refresh the watchlist."
  (interactive)
  ;; 500 is the MediaWiki API maximum for list queries
  (let ((entries (wp--get-watchlist 500)))
    (setq wikipedia-watchlist--entries entries)
    (setq wikipedia-watchlist--grouped-entries
          (wikipedia-watchlist--group-entries entries))
    (setq wikipedia-watchlist--original-group-order nil)
    (wikipedia-watchlist--rebuild-list)
    (tabulated-list-print t)
    (wikipedia--prefetch-watchlist-diffs entries)))

(defun wikipedia-watchlist--group-entries (entries)
  "Group ENTRIES by page title, preserving order of first appearance."
  (let ((groups (make-hash-table :test 'equal))
        (order nil))
    (dolist (entry entries)
      (let ((title (alist-get 'title entry)))
        (unless (gethash title groups)
          (push title order))
        (puthash title
                 (append (gethash title groups) (list entry))
                 groups)))
    (mapcar (lambda (title)
              (cons title (gethash title groups)))
            (nreverse order))))

(defun wikipedia-watchlist--rebuild-list ()
  "Rebuild `tabulated-list-entries' from grouped entries."
  (let ((entries (wikipedia-watchlist--build-display-entries)))
    (setq tabulated-list-entries entries)
    (wikipedia-watchlist--adjust-column-widths entries)))

(defun wikipedia-watchlist--adjust-column-widths (entries)
  "Adjust column widths based on content in ENTRIES."
  (let* ((max-widths '(2 3 40 20 20 8 6))
         (actual-widths (wikipedia-watchlist--compute-column-widths entries))
         (new-format (wikipedia-watchlist--build-format max-widths actual-widths)))
    (setq tabulated-list-format new-format)
    (tabulated-list-init-header)))

(defun wikipedia-watchlist--compute-column-widths (entries)
  "Compute the maximum display width of each column in ENTRIES.
Uses `string-width' to match `tabulated-list-mode' column layout."
  (let ((widths (list 0 0 0 0 0 0 0)))
    (dolist (entry entries)
      (let ((row (cadr entry)))
        (dotimes (i 7)
          (let* ((cell (aref row i))
                 (text (if (stringp cell) cell (format "%s" cell)))
                 (w (string-width text)))
            (when (> w (nth i widths))
              (setf (nth i widths) w))))))
    widths))

(defun wikipedia-watchlist--build-format (max-widths actual-widths)
  "Build column format using minimum of MAX-WIDTHS and ACTUAL-WIDTHS.
Column widths are never smaller than the header name length."
  (cl-flet ((w (i name) (min (nth i max-widths)
                              (max (nth i actual-widths) (length name)))))
    (vector
     (list "" (w 0 "") nil)
     (list "#" (w 1 "#") t)
     (list "Page" (w 2 "Page") t)
     (list "Time" (w 3 "Time") t)
     (list "User" (w 4 "User") t)
     (list "Change" (w 5 "Change") t)
     (list "Score" (w 6 "Score") t)
     (list "Summary" 0 nil))))

(defun wikipedia-watchlist--build-display-entries ()
  "Build the list of display entries from grouped data."
  (let ((result nil))
    (dolist (group wikipedia-watchlist--grouped-entries)
      (let* ((title (car group))
             (entries (cdr group))
             (expanded-p (gethash title wikipedia-watchlist--expanded)))
        (push (wikipedia-watchlist--make-group-entry title entries) result)
        (when expanded-p
          (dolist (entry entries)
            (push (wikipedia-watchlist--make-child-entry entry) result)))))
    (nreverse result)))

(defun wikipedia-watchlist--make-group-entry (title entries)
  "Create a group header entry for TITLE with ENTRIES.
The entry ID is (group . TITLE)."
  (let* ((count (length entries))
         (latest (car entries))
         (timestamp (alist-get 'timestamp latest))
         (users (wikipedia-watchlist--summarize-users entries))
         (total-change (wikipedia-watchlist--total-size-change entries))
         (comment (alist-get 'comment latest))
         (expandable (> count 1))
         (expanded-p (gethash title wikipedia-watchlist--expanded))
         (indicator (if expandable
                        (if expanded-p "▼" "▶")
                      " "))
         (unread-p (wikipedia-watchlist--group-has-unread-p entries)))
    (list (cons 'group title)
          (wikipedia-watchlist--apply-unread-face
           (vector
            indicator
            (if (> count 1) (number-to-string count) "")
            title
            (wikipedia-watchlist--format-timestamp timestamp)
            users
            (wikipedia--format-size-change total-change)
            (wikipedia-watchlist--format-score title)
            (or comment ""))
           unread-p))))

(defun wikipedia-watchlist--group-has-unread-p (entries)
  "Return non-nil if any entry in ENTRIES is unread."
  (seq-some (lambda (entry)
              (not (gethash (alist-get 'revid entry) wikipedia-watchlist--read)))
            entries))

(defun wikipedia-watchlist--apply-unread-face (row unread-p)
  "Apply unread face to ROW if UNREAD-P is non-nil.
Composes with any existing face on the cell rather than replacing it."
  (if unread-p
      (let ((new-row (copy-sequence row)))
        (dotimes (i (length new-row))
          (let ((cell (aref new-row i)))
            (when (stringp cell)
              (let ((existing (get-text-property 0 'face cell)))
                (aset new-row i
                      (propertize cell 'face
                                  (if existing
                                      (list existing 'wikipedia-watchlist-unread)
                                    'wikipedia-watchlist-unread)))))))
        new-row)
    row))

(defun wikipedia-watchlist--make-child-entry (entry)
  "Create a child entry for individual ENTRY.
The entry ID is (child TITLE . REVID)."
  (let* ((title (alist-get 'title entry))
         (timestamp (alist-get 'timestamp entry))
         (user (alist-get 'user entry))
         (comment (alist-get 'comment entry))
         (revid (alist-get 'revid entry))
         (oldlen (alist-get 'oldlen entry))
         (newlen (alist-get 'newlen entry))
         (unread-p (not (gethash revid wikipedia-watchlist--read))))
    (list (cons 'child (cons title revid))
          (wikipedia-watchlist--apply-unread-face
           (vector
            ""
            ""
            (concat "  " (wikipedia-watchlist--format-timestamp timestamp))
            ""
            (or user "")
            (wikipedia--format-size-change (when (and oldlen newlen)
                                             (- newlen oldlen)))
            ""
            (or comment ""))
           unread-p))))

(defun wikipedia-watchlist--summarize-users (entries)
  "Summarize the users who changed ENTRIES."
  (let ((users (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (let ((user (alist-get 'user entry)))
        (when user
          (puthash user (1+ (gethash user users 0)) users))))
    (let ((user-list nil))
      (maphash (lambda (user count)
                 (push (if (> count 1)
                           (format "%s (%d×)" user count)
                         user)
                       user-list))
               users)
      (string-join (nreverse user-list) ", "))))

(defun wikipedia-watchlist--total-size-change (entries)
  "Calculate the total size change across all ENTRIES."
  (let ((total 0))
    (dolist (entry entries)
      (let ((oldlen (alist-get 'oldlen entry))
            (newlen (alist-get 'newlen entry)))
        (when (and oldlen newlen)
          (setq total (+ total (- newlen oldlen))))))
    total))

(defun wikipedia-watchlist--format-timestamp (timestamp)
  "Format TIMESTAMP for display as relative time."
  (if timestamp
      (wikipedia-watchlist--relative-time timestamp)
    ""))

(defun wikipedia-watchlist--relative-time (timestamp)
  "Convert TIMESTAMP to a relative time string."
  (let* ((time (wikipedia-watchlist--parse-timestamp timestamp))
         (seconds-ago (float-time (time-subtract (current-time) time))))
    (wikipedia-watchlist--format-seconds-ago seconds-ago)))

(defun wikipedia-watchlist--parse-timestamp (timestamp)
  "Parse ISO 8601 TIMESTAMP string to Emacs time."
  (encode-time (iso8601-parse timestamp)))

(defun wikipedia-watchlist--format-seconds-ago (seconds)
  "Format SECONDS as a human-readable relative time."
  (let ((minutes (/ seconds 60))
        (hours (/ seconds 3600))
        (days (/ seconds 86400))
        (weeks (/ seconds 604800))
        (months (/ seconds 2592000)))
    (cond
     ((< seconds 60) "just now")
     ((< minutes 60) (format "%d minute%s ago" (floor minutes) (if (< minutes 2) "" "s")))
     ((< hours 24) (format "%d hour%s ago" (floor hours) (if (< hours 2) "" "s")))
     ((< days 7) (format "%d day%s ago" (floor days) (if (< days 2) "" "s")))
     ((< weeks 4) (format "%d week%s ago" (floor weeks) (if (< weeks 2) "" "s")))
     (t (format "%d month%s ago" (floor months) (if (< months 2) "" "s"))))))

(defun wikipedia-watchlist--entry-at-point ()
  "Return the watchlist entry at point.
Entry IDs are tagged cons cells: (group . TITLE) for group headers,
\(child TITLE . REVID) for individual entries."
  (let ((id (tabulated-list-get-id)))
    (cond
     ((and (consp id) (eq (car id) 'group))
      (let ((title (cdr id)))
        (car (alist-get title wikipedia-watchlist--grouped-entries nil nil #'equal))))
     ((and (consp id) (eq (car id) 'child))
      (let ((revid (cddr id)))
        (seq-find (lambda (e) (eql (alist-get 'revid e) revid))
                  wikipedia-watchlist--entries)))
     (t nil))))

(defun wikipedia-watchlist--title-at-point ()
  "Return the page title at point."
  (let ((id (tabulated-list-get-id)))
    (cond
     ((and (consp id) (eq (car id) 'group))
      (cdr id))
     ((and (consp id) (eq (car id) 'child))
      (cadr id))
     (t nil))))

(defun wikipedia-watchlist--revid-at-point ()
  "Return the revision ID at point."
  (let ((entry (wikipedia-watchlist--entry-at-point)))
    (when entry
      (alist-get 'revid entry))))

(defun wikipedia-watchlist--user-at-point ()
  "Return the username at point."
  (let ((entry (wikipedia-watchlist--entry-at-point)))
    (when entry
      (alist-get 'user entry))))

(defun wikipedia-watchlist-toggle-expand ()
  "Toggle expansion of the group at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when (and (consp id) (eq (car id) 'group))
      (let* ((title (cdr id))
             (entries (alist-get title wikipedia-watchlist--grouped-entries nil nil #'equal)))
        (when (> (length entries) 1)
          (if (gethash title wikipedia-watchlist--expanded)
              (remhash title wikipedia-watchlist--expanded)
            (puthash title t wikipedia-watchlist--expanded))
          (wikipedia-watchlist--rebuild-list)
          (tabulated-list-print t)
          (wikipedia-watchlist--goto-title title))))))

(defun wikipedia-watchlist--goto-title (title)
  "Move point to the row for TITLE."
  (goto-char (point-min))
  (while (and (not (eobp))
              (let ((id (tabulated-list-get-id)))
                (not (and (consp id)
                          (eq (car id) 'group)
                          (equal (cdr id) title)))))
    (forward-line 1)))

(defun wikipedia-watchlist-expand-all ()
  "Expand all groups."
  (interactive)
  (dolist (group wikipedia-watchlist--grouped-entries)
    (let ((title (car group))
          (entries (cdr group)))
      (when (> (length entries) 1)
        (puthash title t wikipedia-watchlist--expanded))))
  (wikipedia-watchlist--rebuild-list)
  (tabulated-list-print t))

(defun wikipedia-watchlist-collapse-all ()
  "Collapse all groups."
  (interactive)
  (clrhash wikipedia-watchlist--expanded)
  (wikipedia-watchlist--rebuild-list)
  (tabulated-list-print t))

(defun wikipedia-watchlist-open-page ()
  "Open the page at point for editing."
  (interactive)
  (let ((title (wikipedia-watchlist--title-at-point)))
    (unless title
      (error "No entry at point"))
    (wikipedia-watchlist--mark-at-point-read)
    (wp--open-page-buffer title)))

(defun wikipedia-watchlist--mark-at-point-read ()
  "Mark the entry at point as read and refresh display."
  (let ((id (tabulated-list-get-id))
        (title nil))
    (cond
     ((and (consp id) (eq (car id) 'group))
      (setq title (cdr id))
      (let ((entries (alist-get title wikipedia-watchlist--grouped-entries nil nil #'equal)))
        (dolist (entry entries)
          (puthash (alist-get 'revid entry) t wikipedia-watchlist--read))))
     ((and (consp id) (eq (car id) 'child))
      (setq title (cadr id))
      (let ((revid (cddr id)))
        (puthash revid t wikipedia-watchlist--read))))
    (when title
      (wp--mark-pages-seen-async (list title)))
    (wikipedia-watchlist--rebuild-list)
    (tabulated-list-print t)))

(defun wikipedia-watchlist-show-diff ()
  "Show the diff for the change at point.
For group headers with multiple changes, show the consolidated diff
spanning from the oldest base revision to the newest revision."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         title revid old-revid)
    (cond
     ;; Group header: diff from oldest old_revid to newest revid
     ((and (consp id) (eq (car id) 'group))
      (setq title (cdr id))
      (let ((entries (alist-get title wikipedia-watchlist--grouped-entries
                                nil nil #'equal)))
        (let ((revids (delq nil (mapcar (lambda (e) (alist-get 'revid e)) entries)))
              (old-revids (delq nil
                                (cl-remove-if #'zerop
                                              (delq nil (mapcar (lambda (e) (alist-get 'old_revid e))
                                                                entries))))))
          (setq revid (and revids (apply #'max revids)))
          (setq old-revid (and old-revids (apply #'min old-revids))))))
     ;; Child entry: use the entry's own revisions
     (t
      (let ((entry (wikipedia-watchlist--entry-at-point)))
        (setq title (alist-get 'title entry))
        (setq revid (alist-get 'revid entry))
        (setq old-revid (alist-get 'old_revid entry)))))
    (unless title
      (error "No entry at point"))
    (unless (and revid old-revid)
      (error "Cannot determine revisions for diff"))
    (wikipedia-watchlist--mark-at-point-read)
    (wikipedia--show-diff old-revid revid title)))

(defun wikipedia-watchlist-show-history ()
  "Show the history for the page at point."
  (interactive)
  (let ((title (wikipedia-watchlist--title-at-point)))
    (unless title
      (error "No entry at point"))
    (wikipedia-history title)))

(defun wikipedia-watchlist-browse ()
  "Open the page at point in an external browser."
  (interactive)
  (let ((title (wikipedia-watchlist--title-at-point)))
    (unless title
      (error "No entry at point"))
    (wikipedia-browse title)))

(defun wikipedia-watchlist-mark-all-read ()
  "Mark all entries in the watchlist as read."
  (interactive)
  (let ((titles (mapcar #'car wikipedia-watchlist--grouped-entries)))
    (dolist (entry wikipedia-watchlist--entries)
      (puthash (alist-get 'revid entry) t wikipedia-watchlist--read))
    (wikipedia-watchlist--rebuild-list)
    (tabulated-list-print t)
    (message "Marking %d pages as read..." (length titles))
    (wp--mark-pages-seen-async
     titles
     (lambda (success)
       (if success
           (message "Marked %d pages as read" (length titles))
         (message "Some pages could not be marked as read"))))))

;;;###autoload
(defun wikipedia-watchlist-watch (title)
  "Add page TITLE to the watchlist."
  (interactive (list (wikipedia--read-page-title)))
  (wp--ensure-logged-in)
  (let ((source-buffer (current-buffer)))
    (message "Adding \"%s\" to watchlist..." title)
    (wp--set-watch-async
     title nil
     (lambda (success)
       (if success
           (progn
             (message "Added \"%s\" to watchlist" title)
             (when (buffer-live-p source-buffer)
               (with-current-buffer source-buffer
                 (when (derived-mode-p 'wikipedia-watchlist-mode)
                   (wikipedia-watchlist-refresh)))))
         (message "Failed to watch page \"%s\"" title))))))

(defun wikipedia-watchlist-unwatch (title)
  "Remove page TITLE from the watchlist."
  (interactive (list (wikipedia--read-page-title)))
  (wp--ensure-logged-in)
  (when (yes-or-no-p (format "Remove \"%s\" from watchlist? " title))
    (let ((source-buffer (current-buffer)))
      (message "Removing \"%s\" from watchlist..." title)
      (wp--set-watch-async
       title t
       (lambda (success)
         (if success
             (progn
               (message "Removed \"%s\" from watchlist" title)
               (when (buffer-live-p source-buffer)
                 (with-current-buffer source-buffer
                   (when (derived-mode-p 'wikipedia-watchlist-mode)
                     (wikipedia-watchlist-refresh)))))
           (message "Failed to unwatch page \"%s\"" title)))))))

;;;; Score display and sorting

(defun wikipedia-watchlist--format-score (title)
  "Format the AI review score for TITLE.
Scores range 0.0-1.0 (see `wikipedia-ai-review-system-prompt').
High (>= 0.7) is highlighted as a warning, low (<= 0.3) is dimmed."
  (let ((score-data (gethash title wikipedia-watchlist--scores)))
    (if score-data
        (let* ((score (car score-data))
               (face (cond
                      ((>= score wikipedia-watchlist-score-high-threshold)
                       'wikipedia-watchlist-score-high)
                      ((<= score wikipedia-watchlist-score-low-threshold)
                       'wikipedia-watchlist-score-low)
                      (t 'default))))
          (propertize (format "%5.2f" score)
                      'face face
                      'help-echo (cdr score-data)))
      (format "%5s" "-")))  ;; Same width as "%5.2f" score column

(defun wikipedia-watchlist-sort-by-score ()
  "Toggle sorting watchlist groups by AI review score.
When sorting by score, groups are ordered highest first with
unscored entries after scored ones.  Toggling again restores the
original order (by recency)."
  (interactive)
  (if wikipedia-watchlist--original-group-order
      ;; Restore original order
      (progn
        (setq wikipedia-watchlist--grouped-entries
              wikipedia-watchlist--original-group-order
              wikipedia-watchlist--original-group-order nil)
        (wikipedia-watchlist--rebuild-list)
        (tabulated-list-print t)
        (message "Watchlist sorted by recency"))
    ;; Sort by score
    (setq wikipedia-watchlist--original-group-order
          (copy-sequence wikipedia-watchlist--grouped-entries))
    (setq wikipedia-watchlist--grouped-entries
          (sort wikipedia-watchlist--grouped-entries
                (lambda (a b)
                  (let ((score-a (or (car (gethash (car a) wikipedia-watchlist--scores))
                                     -1.0))
                        (score-b (or (car (gethash (car b) wikipedia-watchlist--scores))
                                     -1.0)))
                    (> score-a score-b)))))
    (wikipedia-watchlist--rebuild-list)
    (tabulated-list-print t)
    (message "Watchlist sorted by score")))

(defun wikipedia-watchlist-show-score-reason ()
  "Show the AI review reason for the entry at point."
  (interactive)
  (let* ((title (wikipedia-watchlist--title-at-point))
         (score-data (and title (gethash title wikipedia-watchlist--scores))))
    (if score-data
        (message "%s" (cdr score-data))
      (message "No AI review score for this entry"))))

;;;; Score reason auto-display

(defcustom wikipedia-watchlist-score-reason-auto nil
  "When non-nil, automatically show the AI score reason in the echo area.
Displays the score and reason for the entry at point whenever the
cursor moves to a different entry."
  :type 'boolean
  :group 'wikipedia-ai)

(defvar-local wikipedia-watchlist--score-reason-last-title nil
  "Last title for which the score reason was auto-displayed.")

(defun wikipedia-watchlist--score-reason-auto-show ()
  "Show score reason for entry at point if it changed."
  (when wikipedia-watchlist-score-reason-auto
    (let* ((title (wikipedia-watchlist--title-at-point))
           (score-data (and title (gethash title wikipedia-watchlist--scores))))
      (when (and score-data (not (equal title wikipedia-watchlist--score-reason-last-title)))
        (setq wikipedia-watchlist--score-reason-last-title title)
        (message "%s" (cdr score-data))))))

(add-hook 'wikipedia-watchlist-mode-hook
          (lambda ()
            (add-hook 'post-command-hook
                      #'wikipedia-watchlist--score-reason-auto-show nil t)))

(provide 'wikipedia-watchlist)

;;; wikipedia-watchlist.el ends here
