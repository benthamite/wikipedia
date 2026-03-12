;;; wikipedia-auto.el --- Automatic background watchlist updates -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Pablo Stafforini

;;; Commentary:

;; This module provides periodic background updates for the Wikipedia
;; watchlist.  When `wikipedia-auto-update-mode' is enabled, a timer
;; refreshes the watchlist at regular intervals and (when
;; `wikipedia-ai-review-auto' is set) scores new entries via AI so
;; that scores are already available when the user opens the buffer.

;;; Code:

(require 'wikipedia-watchlist)

(declare-function wp--ensure-logged-in "wikipedia-adapter")
(declare-function wp--get-watchlist "wikipedia-adapter")
(declare-function wikipedia-ai-review-watchlist "wikipedia-ai-review")
(declare-function wikipedia--prefetch-watchlist-diffs "wikipedia-cache")

(defvar wp--current-site)
(defvar wikipedia-ai-review-auto)
(defvar wikipedia-ai-review--queue)

;;;; User options

(defcustom wikipedia-auto-update-interval (* 15 60)
  "Seconds between automatic watchlist updates.
The default is 15 minutes (900 seconds)."
  :type 'natnum
  :group 'wikipedia)

(defcustom wikipedia-auto-update-notify t
  "When non-nil, show a message after each background update.
The message reports the number of watchlist entries found."
  :type 'boolean
  :group 'wikipedia)

;;;; Internal state

(defvar wikipedia-auto-update--timer nil
  "The repeating timer for automatic watchlist updates.")

(defvar wikipedia-auto-update--last-time nil
  "Time of the last successful automatic update.")

(defvar wikipedia-auto-update--last-count nil
  "Number of watchlist entries at the last update.")

;;;; Core logic

(defun wikipedia-auto-update--logged-in-p ()
  "Return non-nil if a wiki session is active.
Checks without prompting the user."
  (or wp--current-site (bound-and-true-p mediawiki-site)))

(defun wikipedia-auto-update--scoring-p ()
  "Return non-nil if AI scoring is currently in progress."
  (and (boundp 'wikipedia-ai-review--queue)
       wikipedia-ai-review--queue))

(defun wikipedia-auto-update--tick ()
  "Timer callback: refresh the watchlist in the background.
Skips the update if not logged in or if AI scoring is in progress."
  (when (and (wikipedia-auto-update--logged-in-p)
             (not (wikipedia-auto-update--scoring-p)))
    (condition-case err
        (wikipedia-auto-update--refresh)
      (error
       (message "wikipedia-auto-update: %s" (error-message-string err))))))

(defun wikipedia-auto-update--refresh ()
  "Refresh the watchlist buffer, creating it if needed.
Fetches new watchlist data, updates the buffer, and triggers AI
scoring if `wikipedia-ai-review-auto' is set."
  (wp--ensure-logged-in)
  (let* ((buffer (get-buffer "*Wikipedia Watchlist*"))
         (new-buffer-p (null buffer)))
    (unless buffer
      (setq buffer (get-buffer-create "*Wikipedia Watchlist*"))
      (with-current-buffer buffer
        (wikipedia-watchlist-mode)))
    (with-current-buffer buffer
      (wikipedia-watchlist-refresh)
      (let ((count (length wikipedia-watchlist--grouped-entries)))
        (setq wikipedia-auto-update--last-time (current-time)
              wikipedia-auto-update--last-count count)
        (when wikipedia-auto-update-notify
          (message "wikipedia-auto-update: %d %s in watchlist"
                   count
                   (if (= count 1) "page" "pages")))))
    ;; If we just created the buffer, keep it buried
    (when new-buffer-p
      (bury-buffer buffer))))

;;;; Minor mode

;;;###autoload
(define-minor-mode wikipedia-auto-update-mode
  "Periodically refresh the Wikipedia watchlist in the background.
When enabled, a timer runs every `wikipedia-auto-update-interval'
seconds to fetch new watchlist data and (when
`wikipedia-ai-review-auto' is set) score entries via AI.

This means scores are already available when you open the watchlist,
eliminating the wait after a manual refresh."
  :global t
  :group 'wikipedia
  (if wikipedia-auto-update-mode
      (wikipedia-auto-update--start)
    (wikipedia-auto-update--stop)))

(defun wikipedia-auto-update--start ()
  "Start the auto-update timer."
  (wikipedia-auto-update--stop)
  (setq wikipedia-auto-update--timer
        (run-at-time wikipedia-auto-update-interval
                     wikipedia-auto-update-interval
                     #'wikipedia-auto-update--tick))
  (message "wikipedia-auto-update: updates every %s"
           (wikipedia-auto-update--format-interval
            wikipedia-auto-update-interval)))

(defun wikipedia-auto-update--stop ()
  "Stop the auto-update timer."
  (when wikipedia-auto-update--timer
    (cancel-timer wikipedia-auto-update--timer)
    (setq wikipedia-auto-update--timer nil)))

;;;; Utilities

(defun wikipedia-auto-update--format-interval (seconds)
  "Format SECONDS as a human-readable interval."
  (let ((minutes (/ seconds 60))
        (hours (/ seconds 3600)))
    (cond
     ((< seconds 60) (format "%d seconds" seconds))
     ((< minutes 60) (format "%d minutes" minutes))
     (t (format "%d hours" hours)))))

;;;###autoload
(defun wikipedia-auto-update-now ()
  "Trigger an immediate background watchlist update.
This is the same operation the timer performs, but run on demand."
  (interactive)
  (wikipedia-auto-update--tick))

(provide 'wikipedia-auto)

;;; wikipedia-auto.el ends here
