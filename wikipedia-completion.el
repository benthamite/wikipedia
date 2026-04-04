;;; wikipedia-completion.el --- Completion at point for wiki links -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Pablo Stafforini

;;; Commentary:

;; This module provides completion-at-point for wiki links in mediawiki-mode
;; buffers.  When point is inside a `[[...]]' link, typing triggers
;; prefix-based title suggestions from the MediaWiki API.

;;; Code:

(require 'wikipedia-adapter)

(defcustom wikipedia-completion-limit 15
  "Maximum number of completion candidates to fetch from the API."
  :type 'integer
  :group 'wikipedia)

(defvar wikipedia-completion--cache (make-hash-table :test #'equal)
  "Cache mapping prefix strings to (TIMESTAMP . TITLES) results.")

(defvar wikipedia-completion--cache-ttl 300
  "Seconds before a cached completion result expires.")

(defun wikipedia-completion--clear-cache ()
  "Clear the completion cache."
  (clrhash wikipedia-completion--cache))

(defun wikipedia-completion--fetch (prefix)
  "Fetch page titles matching PREFIX from the MediaWiki API.
Uses the generator form of prefixsearch combined with displaytitle
so that titles like \"iPhone\" are returned with correct capitalization.
Returns a list of title strings."
  (when (and prefix (not (string-empty-p prefix)))
    (let ((cached (gethash prefix wikipedia-completion--cache)))
      (if (and cached
               (< (- (float-time) (car cached))
                  wikipedia-completion--cache-ttl))
          (cdr cached)
        (condition-case nil
            (progn
              (wp--ensure-logged-in)
              (let* ((result (wp--api-call
                              "query"
                              (list (cons "generator" "prefixsearch")
                                    (cons "gpssearch" prefix)
                                    (cons "gpslimit"
                                          (number-to-string
                                           wikipedia-completion-limit))
                                    (cons "prop" "info")
                                    (cons "inprop" "displaytitle"))))
                     (pages (cddr (assq 'pages (cddr result))))
                     (titles
                      (mapcar
                       (lambda (page)
                         (let ((attrs (cadr page)))
                           (wikipedia-completion--display-title attrs)))
                       pages)))
                (puthash prefix (cons (float-time) titles)
                         wikipedia-completion--cache)
                titles))
          (error nil))))))

(defun wikipedia-completion--display-title (attrs)
  "Extract the best title from page ATTRS.
Prefer the displaytitle (which preserves intentional capitalization
like \"iPhone\"), falling back to the canonical title.  Strip any HTML
markup that displaytitle may contain (e.g. italics)."
  (let ((display (cdr (assq 'displaytitle attrs)))
        (canonical (cdr (assq 'title attrs))))
    (if display
        (replace-regexp-in-string "<[^>]+>" "" display)
      canonical)))

(defun wikipedia-completion--link-bounds ()
  "Return (BEG . END) of the link target if point is inside [[...]].
BEG is just after `[[' (or after `|' if in display text portion).
END is point.  Returns nil if point is not inside a wiki link."
  (save-excursion
    (let ((pos (point))
          (open nil))
      ;; Search backward for [[ that isn't closed before point.
      (when (re-search-backward "\\[\\[" (line-beginning-position 0) t)
        (setq open (+ (point) 2))
        (goto-char open)
        ;; Check that there's no ]] between the [[ and point.
        (unless (re-search-forward "\\]\\]" pos t)
          ;; If there's a pipe, we're in the display text — don't complete.
          (goto-char open)
          (if (re-search-forward "|" pos t)
              nil
            (cons open pos)))))))

;;;###autoload
(defun wikipedia-completion-at-point ()
  "Completion-at-point function for wiki link targets.
Returns completion data when point is inside `[[...]]'."
  (when-let* ((bounds (wikipedia-completion--link-bounds)))
    (let ((beg (car bounds))
          (end (cdr bounds)))
      (list beg end
            (wikipedia-completion--make-table)
            :exclusive t
            :company-prefix-length t))))

(defun wikipedia-completion--make-table ()
  "Return a case-insensitive completion table backed by the API."
  (let ((inner (completion-table-dynamic #'wikipedia-completion--fetch)))
    (lambda (string pred action)
      (if (eq action 'metadata)
          '(metadata (category . wikipedia-page))
        (let ((completion-ignore-case t))
          (complete-with-action action inner string pred))))))

(provide 'wikipedia-completion)

;;; wikipedia-completion.el ends here
