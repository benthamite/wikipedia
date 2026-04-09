;;; wikipedia-transient.el --- Transient menu for wikipedia.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Pablo Stafforini

;;; Commentary:

;; This module provides a transient menu for quick access to all
;; wikipedia.el commands.

;;; Code:

(require 'transient)
(require 'wikipedia-draft)
(require 'wikipedia-page)
(require 'wikipedia-watchlist)
(require 'wikipedia-history)
(require 'wikipedia-user)
(require 'wikipedia-xtools)
(require 'wikipedia-sync)
(require 'wikipedia-mirror)
(require 'wikipedia-common)
(require 'pangram nil t)
(require 'wikipedia-ai)

;;;###autoload (autoload 'wikipedia-transient "wikipedia-transient" nil t)
(transient-define-prefix wikipedia-transient ()
  "Wikipedia commands."
  :info-manual "(wikipedia)"
  [["Page"
    ("p o" "Open page" wikipedia-open)
    ("p P" "Publish page" wikipedia-publish)
    ("p p" "Preview" wikipedia-preview)
    ("p d" "Diff to live" wikipedia-diff-to-live)
    ("p e" "Edit from EWW" wikipedia-eww-open)
    ("p b" "Browse page" wikipedia-browse)
    ("p l" "Login" wikipedia-login)
    ""
    "Draft"
    ("d s" "Save draft" wikipedia-draft-save)
    ("d o" "Open draft" wikipedia-draft-open)
    ("d D" "Delete draft" wikipedia-draft-delete)]
   ["User"
    ("u c" "User contributions" wikipedia-user-contributions)
    ("u p" "User page" wikipedia-user-page)
    ("u t" "User talk" wikipedia-user-talk)
    ("u b" "Browse user" wikipedia-user-browse)
    ("u k" "Thank user" wikipedia-thank)
    ""
    "XTools"
    ("x u" "User stats" wikipedia-xtools-user-stats)
    ("x p" "Page stats" wikipedia-xtools-page-stats)
    ("x t" "Top editors" wikipedia-xtools-top-editors)]
   ["Watchlist"
    ("w w" "Watchlist" wikipedia-watchlist)
    ("w p" "Watch page" wikipedia-watchlist-watch)
    ("w P" "Unwatch page" wikipedia-watchlist-unwatch)
    ("w b" "Browse watchlist" wikipedia-watchlist-browse)
    ("w a" "Mark all read" wikipedia-watchlist-mark-all-read)
    ""
    "History"
    ("h h" "History" wikipedia-history)
    ("h u" "Undo revision" wikipedia-undo)
    ("h r" "Restore revision" wikipedia-history-restore-revision)]
   ["Sync & Mirror"
    ("s p" "Sync page" wikipedia-sync-page)
    ("s w" "Sync watchlist" wikipedia-sync-watchlist)
    ("s u" "Sync update" wikipedia-sync-update)
    ("s m" "Mirror" wikipedia-mirror)]
   ["AI"
    :if (lambda () (or (locate-library "pangram") (locate-library "gptel")))
    ("a d" "Detect AI" pangram-detect :if (lambda () (locate-library "pangram")))
    ("a x" "Clear overlays" pangram-clear :if (lambda () (locate-library "pangram")))
    ("a c" "Generate citation" wikipedia-ai-cite :if (lambda () (locate-library "gptel")))
    ("a e" "Generate edit summary" wikipedia-ai-summarize :if (lambda () (locate-library "gptel")))
    ("a r" "Review watchlist" wikipedia-ai-review-watchlist :if (lambda () (locate-library "gptel")))]])

(provide 'wikipedia-transient)

;;; wikipedia-transient.el ends here
