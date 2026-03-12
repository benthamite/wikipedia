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
    ("o" "Open page" wikipedia-open)
    ("P" "Publish page" wikipedia-publish)
    ("p" "Preview" wikipedia-preview)
    ("b" "Browse page" wikipedia-browse)
    ("l" "Login" wikipedia-login)]
   ["Draft"
    ("d s" "Save draft" wikipedia-draft-save)
    ("d o" "Open draft" wikipedia-draft-open)
    ("d k" "Delete draft" wikipedia-draft-delete)]
   ["Watchlist"
    ("w w" "Watchlist" wikipedia-watchlist)
    ("w a" "Watch page" wikipedia-watchlist-watch)
    ("w u" "Unwatch page" wikipedia-watchlist-unwatch)
    ("w b" "Browse watchlist" wikipedia-watchlist-browse)
    ("w !" "Mark all read" wikipedia-watchlist-mark-all-read)]
   ["History"
    ("h" "History" wikipedia-history)
    ("U" "Undo revision" wikipedia-undo)
    ("r" "Restore revision" wikipedia-history-restore-revision)]]
  [["User"
    ("u c" "User contributions" wikipedia-user-contributions)
    ("u p" "User page" wikipedia-user-page)
    ("u t" "User talk" wikipedia-user-talk)
    ("u b" "Browse user" wikipedia-user-browse)
    ("u T" "Thank user" wikipedia-thank)]
   ["XTools"
    ("x u" "User stats" wikipedia-xtools-user-stats)
    ("x p" "Page stats" wikipedia-xtools-page-stats)
    ("x e" "Top editors" wikipedia-xtools-top-editors)]
   ["Sync & Mirror"
    ("S p" "Sync page" wikipedia-sync-page)
    ("S w" "Sync watchlist" wikipedia-sync-watchlist)
    ("S u" "Sync update" wikipedia-sync-update)
    ("m" "Mirror" wikipedia-mirror)]
   ["AI Detection"
    :if (lambda () (locate-library "pangram"))
    ("a d" "Detect AI" pangram-detect)
    ("a c" "Clear overlays" pangram-clear)]
   ["AI Editing"
    :if (lambda () (locate-library "gptel"))
    ("c" "Generate citation" wikipedia-ai-cite)
    ("R" "Review watchlist" wikipedia-ai-review-watchlist)]])

(provide 'wikipedia-transient)

;;; wikipedia-transient.el ends here
