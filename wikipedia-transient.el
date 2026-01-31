;;; wikipedia-transient.el --- Transient menu for wikipedia.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This module provides a transient menu for quick access to all
;; wikipedia.el commands.

;;; Code:

(require 'transient)
(require 'wikipedia-page)
(require 'wikipedia-watchlist)
(require 'wikipedia-history)
(require 'wikipedia-user)
(require 'wikipedia-xtools)
(require 'wikipedia-sync)
(require 'wikipedia-mirror)
(require 'wikipedia-common)

;;;###autoload (autoload 'wikipedia-transient "wikipedia-transient" nil t)
(transient-define-prefix wikipedia-transient ()
  "Wikipedia commands."
  [["Page"
    ("o" "Open page" wikipedia-open)
    ("s" "Save page" wikipedia-save)
    ("p" "Preview" wikipedia-preview)
    ("b" "Browse page" wikipedia-browse)
    ("l" "Login" wikipedia-login)]
   ["Watchlist"
    ("w" "Watchlist" wikipedia-watchlist)
    ("W" "Watch page" wikipedia-watchlist-watch)
    ("X" "Unwatch page" wikipedia-watchlist-unwatch)]
   ["History"
    ("h" "Page history" wikipedia-history)
    ("t" "Thank" wikipedia-thank)]]
  [["User"
    ("u c" "User contributions" wikipedia-user-contributions)
    ("u p" "User page" wikipedia-user-page)
    ("u t" "User talk" wikipedia-user-talk)
    ("u b" "User browse" wikipedia-user-browse)]
   ["XTools"
    ("x u" "User stats" wikipedia-xtools-user-stats)
    ("x p" "Page stats" wikipedia-xtools-page-stats)
    ("x e" "Top editors" wikipedia-xtools-top-editors)]
   ["Sync & Mirror"
    ("S p" "Sync page" wikipedia-sync-page)
    ("S w" "Sync watchlist" wikipedia-sync-watchlist)
    ("S u" "Sync update" wikipedia-sync-update)
    ("m" "Mirror" wikipedia-mirror)]])

(provide 'wikipedia-transient)

;;; wikipedia-transient.el ends here
