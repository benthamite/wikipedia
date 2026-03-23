;;; wikipedia.el --- Emacs interface for Wikipedia editing -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Pablo Stafforini

;; Author: Pablo Stafforini
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (mediawiki "2.4.9"))
;; Keywords: wiki, wikipedia, mediawiki
;; URL: https://github.com/pablostafforini/wikipedia.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a comprehensive Emacs interface for Wikipedia and
;; other MediaWiki sites, with a focus on fast editing workflows, review
;; tools, and optional local/offline browsing of watched pages.
;;
;; Main entry points:
;;   `wikipedia-open' - Open a Wikipedia page for editing
;;   `wikipedia-publish' - Publish the current buffer to Wikipedia
;;   `wikipedia-watchlist' - Browse your watchlist
;;   `wikipedia-history' - Browse page revision history
;;
;; Configuration:
;;   Use `mediawiki-site' to configure your wiki sites first.

;;; Code:

(defgroup wikipedia nil
  "Emacs interface for Wikipedia editing."
  :group 'external
  :prefix "wikipedia-")

(require 'wikipedia-adapter)
(require 'wikipedia-cache)
(require 'wikipedia-common)
(require 'wikipedia-diff)
(require 'wikipedia-draft)
(require 'wikipedia-page)
(require 'wikipedia-history)
(require 'wikipedia-watchlist)
(require 'wikipedia-db)
(require 'wikipedia-sync)
(require 'wikipedia-mirror)
(require 'wikipedia-xtools)
(require 'wikipedia-user)
(require 'wikipedia-ai)
(require 'wikipedia-ai-review)
(require 'wikipedia-auto)

(defvar wikipedia-edit-mode-map (make-sparse-keymap)
  "Keymap for Wikipedia editing commands.
This map can be activated as a minor mode in mediawiki-mode buffers.
Overrides `mediawiki-mode' bindings for save/publish so that
`wikipedia-publish' (with AI summary support) is used instead.")

;; Keybindings are set outside `defvar' so they take effect on reload.
(define-key wikipedia-edit-mode-map (kbd "C-x C-s") #'wikipedia-publish)
(define-key wikipedia-edit-mode-map (kbd "C-c C-c") #'wikipedia-publish)
(define-key wikipedia-edit-mode-map (kbd "C-c C-p") #'wikipedia-preview)
(define-key wikipedia-edit-mode-map (kbd "C-c C-d") #'wikipedia-diff-to-live)
(define-key wikipedia-edit-mode-map (kbd "C-c C-s") #'wikipedia-ai-summarize)

;;;###autoload
(define-minor-mode wikipedia-edit-mode
  "Minor mode for Wikipedia editing commands.
Provides keybindings for preview and other wikipedia.el features
when editing a wiki page."
  :lighter " WP"
  :keymap wikipedia-edit-mode-map)

(provide 'wikipedia)

;;; wikipedia.el ends here
