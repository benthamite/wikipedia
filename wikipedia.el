;;; wikipedia.el --- Emacs interface for Wikipedia editing -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (mediawiki "2.4.9"))
;; Keywords: wiki, wikipedia, mediawiki
;; URL: https://github.com/your/wikipedia-el

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
;;   `wikipedia-save' - Save the current buffer to Wikipedia
;;
;; Configuration:
;;   Use `mediawiki-site' to configure your wiki sites first.

;;; Code:

(require 'wikipedia-adapter)
(require 'wikipedia-page)
(require 'wikipedia-history)
(require 'wikipedia-watchlist)
(require 'wikipedia-db)
(require 'wikipedia-sync)
(require 'wikipedia-mirror)
(require 'wikipedia-xtools)

(defgroup wikipedia nil
  "Emacs interface for Wikipedia editing."
  :group 'external
  :prefix "wikipedia-")

(defvar wikipedia-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-p") #'wikipedia-preview)
    map)
  "Keymap for Wikipedia editing commands.
This map can be activated as a minor mode in mediawiki-mode buffers.")

;;;###autoload
(define-minor-mode wikipedia-edit-mode
  "Minor mode for Wikipedia editing commands.
Provides keybindings for preview and other wikipedia.el features
when editing a wiki page."
  :lighter " WP"
  :keymap wikipedia-edit-mode-map)

(provide 'wikipedia)

;;; wikipedia.el ends here
