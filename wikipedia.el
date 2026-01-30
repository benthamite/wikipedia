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
(require 'wikipedia-user)

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

(defun wikipedia-thank (revid &optional user)
  "Thank the author of revision REVID.
If USER is provided, it is used in the confirmation message."
  (interactive (list (wikipedia--revid-at-point)
                     (wikipedia--user-at-point)))
  (unless revid
    (error "No revision at point"))
  (when (yes-or-no-p (format "Thank %s for this edit? "
                             (or user "the user")))
    (condition-case err
        (progn
          (wp--thank-revision revid)
          (message "Thanks sent for revision %s" revid))
      (error
       (message "Failed to send thanks: %s" (error-message-string err))))))

(defun wikipedia--revid-at-point ()
  "Return the revision ID at point, or nil.
This function checks various contexts to find a revision ID."
  (cond
   ((derived-mode-p 'wikipedia-watchlist-mode)
    (wikipedia-watchlist--revid-at-point))
   ((derived-mode-p 'wikipedia-history-mode)
    (wikipedia-history--revid-at-point))
   ((derived-mode-p 'wikipedia-user-contributions-mode)
    (tabulated-list-get-id))
   (t nil)))

(defun wikipedia--user-at-point ()
  "Return the username at point, or nil.
This function checks various contexts to find a username."
  (cond
   ((derived-mode-p 'wikipedia-watchlist-mode)
    (wikipedia-watchlist--user-at-point))
   ((derived-mode-p 'wikipedia-history-mode)
    (wikipedia-history--user-at-point))
   ((derived-mode-p 'wikipedia-user-contributions-mode)
    (bound-and-true-p wikipedia-user--username))
   (t nil)))

(defun wikipedia--page-title-at-point ()
  "Return the page title at point, or nil.
This function checks various contexts to find a page title."
  (cond
   ;; In watchlist mode
   ((derived-mode-p 'wikipedia-watchlist-mode)
    (wikipedia-watchlist--title-at-point))
   ;; In history mode
   ((derived-mode-p 'wikipedia-history-mode)
    (bound-and-true-p wikipedia-history--page-title))
   ;; In user contributions mode
   ((derived-mode-p 'wikipedia-user-contributions-mode)
    (let ((contrib (wikipedia-user--contrib-at-point)))
      (when contrib (alist-get 'title contrib))))
   ;; In a mediawiki editing buffer
   ((bound-and-true-p mediawiki-page-title)
    mediawiki-page-title)
   (t nil)))

(declare-function wikipedia-watchlist--revid-at-point "wikipedia-watchlist")
(declare-function wikipedia-watchlist--user-at-point "wikipedia-watchlist")
(declare-function wikipedia-watchlist--title-at-point "wikipedia-watchlist")
(declare-function wikipedia-history--revid-at-point "wikipedia-history")
(declare-function wikipedia-history--user-at-point "wikipedia-history")
(declare-function wikipedia-user--contrib-at-point "wikipedia-user")

;;;###autoload
(define-minor-mode wikipedia-edit-mode
  "Minor mode for Wikipedia editing commands.
Provides keybindings for preview and other wikipedia.el features
when editing a wiki page."
  :lighter " WP"
  :keymap wikipedia-edit-mode-map)

(provide 'wikipedia)

;;; wikipedia.el ends here
