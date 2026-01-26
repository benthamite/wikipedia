;;; wikipedia-xtools.el --- XTools integration for wikipedia.el -*- lexical-binding: t -*-

;; Copyright (C) 2025

;;; Commentary:

;; Fetch and display user statistics from XTools API.
;; https://www.mediawiki.org/wiki/XTools/API

;;; Code:

(require 'json)
(require 'url)

(defgroup wikipedia-xtools nil
  "XTools integration settings."
  :group 'wikipedia)

(defcustom wikipedia-xtools-base-url "https://xtools.wmcloud.org/api"
  "Base URL for XTools API."
  :type 'string
  :group 'wikipedia-xtools)

(defcustom wikipedia-xtools-project "en.wikipedia.org"
  "Default project for XTools queries."
  :type 'string
  :group 'wikipedia-xtools)

(defun wikipedia-xtools--api-call (endpoint)
  "Make an XTools API call to ENDPOINT and return parsed JSON."
  (let* ((url-request-method "GET")
         (url (concat wikipedia-xtools-base-url "/" endpoint))
         (buffer (url-retrieve-synchronously url t)))
    (when buffer
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (re-search-forward "^$" nil t)
            (forward-char)
            (json-read))
        (kill-buffer buffer)))))

;;;###autoload
(defun wikipedia-xtools-user-stats (username)
  "Display XTools statistics for USERNAME."
  (interactive "sUsername: ")
  (let* ((endpoint (format "user/simple_editcount/%s/%s"
                           wikipedia-xtools-project
                           (url-hexify-string username)))
         (data (wikipedia-xtools--api-call endpoint)))
    (if data
        (wikipedia-xtools--display-user-stats username data)
      (error "Failed to fetch XTools data for %s" username))))

(defun wikipedia-xtools--display-user-stats (username data)
  "Display user stats for USERNAME from DATA."
  (let ((buf (get-buffer-create (format "*XTools: %s*" username))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "XTools Statistics: %s\n" username)
                            'face 'bold))
        (insert (make-string 50 ?─) "\n\n")
        (wikipedia-xtools--insert-stat "User ID" (alist-get 'user_id data))
        (wikipedia-xtools--insert-stat "Edit count" (alist-get 'live_edit_count data))
        (wikipedia-xtools--insert-stat "Deleted edits" (alist-get 'deleted_edit_count data))
        (wikipedia-xtools--insert-stat "First edit" (alist-get 'first_edit data))
        (wikipedia-xtools--insert-stat "Latest edit" (alist-get 'latest_edit data))
        (goto-char (point-min)))
      (special-mode))
    (pop-to-buffer buf)))

(defun wikipedia-xtools--insert-stat (label value)
  "Insert a statistic with LABEL and VALUE."
  (when value
    (insert (format "%-20s %s\n" (concat label ":") value))))

;;;###autoload
(defun wikipedia-xtools-page-stats (title)
  "Display XTools page statistics for TITLE."
  (interactive "sPage title: ")
  (let* ((endpoint (format "page/articleinfo/%s/%s"
                           wikipedia-xtools-project
                           (url-hexify-string title)))
         (data (wikipedia-xtools--api-call endpoint)))
    (if data
        (wikipedia-xtools--display-page-stats title data)
      (error "Failed to fetch XTools data for %s" title))))

(defun wikipedia-xtools--display-page-stats (title data)
  "Display page stats for TITLE from DATA."
  (let ((buf (get-buffer-create (format "*XTools Page: %s*" title))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "XTools Page Statistics: %s\n" title)
                            'face 'bold))
        (insert (make-string 50 ?─) "\n\n")
        (wikipedia-xtools--insert-stat "Page ID" (alist-get 'page_id data))
        (wikipedia-xtools--insert-stat "Revisions" (alist-get 'revisions data))
        (wikipedia-xtools--insert-stat "Editors" (alist-get 'editors data))
        (wikipedia-xtools--insert-stat "Minor edits" (alist-get 'minor_edits data))
        (wikipedia-xtools--insert-stat "Average edit size" (alist-get 'average_edit_size data))
        (wikipedia-xtools--insert-stat "Page size" (alist-get 'page_size data))
        (wikipedia-xtools--insert-stat "Created" (alist-get 'created_at data))
        (wikipedia-xtools--insert-stat "Created by" (alist-get 'author data))
        (let ((top-editors (alist-get 'top_editors data)))
          (when top-editors
            (insert "\n" (propertize "Top Editors:\n" 'face 'bold))
            (dolist (editor top-editors)
              (let ((name (car editor))
                    (count (cdr editor)))
                (insert (format "  %-25s %s edits\n" name count))))))
        (goto-char (point-min)))
      (special-mode))
    (pop-to-buffer buf)))

;;;###autoload
(defun wikipedia-xtools-top-editors (title)
  "Display top editors for page TITLE."
  (interactive "sPage title: ")
  (let* ((endpoint (format "page/top_editors/%s/%s"
                           wikipedia-xtools-project
                           (url-hexify-string title)))
         (data (wikipedia-xtools--api-call endpoint)))
    (if data
        (wikipedia-xtools--display-top-editors title data)
      (error "Failed to fetch XTools data for %s" title))))

(defun wikipedia-xtools--display-top-editors (title data)
  "Display top editors for TITLE from DATA."
  (let ((buf (get-buffer-create (format "*XTools Top Editors: %s*" title)))
        (editors (alist-get 'top_editors data)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Top Editors: %s\n" title)
                            'face 'bold))
        (insert (make-string 50 ?─) "\n\n")
        (if editors
            (dolist (editor editors)
              (let ((name (alist-get 'username editor))
                    (count (alist-get 'count editor))
                    (minor (alist-get 'minor editor))
                    (first (alist-get 'first_edit editor))
                    (last (alist-get 'last_edit editor)))
                (insert (format "%-25s %5d edits" (or name "?") (or count 0)))
                (when minor
                  (insert (format " (%d minor)" minor)))
                (insert "\n")
                (when (or first last)
                  (insert (format "  %s - %s\n"
                                  (or first "?")
                                  (or last "?"))))))
          (insert "No editor data available.\n"))
        (goto-char (point-min)))
      (special-mode))
    (pop-to-buffer buf)))

(provide 'wikipedia-xtools)

;;; wikipedia-xtools.el ends here
