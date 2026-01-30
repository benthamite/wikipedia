;;; wikipedia-xtools.el --- XTools integration for wikipedia.el -*- lexical-binding: t -*-

;; Copyright (C) 2025

;;; Commentary:

;; Fetch and display user statistics from XTools API.
;; https://www.mediawiki.org/wiki/XTools/API

;;; Code:

(require 'json)
(require 'url)
(require 'wikipedia-adapter)

(declare-function wikipedia--user-at-point "wikipedia")

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

(defun wikipedia-xtools--get-mediawiki-user-info (username)
  "Get additional user info from MediaWiki API for USERNAME.
Returns an alist with registration date and contribution timestamps."
  (condition-case err
      (let* ((result (wp--api-call "query"
                                   (list (cons "list" "users")
                                         (cons "ususers" username)
                                         (cons "usprop" "registration|editcount|gender"))))
             (users (cddr (assq 'users (cddr result))))
             (user-data (when users (cadr (car users))))
             (registration (cdr (assq 'registration user-data)))
             (editcount (cdr (assq 'editcount user-data)))
             (gender (cdr (assq 'gender user-data)))
             ;; Get first and last contributions
             (contribs (wp--get-user-contributions username 1))
             (latest-contrib (car contribs))
             (latest-timestamp (when latest-contrib 
                                (alist-get 'timestamp latest-contrib)))
             ;; Get oldest contribution
             (oldest-result (wp--api-call "query"
                                         (list (cons "list" "usercontribs")
                                               (cons "ucuser" username)
                                               (cons "ucprop" "timestamp")
                                               (cons "ucdir" "newer")
                                               (cons "uclimit" "1"))))
             (oldest-contribs (cddr (assq 'usercontribs (cddr oldest-result))))
             (oldest-contrib (when oldest-contribs (cadr (car oldest-contribs))))
             (first-timestamp (cdr (assq 'timestamp oldest-contrib))))
        `((registration . ,registration)
          (api_editcount . ,editcount)
          (gender . ,gender)
          (first_edit . ,first-timestamp)
          (latest_edit . ,latest-timestamp)))
    (error
     (message "Failed to fetch MediaWiki user info: %s" (error-message-string err))
     nil)))

(defun wikipedia-xtools--get-top-edits (username)
  "Get top edits for USERNAME from XTools API.
Returns an alist with top edited pages."
  (condition-case err
      (let* ((endpoint (format "user/top_edits/%s/%s/0"
                               wikipedia-xtools-project
                               (url-hexify-string username)))
             (data (wikipedia-xtools--api-call endpoint))
             (top-edits (alist-get 'top_edits data))
             (namespace-0 (when top-edits (alist-get '0 top-edits))))
        `((top_edits_data . ,namespace-0)))
    (error
     (message "Failed to fetch top edits: %s" (error-message-string err))
     nil)))



;;;###autoload
(defun wikipedia-xtools-user-stats (username)
  "Display XTools statistics for USERNAME.
If called interactively and point is on a user (in watchlist, history,
or user contributions mode), use that user. Otherwise, prompt for username."
  (interactive
   (list (or (wikipedia--user-at-point)
             (read-string "Username: "))))
  (message "Fetching statistics for %s..." username)
  (let* ((endpoint (format "user/simple_editcount/%s/%s"
                           wikipedia-xtools-project
                           (url-hexify-string username)))
         (xtools-data (wikipedia-xtools--api-call endpoint))
         (mw-data (wikipedia-xtools--get-mediawiki-user-info username))
         (top-edits-data (wikipedia-xtools--get-top-edits username))
         (combined-data (append xtools-data mw-data top-edits-data)))
    (if combined-data
        (wikipedia-xtools--display-user-stats username combined-data)
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
        
        ;; Basic info
        (wikipedia-xtools--insert-stat "User ID" (alist-get 'user_id data))
        (wikipedia-xtools--insert-stat "Project" (alist-get 'project data))
        (let ((registration (alist-get 'registration data)))
          (when registration
            (wikipedia-xtools--insert-stat "Registered" 
              (wikipedia-xtools--format-timestamp registration))))
        
        ;; Admin status
        (let ((local-groups (alist-get 'user_groups data)))
          (when (wikipedia-xtools--is-admin local-groups)
            (insert "\n" (propertize "★ Administrator ★" 'face '(:foreground "gold" :weight bold)) "\n")))
        
        ;; Edit counts
        (insert "\n" (propertize "Edit Statistics\n" 'face 'bold))
        (wikipedia-xtools--insert-stat "Live edits" (alist-get 'live_edit_count data))
        (wikipedia-xtools--insert-stat "Deleted edits" (alist-get 'deleted_edit_count data))
        (let ((live (alist-get 'live_edit_count data))
              (deleted (alist-get 'deleted_edit_count data)))
          (when (and live deleted)
            (wikipedia-xtools--insert-stat "Total edits" (+ live deleted))))
        (wikipedia-xtools--insert-stat "Pages created" (alist-get 'creation_count data))
        
        ;; Timestamps
        (let ((first (alist-get 'first_edit data))
              (latest (alist-get 'latest_edit data)))
          (when (or first latest)
            (insert "\n" (propertize "Activity Period\n" 'face 'bold))
            (when first
              (wikipedia-xtools--insert-stat "First edit" 
                (wikipedia-xtools--format-timestamp first)))
            (when latest
              (wikipedia-xtools--insert-stat "Latest edit" 
                (wikipedia-xtools--format-timestamp latest)))))
        
        ;; User groups
        (let ((local-groups (alist-get 'user_groups data))
              (global-groups (alist-get 'global_user_groups data)))
          (when (or local-groups global-groups)
            (insert "\n" (propertize "User Groups\n" 'face 'bold))
            (when local-groups
              (let ((groups-str (if (sequencep local-groups)
                                    (mapconcat #'identity local-groups ", ")
                                  local-groups)))
                (when (and (stringp groups-str) (not (string-empty-p groups-str)))
                  (wikipedia-xtools--insert-stat "Local groups" groups-str))))
            (when global-groups
              (let ((groups-str (if (sequencep global-groups)
                                    (mapconcat #'identity global-groups ", ")
                                  global-groups)))
                (when (and (stringp groups-str) (not (string-empty-p groups-str)))
                  (wikipedia-xtools--insert-stat "Global groups" groups-str))))))
        
        ;; Top edits
        (let ((top-edits (alist-get 'top_edits_data data)))
          (when (and top-edits (sequencep top-edits) (> (length top-edits) 0))
            (insert "\n" (propertize "Top Edited Pages (mainspace)\n" 'face 'bold))
            (let ((count 0))
              (while (and (< count 10) (< count (length top-edits)))
                (let* ((page (elt top-edits count))
                       (title (cdr (assq 'page_title page)))
                       (edits (cdr (assq 'count page))))
                  (when (and title edits)
                    (insert (format "  %-40s %5d edits\n" 
                                    (if (> (length title) 40)
                                        (concat (substring title 0 37) "...")
                                      title)
                                    edits))))
                (setq count (1+ count))))))
        
        (goto-char (point-min)))
      (special-mode))
    (pop-to-buffer buf)))

(defun wikipedia-xtools--format-timestamp (timestamp)
  "Format TIMESTAMP for display.
TIMESTAMP is expected to be in ISO 8601 format from MediaWiki API."
  (if timestamp
      (replace-regexp-in-string "T" " " (substring timestamp 0 (min 19 (length timestamp))))
    ""))

(defun wikipedia-xtools--is-admin (groups)
  "Check if GROUPS contains admin/sysop privileges.
GROUPS can be a vector or list of group names."
  (when groups
    (let ((groups-list (if (sequencep groups)
                          (append groups nil)
                        (list groups))))
      (seq-some (lambda (group) (member group '("sysop" "bureaucrat"))) 
                groups-list))))

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
