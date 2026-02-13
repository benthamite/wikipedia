;;; wikipedia-draft.el --- Per-page local drafts for wikipedia.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This module provides a per-page local draft system for wikipedia.el.
;; Each draft is stored as a separate .wiki file in a dedicated directory,
;; keyed by page title.  Drafts can be saved, opened, and deleted via
;; interactive commands.

;;; Code:

(require 'mediawiki)

(defcustom wikipedia-draft-directory
  (expand-file-name "wikipedia-drafts" user-emacs-directory)
  "Directory where local Wikipedia drafts are stored.
Each draft is saved as a separate .wiki file named after the page title."
  :type 'directory
  :group 'wikipedia)

;;;###autoload
(defun wikipedia-draft-save ()
  "Save the current editing buffer as a local draft.
The draft is stored in `wikipedia-draft-directory' as a .wiki file
named after the page title."
  (interactive)
  (let ((title (wikipedia-draft--require-title)))
    (wikipedia-draft--write title (buffer-substring-no-properties
                                   (point-min) (point-max)))
    (message "Draft saved for \"%s\"" title)))

;;;###autoload
(defun wikipedia-draft-open ()
  "Open a previously saved draft for editing.
Prompts for a draft to open from the list of saved drafts."
  (interactive)
  (let* ((drafts (wikipedia-draft--list))
         (title (wikipedia-draft--read-draft "Open draft: " drafts)))
    (wikipedia-draft--open-file title)))

;;;###autoload
(defun wikipedia-draft-delete ()
  "Delete a previously saved draft.
Prompts for a draft to delete from the list of saved drafts."
  (interactive)
  (let* ((drafts (wikipedia-draft--list))
         (title (wikipedia-draft--read-draft "Delete draft: " drafts)))
    (wikipedia-draft--delete-file title)))

(defun wikipedia-draft--require-title ()
  "Return the page title for the current buffer.
Signal an error if no page title is set."
  (or (bound-and-true-p mediawiki-page-title)
      (error "No page title set in this buffer")))

(defun wikipedia-draft--read-draft (prompt drafts)
  "Read a draft title using PROMPT from DRAFTS.
Signal an error if DRAFTS is empty."
  (when (null drafts)
    (error "No drafts found in %s" wikipedia-draft-directory))
  (completing-read prompt drafts nil t))

(defun wikipedia-draft--write (title content)
  "Write CONTENT to the draft file for TITLE."
  (wikipedia-draft--ensure-directory)
  (let ((file (wikipedia-draft--file-for-title title))
        (coding-system-for-write 'utf-8))
    (with-temp-file file
      (insert content))))

(defun wikipedia-draft--ensure-directory ()
  "Create `wikipedia-draft-directory' if it does not exist."
  (make-directory wikipedia-draft-directory t))

(defun wikipedia-draft--file-for-title (title)
  "Return the draft file path for TITLE."
  (expand-file-name
   (concat (wikipedia-draft--encode-title title) ".wiki")
   wikipedia-draft-directory))

(defun wikipedia-draft--encode-title (title)
  "Encode TITLE into a safe filename.
Characters not matching [a-zA-Z0-9 .-] are replaced with _XX
where XX is the hex code of the character."
  (mapconcat
   (lambda (char)
     (if (string-match-p "[a-zA-Z0-9 .-]" (char-to-string char))
         (char-to-string char)
       (format "_%02X" char)))
   title ""))

(defun wikipedia-draft--decode-title (encoded)
  "Decode ENCODED filename back into a page title.
Reverses the encoding done by `wikipedia-draft--encode-title'."
  (let ((result "")
        (i 0)
        (len (length encoded)))
    (while (< i len)
      (if (and (= (aref encoded i) ?_)
               (<= (+ i 3) len)
               (string-match-p "\\`[0-9A-Fa-f][0-9A-Fa-f]\\'"
                               (substring encoded (1+ i) (+ i 3))))
          (progn
            (setq result (concat result (char-to-string
                                         (string-to-number
                                          (substring encoded (1+ i) (+ i 3))
                                          16))))
            (setq i (+ i 3)))
        (setq result (concat result (char-to-string (aref encoded i))))
        (setq i (1+ i))))
    result))

(defun wikipedia-draft--list ()
  "Return a list of page titles that have saved drafts."
  (when (file-directory-p wikipedia-draft-directory)
    (let ((files (directory-files wikipedia-draft-directory nil "\\.wiki\\'")))
      (mapcar (lambda (f)
                (wikipedia-draft--decode-title
                 (file-name-sans-extension f)))
              files))))

(declare-function wikipedia-edit-mode "wikipedia")

(defun wikipedia-draft--open-file (title)
  "Open the draft file for TITLE and set up the editing environment.
Activates `mediawiki-mode', sets `mediawiki-page-title', and
enables `wikipedia-edit-mode'."
  (let ((file (wikipedia-draft--file-for-title title)))
    (unless (file-exists-p file)
      (error "Draft file not found for \"%s\"" title))
    (find-file file)
    (mediawiki-mode)
    (setq mediawiki-page-title title)
    (wikipedia-edit-mode 1)))

(defun wikipedia-draft--delete-file (title)
  "Delete the draft file for TITLE after confirmation."
  (let ((file (wikipedia-draft--file-for-title title)))
    (unless (file-exists-p file)
      (error "Draft file not found for \"%s\"" title))
    (when (yes-or-no-p (format "Delete draft for \"%s\"?" title))
      (delete-file file)
      (message "Draft deleted for \"%s\"" title))))

(provide 'wikipedia-draft)

;;; wikipedia-draft.el ends here
