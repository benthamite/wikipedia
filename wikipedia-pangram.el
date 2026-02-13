;;; wikipedia-pangram.el --- AI content detection via Pangram API -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Detect AI-generated content using the Pangram Labs API (v3).
;; Highlights AI-generated and AI-assisted text segments with overlays.
;; https://pangram.readthedocs.io/en/latest/api/rest.html

;;; Code:

(require 'json)
(require 'url)
(require 'auth-source-pass)
(require 'seq)

(defgroup wikipedia-pangram nil
  "AI content detection via Pangram Labs."
  :group 'wikipedia)

(defcustom wikipedia-pangram-api-url "https://text.api.pangram.com/v3"
  "URL for the Pangram v3 inference API."
  :type 'string
  :group 'wikipedia-pangram)

(defface wikipedia-pangram-ai
  '((((background dark)) :background "#5c2020")
    (((background light)) :background "#ffcccc"))
  "Face for AI-generated text segments."
  :group 'wikipedia-pangram)

(defface wikipedia-pangram-ai-assisted
  '((((background dark)) :background "#5c4020")
    (((background light)) :background "#fff0cc"))
  "Face for AI-assisted text segments."
  :group 'wikipedia-pangram)

;;;###autoload
(defun wikipedia-pangram-detect ()
  "Detect AI-generated content in the buffer or active region.
Sends the text to the Pangram API and highlights AI-generated and
AI-assisted segments with overlays.  Hover over highlighted text
to see the classification details."
  (interactive)
  (let* ((use-region (use-region-p))
         (beg (if use-region (region-beginning) (point-min)))
         (end (if use-region (region-end) (point-max)))
         (text (buffer-substring-no-properties beg end))
         (source-buffer (current-buffer))
         (offset beg))
    (when (string-empty-p (string-trim text))
      (error "No text to analyze"))
    (message "Sending text to Pangram API...")
    (wikipedia-pangram--api-call
     text
     (lambda (data)
       (wikipedia-pangram--handle-response data source-buffer offset)))))

;;;###autoload
(defun wikipedia-pangram-clear ()
  "Remove Pangram AI detection overlays from the current buffer."
  (interactive)
  (remove-overlays (point-min) (point-max) 'wikipedia-pangram t)
  (message "Pangram overlays cleared"))

(defun wikipedia-pangram--get-api-key ()
  "Retrieve the Pangram API key from auth-source-pass."
  (or (auth-source-pass-get "key"
        (concat "chrome/pangram.com/" (getenv "PERSONAL_EMAIL")))
      (error "Pangram API key not found in pass store")))

(defun wikipedia-pangram--api-call (text callback)
  "Send TEXT to the Pangram API.
CALLBACK is called with the parsed JSON response."
  (let* ((api-key (wikipedia-pangram--get-api-key))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("x-api-key" . ,api-key)))
         (url-request-data
          (json-serialize (list :text text))))
    (url-retrieve
     wikipedia-pangram-api-url
     (lambda (status cb)
       (if (plist-get status :error)
           (progn
             (kill-buffer)
             (message "Pangram API error: %s" (plist-get status :error)))
         (let ((response (wikipedia-pangram--parse-response)))
           (kill-buffer)
           (funcall cb response))))
     (list callback)
     t nil)))

(defun wikipedia-pangram--parse-response ()
  "Parse the JSON response from the current HTTP response buffer."
  (goto-char (point-min))
  (re-search-forward "^$" nil t)
  (forward-char)
  (json-read))

(defun wikipedia-pangram--handle-response (data source-buffer offset)
  "Apply overlays from Pangram response DATA to SOURCE-BUFFER.
OFFSET is the buffer position corresponding to text index 0."
  (unless (buffer-live-p source-buffer)
    (error "Source buffer no longer exists"))
  (let ((windows (alist-get 'windows data))
        (headline (alist-get 'headline data))
        (fraction-ai (alist-get 'fraction_ai data))
        (fraction-assisted (alist-get 'fraction_ai_assisted data))
        (fraction-human (alist-get 'fraction_human data)))
    (with-current-buffer source-buffer
      (remove-overlays (point-min) (point-max) 'wikipedia-pangram t)
      (wikipedia-pangram--apply-overlays windows offset))
    (message "Pangram: %s — AI: %.0f%%, AI-assisted: %.0f%%, Human: %.0f%%"
             headline
             (* 100 fraction-ai)
             (* 100 fraction-assisted)
             (* 100 fraction-human))))

(defun wikipedia-pangram--apply-overlays (windows offset)
  "Create overlays for AI and AI-assisted segments in WINDOWS.
OFFSET is added to each segment's start and end indices to map
them to buffer positions.  Human segments are not highlighted."
  (seq-doseq (window windows)
    (let ((face (wikipedia-pangram--label-face
                 (alist-get 'label window))))
      (when face
        (wikipedia-pangram--make-overlay window offset face)))))

(defun wikipedia-pangram--make-overlay (window offset face)
  "Create a single overlay for WINDOW at OFFSET with FACE."
  (let* ((start (+ offset (alist-get 'start_index window)))
         (end (+ offset (alist-get 'end_index window)))
         (label (alist-get 'label window))
         (score (alist-get 'ai_assistance_score window))
         (confidence (alist-get 'confidence window))
         (ov (make-overlay start end)))
    (overlay-put ov 'wikipedia-pangram t)
    (overlay-put ov 'face face)
    (overlay-put ov 'help-echo
                 (format "%s (score: %.2f, confidence: %s)"
                         label score confidence))))

(defun wikipedia-pangram--label-face (label)
  "Return the face for a segment LABEL, or nil for human text."
  (cond
   ((string-match-p "AI Generated" label) 'wikipedia-pangram-ai)
   ((string-match-p "Human" label) nil)
   (t 'wikipedia-pangram-ai-assisted)))

(provide 'wikipedia-pangram)

;;; wikipedia-pangram.el ends here
