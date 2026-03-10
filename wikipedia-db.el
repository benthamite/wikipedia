;;; wikipedia-db.el --- SQLite storage for Wikipedia pages -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 Pablo Stafforini

;;; Commentary:

;; Local SQLite database for storing Wikipedia pages and revisions.

;;; Code:

(require 'sqlite)

(defgroup wikipedia-db nil
  "Local database settings for Wikipedia."
  :group 'wikipedia)

(defcustom wikipedia-db-file
  (expand-file-name "wikipedia.db" user-emacs-directory)
  "Path to the SQLite database file."
  :type 'file
  :group 'wikipedia-db)

(defvar wikipedia-db--connection nil
  "Active database connection.")

(defun wikipedia-db--ensure-connection ()
  "Ensure database connection is open, creating schema if needed."
  (unless (and wikipedia-db--connection
               (sqlitep wikipedia-db--connection))
    (setq wikipedia-db--connection (sqlite-open wikipedia-db-file))
    (wikipedia-db--init-schema))
  wikipedia-db--connection)

(defun wikipedia-db--init-schema ()
  "Initialize database schema."
  (let ((db wikipedia-db--connection))
    (sqlite-execute db "
      CREATE TABLE IF NOT EXISTS pages (
        id INTEGER PRIMARY KEY,
        title TEXT NOT NULL,
        site TEXT NOT NULL DEFAULT 'en.wikipedia.org',
        last_synced INTEGER,
        watched INTEGER DEFAULT 0,
        UNIQUE(title, site)
      )")
    (sqlite-execute db "
      CREATE TABLE IF NOT EXISTS revisions (
        id INTEGER PRIMARY KEY,
        page_id INTEGER NOT NULL,
        revid INTEGER NOT NULL,
        parentid INTEGER,
        user TEXT,
        timestamp TEXT,
        comment TEXT,
        size INTEGER,
        FOREIGN KEY (page_id) REFERENCES pages(id),
        UNIQUE (page_id, revid)
      )")
    (sqlite-execute db "
      CREATE TABLE IF NOT EXISTS content (
        revision_id INTEGER PRIMARY KEY,
        content TEXT,
        FOREIGN KEY (revision_id) REFERENCES revisions(id)
      )")
    (sqlite-execute db "
      CREATE TABLE IF NOT EXISTS ai_scores (
        page_id INTEGER PRIMARY KEY,
        score REAL NOT NULL,
        reason TEXT DEFAULT '',
        old_revid INTEGER,
        revid INTEGER,
        scored_at INTEGER,
        FOREIGN KEY (page_id) REFERENCES pages(id)
      )")
    (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_pages_title ON pages(title)")
    (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_revisions_page ON revisions(page_id)")
    (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_revisions_revid ON revisions(revid)")))

(defun wikipedia-db-close ()
  "Close the database connection."
  (interactive)
  (when (and wikipedia-db--connection
             (sqlitep wikipedia-db--connection))
    (sqlite-close wikipedia-db--connection)
    (setq wikipedia-db--connection nil)))

(defun wikipedia-db-get-page (title &optional site)
  "Get page record for TITLE from SITE."
  (let ((db (wikipedia-db--ensure-connection))
        (site (or site "en.wikipedia.org")))
    (car (sqlite-select db
                        "SELECT id, title, site, last_synced, watched FROM pages WHERE title = ? AND site = ?"
                        (list title site)))))

(defun wikipedia-db-insert-page (title &optional site)
  "Insert or get page record for TITLE from SITE.  Return page id."
  (let ((db (wikipedia-db--ensure-connection))
        (site (or site "en.wikipedia.org")))
    (sqlite-execute db
                    "INSERT OR IGNORE INTO pages (title, site) VALUES (?, ?)"
                    (list title site))
    (caar (sqlite-select db
                         "SELECT id FROM pages WHERE title = ? AND site = ?"
                         (list title site)))))

(defun wikipedia-db-update-page-synced (page-id)
  "Update last_synced timestamp for PAGE-ID."
  (let ((db (wikipedia-db--ensure-connection)))
    (sqlite-execute db
                    "UPDATE pages SET last_synced = ? WHERE id = ?"
                    (list (time-convert nil 'integer) page-id))))

(defun wikipedia-db-set-watched (title watched &optional site)
  "Set WATCHED status for page TITLE from SITE."
  (let ((db (wikipedia-db--ensure-connection))
        (site (or site "en.wikipedia.org")))
    (wikipedia-db-insert-page title site)
    (sqlite-execute db
                    "UPDATE pages SET watched = ? WHERE title = ? AND site = ?"
                    (list (if watched 1 0) title site))))

(defun wikipedia-db-get-watched-pages (&optional site)
  "Get all watched pages from SITE."
  (let ((db (wikipedia-db--ensure-connection))
        (site (or site "en.wikipedia.org")))
    (sqlite-select db
                   "SELECT id, title, last_synced FROM pages WHERE watched = 1 AND site = ?"
                   (list site))))

(defun wikipedia-db-insert-revision (page-id revid &optional parentid user timestamp comment size)
  "Insert revision for PAGE-ID.  Return revision row id."
  (let ((db (wikipedia-db--ensure-connection)))
    (sqlite-execute db
                    "INSERT OR IGNORE INTO revisions (page_id, revid, parentid, user, timestamp, comment, size)
                     VALUES (?, ?, ?, ?, ?, ?, ?)"
                    (list page-id revid parentid user timestamp comment size))
    (caar (sqlite-select db
                         "SELECT id FROM revisions WHERE page_id = ? AND revid = ?"
                         (list page-id revid)))))

(defun wikipedia-db-get-revisions (page-id &optional limit)
  "Get revisions for PAGE-ID, optionally limited to LIMIT entries."
  (let ((db (wikipedia-db--ensure-connection))
        (limit (or limit 50)))
    (sqlite-select db
                   "SELECT id, revid, parentid, user, timestamp, comment, size
                    FROM revisions WHERE page_id = ? ORDER BY revid DESC LIMIT ?"
                   (list page-id limit))))

(defun wikipedia-db-get-revision-by-revid (revid)
  "Get revision record by REVID."
  (let ((db (wikipedia-db--ensure-connection)))
    (car (sqlite-select db
                        "SELECT r.id, r.page_id, r.revid, r.parentid, r.user, r.timestamp, r.comment, r.size, p.title
                         FROM revisions r JOIN pages p ON r.page_id = p.id WHERE r.revid = ?"
                        (list revid)))))

(defun wikipedia-db-insert-content (revision-id content)
  "Insert CONTENT for REVISION-ID.
CONTENT must be a string or nil."
  (when (and revision-id (stringp content))
    (let ((db (wikipedia-db--ensure-connection)))
      (sqlite-execute db
                      "INSERT OR REPLACE INTO content (revision_id, content) VALUES (?, ?)"
                      (list revision-id content)))))

(defun wikipedia-db-get-content (revision-id)
  "Get content for REVISION-ID."
  (let ((db (wikipedia-db--ensure-connection)))
    (caar (sqlite-select db
                         "SELECT content FROM content WHERE revision_id = ?"
                         (list revision-id)))))

(defun wikipedia-db-has-content-p (revision-id)
  "Check if content exists for REVISION-ID."
  (let ((db (wikipedia-db--ensure-connection)))
    (caar (sqlite-select db
                         "SELECT 1 FROM content WHERE revision_id = ?"
                         (list revision-id)))))

(defun wikipedia-db-get-latest-revision (page-id)
  "Get the latest revision for PAGE-ID."
  (let ((db (wikipedia-db--ensure-connection)))
    (car (sqlite-select db
                        "SELECT id, revid, parentid, user, timestamp, comment, size
                         FROM revisions WHERE page_id = ? ORDER BY revid DESC LIMIT 1"
                        (list page-id)))))

(defun wikipedia-db-stats ()
  "Return database statistics."
  (interactive)
  (let ((db (wikipedia-db--ensure-connection)))
    (let ((pages (caar (sqlite-select db "SELECT COUNT(*) FROM pages")))
          (watched (caar (sqlite-select db "SELECT COUNT(*) FROM pages WHERE watched = 1")))
          (revisions (caar (sqlite-select db "SELECT COUNT(*) FROM revisions")))
          (content (caar (sqlite-select db "SELECT COUNT(*) FROM content"))))
      (message "Wikipedia DB: %d pages (%d watched), %d revisions, %d with content"
               pages watched revisions content)
      (list :pages pages :watched watched :revisions revisions :content content))))

;;;; AI review scores

(defun wikipedia-db-set-ai-score (title score reason &optional old-revid revid site)
  "Store AI review SCORE and REASON for page TITLE."
  (let* ((db (wikipedia-db--ensure-connection))
         (page-id (wikipedia-db-insert-page title site)))
    (sqlite-execute db
                    "INSERT OR REPLACE INTO ai_scores
                     (page_id, score, reason, old_revid, revid, scored_at)
                     VALUES (?, ?, ?, ?, ?, ?)"
                    (list page-id score reason old-revid revid
                          (time-convert nil 'integer)))))

(defun wikipedia-db-get-ai-scores (&optional site)
  "Get all AI review scores for SITE.
Returns a list of (TITLE SCORE REASON) lists."
  (let ((db (wikipedia-db--ensure-connection))
        (site (or site "en.wikipedia.org")))
    (sqlite-select db
                   "SELECT p.title, a.score, a.reason
                    FROM ai_scores a
                    JOIN pages p ON a.page_id = p.id
                    WHERE p.site = ?"
                   (list site))))

(defun wikipedia-db-clear-ai-scores (&optional site)
  "Clear all AI review scores for SITE."
  (let ((db (wikipedia-db--ensure-connection))
        (site (or site "en.wikipedia.org")))
    (sqlite-execute db
                    "DELETE FROM ai_scores WHERE page_id IN
                     (SELECT id FROM pages WHERE site = ?)"
                    (list site))))

(provide 'wikipedia-db)

;;; wikipedia-db.el ends here
