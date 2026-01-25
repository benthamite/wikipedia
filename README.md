# wikipedia.el

This package aims to provide a comprehensive Emacs interface for Wikipedia and
other MediaWiki sites, with a focus on fast editing workflows, review tools, and
optional local/offline browsing of watched pages.

The initial goal is to produce a cohesive "workbench" experience inside Emacs:
browse what needs attention, inspect changes, open pages for editing, preview,
and publish edits, while keeping the UI consistent and keyboard-driven.

## Goals

- Provide a practical, reliable Emacs workflow for Wikipedia editing and review.
- Support both "online" operations (via the MediaWiki API) and "local" operations
  (via a local mirror of selected pages and their revision history).
- Keep the design modular, so features can be developed and tested
  incrementally, in separate Elisp files.
- Prefer predictable behavior and explicit errors over silent fallbacks.

## Non-goals (at least initially)

- Perfect offline rendering of Wikipedia pages as they appear on the website.
  (Templates, Lua modules, site configuration, and CSS/JS make this a large
  project on its own.)
- Replacing the full Wikipedia web UI for every feature.
- Supporting every MediaWiki extension or every third-party wiki configuration.

## Projected functionality

### Editing

- Open a page for editing in an Emacs buffer using wikitext.
- Submit edits with an edit summary.
- Support common editing workflows:
  - open by title
  - open a section
  - minor edit flag (if supported)
  - edit conflict detection and clear error reporting

### Preview

- Preview the current buffer contents before saving.
- Display preview output in an Emacs buffer.
- Provide a clear path from preview to publishing (and back to editing).

### Diffs and revision inspection

- View diffs between:
  - current buffer and latest server revision
  - two arbitrary revisions of a page
  - a revision and its parent
  - edit previews
- Browse a page's revision history with useful metadata:
  - timestamp, user, edit summary, size delta, tags, minor flag
- Open a specific revision's wikitext for inspection.

### Watchlist

- View the user's watchlist in Emacs.
- Filter and search watchlist entries (by namespace, type, user, title, etc.).
- Jump from a watchlist entry to:
  - the page
  - the relevant diff
  - the page history
- Mark watchlist entries as seen/read (subject to API support and permissions).

### Local mirror

A local mirror is intended to support fast offline inspection and local diffs for
a selected set of pages, with the watchlist as the primary driver.

- Sync watched pages to a local store.
- Store full revision history (configurable limits may be added later).
- Browse locally stored pages and their revision history.
- Compare revisions locally without network access.

The local mirror is expected to store wikitext and revision metadata. Storing
rendered HTML is not a goal for the initial implementation.

### User statistics (XTools)

- Fetch and display user statistics via the XTools API.
- Present results in an Emacs buffer suitable for quick inspection and copying.

### AI integration

Integrate with AI services (via `gptel.el`) to assist with various tasks, including
- Suggesting edit improvements based on page context.
- Generating summaries for edit summaries.
- Providing explanations for complex diffs or revisions.
- Assisting in disputes by citing relevant policies or guidelines.
- Helping identify potential vandalism or biased content.
  
## User experience (UI sketch)

The package is expected to provide a small set of "main entry points" that open
dedicated buffers, similar in spirit to other Emacs workbench-style packages.

Planned buffer types:

- Watchlist buffer: a list view with actions at point (open diff, open page,
  mark seen, sync local copy).
- Page buffer: wikitext view/edit buffer with commands for preview, save, diff,
  and history.
- History buffer: revision list for a page with actions (view revision, diff).
- Diff buffer: diff-mode compatible output when possible.
- Local browser buffers: index/history/view for locally mirrored content.
- User stats buffer: a report view for XTools results.

## Architecture

The implementation is split into layers:

1. API client layer
   - HTTP request helpers, authentication, token management, continuation.
   - Returns structured data rather than directly manipulating UI buffers.
   - Uses JSON format (`format=json`) for API responses.

2. Domain operations layer
   - "Edit page", "preview", "fetch watchlist", "compare revisions", "sync page".
   - Encodes MediaWiki-specific workflows and parameters.

3. UI layer
   - Special-mode/tabulated-list buffers and interactive commands.
   - Minimal logic beyond presentation and dispatch to domain operations.

The local mirror subsystem is separate from the online features, but integrated
at the UI level (e.g., "open local copy" from watchlist).

### Relationship to mediawiki.el

This package depends on `mediawiki.el` for basic editing and session management
initially. Usage is isolated behind an adapter layer so that the dependency can
be replaced later if needed. The adapter provides:

- `wp--login`: establish session
- `wp--api-call`: make API requests
- `wp--open-for-edit`: open a page buffer (delegates to `mediawiki-open`)
- `wp--save`: save buffer to wiki (delegates to `mediawiki-save`)

This approach allows reusing proven functionality while keeping the option to
implement a custom client if `mediawiki.el` proves insufficient.

## Technical notes

### Authentication

- Use `auth-source` for credential storage (supports `~/.authinfo.gpg`).
- Modern MediaWiki prefers `action=clientlogin` over legacy `action=login`.
- Users with 2FA enabled need bot passwords (MediaWiki "app passwords").
- The package should clearly report authentication failures.

### API conventions

- Use `format=json` for all API calls (simpler than XML parsing).
- Handle continuation transparently (`continue` parameter in responses).
- Respect rate limits; retry only when safe (idempotent operations).

### Local mirror storage

Storage uses SQLite (built-in in Emacs 29+, or via `emacsql` for older versions).

Schema concepts:

- **pages table**: `pageid` (primary key), `title`, `namespace`, `last_synced_revid`
- **titles table**: `pageid`, `title`, `timestamp` (tracks renames)
- **revisions table**: `revid` (primary key), `pageid`, `parentid`, `timestamp`,
  `user`, `comment`, `minor`, `size`, `sha1`, `tags`
- **content table**: `revid`, `content` (compressed wikitext blob)

Key design decisions:

- `pageid` is the stable identifier (titles change on moves).
- Store compressed wikitext to reduce storage.
- Incremental sync: fetch revisions newer than `last_synced_revid`.

### Sync strategy

1. Fetch watchlist titles via `action=query&list=watchlistraw`.
2. For each page, fetch revisions via `action=query&prop=revisions` with
   `rvprop=ids|timestamp|user|comment|content|sha1|size|flags|tags`.
3. Use continuation (`rvcontinue`) to fetch complete history.
4. For large pages (tens of thousands of revisions), configurable policies:
   - Full history for pages under N revisions
   - Last M revisions for larger pages

### Emacs version compatibility

Target Emacs 29.1+ initially (for built-in SQLite and modern JSON support).
Earlier versions may work with `emacsql-sqlite` but are not a priority.

## Security and authentication

- Authentication should use standard Emacs mechanisms where possible (e.g.,
  auth-source).
- The package should avoid storing credentials in plain text.
- The package should clearly report authentication failures and permission
  issues.

## Roadmap (incremental)

1. Establish a minimal online workflow
   - configure a site
   - open a page
   - save an edit
   - basic error reporting

2. Add preview
   - preview buffer
   - round-trip editing → preview → editing

3. Add diffs and history browsing
   - history list
   - compare revisions
   - diff buffer integration

4. Add watchlist UI
   - list watchlist entries
   - open diff/page from entries
   - mark entries as seen (if supported)

5. Add local mirror
   - local storage schema
   - sync engine
   - local browsing and local diffs

6. Add XTools integration
   - user stats commands and buffers

## Open questions

- Target scope: Wikipedia only, or arbitrary MediaWiki sites?
- Authentication: bot passwords, OAuth, or both?
- Local mirror policy: full history always, or configurable limits for large
  pages?
- Rendering: is on-demand online rendering (without caching) sufficient for
  preview and "readable view" needs?

## Contributing

This README is intentionally a planning document. The plan is to iterate on it
until the scope and architecture are stable enough to start implementing the
first vertical slice (open → edit → preview → save).
