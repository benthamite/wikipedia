# wikipedia.el

An Emacs interface for Wikipedia and other MediaWiki sites, with a focus on fast
editing workflows, review tools, and optional local/offline browsing of watched
pages.

## Installation

Requires Emacs 29.1+ and [mediawiki.el](https://github.com/hexmode/mediawiki-el).

```elisp
(add-to-list 'load-path "/path/to/wikipedia-el")
(require 'wikipedia)
```

Configure your wiki site in mediawiki.el first:

```elisp
(setq mediawiki-site-alist
      '(("Wikipedia" "https://en.wikipedia.org/w/" "username" "" "Main Page")))
```

Store credentials in `~/.authinfo.gpg`:

```
machine en.wikipedia.org login YourUsername password YourBotPassword
```

## Usage

### Basic workflow

1. **Login**: `M-x wikipedia-login` and select your site
2. **Open a page**: `M-x wikipedia-open` and enter the page title
3. **Edit** the wikitext in the buffer
4. **Preview**: `M-x wikipedia-preview` to see rendered HTML
5. **Save**: `M-x wikipedia-save` (or `C-x C-s` in the page buffer)

### Revision history

From a page buffer or anywhere after logging in:

- `M-x wikipedia-history` - view revision history for a page

In the history buffer:

| Key     | Command                              |
|---------|--------------------------------------|
| `RET`/`v` | View revision wikitext             |
| `d`     | Diff to previous revision (prev)     |
| `c`     | Diff to current revision (cur)       |
| `D`     | Diff between two revisions (prompts) |
| `b`     | Browse revision in external browser  |
| `g`     | Refresh history                      |
| `q`     | Quit                                 |

Diffs are displayed using `ediff`, so all your ediff customizations apply.

### Preview buffer

| Key | Command                    |
|-----|----------------------------|
| `e` | Return to editing          |
| `g` | Refresh preview            |
| `q` | Quit                       |

## Package structure

- `wikipedia.el` - Main entry point and customization group
- `wikipedia-adapter.el` - Adapter layer isolating mediawiki.el dependency
- `wikipedia-page.el` - Page operations (open, save, preview)
- `wikipedia-history.el` - Revision history browsing and diffs

## Goals

- Provide a practical, reliable Emacs workflow for Wikipedia editing and review.
- Support both "online" operations (via the MediaWiki API) and "local" operations
  (via a local mirror of selected pages and their revision history).
- Keep the design modular, so features can be developed and tested
  incrementally, in separate Elisp files.
- Prefer predictable behavior and explicit errors over silent fallbacks.

## Non-goals (at least initially)

- Perfect offline rendering of Wikipedia pages as they appear on the website.
- Replacing the full Wikipedia web UI for every feature.
- Supporting every MediaWiki extension or every third-party wiki configuration.

## Planned features

### Watchlist

- View the user's watchlist in Emacs.
- Filter and search watchlist entries.
- Jump from a watchlist entry to the page, diff, or history.
- Mark entries as seen (if supported by the API).

### Local mirror

Sync watched pages to a local SQLite store for offline inspection and local diffs.

### XTools integration

Fetch and display user statistics via the XTools API.

### AI integration

Integration with `gptel.el` for edit suggestions, summaries, and explanations.

## Architecture

### Layers

1. **Adapter layer** (`wikipedia-adapter.el`)
   - Isolates dependency on mediawiki.el
   - Provides: `wp--login`, `wp--api-call`, `wp--open-page-buffer`, `wp--save-page-buffer`
   - Can be replaced to use a different backend

2. **Domain layer** (`wikipedia-page.el`, `wikipedia-history.el`)
   - Page operations, history browsing, diffs
   - Encodes MediaWiki-specific workflows

3. **UI layer**
   - `tabulated-list-mode` for history
   - `special-mode` for preview and revision viewing
   - `ediff` for comparing revisions

## Technical notes

### Authentication

Uses `auth-source` for credential storage. Users with 2FA need bot passwords.

### Emacs version

Requires Emacs 29.1+ (for built-in SQLite and modern JSON support).

## Roadmap

- [x] Phase 1: Minimal online workflow (open, edit, save)
- [x] Phase 2: Preview
- [x] Phase 3: Diffs and history browsing
- [ ] Phase 4: Watchlist UI
- [ ] Phase 5: Local mirror
- [ ] Phase 6: XTools integration

## License

GPL-3.0-or-later
