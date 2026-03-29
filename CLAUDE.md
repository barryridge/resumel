# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Project Does

**resumel** is an Emacs Lisp package that exports Org Mode files to professional PDF resumes using LaTeX templates. It supports four templates: `moderncv`, `altacv`, `awesomecv`, and `modaltacv` (a custom ModernCV-styled AltaCV variant).

## Commands

### Running Tests

```bash
make test        # Run all tests
make clean       # Remove previous test results in tests/results/
```

Tests export Org fixture files and compare generated PDFs against expected outputs in `tests/expected/` using `diff-pdf`. Requires Emacs, a full TeX Live installation, and `diff-pdf`.

**Tolerance env vars** (higher values needed in CI):
- `DIFF_PDF_CHANNEL_TOLERANCE` (default: 0 locally, 150 in CI)
- `DIFF_PDF_PER_PAGE_PIXEL_TOLERANCE` (default: 0 locally, 50000 in CI)

### Running a Single Test

```bash
emacs --batch -l ./test-init.el -l ./tests/test-resumel.el \
  -eval "(ert-run-tests-interactively \"test-resumel-moderncv-basic-blue\")"
```

Test names follow the pattern `test-resumel-<fixture-name>` (e.g., `test-resumel-altacv-complex`).

## Architecture

### Core Flow

1. User calls `resumel-setup` on an Org buffer → parses `RESUMEL_*` keywords, loads template's `.el` (registers LaTeX class), inserts `#+INCLUDE` for template macros
2. User calls `resumel-export` → creates a temp buffer, calls `resumel-setup`, then runs `org-latex-export-to-pdf`

### Key Files

- **`resumel.el`** — core package: `resumel-setup`, `resumel-export`, `resumel-select-template`, helper functions for cvtags/wheelchart
- **`resumel.org`** — shared Org macros used by all templates (cvtag, cvtags, cvltag, wheelchart, formatting)
- **`templates/{name}/{name}.el`** — registers the LaTeX class via `org-latex-classes` with full preamble, fonts, colors, and custom commands
- **`templates/{name}/{name}.org`** — template-specific Org macros (cventry, cvitem, cvevent, etc.)

### Template Configuration

Templates are controlled via Org keywords in the user's file:
```
#+RESUMEL_MODERNCV_COLOR: blue
#+RESUMEL_ALTACV_FONT: Times
```
These are parsed into `resumel-template-vars` during setup and substituted into the LaTeX class definition.

### Test Structure

- `tests/fixtures/` — input Org files (one per test case)
- `tests/expected/` — reference PDFs for comparison
- `tests/results/` — generated PDFs (git-ignored)
- `tests/test-resumel.el` — ERT test suite
- `test-init.el` — bootstraps isolated Emacs env at `tests/test_emacs.d/`, installs org/org-contrib/ox-extra

### Adding a Template

1. Create `templates/newname/newname.el` (define `org-latex-classes` entry)
2. Create `templates/newname/newname.org` (define Org macros, include `resumel.org`)
3. Add `newname` to the `resumel-default-template` custom choice in `resumel.el`
4. Add test fixture to `tests/fixtures/` and generate expected PDF into `tests/expected/`

## Dependencies

**Emacs packages**: `org`, `ox-latex`, `ox-extra` (from org-contrib), `subr-x`

**External tools**: TeX Live (with moderncv, paracol, tikz, fontawesome5, biblatex, koma-script, etc.), `diff-pdf`, `latexmk`, `xetex`/`pdflatex`

**External LaTeX templates** (cloned separately in CI): AltaCV, AwesomeCV — their `.cls` files must be on the LaTeX path.
