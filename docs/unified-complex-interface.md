# Unified complex interface

This document complements [unified-basic-interface.md](unified-basic-interface.md). The shared body `tests/fixtures/unified-complex-body.org` is included from each `*-complex.org` wrapper (same pattern as unified basic).

## Macros (shared names)

| Macro | Typical arguments | Notes |
|-------|-------------------|--------|
| `beginparacol` / `endparacol` | — | Real `paracol` on AltaCV / modAltacv; no-ops on moderncv, awesomecv, jakes (single-column flow). |
| `switchcol` | — | Column break on AltaCV / modAltacv; spacing or `\clearpage` on other templates (layout degrades by design). |
| `cvevent` | title, organization, date, location | AltaCV-native; moderncv maps to `\subsection*` + `\cvitem` lines; awesomecv maps to `\cventry`; jakes uses existing `\cvevent` tabular. |
| `cvproject` | name, kind, url, link-text | Paracol templates use `cvevent`-style blocks; awesomecv uses `\cventry`; jakes uses `resumeProjectHeading`. |
| `cvthesis` | url, title | Thesis line after education `cvevent`. |
| `wheelchart` | (see `resumel.org`) | AltaCV / modAltacv use class `\wheelchart`. Other templates load `templates/resumel-wheelchart.tex` when the class does not define it. Requires `\colorlet{accent}{...}` (set in template `.el` where needed). |
| `cvhonors-begin` / `cvhonors-end` | — | awesomecv: wraps `\begin{cvhonors}` / `\end{cvhonors}`; other templates: empty. |
| `cvachievement` | icon, level, title, date, details | AltaCV / modAltacv: `\cvachievement` (icon separate, level+title as heading); awesomecv: maps to `\cvhonor` (icon+level in position column); moderncv: `\cvitem`; jakes: `\cvachievement`. |

Org comma rules from the basic doc still apply (use `\,` inside macro arguments where needed).

In raw `#+begin_export latex` blocks, a word starting with `L` immediately after TeX control sequences can be parsed as `\L` (Polish Ł). Write `{L}orem` at the start of the paragraph if you see a missing “L” in the PDF.

## Per-template support

| Feature | moderncv | altacv | modaltacv | awesomecv | jakes |
|---------|----------|--------|-----------|-----------|-------|
| Two-column paracol | Degraded (linear flow) | Full | Full | Degraded | Degraded |
| Wheel chart | `resumel-wheelchart.tex` | Class | Class | `resumel-wheelchart.tex` | `resumel-wheelchart.tex` |
| Awards block | `\cvitem` | `\cvachievement` | `\cvachievement` | `cvhonors` + `\cvhonor` | `\resumeItem` |

## Color registry

Portable colors for shared content are documented in [colors.md](colors.md).
