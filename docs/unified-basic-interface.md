# Unified basic interface (Phase 0–1)

This document fixes the **portable surface** shared by all templates in the basic PDF tests. Template-specific `#+RESUMEL_*` options and color customization stay in each thin fixture wrapper; the **document body** lives in `tests/fixtures/unified-basic-body.org` and is included from every `*-basic*.org` fixture.

## Allowed Org (basic layer)

- Headings up to depth 2 (`#+OPTIONS: H:2`)
- Paragraphs, plain emphasis (`*bold*`, `/italic/`), `[[url][desc]]` links
- Unordered lists (exported via the template’s normal list path)
- One Org table and one `tabular` block using shared `amp` / `endl` macros
- `tags` / `ltags` from `resumel.org` (`cvtags` / `cvltags` remain as aliases)

## Awesome-CV: local spacing vs CI

Awesome-CV PDFs may look **vertically tight** on some local TeX setups (fonts, class versions) while the same fixture **passes** `diff-pdf` on GitHub Actions. That is expected; do not treat it as a regression if CI is green. Fixtures sometimes leave `{{{vspace(...)}}}` commented so you can tune spacing locally without changing committed expectations.

## Non-goals (basic layer)

- Wheelcharts, bibliography stress, every LaTeX command each class exposes
- AwesomeCV-only blocks (`cvparagraph`, `cvhonors` blocks, extra page breaks) — covered by complex fixtures

## Unified macros (same call signature in every template)

| Macro | Arguments | Role |
|-------|-----------|------|
| `fa-basic-icons` | none | Row of Font Awesome icons; templates map glyph names (e.g. Cogs vs Gears) |
| `fa-briefcase-colors` | none | Briefcase icon color samples valid for that template’s palette |
| `fa-briefcase-sizes-colors` | none | Briefcase size + color samples for that template |
| `item` | title, body | Generic titled block (`\cvitem`-style where available) |
| `double` | a, b, c, d | Two labeled pairs (e.g. languages / tools) |
| `itemc` | title, main, comment | Title + body + short comment |
| `listdouble` | left, right | Two-column list-style line |
| `entries-begin` / `entries-end` | none | Wrap experience rows (e.g. AwesomeCV `cventries`; empty on others) |
| `entry` | title, organization, location, dates, description | One experience row |
| `achievement` | icon, title, details | Single highlight line (maps to `cvachievement` or a fallback) |
| `reference` | name, place, contact | Reference line |
| `skill` | name, level | Discrete skill level (1–5 style) |

Template-specific names such as `cvitem`, `cvevent`, and raw `\cventry{...}` remain available for complex and upstream-faithful fixtures; **basic** tests use the unified names above.

## Org macro arguments

Macro arguments are comma-separated. Use **backslash-comma** (`\,`) inside an argument when the text must contain commas (see the `item` example in `unified-basic-body.org`).

## Font Awesome policy

Shared body does not hard-code icon names that differ between stacks. `fa-basic-icons` and the briefcase demo macros are defined per template in `templates/<name>/<name>.org`. The global `fa` macro in `resumel.org` is unchanged.

## Compatibility (basic)

Each template implements the same unified macros; behavior follows that class’s idioms (e.g. moderncv `\cventry` argument order vs AwesomeCV `\cventry`).
