# Portable profile keywords (`#+RESUMEL_*`)

Use the same Org file with any template by setting a **portable** profile in the header (no extra `#+INCLUDE` for contact info). Template-specific options (e.g. `#+RESUMEL_MODERNCV_COLOR:`) stay as today.

## Precedence

1. **Portable profile** — `#+RESUMEL_NAME`, `#+RESUMEL_HEADLINE`, `#+RESUMEL_EMAIL`, etc.
2. **Template-specific** — e.g. `#+RESUMEL_JAKES_EMAIL:` when the portable key is unset.
3. **Standard Org** — `#+AUTHOR`, `#+TITLE`, `#+EMAIL` in the file when profile keys are absent.

When exporting (via `M-x resumel-export` or the test harness), portable `NAME` / `HEADLINE` / `EMAIL` are merged into `#+AUTHOR` / `#+TITLE` / `#+EMAIL` so LaTeX classes that rely on Org export (moderncv, Awesome-CV, AltaCV, modAltacv, jakes) receive a consistent header.

**Override rules**: `RESUMEL_NAME` overrides `#+AUTHOR`; `RESUMEL_HEADLINE` overrides `#+TITLE`; `RESUMEL_EMAIL` overrides `#+EMAIL`. The standard Org keywords serve as fallbacks for users who do not set the portable RESUMEL_ keywords.

## Keyword reference

| Keyword | Role |
|---------|------|
| `RESUMEL_NAME` | Full name → `#+AUTHOR` / `\author` |
| `RESUMEL_HEADLINE` | Tagline or document title line → `#+TITLE` |
| `RESUMEL_EMAIL` | Email (portable) → `#+EMAIL` and jakes contact line |
| `RESUMEL_PHONE` | Phone (jakes contact; reserved for future use elsewhere) |
| `RESUMEL_LOCATION` | Reserved for future template wiring |
| `RESUMEL_LINKEDIN` | LinkedIn slug (`user-id`) |
| `RESUMEL_LINKEDIN_LABEL` | Optional display text for LinkedIn link |
| `RESUMEL_GITHUB` | GitHub username |
| `RESUMEL_GITHUB_LABEL` | Optional display text for GitHub link |

Jakes-specific keys (`RESUMEL_JAKES_*`) still work and are used when the portable key above is not set.
