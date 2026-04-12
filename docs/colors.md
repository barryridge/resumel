# Resumel color registry

All resumel LaTeX preambles inject `templates/resumel-colors.tex` (via Emacs when loading a template). It defines a single source of truth for cross-template color names.

## Naming

- **`ResumelMc*`** — Moderncv-style lowercase palette (RGB), e.g. `ResumelMcLightblue`.
- **`ResumelAlta*`** — AltaCV-style names (HTML), e.g. `ResumelAltaBlue`.
- **`awesome-*`** — Awesome-CV class palette (HTML), same names as upstream `awesome-cv.cls`.
- **`Resumel*`** (semantic) — Portable names for template-agnostic Org content:
  - `ResumelAccent`, `ResumelBody`, `ResumelMuted`, `ResumelHeading`, `ResumelName`

## Backward-compatible aliases

After the canonical `\providecolor` lines, the registry issues `\colorlet` aliases so existing documents keep working:

- Moderncv: `black`, `red`, `darkgrey`, `orange`, `burgundy`, `purple`, `lightblue`, `green`
- AltaCV: `Black`, `SlateGrey`, `LightGrey`, `DarkPastelRed`, `PastelRed`, `Blue`, `DarkBlue`, `GoldenEarth`, `CoolSky`, `SoftSkyBlue`

New portable content should prefer **`Resumel*`** or explicit **`ResumelMc*`** / **`ResumelAlta*`** names to avoid ambiguity between schemes.

## Portable vs template-specific

- **Portable** — Use `\textcolor{ResumelAccent}{...}` or `{{{resumel-color-samples}}}` in Org; safe across templates.
- **Template-specific** — Class-facing names like `awesome`, `heading`, `color1` are still set in each template `.el` after the registry loads.

## Org `colorlet`

The macro `{{{colorlet(var,source)}}}` from `resumel.org` expands to `\colorlet{var}{source}`. Both names must exist (or `source` must be a color model literal). With the registry loaded, `source` can be any alias or `Resumel*` name above.

## Upstream sync

To refresh hex/RGB values from upstream classes or docs, compare against your own local checkouts of the upstream LaTeX projects (moderncv, AltaCV, Awesome-CV, Jake’s Resume). Those sources are not committed in this repository. Then update `templates/resumel-colors.tex` and run `make test`.

## Implementation note

Definitions use **`\providecolor`** (xcolor) so a class that already defined the same name does not cause redefinition errors. Template `.el` files may still `\colorlet` semantic names (e.g. `\colorlet{awesome}{awesome-red}`) after the registry.
