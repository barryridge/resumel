# Macro naming conventions

## Tier 1: Shared utilities (`resumel.org`)

Generic formatting helpers with **no `cv` prefix**. These are LaTeX plumbing.

- **Formatting**: `bf`, `it`, `href`, `vspace`, `hspace`, `divider`, `fa`, etc.
- **Tag primitives**: `tag` (single tag), `ltag` (single tag with level)
- **Tag list helpers**: `tags` (multiple tags), `ltags` (multiple tags with levels)
- **Wheelchart**: `wheelchart`

The underlying LaTeX command is still `\cvtag`; the Org macro name drops the `cv` prefix for brevity.

## Tier 2: Unified interface (template `.org` files)

Two sub-tiers distinguished by prefix:

### `cv`-prefixed: template-native rendering

Maps to the template's native LaTeX command. Rendering varies per template; argument count/meaning may differ.

- `cvevent` — job/education entry (multi-row heading)
- `cvproject` — project entry
- `cvthesis` — thesis citation
- `cvachievement` — award/achievement with icon
- `cvref` — reference with contact info
- `cvskill` — skill with graphical level indicator
- `cvitem` — template-specific item

### Unprefixed: portable simplified formatting

Consistent rendering across all templates. Simple inline text formatting.

- `item` — bold label + description
- `double` — two label+value pairs side by side
- `itemc` — label + description + right-aligned comment
- `listdouble` — two-column list row
- `entry` — structured experience entry (title, org, location, dates, description)
- `achievement` — plain-text achievement
- `reference` — plain-text reference
- `skills-begin` / `skills-end` — wrap skill rows (AwesomeCV `cvskills`; no-ops on others)
- `skill` — skill (may alias to `cvskill`)

Where both `cv`-prefixed and unprefixed forms exist (e.g. `cvachievement` vs `achievement`), they are **intentionally different**: the `cv` version uses the template's native styling (icons, spacing), while the unprefixed version is a simple inline format.

## Tier 3: Template-native (template `.org` files)

Upstream template commands for power users. Keep native naming:

- **moderncv**: `cventry`, `cvdoubleitem`, `cvlistitem`, etc.
- **jakes**: `resumeItem`, `resumeSubheading`, `resumeProjectHeading`, etc.
- **awesomecv**: `cventry`, `cvhonor`, `cvsection`, etc.
- **altacv / modaltacv**: `cvsection`, `cvsubsection`, `cvaward`, etc.

These are opt-in for users who want template-specific control beyond the unified interface.
