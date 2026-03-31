#!/usr/bin/env python3
"""Update auto-generated sections of README.org from template metadata.

Two marker pairs in README.org are managed by this script:

  # --- resumel-auto:overview ---
  ...template bullet list...
  # --- end-resumel-auto:overview ---

  # --- resumel-auto:gallery ---
  ...HTML image table...
  # --- end-resumel-auto:gallery ---

Run from the repository root:

  python3 scripts/update-readme.py

Templates are discovered automatically: any subdirectory of templates/ that
contains a metadata.yaml file is included.  Display order is controlled by the
optional `order` field in each metadata.yaml (lower numbers appear first);
templates without an `order` field sort after those with one, then
alphabetically.

To add a new template:
  1. Create templates/<name>/metadata.yaml  (see existing files for the schema)
  2. Run this script (or let CI do it on the next push).
"""

import re
import sys
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parent.parent


# ---------------------------------------------------------------------------
# Helpers


def load_metadata() -> tuple[list[str], dict]:
    """Discover templates and load their metadata.yaml files.

    Returns (ordered_names, meta_dict) where ordered_names respects the
    `order` field in each file (then alphabetical as a tiebreaker).
    """
    templates_dir = ROOT / "templates"
    meta = {}
    for subdir in templates_dir.iterdir():
        if not subdir.is_dir():
            continue
        yaml_path = subdir / "metadata.yaml"
        if not yaml_path.exists():
            continue
        with open(yaml_path) as f:
            meta[subdir.name] = yaml.safe_load(f)

    if not meta:
        print("ERROR: no template metadata.yaml files found", file=sys.stderr)
        sys.exit(1)

    ordered = sorted(meta, key=lambda k: (meta[k].get("order", 999), k))
    return ordered, meta


def build_overview(ordered: list[str], meta: dict) -> str:
    """Return the Org-mode bullet list of templates (links + descriptions)."""
    lines = []
    for tmpl in ordered:
        m = meta[tmpl]
        name = m["display_name"]
        url = m.get("url")
        desc = m["description"]
        if url:
            lines.append(f"- *[[{url}][{name}]]*: {desc}")
        else:
            lines.append(f"- *{name}*: {desc}")
    return "\n".join(lines)


def build_gallery(ordered: list[str], meta: dict) -> str:
    """Return #+html: lines for the Template Gallery section."""
    def h(html: str) -> str:
        return f"#+html: {html}"

    lines = [h("<table>")]
    # Pair templates two per row; an odd final template gets its own row.
    it = iter(ordered)
    for left in it:
        right = next(it, None)
        lm = meta[left]

        # Header row (template names)
        lines.append(h("  <tr>"))
        lines.append(h(f'    <td align="center"><strong>{lm["display_name"]}</strong></td>'))
        if right:
            rm = meta[right]
            lines.append(h(f'    <td align="center"><strong>{rm["display_name"]}</strong></td>'))
        lines.append(h("  </tr>"))

        # Image row
        lines.append(h("  <tr>"))
        lines.append(h(
            f'    <td><img src="docs/previews/{left}.png"'
            f' alt="{lm["display_name"]} template preview" width="400"/></td>'
        ))
        if right:
            rm = meta[right]
            lines.append(h(
                f'    <td><img src="docs/previews/{right}.png"'
                f' alt="{rm["display_name"]} template preview" width="400"/></td>'
            ))
        lines.append(h("  </tr>"))

    lines.append(h("</table>"))
    return "\n".join(lines)


def replace_section(text: str, key: str, new_content: str) -> str:
    """Replace everything between the resumel-auto markers for KEY."""
    begin = f"# --- resumel-auto:{key} ---"
    end = f"# --- end-resumel-auto:{key} ---"
    pattern = re.compile(re.escape(begin) + r".*?" + re.escape(end), flags=re.DOTALL)
    replacement = begin + "\n" + new_content + "\n" + end
    updated, count = pattern.subn(replacement, text)
    if count == 0:
        print(
            f"WARNING: marker pair for '{key}' not found in README.org",
            file=sys.stderr,
        )
    return updated


# ---------------------------------------------------------------------------
# Main


def main() -> None:
    readme_path = ROOT / "README.org"
    if not readme_path.exists():
        print("ERROR: README.org not found", file=sys.stderr)
        sys.exit(1)

    ordered, meta = load_metadata()
    original = readme_path.read_text()

    updated = original
    updated = replace_section(updated, "overview", build_overview(ordered, meta))
    updated = replace_section(updated, "gallery", build_gallery(ordered, meta))

    if updated == original:
        print("README.org is already up to date.")
    else:
        readme_path.write_text(updated)
        print("README.org updated successfully.")


if __name__ == "__main__":
    main()
