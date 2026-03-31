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

To add a new template:
  1. Create templates/<name>/metadata.json  (see existing files for the schema)
  2. Add "<name>" to the TEMPLATES list below in the desired display order.
  3. Run this script (or let CI do it on the next push).
"""

import json
import re
import sys
from pathlib import Path

# Display order for the README.  Add new template names here.
TEMPLATES = ["moderncv", "altacv", "awesomecv", "modaltacv"]

ROOT = Path(__file__).resolve().parent.parent


# ---------------------------------------------------------------------------
# Helpers


def load_metadata() -> dict:
    """Load metadata.json for every template in TEMPLATES."""
    meta = {}
    for tmpl in TEMPLATES:
        path = ROOT / "templates" / tmpl / "metadata.json"
        if not path.exists():
            print(f"ERROR: metadata file missing: {path}", file=sys.stderr)
            sys.exit(1)
        with open(path) as f:
            meta[tmpl] = json.load(f)
    return meta


def build_overview(meta: dict) -> str:
    """Return the Org-mode bullet list of templates (links + descriptions)."""
    lines = []
    for tmpl in TEMPLATES:
        m = meta[tmpl]
        name = m["display_name"]
        url = m.get("url")
        desc = m["description"]
        if url:
            lines.append(f"- *[[{url}][{name}]]*: {desc}")
        else:
            lines.append(f"- *{name}*: {desc}")
    return "\n".join(lines)


def build_gallery(meta: dict) -> str:
    """Return the #+begin_export html block for the Template Gallery section."""
    rows = []
    # Pair templates two per row; an odd final template gets its own row.
    it = iter(TEMPLATES)
    for left in it:
        right = next(it, None)
        lm = meta[left]

        # Header row (template names)
        header = "  <tr>\n"
        header += f'    <td align="center"><strong>{lm["display_name"]}</strong></td>\n'
        if right:
            rm = meta[right]
            header += f'    <td align="center"><strong>{rm["display_name"]}</strong></td>\n'
        header += "  </tr>"
        rows.append(header)

        # Image row
        img_row = "  <tr>\n"
        img_row += (
            f'    <td><img src="docs/previews/{left}.png"'
            f' alt="{lm["display_name"]} template preview" width="400"/></td>\n'
        )
        if right:
            rm = meta[right]
            img_row += (
                f'    <td><img src="docs/previews/{right}.png"'
                f' alt="{rm["display_name"]} template preview" width="400"/></td>\n'
            )
        img_row += "  </tr>"
        rows.append(img_row)

    table_rows = "\n".join(rows)
    return (
        "#+begin_export html\n"
        "<table>\n"
        + table_rows
        + "\n</table>\n"
        "#+end_export"
    )


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

    meta = load_metadata()
    original = readme_path.read_text()

    updated = original
    updated = replace_section(updated, "overview", build_overview(meta))
    updated = replace_section(updated, "gallery", build_gallery(meta))

    if updated == original:
        print("README.org is already up to date.")
    else:
        readme_path.write_text(updated)
        print("README.org updated successfully.")


if __name__ == "__main__":
    main()
