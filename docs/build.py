#!/usr/bin/env python3
"""Word (.docx) and PDF versions of the manuals.

    pip install markdown pypandoc_binary    # once (pandoc for Word)
    python3 docs/build.py                   # PDF: Chromium via Playwright (docs/pdf.mjs)

Each manual's «<!-- include: algorithm.md -->» section gets the text of
docs/algorithm.md; the teachers' manual also gets docs/algorithm-theory.md
as an appendix. The parents' and teachers' manuals are also copied to
platform/public/docs/, so the platform's help page can link to them.
"""

import os
import re
import shutil
import subprocess
import tempfile
from pathlib import Path

import markdown
import pypandoc

DOCS = Path(__file__).resolve().parent
REFERENCE = DOCS / "reference.docx"  # optional Word styles (fonts, colours)
PUBLIC = DOCS.parent / "platform" / "public" / "docs"

MANUALS = [
    # source, output name, appendix, copy to the platform
    ("manual-parents.md", "Egxeiridio_Goneis", None, True),
    ("manual-teachers.md", "Egxeiridio_Ekpaideutikoi", "algorithm-theory.md", True),
    ("manual-R.md", "Egxeiridio_R", None, True),
]

CSS = """
@page { size: A4; margin: 2cm; }
body { font-family: Arial, sans-serif; font-size: 11pt; line-height: 1.4; color: #1a1a1a; }
h1 { font-size: 20pt; color: #1f4e79; margin-bottom: 4pt; }
h2 { font-size: 15pt; color: #1f4e79; margin-top: 18pt; border-bottom: 1px solid #1f4e79; }
h3 { font-size: 12.5pt; color: #1f4e79; margin-top: 12pt; }
h4 { font-size: 11.5pt; margin-top: 10pt; }
table { border-collapse: collapse; margin: 6pt 0; }
th, td { border: 1px solid #999; padding: 3pt 6pt; vertical-align: top; }
th { background: #dce6f1; }
code { font-family: "Courier New", monospace; font-size: 9.5pt; }
pre { background: #f2f2f2; padding: 6pt; font-size: 9pt; }
blockquote { border-left: 3px solid #1f4e79; margin-left: 0; padding-left: 10pt; color: #333; }
"""


def demote(text, levels):
    """Shift Markdown headings down by `levels` (outside code blocks)."""
    out, code = [], False
    for line in text.splitlines():
        if line.startswith("```"):
            code = not code
        if not code and re.match(r"^#{1,5} ", line):
            line = "#" * levels + line
        out.append(line)
    return "\n".join(out)


def section(name, levels):
    """A document without its title, headings shifted, internal links removed."""
    text = (DOCS / name).read_text(encoding="utf-8")
    text = re.sub(r"\A# .*\n", "", text)                # its own title
    text = re.sub(r"\n\*Για τη θεωρία[^\n]*\n?\Z", "\n", text)  # link to the theory text
    return demote(text, levels)


def unlink(text):
    """[label](other.md) → label: the Word/PDF version has no other files."""
    return re.sub(r"\[([^\]]+)\]\((?!https?:)[^)]+\)", r"\1", text)


def build(source, out_name, appendix):
    text = (DOCS / source).read_text(encoding="utf-8")
    # the include marker and the link line after it → the algorithm text
    text = re.sub(r"<!-- include: algorithm\.md -->\n[^\n]*\n", lambda m: section("algorithm.md", 1) + "\n", text)
    if appendix:
        theory = (DOCS / appendix).read_text(encoding="utf-8")
        title = re.match(r"# (.*)", theory).group(1)
        text += f"\n\n---\n\n## Παράρτημα: {title}\n\n" + section(appendix, 1)
    text = unlink(text)
    title = re.search(r"^# (.*)$", text, flags=re.M).group(1)
    # Word
    pypandoc.convert_text(text, "docx", format="gfm", outputfile=str(DOCS / f"{out_name}.docx"),
                          extra_args=["--metadata", "lang=el-GR"]
                          + (["--reference-doc", str(REFERENCE)] if REFERENCE.exists() else []))
    # PDF
    body = markdown.markdown(text, extensions=["tables", "fenced_code", "sane_lists"])
    html = f'<!doctype html><html lang="el"><head><meta charset="utf-8"><title>{title}</title><style>{CSS}</style></head><body>{body}</body></html>'
    with tempfile.TemporaryDirectory() as tmp:
        src = Path(tmp) / f"{out_name}.html"
        src.write_text(html, encoding="utf-8")
        env = dict(os.environ)
        bundled = Path("/opt/node22/lib/node_modules/playwright/index.mjs")
        if "PLAYWRIGHT" not in env and bundled.exists():
            env["PLAYWRIGHT"] = str(bundled)
        subprocess.run(["node", str(DOCS / "pdf.mjs"), str(src), str(DOCS / f"{out_name}.pdf")], check=True, env=env)
    print(f"✓ {out_name}.docx, {out_name}.pdf")


if __name__ == "__main__":
    PUBLIC.mkdir(parents=True, exist_ok=True)
    for source, out_name, appendix, public in MANUALS:
        build(source, out_name, appendix)
        if public:
            for ext in ("docx", "pdf"):
                shutil.copy(DOCS / f"{out_name}.{ext}", PUBLIC / f"{out_name}.{ext}")
