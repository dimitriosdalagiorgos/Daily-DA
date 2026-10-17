#!/usr/bin/env python3
"""Word (.docx) and PDF versions of the manuals.

    pip install markdown pypandoc_binary    # once (pandoc for Word)
    python3 docs/build.py                   # PDF: Chromium via Playwright (docs/pdf.mjs)

Each manual's «<!-- include: algorithm.md -->» section gets the text of
docs/algorithm.md; the teachers' manual also gets docs/algorithm-theory.md
as an appendix. The parents' and teachers' manuals are also copied to
platform/public/docs/, so the platform's help page can link to them, and
so are the interactive pages (docs/*.html: the worked example and its
animation), which need no building.

The parents' information note (enimerosi-goneon.md) is also written as a
stand-alone web page for the school's site (enimerosi-goneon.html). It
has blanks [in brackets] for the school's details, so it is not copied
to the platform.
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
    ("enimerosi-goneon.md", "Enimerosi_Goneon", None, False),
]

NOTE = "enimerosi-goneon"  # also a web page for the school's site; not for the platform

# Scoped under .omiloi-info, so the part between the two comments can also be
# pasted into a site editor's HTML mode without touching the site's own styles.
PAGE_CSS = """
.omiloi-info { --ink:#1a1a1a; --muted:#555; --accent:#1f4e79; --line:#d0d7de; --fill:#fff3b0;
  max-width:46rem; margin:0 auto; padding:1.5rem 1rem 3rem; background:#fff; color:var(--ink);
  font-family:Arial, "Helvetica Neue", sans-serif; font-size:1.05rem; line-height:1.55; }
.omiloi-info h1 { font-size:1.7rem; line-height:1.25; color:var(--accent); margin:0 0 1rem; }
.omiloi-info h2 { font-size:1.3rem; color:var(--accent); margin:2rem 0 .6rem; padding-bottom:.25rem; border-bottom:2px solid var(--line); }
.omiloi-info p, .omiloi-info li { margin:.45rem 0; }
.omiloi-info ol, .omiloi-info ul { padding-left:1.4rem; }
.omiloi-info a { color:var(--accent); }
.omiloi-info nav { background:#f4f7fa; border:1px solid var(--line); border-radius:8px; padding:.6rem 1rem; margin:1rem 0 1.5rem; }
.omiloi-info nav ul { list-style:none; padding:0; margin:0; display:flex; flex-wrap:wrap; gap:.3rem 1.2rem; }
.omiloi-info .fill { background:var(--fill); padding:0 .2em; border-radius:3px; }
.omiloi-info .faq p strong:first-child { display:block; margin-top:1rem; }
"""


def slug(text):
    return re.sub(r"[^\w]+", "-", text.lower()).strip("-")


def build_page():
    """enimerosi-goneon.md → a stand-alone page: contents, blanks marked in yellow."""
    text = (DOCS / f"{NOTE}.md").read_text(encoding="utf-8")
    title = re.match(r"# (.*)", text).group(1)
    body = markdown.markdown(re.sub(r"\A# .*\n", "", text), extensions=["sane_lists", "nl2br"])
    heads = re.findall(r"<h2>(.*?)</h2>", body)
    for h in heads:
        body = body.replace(f"<h2>{h}</h2>", f'<h2 id="{slug(h)}">{h}</h2>', 1)
    body = re.sub(r"\[([^\]<]+)\]", r'<span class="fill">[\1]</span>', body)
    body = body.replace('<h2 id="συχνές-ερωτήσεις">Συχνές ερωτήσεις</h2>', '<h2 id="συχνές-ερωτήσεις">Συχνές ερωτήσεις</h2>\n<div class="faq">', 1)
    body = body.replace(f'<h2 id="{slug("Επικοινωνία")}">', f'</div>\n<h2 id="{slug("Επικοινωνία")}">', 1)
    nav = "".join(f'<li><a href="#{slug(h)}">{h}</a></li>' for h in heads)
    page = f"""<!doctype html>
<html lang="el">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{title}</title>
<!-- Φτιάχνεται από το docs/{NOTE}.md με το docs/build.py: αλλαγές εκεί, όχι εδώ.
     Πριν το ανεβάσετε στο site του σχολείου, αντικαταστήστε ό,τι είναι [σε αγκύλες] (κίτρινο φόντο).
     Σε editor ιστοσελίδας (λειτουργία HTML) αρκεί το κομμάτι ανάμεσα στα δύο σχόλια «ΑΡΧΗ» και «ΤΕΛΟΣ». -->
</head>
<body style="margin:0;background:#fff">
<!-- ΑΡΧΗ: ενημέρωση γονέων -->
<style>{PAGE_CSS}</style>
<div class="omiloi-info">
<h1>{title}</h1>
<nav aria-label="Περιεχόμενα"><ul>{nav}</ul></nav>
{body}
</div>
<!-- ΤΕΛΟΣ: ενημέρωση γονέων -->
</body>
</html>
"""
    (DOCS / f"{NOTE}.html").write_text(page, encoding="utf-8")
    print(f"✓ {NOTE}.html")

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
    build_page()
    for page in DOCS.glob("*.html"):
        if page.stem != NOTE:
            shutil.copy(page, PUBLIC / page.name)
