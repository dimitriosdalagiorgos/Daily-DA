import { test } from "node:test";
import assert from "node:assert/strict";
import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";

const DOCS = new URL("../../docs/", import.meta.url).pathname;
const PUBLIC = new URL("../public/docs/", import.meta.url).pathname;

test("interactive pages: the platform serves the same files as docs/", () => {
  const pages = readdirSync(DOCS).filter((f) => f.endsWith(".html"));
  assert.ok(pages.length > 0);
  for (const page of pages) {
    assert.equal(readFileSync(join(PUBLIC, page), "utf8"), readFileSync(join(DOCS, page), "utf8"), `${page}: run python3 docs/build.py or copy it`);
  }
});

test("interactive pages: links between them resolve", () => {
  for (const page of readdirSync(DOCS).filter((f) => f.endsWith(".html"))) {
    const html = readFileSync(join(DOCS, page), "utf8");
    for (const [, href] of html.matchAll(/href="([^"#:]+\.html)(#\w+)?"/g)) {
      assert.ok(readdirSync(DOCS).includes(href), `${page} links to missing ${href}`);
    }
  }
});
