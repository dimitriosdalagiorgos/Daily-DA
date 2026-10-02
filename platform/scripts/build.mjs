// Build the static site for Netlify: public/ + the browser-safe parts of
// src/ as /lib/ + the clubs template. Output: dist/
//
// The teachers' and admin's pages go only to their hidden addresses
// (TEACHER_PATH, ADMIN_PATH — Netlify environment variables; see
// src/server/paths.js); without them the build stops.
import { cpSync, mkdirSync, rmSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { ROLE_PAGES, rolePaths } from "../src/server/paths.js";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const dist = join(root, "dist");
let paths;
try {
  paths = rolePaths(process.env);
} catch (err) {
  console.error(`✗ ${err.message}\n  (Netlify → Project configuration → Environment variables)`);
  process.exit(1);
}
rmSync(dist, { recursive: true, force: true });
mkdirSync(dist, { recursive: true });
const hidden = new Set(Object.values(ROLE_PAGES).map((f) => join(root, "public", f)));
cpSync(join(root, "public"), dist, { recursive: true, filter: (src) => !hidden.has(src) });
for (const [role, file] of Object.entries(ROLE_PAGES)) {
  mkdirSync(join(dist, paths[role]), { recursive: true });
  cpSync(join(root, "public", file), join(dist, paths[role], "index.html"));
}
for (const part of ["algorithm", "import", "export"]) cpSync(join(root, "src", part), join(dist, "lib", part), { recursive: true });
cpSync(join(root, "templates"), join(dist, "templates"), { recursive: true });
console.log(`Built ${dist}`);
