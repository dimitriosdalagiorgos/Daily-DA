// Build the static site for Netlify: public/ + the browser-safe parts of
// src/ as /lib/ + the clubs template. Output: dist/
import { cpSync, mkdirSync, rmSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const dist = join(root, "dist");
rmSync(dist, { recursive: true, force: true });
mkdirSync(dist, { recursive: true });
cpSync(join(root, "public"), dist, { recursive: true });
for (const part of ["algorithm", "import", "export"]) cpSync(join(root, "src", part), join(dist, "lib", part), { recursive: true });
cpSync(join(root, "templates"), join(dist, "templates"), { recursive: true });
console.log(`Built ${dist}`);
