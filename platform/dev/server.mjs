// Local development server:
//   npm run dev              → http://localhost:8888
//   npm run dev -- --demo    → with a fictional school loaded (fresh store)
//   … --no-mail              → as in production without an e-mail service:
//                              teachers get their login link from the admin
//
// Serves public/ as the site, src/ as /lib/ (the browser uses the same
// algorithm and import code), and /api/* through the same handler as the
// Netlify Function. Data is kept in .data/dev-store.json. E-mails are not
// sent: they are printed here (teacher login links included).

import { createServer } from "node:http";
import { existsSync, readFileSync, rmSync, statSync } from "node:fs";
import { dirname, extname, join, normalize } from "node:path";
import { fileURLToPath } from "node:url";
import { createApp } from "../src/server/app.js";
import { createFileStore } from "../src/server/store.js";
import { importClubs } from "../src/import/clubs.js";
import { importStudents } from "../src/import/students.js";
import { demoClubRows, demoStudentRows } from "./demo-data.mjs";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const port = Number(process.env.PORT ?? 8888);
const storePath = join(root, ".data", port === 8888 ? "dev-store.json" : `dev-store-${port}.json`);
const demo = process.argv.includes("--demo");
const noMail = process.argv.includes("--no-mail");

if (demo) rmSync(storePath, { force: true });
const store = createFileStore(storePath);
if (demo) {
  const students = importStudents(demoStudentRows()).students;
  const { clubs, teachers } = importClubs(demoClubRows());
  await store.set("students", students.map((s) => ({ ...s, loginException: false })));
  await store.set("clubs", clubs);
  await store.set("teachers", teachers);
  await store.set("settings", { phase: "setup", schoolName: "Δοκιμαστικό Γυμνάσιο", contact: "Γραμματεία: 2310 000000" });
}

const env = {
  SESSION_SECRET: process.env.SESSION_SECRET ?? "dev-only-secret-change-me",
  ADMIN_PASSWORD: process.env.ADMIN_PASSWORD ?? "admin",
  BASE_URL: `http://localhost:${port}`,
  DEV: true,
};
const handle = createApp({
  store,
  env,
  sendMail: noMail ? undefined : async (m) => console.log(`\n✉  Προς: ${m.to}\n   Θέμα: ${m.subject}\n   ${m.text.replace(/\n/g, "\n   ")}\n`),
});

const TYPES = {
  ".html": "text/html; charset=utf-8", ".js": "text/javascript; charset=utf-8", ".mjs": "text/javascript; charset=utf-8",
  ".css": "text/css; charset=utf-8", ".svg": "image/svg+xml", ".xlsx": "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
  ".json": "application/json", ".ico": "image/x-icon",
};

function staticFile(pathname) {
  const map = [
    ["/lib/", join(root, "src")],
    ["/templates/", join(root, "templates")],
    ["/vendor/xlsx.mjs", join(root, "node_modules", "xlsx", "xlsx.mjs")],
    ["/", join(root, "public")],
  ];
  for (const [prefix, dir] of map) {
    if (!pathname.startsWith(prefix)) continue;
    const rel = prefix.endsWith("/") ? pathname.slice(prefix.length) : "";
    let file = normalize(join(dir, decodeURIComponent(rel)));
    if (!file.startsWith(dir)) return null;
    if (existsSync(file) && statSync(file).isDirectory()) file = join(file, "index.html");
    // /lib/ exposes only the browser-safe parts of src/
    if (prefix === "/lib/" && !/^(algorithm|import|export)\//.test(rel)) return null;
    return existsSync(file) ? file : null;
  }
  return null;
}

createServer(async (req, res) => {
  const url = new URL(req.url, `http://localhost:${port}`);
  try {
    if (url.pathname.startsWith("/api/")) {
      const chunks = [];
      for await (const c of req) chunks.push(c);
      const request = new Request(url, {
        method: req.method,
        headers: req.headers,
        body: ["GET", "HEAD"].includes(req.method) ? undefined : Buffer.concat(chunks),
      });
      const response = await handle(request);
      res.writeHead(response.status, Object.fromEntries(response.headers));
      res.end(Buffer.from(await response.arrayBuffer()));
      return;
    }
    const file = staticFile(url.pathname);
    if (!file) {
      res.writeHead(404, { "content-type": "text/plain; charset=utf-8" }).end("Δεν βρέθηκε");
      return;
    }
    res.writeHead(200, { "content-type": TYPES[extname(file)] ?? "application/octet-stream", "cache-control": "no-store" });
    res.end(readFileSync(file));
  } catch (err) {
    console.error(err);
    res.writeHead(500).end("Σφάλμα");
  }
}).listen(port, () => {
  console.log(`Πλατφόρμα ομίλων (τοπικά): http://localhost:${port}`);
  console.log(`  Διαχείριση: http://localhost:${port}/admin.html  (κωδικός: ${env.ADMIN_PASSWORD === "admin" ? "admin" : "από ADMIN_PASSWORD"})`);
  if (demo) console.log("  Φορτώθηκε δοκιμαστικό σχολείο (60 μαθητές, 13 όμιλοι).");
  if (noMail) console.log("  Χωρίς email: σύνδεσμοι εκπαιδευτικών από τη διαχείριση (καρτέλα «Όμιλοι»).");
  console.log(`  Δεδομένα: ${storePath}`);
});
