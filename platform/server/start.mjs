// Production server for a school's own Linux machine (instead of Netlify +
// Supabase). Same site and API; data in one JSON file. See the repository
// README, «Εγκατάσταση σε δικό σας server».
//
// Environment variables:
//   ADMIN_PASSWORD   required, at least 10 characters
//   BASE_URL         the public address, e.g. https://omiloi.example.gr
//                    (used in the teachers' login links)
//   TEACHER_PATH     required: hidden address of the teachers' page, e.g.
//                    e-7kq3m9xa → https://omiloi.example.gr/e-7kq3m9xa/
//   ADMIN_PATH       required: hidden address of the admin's page
//                    (see src/server/paths.js)
//   DATA_DIR         folder for the data file (default: platform/.data)
//   PORT             default 8080
//   HOST             default 127.0.0.1 — only the web server on the same
//                    machine (Apache/nginx with HTTPS) talks to it
//   SESSION_SECRET   optional; if missing, a random one is created once and
//                    kept in the data file
//
// Run: node server/start.mjs   (or: npm start)

import { randomBytes } from "node:crypto";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { createApp } from "../src/server/app.js";
import { createFileStore } from "../src/server/store.js";
import { serveNode } from "../src/server/node-http.js";
import { rolePaths } from "../src/server/paths.js";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const env = process.env;

const fail = (message) => {
  console.error(`✗ ${message}`);
  process.exit(1);
};
if (!env.ADMIN_PASSWORD || env.ADMIN_PASSWORD.length < 10) fail("Ορίστε ADMIN_PASSWORD (τουλάχιστον 10 χαρακτήρες).");
if (!env.BASE_URL) fail("Ορίστε BASE_URL (η δημόσια διεύθυνση της πλατφόρμας, π.χ. https://omiloi.example.gr).");
let paths;
try {
  paths = rolePaths(env);
} catch (err) {
  fail(err.message);
}

const port = Number(env.PORT ?? 8080);
const host = env.HOST ?? "127.0.0.1";
const dataFile = join(resolve(env.DATA_DIR ?? join(root, ".data")), "store.json");
const store = createFileStore(dataFile);
const secret = env.SESSION_SECRET ?? (await store.update("sessionSecret", (s) => s ?? randomBytes(32).toString("base64url")));

const handle = createApp({
  store,
  env: { SESSION_SECRET: secret, ADMIN_PASSWORD: env.ADMIN_PASSWORD, BASE_URL: env.BASE_URL.replace(/\/$/, ""), TEACHER_PATH: paths.teacher.slice(1, -1), SHOW_OUTBOX: true },
});

serveNode({
  handle, root, port, host, paths,
  headers: { "x-content-type-options": "nosniff", "referrer-policy": "same-origin", "x-frame-options": "DENY" },
  onListen: () => {
    console.log(`Πλατφόρμα ομίλων: http://${host}:${port} → δημόσια: ${env.BASE_URL}`);
    console.log(`  Δεδομένα: ${dataFile}`);
  },
});
