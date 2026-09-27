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

import { rmSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { createApp } from "../src/server/app.js";
import { createFileStore } from "../src/server/store.js";
import { serveNode } from "../src/server/node-http.js";
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

serveNode({ handle, root, port, onListen: () => {
  console.log(`Πλατφόρμα ομίλων (τοπικά): http://localhost:${port}`);
  console.log(`  Διαχείριση: http://localhost:${port}/admin.html  (κωδικός: ${env.ADMIN_PASSWORD === "admin" ? "admin" : "από ADMIN_PASSWORD"})`);
  if (demo) console.log("  Φορτώθηκε δοκιμαστικό σχολείο (60 μαθητές, 13 όμιλοι).");
  if (noMail) console.log("  Χωρίς email: σύνδεσμοι εκπαιδευτικών από τη διαχείριση (καρτέλα «Όμιλοι»).");
  console.log(`  Δεδομένα: ${storePath}`);
} });
