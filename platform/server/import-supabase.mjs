// Move an installation from Netlify + Supabase to a school's own server
// (README §6): copies every row from Supabase into DATA_DIR/store.json, so
// students, clubs, teacher lists, declarations and the history carry over.
// Messages are in Greek: the school's administrator reads them.
//
//   sudo node server/import-supabase.mjs --dry-run   only read and report
//   sudo node server/import-supabase.mjs             read, report, ask, write
//
// Asks for the Supabase Project URL and secret key (the key is not shown as
// it is typed), or takes SUPABASE_URL / SUPABASE_SECRET_KEY from the
// environment. Reads DATA_DIR and SESSION_SECRET from /etc/omiloi.env
// (OMILOI_ENV overrides the path). The platform must be stopped first
// (sudo systemctl stop omiloi): a running server keeps the data in memory
// and would overwrite the file. The previous file is kept in
// /var/backups/omiloi-before-import.json (OMILOI_BACKUP_DIR overrides).
// --yes skips the final question (tests).

import { spawnSync } from "node:child_process";
import { chmodSync, chownSync, copyFileSync, existsSync, mkdirSync, readFileSync, renameSync, statSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { createInterface } from "node:readline";
import { fileURLToPath } from "node:url";
import { createSupabaseStore } from "../src/server/store-supabase.js";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const args = process.argv.slice(2);
const dryRun = args.includes("--dry-run");
const fail = (message) => {
  console.error(`✗ ${message}`);
  process.exit(1);
};

async function ask(question, { hidden = false } = {}) {
  const rl = createInterface({ input: process.stdin, output: process.stdout, terminal: Boolean(process.stdin.isTTY) });
  let muted = false;
  rl._writeToOutput = (s) => { if (!muted) rl.output.write(s); };
  const answer = new Promise((r) => rl.question(question, r));
  muted = hidden;
  const value = (await answer).trim();
  rl.close();
  if (hidden) process.stdout.write("\n");
  return value;
}

// Settings of the self-hosted install: plain KEY=VALUE lines
const envFile = process.env.OMILOI_ENV ?? "/etc/omiloi.env";
if (!existsSync(envFile)) fail(`Δεν βρέθηκε το ${envFile}. Τρέξτε το με sudo, στο μηχάνημα της πλατφόρμας.`);
const conf = Object.fromEntries(readFileSync(envFile, "utf8").split("\n")
  .map((line) => line.match(/^\s*([A-Z_]+)=(.*)$/)).filter(Boolean).map((m) => [m[1], m[2].trim()]));
const dataDir = resolve(conf.DATA_DIR || join(root, ".data"));
const target = join(dataDir, "store.json");

if (!dryRun && spawnSync("systemctl", ["is-active", "--quiet", "omiloi"]).status === 0) {
  fail("Η πλατφόρμα τρέχει. Σταματήστε την πρώτα: sudo systemctl stop omiloi");
}

const url = process.env.SUPABASE_URL || await ask("Supabase Project URL (https://….supabase.co): ");
const key = process.env.SUPABASE_SECRET_KEY || await ask("Supabase Secret key (δεν εμφανίζεται καθώς το γράφετε): ", { hidden: true });
if (!/^https?:\/\/\S+$/.test(url)) fail("Η διεύθυνση της Supabase πρέπει να αρχίζει με https://");

let data;
try {
  data = await createSupabaseStore({ url, key }).exportAll();
} catch (err) {
  fail(`Η ανάγνωση από τη Supabase απέτυχε: ${err.message}`);
}
if (!data.settings) fail("Η Supabase δεν έχει ρυθμίσεις πλατφόρμας: λάθος project ή άδεια βάση; Δεν άλλαξε τίποτα.");

const PHASE_LABELS = { setup: "Προετοιμασία", teachers: "Εκπαιδευτικοί", parents: "Δηλώσεις γονέων", closed: "Κλειστές δηλώσεις", allocated: "Κατανομή", published: "Ανακοίνωση" };
const count = (prefix) => Object.keys(data).filter((k) => k.startsWith(prefix)).length;
const lists = Object.entries(data).filter(([k, v]) => k.startsWith("teacherList:") && v?.ams?.length);
console.log("Στη Supabase βρέθηκαν:");
console.log(`  Φάση: ${PHASE_LABELS[data.settings.phase] ?? data.settings.phase}${data.settings.deadline ? `, προθεσμία ${data.settings.deadline}` : ""}`);
console.log(`  Μαθητές: ${data.students?.length ?? 0}, όμιλοι: ${data.clubs?.length ?? 0}, εκπαιδευτικοί: ${data.teachers?.length ?? 0}`);
console.log(`  Δηλώσεις γονέων: ${count("submission:")}`);
console.log(`  Λίστες εκπαιδευτικών με μαθητές: ${lists.length} (${lists.reduce((n, [, v]) => n + v.ams.length, 0)} μαθητές συνολικά)`);
console.log(`  Ιστορικό: ${data["log:events"]?.length ?? 0} ενέργειες, ${data["log:outbox"]?.length ?? 0} μηνύματα`);

// Teachers' ΑΜ/ΑΦΜ are stored as HMACs with the session secret, and login
// links are signed with it: the new server must use the same one.
if (data.sessionSecret && conf.SESSION_SECRET && conf.SESSION_SECRET !== data.sessionSecret) {
  fail(`Στο ${envFile} υπάρχει SESSION_SECRET διαφορετικό από της Supabase, και θα υπερίσχυε. Σβήστε τη γραμμή SESSION_SECRET από το ${envFile} και ξανατρέξτε. Δεν άλλαξε τίποτα.`);
}
if (!data.sessionSecret && !conf.SESSION_SECRET) {
  fail(`Το μυστικό κλειδί δεν είναι στη Supabase, άρα είναι στο Netlify (Environment variables → SESSION_SECRET). Αντιγράψτε την τιμή του σε μια γραμμή SESSION_SECRET=… στο ${envFile} και ξανατρέξτε. Χωρίς αυτό οι εκπαιδευτικοί δεν θα μπορούν να συνδεθούν. Δεν άλλαξε τίποτα.`);
}
if (!data.sessionSecret) console.log(`  Μυστικό κλειδί: από το ${envFile}· πρέπει να είναι ίδιο με το SESSION_SECRET του Netlify.`);
else console.log("  Μυστικό κλειδί: μεταφέρεται μαζί με τα δεδομένα.");

if (dryRun) {
  console.log("✓ Δοκιμή: η ανάγνωση πέτυχε. Δεν γράφτηκε τίποτα.");
  process.exit(0);
}
if (!args.includes("--yes")) {
  const answer = await ask(`Να αντικατασταθούν τα δεδομένα στο ${target} με αυτά; Γράψτε «ναι»: `);
  if (answer.toLowerCase() !== "ναι") fail("Ακυρώθηκε. Δεν άλλαξε τίποτα.");
}

if (existsSync(target)) {
  const backupDir = process.env.OMILOI_BACKUP_DIR ?? "/var/backups";
  const backup = join(backupDir, "omiloi-before-import.json");
  try {
    mkdirSync(backupDir, { recursive: true });
    copyFileSync(target, backup);
    chmodSync(backup, 0o600);
  } catch (err) {
    fail(`Δεν ήταν δυνατό να κρατηθεί αντίγραφο του τωρινού αρχείου (${err.message}). Δεν άλλαξε τίποτα.`);
  }
  console.log(`  Το προηγούμενο αρχείο κρατήθηκε στο ${backup}.`);
}
mkdirSync(dataDir, { recursive: true });
writeFileSync(`${target}.tmp`, JSON.stringify(data, null, 1), { mode: 0o600 });
// Same owner as the data folder (the «omiloi» user), so the platform can write it.
if (process.getuid?.() === 0) {
  const { uid, gid } = statSync(dataDir);
  chownSync(`${target}.tmp`, uid, gid);
}
renameSync(`${target}.tmp`, target);
console.log(`✓ Γράφτηκε το ${target}.`);
console.log("  Τώρα: sudo systemctl start omiloi, και έλεγχος στη σελίδα διαχείρισης.");
