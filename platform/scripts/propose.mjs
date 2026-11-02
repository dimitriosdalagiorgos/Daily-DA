// The «Προτεινόμενη κατανομή» of the admin page, offline: reads an R data
// package (the platform's «Δεδομένα για R (.zip)», as a .zip or unzipped
// into a folder), finds the seats to add so that students of mandatory
// grades who did not fit get a club (src/planning/capacity.js), and prints
// it. For trying out changed data (a corrected declaration, teachers' lists,
// capacities) before touching the platform. Nothing is sent anywhere.
//
//   node platform/scripts/propose.mjs <folder or .zip> [--out <folder>] [--time <seconds>]
//
// --out   writes a copy of the package with the suggested capacities in
//         clubs.csv, to rerun with R/run_week.R or this script
// --time  stops the search after that many seconds (default: no limit)
//
// The platform caps increases at 22 seats, or 30 for a club the clubs file
// gave more than 22 (capacityCeiling). The package holds today's
// capacities, after any «Διόρθωση», so a club corrected above 22 counts as
// a «σύνολο» here.

import { readFileSync, readdirSync, statSync, mkdirSync, writeFileSync } from "node:fs";
import { join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { DAYS, DAY_LABELS } from "../src/algorithm/days.js";
import { drawLottery } from "../src/algorithm/lottery.js";
import { readRPackage, readZip } from "../src/export/rounds.js";
import { decodeCsv, parseCsv } from "../src/import/csv.js";
import { toCsv } from "../src/export/rPackage.js";
import { suggestCapacityIncreases } from "../src/planning/capacity.js";

/** Files of an R data package: a .zip or a folder. */
export async function readPackage(path) {
  if (statSync(path).isDirectory()) {
    const files = {};
    for (const name of readdirSync(path)) {
      if (/\.(csv|txt)$/i.test(name)) files[name] = readFileSync(join(path, name));
    }
    return files;
  }
  return readZip(new Uint8Array(readFileSync(path)));
}

/**
 * The suggestion for a package, and a report in Greek.
 * @param {Record<string, string|Uint8Array>} files
 */
export function propose(files, { timeLimitMs = Infinity } = {}) {
  const input = readRPackage(files, drawLottery);
  const fileCapacity = Object.fromEntries(input.clubs.map((c) => [c.code, c.capacity]));
  const suggestion = suggestCapacityIncreases(input, fileCapacity, { timeLimitMs });

  const student = new Map(input.students.map((s) => [String(s.am), s]));
  const club = new Map(input.clubs.map((c) => [String(c.code), c]));
  const dayOrder = (a, b) => DAYS.indexOf(a.day) - DAYS.indexOf(b.day) || a.am.localeCompare(b.am, "el", { numeric: true });
  const who = ({ am, day }) => {
    const s = student.get(am);
    const name = [s?.surname, s?.name].filter((x) => x && x !== "-").join(" ");
    return `  ${DAY_LABELS[day].padEnd(10)} ΑΜ ${am}${name ? ` ${name}` : ""}${s?.grade ? ` (${s.grade})` : ""}`;
  };
  const placedNow = suggestion.before.filter((b) => !suggestion.after.some((a) => a.am === b.am && a.day === b.day));

  const lines = [];
  lines.push(`Μαθητές: ${input.students.length}, όμιλοι: ${input.clubs.length}, δηλώσεις: ${Object.keys(input.preferences).length}${input.seed ? `, seed: ${input.seed}` : ""}`);
  if (!input.mandatoryGrades.length) {
    lines.push("", "Καμία τάξη δεν έχει υποχρεωτική ένταξη (mandatory_grades.csv): η πρόταση αφορά μόνο αυτές τις τάξεις.");
    return { suggestion, input, text: lines.join("\n") };
  }
  lines.push(`Τάξεις με υποχρεωτική ένταξη: ${input.mandatoryGrades.join(", ")}`);
  lines.push("", `Χωρίς όμιλο, ενώ δεν χώρεσαν (τάξεις με υποχρεωτική ένταξη): ${suggestion.before.length}`);
  for (const u of [...suggestion.before].sort(dayOrder)) lines.push(who(u));
  if (!suggestion.before.length) {
    lines.push("", "✓ Όλοι οι μαθητές των τάξεων με υποχρεωτική ένταξη έχουν όμιλο. Δεν χρειάζεται αύξηση θέσεων.");
    return { suggestion, input, text: lines.join("\n") };
  }
  lines.push("", "Προτεινόμενες αυξήσεις θέσεων:");
  if (!suggestion.increases.length) lines.push("  καμία: καμία αύξηση, μέσα στα όρια, δεν τοποθετεί κάποιον ακόμα");
  for (const i of suggestion.increases) {
    const c = club.get(i.code);
    const days = c.days.map((d) => DAY_LABELS[d]).join(", ");
    lines.push(`  ${i.code} ${c.name} (${days}): ${i.from} → ${i.to} θέσεις (+${i.to - i.from}, όριο ${i.ceiling})`);
  }
  lines.push("", `Τοποθετούνται με τις αυξήσεις: ${placedNow.length}`);
  for (const u of placedNow.sort(dayOrder)) lines.push(who(u));
  lines.push("", `Μένουν χωρίς όμιλο και μετά: ${suggestion.after.length}`);
  for (const u of [...suggestion.after].sort(dayOrder)) lines.push(who(u));
  if (!suggestion.complete) lines.push("", "Προσοχή: η αναζήτηση σταμάτησε στο χρονικό όριο (--time). Η πρόταση είναι μερική.");
  lines.push("", "Η θέση που προστίθεται δεν πηγαίνει πάντα στον μαθητή που έμεινε εκτός: η κατανομή ξανατρέχει με το ίδιο seed και μπορεί να μετακινηθούν και άλλοι μαθητές. Για να δείτε ποιοι, τρέξτε με --out και συγκρίνετε με το R.");
  lines.push("Όριο αύξησης: 22 θέσεις, ή 30 για όμιλο με περισσότερες από 22. Εδώ μετρούν οι σημερινές θέσεις του zip, μαζί με τις «Διορθώσεις».");
  return { suggestion, input, text: lines.join("\n") };
}

/** The package with new capacities in clubs.csv (other files unchanged). */
export function withCapacities(files, increases) {
  const to = new Map(increases.map((i) => [String(i.code), i.to]));
  const text = (f) => (typeof f === "string" ? f : decodeCsv(f));
  const [header, ...rows] = parseCsv(text(files["clubs.csv"])).filter((r) => r.some((c) => c.trim() !== ""));
  const code = header.indexOf("code");
  const capacity = header.indexOf("capacity");
  const updated = rows.map((r) => r.map((cell, i) => (i === capacity && to.has(r[code].trim()) ? String(to.get(r[code].trim())) : cell)));
  return { ...files, "clubs.csv": toCsv(header, updated) };
}

async function main(args) {
  const usage = "Χρήση: node platform/scripts/propose.mjs <φάκελος ή .zip> [--out <φάκελος>] [--time <δευτερόλεπτα>]";
  let path;
  let out;
  let seconds;
  for (let i = 0; i < args.length; i++) {
    if (args[i] === "--out") out = args[++i];
    else if (args[i] === "--time") seconds = Number(args[++i]);
    else if (!path) path = args[i];
    else throw new Error(usage);
  }
  if (!path || (args.includes("--out") && !out) || (seconds !== undefined && !(seconds > 0))) throw new Error(usage);

  const files = await readPackage(resolve(path));
  const { suggestion, text } = propose(files, { timeLimitMs: seconds ? seconds * 1000 : Infinity });
  console.log(text);
  if (out) {
    const copy = withCapacities(files, suggestion.increases);
    mkdirSync(out, { recursive: true });
    for (const [name, content] of Object.entries(copy)) writeFileSync(join(out, name), content);
    console.log(`\nΑντίγραφο με τις προτεινόμενες θέσεις: ${resolve(out)}`);
  }
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  main(process.argv.slice(2)).catch((err) => {
    console.error(err.message);
    process.exit(1);
  });
}
