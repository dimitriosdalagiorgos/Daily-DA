// How much the lottery rule matters, on a real data package: one lottery
// number per student for the whole week (the platform's rule) vs a new
// number each day. Reads an R data package (the platform's «Δεδομένα για R
// (.zip)», as a .zip or unzipped into a folder), reruns the week with many
// seeds under each rule and prints aggregate counts only: no registry
// numbers or names, so the output can be shared. Nothing is sent anywhere.
//
//   node platform/scripts/lottery-compare.mjs <folder or .zip> [--runs N]
//
// --runs  seeds per rule (default 200). Seeds are `<package seed>#<i>`, and
//         the per-day rule draws `<seed>|<day>` for each day, so every run
//         can be reproduced.
//
// Per student, a "decision" is a day the student takes part in that day's
// allocation with a non-empty list (multi-day clubs count once, on their
// first day). A decision is a "1st choice" when the student gets the first
// club of the list that reached the allocation (clubs removed beforehand,
// e.g. ⛔ or a similar club already held, are not lottery losses), and a
// "miss" otherwise (a lower club, or none).

import { resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { DAYS } from "../src/algorithm/days.js";
import { drawLottery } from "../src/algorithm/lottery.js";
import { allocateWeek } from "../src/algorithm/allocate.js";
import { readRPackage } from "../src/export/rounds.js";
import { readPackage } from "./propose.mjs";

/** Aggregate outcome of one allocation. */
export function measure(result) {
  const perStudent = new Map();
  let firsts = 0, decisions = 0, unplaced = 0, rankSum = 0, placed = 0;
  for (const day of DAYS) {
    const { assignments, unassigned } = result.days[day];
    for (const a of assignments) {
      if (a.via !== "da") continue;
      const s = perStudent.get(a.am) ?? { decisions: 0, misses: 0, unplaced: 0 };
      s.decisions++;
      decisions++;
      placed++;
      rankSum += a.rank;
      if (a.rank === 1) firsts++;
      else s.misses++;
      perStudent.set(a.am, s);
    }
    for (const u of unassigned) {
      if (u.reason !== "all_rejected") continue;
      const s = perStudent.get(u.am) ?? { decisions: 0, misses: 0, unplaced: 0 };
      s.decisions++;
      s.misses++;
      s.unplaced++;
      decisions++;
      unplaced++;
      perStudent.set(u.am, s);
    }
  }
  const students = [...perStudent.values()];
  const several = students.filter((s) => s.decisions >= 2);
  const missHistogram = [0, 0, 0, 0, 0, 0];
  for (const s of students) missHistogram[Math.min(s.misses, 5)]++;
  return {
    students: students.length,
    decisions,
    firsts,
    meanRank: placed ? rankSum / placed : 0,
    unplaced,
    noFirstAtAll: several.filter((s) => s.misses === s.decisions).length,
    firstEveryDay: several.filter((s) => s.misses === 0).length,
    missedAtLeast3: students.filter((s) => s.misses >= 3).length,
    unplacedAtLeast2: students.filter((s) => s.unplaced >= 2).length,
    several: several.length,
    missHistogram,
  };
}

const METRICS = [
  ["firsts", "Αποφάσεις με 1η επιλογή (σύνολο)"],
  ["meanRank", "Μέση θέση του ομίλου που πήραν"],
  ["unplaced", "Αποφάσεις χωρίς όμιλο (σύνολο)"],
  ["noFirstAtAll", "Μαθητές που δεν πήραν ποτέ την 1η τους επιλογή*"],
  ["firstEveryDay", "Μαθητές που πήραν την 1η τους επιλογή κάθε ημέρα*"],
  ["missedAtLeast3", "Μαθητές που έχασαν την 1η επιλογή σε 3+ ημέρες"],
  ["unplacedAtLeast2", "Μαθητές χωρίς όμιλο σε 2+ ημέρες"],
];

function summarize(runs, key) {
  const values = runs.map((r) => r[key]).sort((a, b) => a - b);
  const mean = values.reduce((x, y) => x + y, 0) / values.length;
  const at = (q) => values[Math.min(values.length - 1, Math.floor(q * values.length))];
  return { mean, low: at(0.05), high: at(0.95) };
}

const fmt = (key, v) => (key === "meanRank" ? v.toFixed(2) : Number.isInteger(v) ? String(v) : v.toFixed(1));

/**
 * Runs both rules and returns the report in Greek.
 * @param {Record<string, string|Uint8Array>} files
 */
export function compareLotteries(files, { runs = 200, onProgress = () => {} } = {}) {
  const input = readRPackage(files, drawLottery);
  const ams = input.students.map((s) => String(s.am));
  const base = input.seed ?? "omiloi";
  const actual = measure(allocateWeek({ ...input, audit: false }));

  const weekly = [];
  const daily = [];
  for (let i = 1; i <= runs; i++) {
    const seed = `${base}#${i}`;
    weekly.push(measure(allocateWeek({ ...input, lottery: drawLottery(ams, seed), audit: false })));
    const lotteryByDay = Object.fromEntries(DAYS.map((d) => [d, drawLottery(ams, `${seed}|${d}`)]));
    daily.push(measure(allocateWeek({ ...input, lottery: lotteryByDay.mon, lotteryByDay, audit: false })));
    onProgress(i, runs);
  }

  const lines = [];
  lines.push(`Μαθητές: ${input.students.length}, όμιλοι: ${input.clubs.length}, δηλώσεις: ${Object.keys(input.preferences).length}`);
  lines.push(`Αποφάσεις (μαθητής × ημέρα κατανομής): ${actual.decisions}, μαθητές με 2+ αποφάσεις: ${actual.several}`);
  lines.push(`Επαναλήψεις ανά κανόνα: ${runs} (σε αγκύλες: το 90% των επαναλήψεων)`);
  lines.push("");
  const w = 52;
  lines.push(`${"".padEnd(w)}${"Πραγματική".padEnd(12)}${"Ένας αριθμός".padEnd(22)}Αριθμός ανά ημέρα`);
  lines.push(`${"".padEnd(w)}${"κατανομή".padEnd(12)}${"(εβδομάδα)".padEnd(22)}`);
  for (const [key, label] of METRICS) {
    const a = summarize(weekly, key);
    const b = summarize(daily, key);
    const cell = (s) => `${fmt(key, s.mean)} [${fmt(key, s.low)}–${fmt(key, s.high)}]`;
    lines.push(`${label.padEnd(w)}${fmt(key, actual[key]).padEnd(12)}${cell(a).padEnd(22)}${cell(b)}`);
  }
  lines.push(`* μόνο μαθητές με 2+ αποφάσεις`);
  lines.push("");
  lines.push("Πόσες φορές έχασε ο καθένας την 1η επιλογή του (μέσος αριθμός μαθητών):");
  lines.push(`${"".padEnd(14)}${"Πραγματική".padEnd(12)}${"Ένας αριθμός".padEnd(14)}Ανά ημέρα`);
  for (let k = 0; k <= 5; k++) {
    const mean = (rs) => rs.reduce((x, r) => x + r.missHistogram[k], 0) / rs.length;
    const label = k === 5 ? "5 φορές" : k === 1 ? "1 φορά" : `${k} φορές`;
    lines.push(`${label.padEnd(14)}${String(actual.missHistogram[k]).padEnd(12)}${mean(weekly).toFixed(1).padEnd(14)}${mean(daily).toFixed(1)}`);
  }
  lines.push("");
  lines.push("Η «Πραγματική κατανομή» είναι με το seed του πακέτου και τον ένα αριθμό. Οι άλλες στήλες είναι μέσοι όροι");
  lines.push("από νέες κληρώσεις, για να φανεί τι κάνει ο κανόνας και όχι η τύχη μιας κλήρωσης. Δεν εμφανίζονται ΑΜ ή ονόματα.");
  return { actual, weekly, daily, text: lines.join("\n") };
}

async function main(args) {
  const usage = "Χρήση: node platform/scripts/lottery-compare.mjs <φάκελος ή .zip> [--runs N]";
  let path;
  let runs = 200;
  for (let i = 0; i < args.length; i++) {
    if (args[i] === "--runs") runs = Number(args[++i]);
    else if (!path) path = args[i];
    else throw new Error(usage);
  }
  if (!path || !(Number.isInteger(runs) && runs > 0)) throw new Error(usage);
  const files = await readPackage(resolve(path));
  const { text } = compareLotteries(files, {
    runs,
    onProgress: (i, n) => { if (process.stderr.isTTY) process.stderr.write(`\r${i}/${n}`); },
  });
  if (process.stderr.isTTY) process.stderr.write("\r          \r");
  console.log(text);
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  main(process.argv.slice(2)).catch((err) => {
    console.error(err.message);
    process.exit(1);
  });
}
