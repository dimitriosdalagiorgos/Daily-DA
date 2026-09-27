// Compare the platform's allocation with the R reference on the same data
// and the same lottery.
//
//   node reference/compare.mjs [--random N] [--seed S]
//
// Scenarios:
//   sample   the repository's dailyclubs.csv + dailyresponses.csv (one day),
//            with teacher lists for two clubs
//   random   N random weeks with single, double and triple clubs, grade
//            restrictions, teacher lists, and some students skipping a day
// Needs Rscript with dplyr, readr, tidyr, purrr, stringr, writexl.

import { execFileSync } from "node:child_process";
import { mkdtempSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { allocateWeek, drawLottery, seededRandom, DAYS } from "../src/algorithm/index.js";

const here = dirname(fileURLToPath(import.meta.url));
const repoRoot = join(here, "..", "..");

// ---------- CSV ----------

function parseCsv(text) {
  const rows = [];
  let row = [], field = "", quoted = false;
  for (let i = 0; i < text.length; i++) {
    const ch = text[i];
    if (quoted) {
      if (ch === '"' && text[i + 1] === '"') { field += '"'; i++; }
      else if (ch === '"') quoted = false;
      else field += ch;
    } else if (ch === '"') quoted = true;
    else if (ch === ",") { row.push(field); field = ""; }
    else if (ch === "\n" || ch === "\r") {
      if (ch === "\r" && text[i + 1] === "\n") i++;
      row.push(field); rows.push(row); row = []; field = "";
    } else field += ch;
  }
  if (field !== "" || row.length) { row.push(field); rows.push(row); }
  const [header, ...body] = rows.filter((r) => r.some((c) => c !== ""));
  return body.map((r) => Object.fromEntries(header.map((h, i) => [h.trim(), r[i] ?? ""])));
}

const csvLine = (values) => values.map((v) => (/[",\n]/.test(String(v)) ? `"${String(v).replace(/"/g, '""')}"` : String(v))).join(",");
const writeCsv = (path, header, rows) => writeFileSync(path, [csvLine(header), ...rows.map(csvLine)].join("\n") + "\n");

// ---------- R input from a platform scenario ----------

// R identifies clubs by name; use "c<code>" so names are safe file names.
const rName = (code) => `c${code}`;

/**
 * Old-form preferences for R: every day lists the clubs the student ranked
 * that day, with rank numbers. Multi-day clubs may appear on later days
 * (as parents filled them last year); both sides must drop them.
 */
function writeRInput(dir, scenario) {
  const { students, clubs, preferences, teacherLists, lottery } = scenario;
  writeCsv(join(dir, "clubs.csv"), ["club_name", "club_capacity", "days"],
    clubs.map((c) => [rName(c.code), c.capacity, c.days.join(";")]));
  for (const day of DAYS) {
    const dayClubs = clubs.filter((c) => c.days.includes(day)).map((c) => String(c.code));
    if (dayClubs.length === 0) continue;
    writeCsv(join(dir, `responses_${day}.csv`), ["RegistryNr", "Surname", "Name", ...dayClubs.map(rName)],
      students.map((s) => {
        const list = (preferences[s.am]?.[day] ?? []).map(String);
        return [s.am, `S${s.am}`, `N${s.am}`, ...dayClubs.map((code) => (list.includes(code) ? list.indexOf(code) + 1 : ""))];
      }));
  }
  mkdirSync(join(dir, "teacherpreferences"), { recursive: true });
  for (const [code, ams] of Object.entries(teacherLists)) {
    if (ams.length === 0) continue;
    writeCsv(join(dir, "teacherpreferences", `${rName(code)}.csv`), ["RegistryNr", "teacher_preference_rank"],
      ams.map((am, i) => [am, i + 1]));
  }
  writeCsv(join(dir, "lottery.csv"), ["RegistryNr", "lottery_number"], [...lottery].map(([am, n]) => [am, n]));
}

function runR(dir) {
  execFileSync("Rscript", [join(here, "run_week_reference.R"), dir], {
    stdio: ["ignore", "pipe", "pipe"],
    env: { ...process.env, LANG: "C.UTF-8", LC_ALL: "C.UTF-8" },
  });
  return parseCsv(readFileSync(join(dir, "week_assignments.csv"), "utf8"));
}

/** Differences between the two allocations, as readable lines. */
function compare(scenario, rRows) {
  const result = allocateWeek(scenario);
  const platform = result.byStudent;
  const r = Object.fromEntries(scenario.students.map((s) => [s.am, Object.fromEntries(DAYS.map((d) => [d, null]))]));
  for (const row of rRows) r[row.RegistryNr][row.day] = row.club_name.replace(/^c/, "");
  const diffs = [];
  let seats = 0;
  for (const s of scenario.students) {
    for (const day of DAYS) {
      if (platform[s.am][day] !== null) seats++;
      if (platform[s.am][day] !== r[s.am][day]) {
        diffs.push(`ΑΜ ${s.am} ${day}: πλατφόρμα=${platform[s.am][day]} R=${r[s.am][day]}`);
      }
    }
  }
  const all = DAYS.flatMap((d) => result.days[d].log);
  const carried = all.filter((e) => e.event === "CARRIED").length;
  const conflicts = all.filter((e) => e.event === "CLUB_DROPPED" && e.reason === "day_conflict").length;
  return { diffs, seats, carried, conflicts };
}

// ---------- Scenarios ----------

function sampleScenario() {
  const clubsCsv = parseCsv(readFileSync(join(repoRoot, "dailyclubs.csv"), "utf8"));
  const responses = parseCsv(readFileSync(join(repoRoot, "dailyresponses.csv"), "utf8"));
  const codeOf = new Map(clubsCsv.map((c, i) => [c.club_name.trim().toLowerCase(), 101 + i]));
  const clubs = clubsCsv.map((c) => ({
    code: codeOf.get(c.club_name.trim().toLowerCase()), name: c.club_name, days: ["mon"],
    grades: ["Α"], capacity: Number(c.club_capacity),
  }));
  const students = responses.map((r) => ({ am: r.RegistryNr, grade: "Α" }));
  const preferences = {};
  for (const r of responses) {
    const ranked = Object.entries(r)
      .filter(([k, v]) => !["RegistryNr", "Surname", "Name"].includes(k) && v.trim() !== "")
      .sort((a, b) => Number(a[1]) - Number(b[1]))
      .map(([k]) => codeOf.get(k.trim().toLowerCase()));
    preferences[r.RegistryNr] = { mon: ranked };
  }
  const lottery = drawLottery(students.map((s) => s.am), "sample-2026");
  // Teacher lists for the two most oversubscribed clubs: first-choice
  // students with the worst lottery numbers (who would lose without the
  // teacher) and one who ranked the club second.
  const teacherLists = {};
  const firstChoices = (c) => students.filter((s) => preferences[s.am].mon[0] === c.code);
  const demand = (c) => firstChoices(c).length / c.capacity;
  for (const c of [...clubs].sort((a, b) => demand(b) - demand(a)).slice(0, 2)) {
    const losers = firstChoices(c).sort((a, b) => lottery.get(b.am) - lottery.get(a.am)).slice(0, 2);
    const second = students.find((s) => preferences[s.am].mon[1] === c.code);
    teacherLists[c.code] = [...losers, ...(second ? [second] : [])].map((s) => s.am);
  }
  return { name: "sample", students, clubs, preferences, teacherLists, lottery };
}

function randomScenario(rng, name) {
  const pick = (arr) => arr[Math.floor(rng() * arr.length)];
  const shuffle = (arr) => {
    const a = [...arr];
    for (let i = a.length - 1; i > 0; i--) {
      const j = Math.floor(rng() * (i + 1));
      [a[i], a[j]] = [a[j], a[i]];
    }
    return a;
  };
  const grades = ["Α", "Β", "Γ"];
  const students = Array.from({ length: 30 + Math.floor(rng() * 50) }, (_, i) => ({ am: String(5000 + i), grade: pick(grades) }));
  const clubs = [];
  const nClubs = 8 + Math.floor(rng() * 10);
  for (let code = 101; code < 101 + nClubs; code++) {
    const len = rng() < 0.6 ? 1 : rng() < 0.7 ? 2 : 3;
    const days = shuffle(DAYS).slice(0, len).sort((a, b) => DAYS.indexOf(a) - DAYS.indexOf(b));
    clubs.push({ code, name: `Όμιλος ${code}`, days, grades: shuffle(grades).slice(0, 1 + Math.floor(rng() * 3)), capacity: 2 + Math.floor(rng() * 8) });
  }
  const preferences = {};
  for (const s of students) {
    preferences[s.am] = {};
    for (const day of DAYS) {
      if (rng() < 0.05) continue; // some parents skip a day
      const eligible = clubs.filter((c) => c.days.includes(day) && c.grades.includes(s.grade)).map((c) => c.code);
      preferences[s.am][day] = shuffle(eligible);
    }
  }
  const teacherLists = {};
  for (const c of clubs) {
    if (rng() < 0.3) {
      const pool = students.filter((s) => c.grades.includes(s.grade)).map((s) => s.am);
      teacherLists[c.code] = shuffle(pool).slice(0, Math.min(c.capacity, 1 + Math.floor(rng() * 3)));
    }
  }
  const lottery = drawLottery(students.map((s) => s.am), `${name}-${rng()}`);
  return { name, students, clubs, preferences, teacherLists, lottery };
}

// ---------- Main ----------

const argv = process.argv.slice(2);
const opt = (flag, fallback) => (argv.includes(flag) ? argv[argv.indexOf(flag) + 1] : fallback);
const nRandom = Number(opt("--random", "8"));
const rng = seededRandom(opt("--seed", "compare"));

const scenarios = [sampleScenario(), ...Array.from({ length: nRandom }, (_, i) => randomScenario(rng, `random-${i + 1}`))];
let failed = 0;
for (const scenario of scenarios) {
  const dir = mkdtempSync(join(tmpdir(), `compare-${scenario.name}-`));
  writeRInput(dir, scenario);
  const { diffs, seats, carried, conflicts } = compare(scenario, runR(dir));
  const multi = scenario.clubs.filter((c) => c.days.length > 1).length;
  const summary = `${scenario.name}: ${scenario.students.length} μαθητές, ${scenario.clubs.length} όμιλοι (${multi} πολυήμεροι), ${seats} θέσεις, ${carried} μεταφορές, ${conflicts} συγκρούσεις ημερών`;
  if (diffs.length === 0) {
    console.log(`✓ ${summary} — ίδια αποτελέσματα`);
  } else {
    failed++;
    console.log(`✗ ${summary} — ${diffs.length} διαφορές (αρχεία: ${dir})`);
    for (const d of diffs.slice(0, 20)) console.log(`    ${d}`);
  }
}
if (failed) {
  console.log(`\n${failed} από ${scenarios.length} σενάρια διαφέρουν.`);
  process.exit(1);
}
console.log(`\nΚαι τα ${scenarios.length} σενάρια δίνουν ίδια αποτελέσματα.`);
