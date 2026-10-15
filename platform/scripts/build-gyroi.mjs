// Builds docs/gyroi-katanomis.html (and its copy in public/docs/): the
// allocation round by round, as one self-contained page that works offline.
// It inlines the allocation code itself (src/algorithm), the reader of the R
// data package and the round view (src/export/rounds.js), and a sample
// package of the fictional demo school (dev/demo-data.mjs) with made-up
// declarations. Run after changing any of them:
//
//   node scripts/build-gyroi.mjs
//
// test/gyroi-page.test.js fails while the page is out of date.

import { readFileSync, writeFileSync } from "node:fs";
import { buildRPackage } from "../src/export/rPackage.js";
import { importClubs, importStudents } from "../src/import/index.js";
import { DAYS, drawLottery, seededRandom } from "../src/algorithm/index.js";
import { demoClubRows, demoStudentRows } from "../dev/demo-data.mjs";

const root = new URL("..", import.meta.url);
const read = (path) => readFileSync(new URL(path, root), "utf8");

/** ES module → plain script: drop the imports and the `export` keywords. */
const inline = (path) => `// ---- ${path} ----\n${read(path).replace(/^import [^;]*;\n/gm, "").replace(/^export /gm, "")}`;

const SAMPLE_SEED = "demo-2026";

/**
 * The demo school with made-up declarations: some clubs are more popular,
 * so that seats run out and the rounds have something to show.
 */
export function samplePackage() {
  const students = importStudents(demoStudentRows()).students;
  const clubs = importClubs(demoClubRows()).clubs;
  const weight = { 101: 6, 102: 2, 103: 7, 104: 2, 105: 6, 106: 2, 107: 6, 108: 2, 109: 4, 110: 3, 111: 6, 112: 3, 113: 5 };
  const rng = seededRandom(`${SAMPLE_SEED}-declarations`);
  const preferences = {};
  for (const s of students) {
    preferences[s.am] = {};
    for (const day of DAYS) {
      const pool = clubs.filter((c) => c.days.includes(day) && c.grades.includes(s.grade)).map((c) => String(c.code));
      const list = [];
      while (pool.length) {
        let x = rng() * pool.reduce((sum, code) => sum + weight[code], 0);
        let k = 0;
        while (k < pool.length - 1 && (x -= weight[pool[k]]) >= 0) k++;
        list.push(...pool.splice(k, 1));
      }
      if (list.length) preferences[s.am][day] = list;
    }
  }
  // Teachers' lists: a few ⭐ in two clubs, one ⛔.
  const teacherLists = { 103: ["9025", "9032"], 107: ["9047"] };
  const exclusions = { 101: ["9056"] };
  const lottery = drawLottery(students.map((s) => s.am), SAMPLE_SEED);
  return buildRPackage({ students, clubs, preferences, teacherLists, exclusions, lottery, seed: SAMPLE_SEED, mandatoryGrades: [] });
}

export function buildPage() {
  const code = ["src/algorithm/days.js", "src/algorithm/lottery.js", "src/algorithm/allocate.js", "src/import/csv.js", "src/export/rounds.js"].map(inline).join("\n");
  const sample = JSON.stringify(samplePackage()).replace(/</g, "\\u003c");
  return read("scripts/gyroi-template.html")
    .replace("/*@CODE@*/", () => code)
    .replace("/*@SAMPLE@*/", () => `const SAMPLE = ${sample};`);
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const page = buildPage();
  for (const out of ["../docs/gyroi-katanomis.html", "public/docs/gyroi-katanomis.html"]) writeFileSync(new URL(out, root), page);
  console.log("✓ docs/gyroi-katanomis.html, public/docs/gyroi-katanomis.html");
}
