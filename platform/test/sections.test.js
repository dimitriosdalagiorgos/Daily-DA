import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { decodeCsv, parseCsv } from "../src/import/csv.js";
import { importSections, sectionName, sectionsFileKind } from "../src/import/sections.js";

// Fictional reports in myschool's format (Windows-1253, «;», CRLF)
const read = (name) => parseCsv(decodeCsv(readFileSync(new URL(`./fixtures/${name}`, import.meta.url))));
const DEFINITIONS = read("myschool_genika_stoixeia_tmimaton_fictional.csv");
const STUDENTS = read("myschool_tmimata_mathiton_fictional.csv");

test("sections: which report is which", () => {
  assert.equal(sectionsFileKind(DEFINITIONS), "definitions");
  assert.equal(sectionsFileKind(STUDENTS), "students");
  assert.equal(sectionsFileKind([["Τάξη", "Αριθμός Μητρώου"]]), null);
});

test("sections: only the general-education section, by its official type; Latin look-alikes become Greek", () => {
  assert.equal(sectionName(" a1 "), "Α1");
  const { sections, problems, summary } = importSections(DEFINITIONS, STUDENTS);
  assert.deepEqual(problems, []);
  assert.deepEqual(summary, { count: 5, byGrade: { Α: 2, Β: 2, Γ: 1 } });
  assert.deepEqual(sections, {
    9101: { grade: "Α", section: "Α1" }, // «A1» with a Latin A in both reports
    9102: { grade: "Α", section: "Α2" },
    9201: { grade: "Β", section: "Β2" }, // ΒΑ1 is a direction section, listed first
    9202: { grade: "Β", section: "Β1" },
    9301: { grade: "Γ", section: "Γ1" }, // not ΓΑ1-ΜΑΘΗΜΑΤΙΚΑ («βάσει Προσανατολισμού»)
  });
});

test("sections: unknown sections and students without one are warnings; a missing table is an error", () => {
  const rows = STUDENTS.map((r) => (r[1] === "9102" ? [...r.slice(0, 5), "Α-ΙΣΠΑΝΙΚΑ 1", ""] : r));
  const report = importSections(DEFINITIONS, rows);
  assert.equal(report.sections[9102].section, "");
  assert.deepEqual(report.problems.map((p) => p.level), ["warning", "warning"]);
  assert.match(report.problems[0].message, /Α-ΙΣΠΑΝΙΚΑ 1/);
  assert.match(report.problems[1].message, /ΑΜ 9102/);

  assert.equal(importSections(STUDENTS, STUDENTS).problems[0].level, "error");
  const badGrade = STUDENTS.map((r) => (String(r[0]).startsWith("Τάξη Εγγραφής") ? [r[0], "", "", "Δ"] : r));
  assert.match(importSections(DEFINITIONS, badGrade).problems[0].message, /τάξη/);
});
