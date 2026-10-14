import { test } from "node:test";
import assert from "node:assert/strict";
import { teacherListRows, teacherListFileName } from "../src/export/teacherListFile.js";
import { importTeacherLists, readTeacherListRows } from "../src/import/teacherLists.js";

const clubs = [
  { code: 102, name: "Θεατρική παράσταση «Αντιγόνη»", days: ["mon", "thu"], grades: ["Β", "Γ"], capacity: 2 },
  { code: 105, name: "Σκάκι", days: ["tue"], grades: ["Α"], capacity: 3 },
];
const students = [
  { am: "9001", grade: "Β", surname: "ΓΕΩΡΓΙΟΥ", name: "ΕΛΕΝΗ" },
  { am: "9002", grade: "Γ", surname: "ΑΘΑΝΑΣΙΟΥ", name: "ΝΙΚΟΣ" },
  { am: "9003", grade: "Β", surname: "ΔΗΜΟΥ", name: "ΣΟΦΙΑ" },
  { am: "9004", grade: "Α", surname: "ΠΕΤΡΙΔΗΣ", name: "ΓΙΑΝΝΗΣ" },
];

test("per-club file: only the club's grades, sorted; the code on top", () => {
  const rows = teacherListRows(clubs[0], students, ["9003"]);
  assert.deepEqual(rows[0], ["Κωδικός ομίλου", 102]);
  assert.deepEqual(rows.slice(6).map((r) => [r[0], r[1]]), [["", 9001], ["Χ", 9003], ["", 9002]], "Β then Γ, by surname; existing list pre-marked");
  assert.equal(teacherListFileName(clubs[0]), "102_Θεατρική_παράσταση_Αντιγόνη.xlsx");
});

test("per-club file: marked rows are the list (any mark)", () => {
  const rows = teacherListRows(clubs[0], students);
  rows[6][0] = "x"; rows[8][0] = "✓";
  assert.deepEqual(readTeacherListRows(rows, { clubs, students }), { lists: [{ code: 102, ams: ["9001", "9002"], blocked: [], closed: false }], problems: [], warnings: [] });
});

test("per-club file: nothing marked → the rows left are the list; untouched → empty list with a warning", () => {
  const rows = teacherListRows(clubs[0], students);
  const kept = [...rows.slice(0, 6), rows[7]]; // teacher deleted the other rows
  assert.deepEqual(readTeacherListRows(kept, { clubs, students }).lists, [{ code: 102, ams: ["9003"], blocked: [], closed: false }]);
  const untouched = readTeacherListRows(rows, { clubs, students });
  assert.deepEqual(untouched.lists, [{ code: 102, ams: [], blocked: [], closed: false }]);
  assert.match(untouched.warnings[0], /Δεν σημειώθηκε/);
});

test("one table for many clubs: «Κωδικός ομίλου» and «ΑΜ» per row", () => {
  const rows = [["Κωδικός ομίλου", "ΑΜ", "Ονοματεπώνυμο (για έλεγχο)"], [102, 9001, ""], [105, "9004", ""], [102, 9002, ""]];
  assert.deepEqual(readTeacherListRows(rows, { clubs, students }).lists, [{ code: 102, ams: ["9001", "9002"] }, { code: 105, ams: ["9004"] }]);
});

test("several files: checks per club, duplicate clubs, replacing an existing list", () => {
  const ok = teacherListRows(clubs[0], students); ok[6][0] = "Χ";
  const tooMany = teacherListRows(clubs[1], students); // only 9004 is Α
  const wrongGrade = [["Κωδικός ομίλου", "ΑΜ"], [105, 9001], [105, 1234]];
  const report = importTeacherLists(
    [{ fileName: "a.xlsx", rows: ok }, { fileName: "b.xlsx", rows: wrongGrade }, { fileName: "c.xlsx", rows: [["κάτι άλλο"]] }],
    { clubs, students, lists: { 102: { ams: ["9002"], updatedBy: "etheatr@sch.gr" } } },
  );
  assert.deepEqual(report.map((r) => [r.fileName, r.code, r.ams]), [["a.xlsx", 102, ["9001"]], ["b.xlsx", 105, ["9001", "1234"]], ["c.xlsx", null, []]]);
  assert.deepEqual(report[0].problems, []);
  assert.match(report[0].warnings[0], /ήδη λίστα 1 μαθητών \(από etheatr@sch\.gr\)· θα αντικατασταθεί/);
  assert.deepEqual(report[1].problems, ["Ο ΑΜ 9001 είναι στην τάξη Β, που δεν αφορά τον όμιλο.", "Άγνωστος ΑΜ 1234."]);
  assert.match(report[2].problems[0], /ΑΜ/);
  const twice = importTeacherLists([{ fileName: "a.xlsx", rows: ok }, { fileName: "a2.xlsx", rows: ok }], { clubs, students });
  assert.match(twice[1].problems.at(-1), /υπάρχει και στο αρχείο «a\.xlsx»/);
  assert.ok(tooMany);
});

test("per-club file: «ΟΧΙ» excludes, «Κλειστή λίστα: ΝΑΙ» closes the list; the saved list comes back pre-marked", () => {
  const rows = teacherListRows(clubs[0], students, ["9003"], { blocked: ["9002"] });
  assert.deepEqual(rows[4], ["Κλειστή λίστα", "ΟΧΙ"]);
  assert.deepEqual(rows.slice(6).map((r) => r[0]), ["", "Χ", "ΟΧΙ"]);
  assert.deepEqual(readTeacherListRows(rows, { clubs, students }).lists, [{ code: 102, ams: ["9003"], blocked: ["9002"], closed: false }]);
  rows[6][0] = "όχι";
  assert.deepEqual(readTeacherListRows(rows, { clubs, students }).lists[0].blocked, ["9001", "9002"], "accents and case ignored");

  const closed = teacherListRows(clubs[0], students, ["9003"], { closed: true, blocked: ["9002"] });
  assert.deepEqual(closed[4], ["Κλειστή λίστα", "ΝΑΙ"]);
  assert.deepEqual(closed.slice(6).map((r) => r[0]), ["", "Χ", ""], "closed: no ΟΧΙ needed");
  assert.deepEqual(readTeacherListRows(closed, { clubs, students }).lists, [{ code: 102, ams: ["9003"], blocked: [], closed: true }]);
});

test("older per-club files (no «Κλειστή λίστα» row) leave the closed setting alone", () => {
  const rows = teacherListRows(clubs[0], students);
  rows[4] = [];
  rows[7][0] = "Χ";
  assert.deepEqual(readTeacherListRows(rows, { clubs, students }).lists, [{ code: 102, ams: ["9003"], blocked: [] }]);
});

test("one table for many clubs: «ΟΧΙ» rows are exclusions; a student both listed and excluded is a problem", () => {
  const rows = [["Κωδικός ομίλου", "ΑΜ", "Επιλογή"], [102, 9001, "Χ"], [102, 9002, "ΟΧΙ"], [105, 9004, "ΟΧΙ"]];
  assert.deepEqual(readTeacherListRows(rows, { clubs, students }).lists, [{ code: 102, ams: ["9001"], blocked: ["9002"] }, { code: 105, ams: [], blocked: ["9004"] }]);
  const both = importTeacherLists([{ fileName: "a.xlsx", rows: [["Κωδικός ομίλου", "ΑΜ", "Επιλογή"], [102, 9001, "Χ"], [102, 9001, "ΟΧΙ"]] }], { clubs, students });
  assert.match(both[0].problems.join(" "), /και στη λίστα και στους αποκλεισμένους/);
});
