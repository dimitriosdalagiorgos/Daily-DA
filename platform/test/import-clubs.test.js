import { test } from "node:test";
import assert from "node:assert/strict";
import { checkReadiness, importClubs } from "../src/import/index.js";

const CLUBS_HEADER = ["Κωδικός", "Όνομα ομίλου", "Ημέρα 1", "Ημέρα 2", "Ημέρα 3", "Τάξεις", "Χωρητικότητα", "Ώρες", "Περιγραφή"];
const TEACHERS_HEADER = ["Κωδικός ομίλου", "Επώνυμο", "Όνομα", "Email", "Όμιλος (έλεγχος)"];
// Formula columns come back as numbers / "" / ";;" and must be ignored.
const clubRow = (code, name, d1, d2, d3, grades, cap, desc = "") => [code, name, d1, d2, d3, grades, cap, "", desc];
const EMPTY_CLUB = clubRow("", "", "", "", "", "", "", "");
const EMPTY_TEACHER = ["", "", "", "", ""];

const good = {
  clubs: [
    CLUBS_HEADER,
    clubRow(101, "Ρομποτική", "Δευτέρα", "", "", "Α", 15, "Κατασκευή ρομπότ"),
    clubRow(102, "Αντιγόνη", "Δευτέρα", "Πέμπτη", "", "Β-Γ", 20),
    clubRow(103, "Χορωδία", "τρίτη", "Τετάρτη", "Παρασκευη", "Α-Β-Γ", 30),
    EMPTY_CLUB, EMPTY_CLUB,
  ],
  teachers: [
    TEACHERS_HEADER,
    [102, "ΠΑΠΑΔΟΠΟΥΛΟΥ", "ΕΛΕΝΗ", "EPapadopoulou@sch.gr", "Αντιγόνη"],
    [102, "ΓΕΩΡΓΙΟΥ", "ΝΙΚΟΛΑΟΣ", "ngeorgiou@sch.gr", "Αντιγόνη"],
    [101, "ΓΕΩΡΓΙΟΥ", "ΝΙΚΟΛΑΟΣ", "ngeorgiou@sch.gr", "Ρομποτική"],
    [103, "ΔΗΜΟΥ", "ΣΟΦΙΑ", "sdimou@sch.gr", "Χορωδία"],
    EMPTY_TEACHER,
  ],
};

test("reads the template: days, grades, multi-day clubs, several teachers per club", () => {
  const r = importClubs(good);
  assert.deepEqual(r.problems, []);
  assert.deepEqual(r.summary, { clubs: 3, multiDay: 2, teachers: 3 });
  assert.deepEqual(r.clubs[1], { code: 102, name: "Αντιγόνη", days: ["mon", "thu"], grades: ["Β", "Γ"], capacity: 20, description: "" });
  assert.deepEqual(r.clubs[2].days, ["tue", "wed", "fri"]);
  assert.deepEqual(r.teachers.find((t) => t.email === "ngeorgiou@sch.gr").clubs, [102, 101]);
  assert.equal(r.teachers[0].email, "epapadopoulou@sch.gr", "email stored in lower case");
});

test("club rows: each mistake reported once, with sheet and row", () => {
  const r = importClubs({
    clubs: [
      CLUBS_HEADER,
      clubRow(101, "Ρομποτική", "Δευτέρα", "", "", "Α", 15),
      clubRow(101, "Διπλός κωδικός", "Τρίτη", "", "", "Α", 10),
      clubRow("Α1", "Κακός κωδικός", "Τρίτη", "", "", "Α", 10),
      clubRow(104, "Κενό στις ημέρες", "Δευτέρα", "", "Πέμπτη", "Α", 10),
      clubRow(105, "Ανάποδες ημέρες", "Πέμπτη", "Δευτέρα", "", "Α", 10),
      clubRow(106, "Άγνωστη ημέρα", "Σάββατο", "", "", "Α", 10),
      clubRow(107, "Κακές τάξεις", "Τρίτη", "", "", "Α-Δ", 10),
      clubRow(108, "Χωρίς τάξεις", "Τρίτη", "", "", "", 10),
      clubRow(109, "Χωρητικότητα", "Τρίτη", "", "", "Α", "δέκα"),
      clubRow(110, "", "Τρίτη", "", "", "Α", 10),
      clubRow(111, "Χωρίς ημέρα", "", "", "", "Α", 10),
    ],
    teachers: [TEACHERS_HEADER],
  });
  const got = r.problems.filter((p) => p.level === "error").map((p) => [p.row, p.field]);
  assert.deepEqual(got, [
    [3, "code"], [4, "code"], [5, "days"], [6, "days"], [7, "day1"], [8, "grades"], [9, "grades"],
    [10, "capacity"], [11, "name"], [12, "days"],
  ]);
  assert.ok(r.problems.every((p) => p.sheet === "Όμιλοι" || p.sheet === "Εκπαιδευτικοί"));
  assert.deepEqual(r.clubs.map((c) => c.code), [101]);
});

test("teacher rows: unknown club, bad email, duplicates, clubs without teacher", () => {
  const r = importClubs({
    clubs: good.clubs,
    teachers: [
      TEACHERS_HEADER,
      [102, "ΠΑΠΑΔΟΠΟΥΛΟΥ", "ΕΛΕΝΗ", "epapadopoulou@sch.gr", ""],
      [102, "ΠΑΠΑΔΟΠΟΥΛΟΥ", "ΕΛΕΝΗ", "epapadopoulou@sch.gr", ""],
      [999, "ΑΛΦΑ", "ΒΗΤΑ", "alfa@sch.gr", ";;"],
      [101, "ΓΑΜΜΑ", "ΔΕΛΤΑ", "gamma@gmail.com", ""],
      [103, "", "ΔΕΛΤΑ", "delta@sch.gr", ""],
    ],
  });
  const got = r.problems.map((p) => [p.level, p.row ?? null, p.field ?? null]);
  assert.deepEqual(got, [
    ["warning", 3, null],
    ["error", 4, "code"],
    ["error", 5, "email"],
    ["error", 6, "surname"],
    ["warning", null, null],
    ["warning", null, null],
  ]);
  assert.match(r.problems.at(-2).message, /101 «Ρομποτική» δεν έχει εκπαιδευτικό/);
  assert.match(r.problems.at(-1).message, /103 «Χορωδία» δεν έχει εκπαιδευτικό/);
});

test("wrong file: missing columns reported per sheet", () => {
  const r = importClubs({ clubs: [["Όνομα", "Ημέρα"]], teachers: [] });
  assert.equal(r.problems.length, 1);
  assert.match(r.problems[0].message, /«Όμιλοι».*«Κωδικός»/);
});

test("readiness: shared clubs are not counted twice (groups of mandatory grades)", () => {
  // Α 140, Β 130, Γ 50. Monday: Α-only 60, Β-only 50, shared Α+Β 120.
  const students = [
    ...Array.from({ length: 140 }, (_, i) => ({ am: `a${i}`, grade: "Α" })),
    ...Array.from({ length: 130 }, (_, i) => ({ am: `b${i}`, grade: "Β" })),
    ...Array.from({ length: 50 }, (_, i) => ({ am: `c${i}`, grade: "Γ" })),
  ];
  const clubs = [
    { code: 1, days: ["mon"], grades: ["Α"], capacity: 60 },
    { code: 2, days: ["mon"], grades: ["Β"], capacity: 50 },
    { code: 3, days: ["mon"], grades: ["Α", "Β"], capacity: 120 },
    { code: 4, days: ["mon"], grades: ["Γ"], capacity: 20 },
  ];
  // Each grade alone passes (180 ≥ 140, 170 ≥ 130) but together 230 < 270.
  let p = checkReadiness(students, clubs, { mandatoryGrades: ["Α", "Β"] });
  assert.deepEqual(p.map((x) => [x.level, x.grades.join("+"), x.seats, x.students]), [
    ["error", "Α+Β", 230, 270],
    ["warning", "Γ", 20, 50],
  ]);
  assert.match(p[0].message, /Δευτέρα: οι όμιλοι για τις τάξεις Α \+ Β έχουν 230 θέσεις για 270 μαθητές \(υποχρεωτική ένταξη\) — λείπουν 40 θέσεις\./);

  // Last year: only Α mandatory → no error
  p = checkReadiness(students, clubs, { mandatoryGrades: ["Α"] });
  assert.deepEqual(p.filter((x) => x.level === "error"), []);

  // All three mandatory: Γ alone fails, and every group containing it
  p = checkReadiness(students, clubs, { mandatoryGrades: ["Α", "Β", "Γ"] });
  const failing = new Set(p.filter((x) => x.level === "error").map((x) => x.grades.join("+")));
  assert.ok(failing.has("Γ") && failing.has("Α+Β") && failing.has("Α+Β+Γ"));
  assert.ok(!failing.has("Α") && !failing.has("Β"));
});

test("readiness: a day without any club for a mandatory grade is an error; days without clubs are skipped", () => {
  const students = [{ am: "1", grade: "Α" }, { am: "2", grade: "Β" }];
  const clubs = [{ code: 1, days: ["mon"], grades: ["Α"], capacity: 5 }, { code: 2, days: ["tue"], grades: ["Α", "Β"], capacity: 5 }];
  const p = checkReadiness(students, clubs, { mandatoryGrades: ["Α", "Β"] });
  assert.deepEqual(p.map((x) => [x.level, x.day, x.message]), [["error", "mon", "Δευτέρα: δεν υπάρχει όμιλος για την Β τάξη (υποχρεωτική ένταξη)."]]);
  assert.deepEqual(checkReadiness(students, clubs, { mandatoryGrades: [] }).map((x) => x.level), ["warning"]);
});

test("readiness: a grade without any club all week is reported once", () => {
  const students = [{ am: "1", grade: "Α" }, { am: "2", grade: "Β" }, { am: "3", grade: "Β" }];
  const clubs = [{ code: 1, days: ["mon"], grades: ["Α"], capacity: 5 }, { code: 2, days: ["tue"], grades: ["Α"], capacity: 5 }];
  const p = checkReadiness(students, clubs, { mandatoryGrades: ["Α"] });
  assert.equal(p.length, 1);
  assert.equal(p[0].level, "warning");
  assert.match(p[0].message, /^Η Β τάξη \(2 μαθητές\) δεν έχει κανέναν όμιλο σε καμία ημέρα:/);
  // For a mandatory grade the same problem blocks the declarations.
  assert.deepEqual(checkReadiness(students, clubs, { mandatoryGrades: ["Α", "Β"] }).map((x) => x.level), ["error"]);
});

test("optional column «Παρεμφερείς»: same word (any case or accents) = similar clubs", () => {
  const r = importClubs({
    clubs: [
      [...CLUBS_HEADER, "Παρεμφερείς"],
      [201, "Αγγλικά", "Δευτέρα", "", "", "Α-Β", 20, "", "", "Αγγλικά"],
      [202, "Αγγλικά", "Πέμπτη", "", "", "Α-Β", 20, "", "", " ΑΓΓΛΙΚΆ "],
      [203, "Χορωδία", "Τρίτη", "", "", "Α", 20, "", "", ""],
    ],
    teachers: [TEACHERS_HEADER],
  });
  assert.deepEqual(r.problems.filter((p) => p.level === "error"), []);
  assert.deepEqual(r.clubs.map((c) => c.similar), ["ΑΓΓΛΙΚΑ", "ΑΓΓΛΙΚΑ", undefined]);
});

test("teachers' ΑΜ/ΑΦΜ: optional; digits only; leading zeros dropped; one per teacher", () => {
  const clubs = [["Κωδικός", "Όνομα ομίλου", "Ημέρα 1", "Ημέρα 2", "Ημέρα 3", "Τάξεις", "Χωρητικότητα"], [1, "Α", "Δευτέρα", "", "", "Α", 5], [2, "Β", "Τρίτη", "", "", "Α", 5]];
  const head = ["Κωδικός ομίλου", "Επώνυμο", "Όνομα", "Email", "ΑΦΜ"];
  let r = importClubs({ clubs, teachers: [head, [1, "Χ", "Α", "a@sch.gr", "012 345 678"], [2, "Χ", "Α", "a@sch.gr", ""], [2, "Ψ", "Β", "b@sch.gr", 612345]] });
  assert.deepEqual(r.problems, []);
  assert.deepEqual(r.teachers.map((t) => [t.email, t.personalId, t.clubs]), [["a@sch.gr", "12345678", [1, 2]], ["b@sch.gr", "612345", [2]]]);
  r = importClubs({ clubs, teachers: [head, [1, "Χ", "Α", "a@sch.gr", "12-34"], [2, "Ψ", "Β", "b@sch.gr", ""]] });
  assert.match(r.problems.map((p) => p.message).join(" "), /Μη έγκυρος ΑΜ ή ΑΦΜ «12-34»/);
  r = importClubs({ clubs, teachers: [head, [1, "Χ", "Α", "a@sch.gr", "612345"], [2, "Ψ", "Β", "b@sch.gr", "0612345"]] });
  assert.ok(r.problems.some((p) => p.level === "error" && /ίδιος ΑΜ\/ΑΦΜ/.test(p.message)));
  r = importClubs({ clubs, teachers: [head, [1, "Χ", "Α", "a@sch.gr", "612345"], [2, "Χ", "Α", "a@sch.gr", "700100"]] });
  assert.ok(r.problems.some((p) => p.level === "error" && /άλλον ΑΜ\/ΑΦΜ/.test(p.message)));
  // Without the column: no warning, nobody has one
  r = importClubs({ clubs, teachers: [head.slice(0, 4), [1, "Χ", "Α", "a@sch.gr"], [2, "Ψ", "Β", "b@sch.gr"]] });
  assert.deepEqual(r.problems, []);
  assert.ok(r.teachers.every((t) => !("personalId" in t)));
});
