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

test("readiness: days without clubs for a grade, and too few seats", () => {
  const { clubs } = importClubs(good);
  const students = [
    ...Array.from({ length: 20 }, (_, i) => ({ am: `a${i}`, grade: "Α" })),
    ...Array.from({ length: 12 }, (_, i) => ({ am: `b${i}`, grade: "Β" })),
    ...Array.from({ length: 12 }, (_, i) => ({ am: `c${i}`, grade: "Γ" })),
  ];
  const messages = checkReadiness(students, clubs).map((p) => p.message);
  assert.deepEqual(messages, [
    "Δευτέρα: οι όμιλοι για την Α τάξη έχουν 15 θέσεις για 20 μαθητές — κάποιοι θα μείνουν χωρίς όμιλο.",
    "Δευτέρα: συνολικά 35 θέσεις για 44 μαθητές (τάξεις Α, Β, Γ) — κάποιοι θα μείνουν χωρίς όμιλο.",
    "Τρίτη: συνολικά 30 θέσεις για 44 μαθητές (τάξεις Α, Β, Γ) — κάποιοι θα μείνουν χωρίς όμιλο.",
    "Τετάρτη: συνολικά 30 θέσεις για 44 μαθητές (τάξεις Α, Β, Γ) — κάποιοι θα μείνουν χωρίς όμιλο.",
    "Πέμπτη: δεν υπάρχει όμιλος για την Α τάξη.",
    // Β and Γ fit one at a time (20 ≥ 12) but share the same 20 seats.
    "Πέμπτη: συνολικά 20 θέσεις για 24 μαθητές (τάξεις Β, Γ) — κάποιοι θα μείνουν χωρίς όμιλο.",
    "Παρασκευή: συνολικά 30 θέσεις για 44 μαθητές (τάξεις Α, Β, Γ) — κάποιοι θα μείνουν χωρίς όμιλο.",
  ]);
});
