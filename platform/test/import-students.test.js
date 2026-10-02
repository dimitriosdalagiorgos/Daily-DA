import { test } from "node:test";
import assert from "node:assert/strict";
import { importStudents } from "../src/import/index.js";

// Fictional students; same layout as the myschool «Κατάλογος Μαθητών».
const HEADER = ["Τάξη", "Αριθμός Μητρώου", "Επώνυμο", "Όνομα", "Όνομα πατέρα", "Όνομα μητέρας"];
const rows = (...body) => [HEADER, ...body];

test("reads the myschool layout; ΑΜ stored as number becomes a string", () => {
  const r = importStudents(rows(
    ["Α", 9001, "ΠΑΠΑΔΟΠΟΥΛΟΣ", "ΝΙΚΟΛΑΟΣ", "ΓΕΩΡΓΙΟΣ", "ΜΑΡΙΑ"],
    ["Γ", 9002.0, "ΧΡΙΣΤΟΦΟΡΙΔΗΣ", "ΑΝΝΑ ΠΑΝΩΡΙΑ", "ΙΩΑΝΝΗΣ", "ΕΛΕΝΗ-ΜΑΡΙΑ"],
    ["", "", "", "", "", ""],
    ["Β", "9003", "ΔΗΜΟΥ", "ΣΟΦΙΑ", "ΠΕΤΡΟΣ", "ΒΑΪΑ"],
  ));
  assert.deepEqual(r.problems, []);
  assert.deepEqual(r.summary, { count: 3, byGrade: { Α: 1, Β: 1, Γ: 1 } });
  assert.deepEqual(r.students[1], { am: "9002", grade: "Γ", surname: "ΧΡΙΣΤΟΦΟΡΙΔΗΣ", name: "ΑΝΝΑ ΠΑΝΩΡΙΑ", father: "ΙΩΑΝΝΗΣ", mother: "ΕΛΕΝΗ-ΜΑΡΙΑ" });
});

test("finds the header below title rows and in any column order", () => {
  const r = importStudents([
    ["ΚΑΤΑΛΟΓΟΣ ΜΑΘΗΤΩΝ"],
    [],
    ["Επώνυμο", "Όνομα", "ΑΜ", "Τάξη", "Πατρώνυμο", "Μητρώνυμο"],
    ["ΔΗΜΟΥ", "ΣΟΦΙΑ", 9003, "Β΄", "ΠΕΤΡΟΣ", "ΒΑΙΑ"],
  ]);
  assert.deepEqual(r.problems, []);
  assert.equal(r.students[0].grade, "Β");
  assert.equal(r.students[0].am, "9003");
});

test("missing columns: one clear error", () => {
  const r = importStudents([["Επώνυμο", "Όνομα", "Τάξη"], ["ΔΗΜΟΥ", "ΣΟΦΙΑ", "Α"]]);
  assert.equal(r.problems.length, 1);
  assert.match(r.problems[0].message, /Αριθμός Μητρώου, Όνομα πατέρα, Όνομα μητέρας/);
});

test("row errors carry the Excel row number; bad rows are left out", () => {
  const r = importStudents(rows(
    ["Α", 9001, "ΠΑΠΑΔΟΠΟΥΛΟΣ", "ΝΙΚΟΛΑΟΣ", "ΓΕΩΡΓΙΟΣ", "ΜΑΡΙΑ"],
    ["Δ", 9004, "ΑΛΦΑ", "ΒΗΤΑ", "ΓΑΜΜΑ", "ΔΕΛΤΑ"],
    ["Α", "9A05", "ΑΛΦΑ", "ΒΗΤΑ", "ΓΑΜΜΑ", "ΔΕΛΤΑ"],
    ["Α", 9001, "ΑΛΛΟΣ", "ΜΑΘΗΤΗΣ", "ΓΑΜΜΑ", "ΔΕΛΤΑ"],
    ["Α", 9006, "", "ΒΗΤΑ", "ΓΑΜΜΑ", "ΔΕΛΤΑ"],
  ));
  assert.deepEqual(r.students.map((s) => s.am), ["9001"]);
  assert.deepEqual(r.problems.map((p) => [p.level, p.row, p.field]), [
    ["error", 3, "grade"],
    ["error", 4, "am"],
    ["error", 6, "surname"],
    ["error", 5, "am"],
  ].sort((a, b) => a[1] - b[1]));
  assert.match(r.problems.find((p) => p.row === 5).message, /υπάρχει ήδη στη γραμμή 2/);
});

test("missing parent names are a warning, not an error", () => {
  const r = importStudents(rows(["Β", 9007, "ΝΤΟΚΑ", "ΑΡΜΠΕΡΑ", "", "ΛΙΝΤΙΑ"]));
  assert.equal(r.students.length, 1);
  assert.deepEqual(r.problems.map((p) => [p.level, p.field]), [["warning", "father"]]);
  assert.match(r.problems[0].message, /μόνο ΑΜ \+ επώνυμο/);
});

test("an empty list is an error", () => {
  assert.match(importStudents(rows()).problems[0].message, /δεν περιέχει μαθητές/);
});
