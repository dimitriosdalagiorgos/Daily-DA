import { test } from "node:test";
import assert from "node:assert/strict";
import { clubMembers, rosterFileName, rosterRows, weekHeader, weekRows } from "../src/export/rosters.js";

const students = [
  { am: "3", grade: "Β", surname: "ΑΛΦΑ", name: "ΓΙΩΡΓΟΣ", father: "ΝΙΚΟΣ", mother: "ΜΑΡΙΑ" },
  { am: "1", grade: "Α", surname: "ΒΗΤΑ", name: "ΕΛΕΝΗ", father: "ΠΕΤΡΟΣ", mother: "ΑΝΝΑ" },
  { am: "2", grade: "Α", surname: "ΑΛΦΑ", name: "ΕΛΕΝΗ", father: "ΚΩΣΤΑΣ", mother: "ΑΝΝΑ" },
  { am: "4", grade: "Α", surname: "ΑΛΦΑ", name: "ΕΛΕΝΗ", father: "ΑΝΤΩΝΗΣ", mother: "ΖΩΗ" },
];
const clubs = [
  { code: 10, name: "Ρομποτική", days: ["mon"], capacity: 5 },
  { code: 20, name: "Αντιγόνη", days: ["tue", "thu"], capacity: 3 },
];
const results = {
  byStudent: { 1: { mon: "10", tue: "20", thu: "20" }, 2: { mon: "10" }, 3: { tue: "20", thu: "20" }, 4: {} },
  gaps: { 2: { tue: "no_preferences", thu: "all_rejected" }, 4: { mon: "all_rejected" } },
};

test("the week per student: details, then the club of each day; sorted by grade, surname, name, father", () => {
  assert.deepEqual(weekHeader(students).slice(0, 6), ["ΑΜ", "Τάξη", "Επώνυμο", "Όνομα", "Πατρώνυμο", "Μητρώνυμο"]);
  const rows = weekRows(students, results, clubs);
  assert.deepEqual(rows.map((r) => r[0]), [4, 2, 1, 3], "Α before Β; same name → father's name");
  assert.deepEqual(rows[2], [1, "Α", "ΒΗΤΑ", "ΕΛΕΝΗ", "ΠΕΤΡΟΣ", "ΑΝΝΑ", "Ρομποτική", "Αντιγόνη", "", "Αντιγόνη", ""]);
  assert.deepEqual(rows[1].slice(6), ["Ρομποτική", "— χωρίς προτιμήσεις", "", "— δεν χώρεσε", ""]);
});

test("a club's roster: its members on its first day, alphabetically, with the club on top", () => {
  const members = clubMembers(clubs[1], students, results);
  assert.deepEqual(members.map((s) => s.am), ["3", "1"]);
  const rows = rosterRows(clubs[1], members, ["ΜΑΡΙΑ ΘΕΑΤΡΙΚΟΥ"]);
  assert.deepEqual(rows.slice(0, 4), [["Όμιλος", "Αντιγόνη (20)"], ["Ημέρες", "Τρίτη + Πέμπτη"], ["Εκπαιδευτικοί", "ΜΑΡΙΑ ΘΕΑΤΡΙΚΟΥ"], ["Μαθητές", "2 (θέσεις 3)"]]);
  assert.deepEqual(rows.slice(5), [["Α/Α", "ΑΜ", "Επώνυμο", "Όνομα", "Πατρώνυμο", "Τάξη"], [1, 3, "ΑΛΦΑ", "ΓΙΩΡΓΟΣ", "ΝΙΚΟΣ", "Β"], [2, 1, "ΒΗΤΑ", "ΕΛΕΝΗ", "ΠΕΤΡΟΣ", "Α"]]);
  assert.equal(rosterFileName({ code: 102, name: "Θεατρική παράσταση «Αντιγόνη»" }), "parousiologio_102_Θεατρική_παράσταση_Αντιγόνη.xlsx");
});

test("with the sections file: «Τμήμα» after «Τάξη» in the week, last in the rosters", () => {
  const withSections = students.map((s) => ({ ...s, section: { 1: "Α1", 3: "Β2" }[s.am] ?? "" }));
  assert.deepEqual(weekHeader(withSections).slice(0, 4), ["ΑΜ", "Τάξη", "Τμήμα", "Επώνυμο"]);
  assert.deepEqual(weekRows(withSections, results, clubs)[2].slice(0, 4), [1, "Α", "Α1", "ΒΗΤΑ"]);
  const members = clubMembers(clubs[1], withSections, results);
  const rows = rosterRows(clubs[1], members, [], true);
  assert.deepEqual(rows.slice(4), [["Α/Α", "ΑΜ", "Επώνυμο", "Όνομα", "Πατρώνυμο", "Τάξη", "Τμήμα"], [1, 3, "ΑΛΦΑ", "ΓΙΩΡΓΟΣ", "ΝΙΚΟΣ", "Β", "Β2"], [2, 1, "ΒΗΤΑ", "ΕΛΕΝΗ", "ΠΕΤΡΟΣ", "Α", "Α1"]]);
  // A club whose members have no section still gets the column, like the other rosters
  assert.equal(rosterRows(clubs[0], [], [], true)[4].at(-1), "Τμήμα");
});
