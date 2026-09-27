import { test } from "node:test";
import assert from "node:assert/strict";
import { clubsToRank, validateClubs, validateSubmission, validateTeacherList } from "../src/algorithm/index.js";

const clubs = [
  { code: 100, name: "Αντιγόνη", days: ["mon", "thu"], grades: ["Β", "Γ"], capacity: 20 },
  { code: 101, name: "Ρομποτική", days: ["mon"], grades: ["Α", "Β"], capacity: 15 },
  { code: 102, name: "Άλγεβρα", days: ["thu"], grades: ["Β"], capacity: 15 },
  { code: 103, name: "Χορωδία", days: ["tue", "wed", "fri"], grades: ["Α", "Β", "Γ"], capacity: 30 },
];

test("clubs to rank: by grade, and multi-day clubs only on their first day", () => {
  assert.deepEqual(clubsToRank(clubs, "Β", "mon"), ["100", "101"]);
  assert.deepEqual(clubsToRank(clubs, "Α", "mon"), ["101"]);
  assert.deepEqual(clubsToRank(clubs, "Β", "thu"), ["102"]);
  assert.deepEqual(clubsToRank(clubs, "Β", "wed"), []);
  assert.deepEqual(clubsToRank(clubs, "Γ", "tue"), ["103"]);
});

test("a complete submission passes", () => {
  const prefs = { mon: [101, 100], tue: [103], thu: [102] };
  assert.deepEqual(validateSubmission(clubs, { grade: "Β" }, prefs), []);
});

test("missing, extra and repeated clubs are reported per day", () => {
  const problems = validateSubmission(clubs, { grade: "Β" }, { mon: [101, 101], tue: [103], thu: [102, 100] });
  assert.equal(problems.length, 3);
  assert.match(problems[0], /Δευτέρα: κάποιος όμιλος εμφανίζεται δύο φορές/);
  assert.match(problems[1], /Δευτέρα: λείπουν.*100/);
  assert.match(problems[2], /Πέμπτη: όμιλοι που δεν επιτρέπονται \(100\)/);
});

test("club rules from the template", () => {
  assert.deepEqual(validateClubs(clubs), []);
  const bad = validateClubs([
    { code: 1, name: "", days: [], grades: [], capacity: 0 },
    { code: 1, name: "Χ", days: ["thu", "mon"], grades: ["Δ"], capacity: 5 },
    { code: 2, name: "Υ", days: ["mon", "tue", "wed", "thu"], grades: ["Α"], capacity: 5 },
  ]);
  for (const pattern of [/όνομα/, /λείπει η ημέρα/, /λείπουν οι τάξεις/, /χωρητικότητα/, /υπάρχει ήδη/, /σειρά της εβδομάδας/, /άγνωστη τάξη/, /έως 3 ημέρες/]) {
    assert.ok(bad.some((p) => pattern.test(p)), `expected a problem matching ${pattern}`);
  }
});

test("teacher list: capacity, grade and unknown students", () => {
  const students = new Map([["1", { grade: "Β" }], ["2", { grade: "Α" }]]);
  const small = { ...clubs[0], capacity: 2 };
  assert.deepEqual(validateTeacherList(small, ["1"], students), []);
  const problems = validateTeacherList(small, ["1", "2", "9"], students);
  assert.ok(problems.some((p) => /χωρητικότητα/.test(p)));
  assert.ok(problems.some((p) => /ΑΜ 2 είναι στην τάξη Α/.test(p)));
  assert.ok(problems.some((p) => /Άγνωστος ΑΜ 9/.test(p)));
});
