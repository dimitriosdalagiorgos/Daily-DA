import { test } from "node:test";
import assert from "node:assert/strict";
import { allocateWeek, seededRandom, DAYS } from "../src/algorithm/index.js";
import { buildVerification } from "../src/export/verification.js";
import { randomInstance } from "./random-instance.js";

const club = (code, days, capacity, grades = ["Α", "Β", "Γ"]) => ({ code, name: `Όμιλος ${code}`, days, grades, capacity });
const sheet = (v, name) => v.sheets.find((s) => s.name === name).rows;
const run = (input) => {
  const result = allocateWeek(input);
  const results = { seed: "x", byStudent: result.byStudent };
  const log = Object.fromEntries(DAYS.map((d) => [d, result.days[d].log]));
  return { results, log, v: buildVerification(input, results, log) };
};

test("verification: a full club shows its cut-off, and the student moves to the next choice", () => {
  // 1 seat in club 10; lottery: 1 → 1, 2 → 2. Student 2 falls to club 11.
  const input = {
    students: [{ am: "1", grade: "Α" }, { am: "2", grade: "Α" }],
    clubs: [club(10, ["mon"], 1), club(11, ["mon"], 5)],
    preferences: { 1: { mon: [10, 11] }, 2: { mon: [10, 11] } },
    lottery: new Map([["1", 1], ["2", 2]]),
  };
  const { v } = run(input);
  assert.equal(v.problems, 0);
  const cut = sheet(v, "Βάσεις").find((r) => r[1] === "10");
  assert.deepEqual(cut.slice(4, 13), [1, 2, 1, "Ναι", "1", "κλ. 1", "2", "κλ. 2", "✓"]);
  const two = sheet(v, "Μαθητές").find((r) => r[0] === "2");
  assert.equal(two[6], "Όμιλος 11 (2η επιλογή)");
  assert.equal(two[7], "✓");
  assert.equal(two[8], "✗ Όμιλος 10: γεμάτος (τελευταίος που μπήκε: κλ. 1 · μαθητής: κλ. 2)");
  assert.equal(two[9], "✔ Όμιλος 11: ΘΕΣΗ");
  assert.equal(two[10], "Γ1: αίτηση στον όμιλο «Όμιλος 10» (1η επιλογή) → δεν χώρεσε · Γ2: αίτηση στον όμιλο «Όμιλος 11» (2η επιλογή) → προσωρινή θέση · Τέλος: οριστική θέση στον όμιλο «Όμιλος 11»");
  const rounds = sheet(v, "Γύροι");
  assert.deepEqual(rounds[1], ["Δευτέρα", 1, "10", "Όμιλος 10", 1, "", "1 (κλ. 1, 1η επιλ.), 2 (κλ. 2, 1η επιλ.)", "1 (κλ. 1)", "2 (κλ. 2)"]);
  assert.deepEqual(sheet(v, "Κλήρωση").slice(1).map((r) => r[1]), ["1", "2"]);
});

test("verification: a ⭐ beats a better lottery number, and a multi-day seat is carried", () => {
  // Club 10 (Mon+Thu, 1 seat): student 2 (worse lottery) is on the teacher's list.
  const input = {
    students: [{ am: "1", grade: "Α" }, { am: "2", grade: "Α" }],
    clubs: [club(10, ["mon", "thu"], 1), club(11, ["mon"], 5), club(12, ["thu"], 5)],
    preferences: { 1: { mon: [10, 11], thu: [10, 12] }, 2: { mon: [10, 11], thu: [10, 12] } },
    teacherLists: { 10: ["2"] },
    lottery: new Map([["1", 1], ["2", 2]]),
  };
  const { v } = run(input);
  assert.equal(v.problems, 0);
  const rows = sheet(v, "Μαθητές");
  const twoThu = rows.find((r) => r[0] === "2" && r[5] === "Πέμπτη");
  assert.equal(twoThu[6], "Όμιλος 10 (από Δευτέρα)");
  assert.equal(twoThu[8], "✔ Όμιλος 10: θέση από Δευτέρα");
  const oneMon = rows.find((r) => r[0] === "1" && r[5] === "Δευτέρα");
  assert.match(oneMon[8], /^✗ Όμιλος 10: γεμάτος \(τελευταίος που μπήκε: ⭐ · κλ\. 2 · μαθητής: κλ\. 1\)$/);
  const oneThu = rows.find((r) => r[0] === "1" && r[5] === "Πέμπτη");
  assert.equal(oneThu[8], "Όμιλος 10: αφαιρέθηκε (κρίθηκε την 1η ημέρα του ομίλου)");
  assert.equal(oneThu[9], "✔ Όμιλος 12: ΘΕΣΗ");
});

test("verification: a tampered result is caught", () => {
  const input = {
    students: [{ am: "1", grade: "Α" }, { am: "2", grade: "Α" }],
    clubs: [club(10, ["mon"], 1), club(11, ["mon"], 5)],
    preferences: { 1: { mon: [10, 11] }, 2: { mon: [10, 11] } },
    lottery: new Map([["1", 1], ["2", 2]]),
  };
  const { results, log } = run(input);
  // Swap: the worse lottery number gets the seat
  results.byStudent["1"].mon = "11";
  results.byStudent["2"].mon = "10";
  const v = buildVerification(input, results, log);
  assert.ok(v.problems > 0);
  assert.equal(v.checks.find((c) => c.label.startsWith("Κανένας μαθητής δεν αδικήθηκε")).problems.length, 1);
  assert.match(sheet(v, "Μαθητές").find((r) => r[0] === "1")[7], /^⚠ «Όμιλος 10» είχε καλύτερη προτεραιότητα/);
});

test("random instances: every allocation passes all the checks", () => {
  const rng = seededRandom("verification");
  for (let i = 0; i < 150; i++) {
    const input = randomInstance(rng);
    // A few exclusions too
    input.exclusions = {};
    for (const c of input.clubs) if (rng() < 0.3) input.exclusions[c.code] = input.students.filter(() => rng() < 0.2).map((s) => s.am);
    const { v } = run(input);
    assert.equal(v.problems, 0, JSON.stringify(v.checks.filter((c) => c.problems.length)));
  }
});

test("verification: the path shows a seat lost in a later round and the move to the next choice", () => {
  // Round 1: 1 → 11 (loses to 3), 2 → 10 (held). Round 2: 1 → 10 and takes 2's seat.
  // Round 3: 2 → 12.
  const input = {
    students: ["1", "2", "3"].map((am) => ({ am, grade: "Α" })),
    clubs: [club(10, ["mon"], 1), club(11, ["mon"], 1), club(12, ["mon"], 5)],
    preferences: { 1: { mon: [11, 10] }, 2: { mon: [10, 12] }, 3: { mon: [11] } },
    lottery: new Map([["3", 1], ["1", 2], ["2", 3]]),
  };
  const { v } = run(input);
  assert.equal(v.problems, 0);
  const two = sheet(v, "Μαθητές").find((r) => r[0] === "2");
  assert.equal(two.at(-1), "Γ1: αίτηση στον όμιλο «Όμιλος 10» (1η επιλογή) → προσωρινή θέση · Γ2: έχασε τη θέση στον όμιλο «Όμιλος 10» από μαθητή καλύτερης προτεραιότητας · Γ3: αίτηση στον όμιλο «Όμιλος 12» (2η επιλογή) → προσωρινή θέση · Τέλος: οριστική θέση στον όμιλο «Όμιλος 12»");
  const r2 = sheet(v, "Γύροι").find((r) => r[1] === 2 && r[2] === "10");
  assert.deepEqual(r2.slice(5), ["2 (κλ. 3)", "1 (κλ. 2, 2η επιλ.)", "1 (κλ. 2)", "2 (κλ. 3)"]);
});

test("verification: a teacher's list longer than the capacity shows the ⭐ position, which decides", () => {
  // Capacity 1, two ⭐: the 1st on the list wins despite the worse lottery number
  const input = {
    students: [{ am: "1", grade: "Α" }, { am: "2", grade: "Α" }],
    clubs: [club(10, ["mon"], 1), club(11, ["mon"], 5)],
    preferences: { 1: { mon: [10, 11] }, 2: { mon: [10, 11] } },
    teacherLists: { 10: ["2", "1"] },
    lottery: new Map([["1", 1], ["2", 2]]),
  };
  const { v } = run(input);
  assert.equal(v.problems, 0);
  const one = sheet(v, "Μαθητές").find((r) => r[0] === "1");
  assert.equal(one[8], "✗ Όμιλος 10: γεμάτος (τελευταίος που μπήκε: ⭐ 1ος στη λίστα · κλ. 2 · μαθητής: ⭐ 2ος στη λίστα · κλ. 1)");
  // Within the capacity the order does not matter, and is not shown
  input.clubs[0].capacity = 2;
  const two = run(input).v;
  assert.match(sheet(two, "Γύροι")[1][7], /^1 \(⭐ · κλ\. 1\), 2 \(⭐ · κλ\. 2\)$/);
});
