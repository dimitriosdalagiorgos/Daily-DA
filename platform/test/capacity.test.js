import { test } from "node:test";
import assert from "node:assert/strict";
import { capacityCeiling, guaranteedSeats, suggestCapacityIncreases } from "../src/planning/capacity.js";
import { allocateWeek } from "../src/algorithm/allocate.js";
import { seededRandom } from "../src/algorithm/lottery.js";
import { randomInstance } from "./random-instance.js";

const club = (code, grades, capacity, days = ["mon"]) => ({ code, name: `Όμιλος ${code}`, days, grades, capacity });
const fileOf = (clubs) => Object.fromEntries(clubs.map((c) => [c.code, c.capacity]));

test("ceiling: 22, or 30 when the clubs file gave more than 22", () => {
  assert.deepEqual([10, 22, 23, 30].map(capacityCeiling), [22, 22, 30, 30]);
});

test("guaranteed seats: shared seats count only beyond what other grades could fill", () => {
  const students = [
    ...Array.from({ length: 10 }, (_, i) => ({ am: `a${i}`, grade: "Α" })),
    ...Array.from({ length: 6 }, (_, i) => ({ am: `b${i}`, grade: "Β" })),
  ];
  const clubs = [club(1, ["Α", "Β"], 12), club(2, ["Α"], 4)];
  const rows = guaranteedSeats(students, clubs);
  const a = rows.find((r) => r.grade === "Α");
  const b = rows.find((r) => r.grade === "Β");
  // Hall passes (16 seats, 16 students), yet Β is sure of only 12 − 10 = 2
  assert.deepEqual([a.own, a.shared, a.others, a.guaranteed, a.atRisk], [4, 12, 6, 10, 0]);
  assert.deepEqual([b.own, b.shared, b.others, b.guaranteed, b.atRisk], [0, 12, 10, 2, 4]);
  assert.equal(rows.length, 2, "only days with clubs for the grade");
});

// Shared club 1 (Α+Β, 1 seat), club 2 only for Α (1 seat). The Α student
// prefers the shared club and has the better number: the Β student is left
// out and club 2 stays empty, although both could be placed.
const sink = () => {
  const clubs = [club(1, ["Α", "Β"], 1), club(2, ["Α"], 1)];
  return {
    students: [{ am: "a", grade: "Α" }, { am: "b", grade: "Β" }],
    clubs,
    preferences: { a: { mon: ["1", "2"] }, b: { mon: ["1"] } },
    lottery: new Map([["a", 0.1], ["b", 0.9]]),
    mandatoryGrades: ["Α", "Β"],
  };
};

test("suggestion: the shared club gets the seat the Β student needs", () => {
  const input = sink();
  const r = suggestCapacityIncreases(input, fileOf(input.clubs));
  assert.deepEqual(r.before, [{ am: "b", day: "mon" }]);
  assert.deepEqual(r.after, []);
  assert.deepEqual(r.increases, [{ code: "1", from: 1, to: 2, ceiling: 22 }]);
});

test("suggestion: a club of one grade wins a tie, so shared clubs do not swell", () => {
  // Α students a0, a1 both prefer club 3 (only Α, 1 seat) to the shared club
  // 1; a0 gets 3, a1 takes the shared seat and pushes b out. A seat in club 3
  // draws a1 away and frees the shared seat for b: same cost as a seat in 1.
  const clubs = [club(1, ["Α", "Β"], 1), club(3, ["Α"], 1)];
  const input = {
    students: [{ am: "a0", grade: "Α" }, { am: "a1", grade: "Α" }, { am: "b", grade: "Β" }],
    clubs,
    preferences: { a0: { mon: ["3", "1"] }, a1: { mon: ["3", "1"] }, b: { mon: ["1"] } },
    lottery: new Map([["a0", 0.1], ["a1", 0.2], ["b", 0.9]]),
    mandatoryGrades: ["Α", "Β"],
  };
  const r = suggestCapacityIncreases(input, fileOf(clubs));
  assert.deepEqual(r.increases, [{ code: "3", from: 1, to: 2, ceiling: 22 }]);
  assert.deepEqual(r.after, []);
});

test("suggestion: within the ceiling; a «σύνολο» may grow to 30", () => {
  const input = sink();
  input.clubs[0].capacity = 22;
  input.students.push(...Array.from({ length: 22 }, (_, i) => ({ am: `x${i}`, grade: "Α" })));
  for (let i = 0; i < 22; i++) {
    input.preferences[`x${i}`] = { mon: ["1", "2"] };
    input.lottery.set(`x${i}`, i / 100);
  }
  // Club 1 is full at its ceiling (22 in the file): nothing to suggest
  let r = suggestCapacityIncreases(input, fileOf(input.clubs));
  assert.deepEqual(r.increases, []);
  assert.ok(r.after.some((u) => u.am === "b"));
  // The same club with 23 seats in the file counts as a «σύνολο»
  r = suggestCapacityIncreases(input, { 1: 23, 2: 1 });
  assert.ok(r.increases.every((i) => i.to <= 30));
  assert.ok(!r.after.some((u) => u.am === "b"));
});

test("suggestion: only students of mandatory grades who declared", () => {
  const input = sink();
  input.mandatoryGrades = ["Α"];
  const r = suggestCapacityIncreases(input, fileOf(input.clubs));
  assert.deepEqual([r.before, r.increases], [[], []]);
});

test("suggestion on random schools: what it promises is what the allocation gives", () => {
  for (let seed = 1; seed <= 40; seed++) {
    const input = randomInstance(seededRandom(`capacity-${seed}`));
    const r = suggestCapacityIncreases(input, fileOf(input.clubs));
    assert.ok(r.after.length <= r.before.length);
    const raised = new Map(r.increases.map((i) => [String(i.code), i.to]));
    for (const i of r.increases) assert.ok(i.to > i.from && i.to <= i.ceiling);
    const result = allocateWeek({ ...input, clubs: input.clubs.map((c) => ({ ...c, capacity: raised.get(String(c.code)) ?? c.capacity })) });
    const grade = new Map(input.students.map((s) => [String(s.am), s.grade]));
    const left = Object.entries(result.days).flatMap(([day, d]) => d.unassigned
      .filter((u) => u.reason === "all_rejected" && input.mandatoryGrades.includes(grade.get(String(u.am))))
      .map((u) => ({ am: String(u.am), day })));
    assert.deepEqual(left.sort((a, b) => a.am.localeCompare(b.am) || a.day.localeCompare(b.day)),
      [...r.after].sort((a, b) => a.am.localeCompare(b.am) || a.day.localeCompare(b.day)), `seed ${seed}`);
  }
});

test("suggestion: out of time, it stops with what it found", () => {
  const input = sink();
  let t = 0;
  const r = suggestCapacityIncreases(input, fileOf(input.clubs), { timeLimitMs: 0, now: () => (t += 1) });
  assert.equal(r.complete, false);
  assert.deepEqual(r.increases, []);
  assert.deepEqual(r.after, r.before);
});
