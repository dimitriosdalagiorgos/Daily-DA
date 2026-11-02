import { test } from "node:test";
import assert from "node:assert/strict";
import { buildRPackage } from "../src/export/rPackage.js";
import { allocateWeek } from "../src/algorithm/allocate.js";
import { compareLotteries } from "../scripts/lottery-compare.mjs";

const DAYS = ["mon", "tue", "wed", "thu", "fri"];
// Two students, one 1-seat club and one big club every day: the lottery
// decides who gets the popular club each day.
const clubs = DAYS.flatMap((d, i) => [
  { code: String(10 + i), name: `Δημοφιλής ${d}`, days: [d], grades: ["Α"], capacity: 1 },
  { code: String(20 + i), name: `Άλλος ${d}`, days: [d], grades: ["Α"], capacity: 5 },
]);
const students = [{ am: "1", grade: "Α" }, { am: "2", grade: "Α" }];
const preferences = Object.fromEntries(students.map((s) => [s.am, Object.fromEntries(DAYS.map((d, i) => [d, [String(10 + i), String(20 + i)]]))]));

test("allocateWeek: lotteryByDay changes the winner day by day", () => {
  const lottery = new Map([["1", 1], ["2", 2]]);
  const lotteryByDay = Object.fromEntries(DAYS.map((d, i) => [d, new Map([["1", i % 2 ? 2 : 1], ["2", i % 2 ? 1 : 2]])]));
  const single = allocateWeek({ students, clubs, preferences, lottery, audit: false });
  const daily = allocateWeek({ students, clubs, preferences, lottery, lotteryByDay, audit: false });
  assert.deepEqual(DAYS.map((d) => single.byStudent["1"][d]), ["10", "11", "12", "13", "14"]);
  assert.deepEqual(DAYS.map((d) => daily.byStudent["1"][d]), ["10", "21", "12", "23", "14"]);
  assert.throws(() => allocateWeek({ students, clubs, preferences, lottery, lotteryByDay: { mon: lottery }, audit: false }), /κάθε ημέρα/);
});

test("lottery-compare script: one number gives all-or-nothing weeks, aggregates only", () => {
  const files = buildRPackage({ students, clubs, preferences, lottery: new Map([["1", 1], ["2", 2]]), seed: "t" });
  const { actual, weekly, daily, text } = compareLotteries(files, { runs: 30 });
  assert.equal(actual.decisions, 10);
  assert.equal(actual.firstEveryDay, 1);
  assert.equal(actual.noFirstAtAll, 1);
  // With one number per week, every run is all-or-nothing
  assert.ok(weekly.every((r) => r.firstEveryDay === 1 && r.noFirstAtAll === 1));
  // With a number per day, that is rare
  assert.ok(daily.filter((r) => r.noFirstAtAll === 1).length < 10);
  assert.ok(!/ΑΜ \d/.test(text));
});

test("lottery-compare script: alternating rule and the JSON report hold aggregates only", () => {
  const files = buildRPackage({ students, clubs, preferences, lottery: new Map([["1", 1], ["2", 2]]), seed: "t" });
  const { alternating, report } = compareLotteries(files, { runs: 10 });
  // Two students, order reversed on Tue and Thu: each gets the popular club on some days
  assert.ok(alternating.every((r) => r.firstEveryDay === 0 && r.noFirstAtAll === 0));
  assert.deepEqual(Object.keys(report.rules), ["weekly", "daily", "alternating"]);
  assert.equal(report.rules.weekly.missHistogram.length, 6);
  assert.equal(report.demand.find((d) => d.code === "10").firstChoice, 2);
  assert.equal(report.demand.find((d) => d.code === "10").rejected, 1);
  const json = JSON.stringify(report);
  assert.ok(!json.includes('"am"') && !/"1"\s*:\s*\{/.test(JSON.stringify(report.rules.weekly.exAnte)));
});
