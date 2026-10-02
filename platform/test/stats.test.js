import { test } from "node:test";
import assert from "node:assert/strict";
import { allocateWeek } from "../src/algorithm/index.js";
import { allocationStats } from "../src/export/stats.js";

const clubs = [
  { code: 10, name: "Ρομποτική", days: ["mon"], grades: ["Α", "Β"], capacity: 1 },
  { code: 11, name: "Ζωγραφική", days: ["mon"], grades: ["Α", "Β"], capacity: 2 },
  { code: 20, name: "Αντιγόνη", days: ["tue", "thu"], grades: ["Α", "Β"], capacity: 5 },
  { code: 30, name: "Χορωδία", days: ["thu"], grades: ["Α", "Β"], capacity: 5 },
];
const students = [{ am: "1", grade: "Α" }, { am: "2", grade: "Α" }, { am: "3", grade: "Β" }, { am: "4", grade: "Β" }];
const lottery = new Map([["1", 1], ["2", 2], ["3", 3], ["4", 4]]);

test("statistics: which choice, per day and grade; histogram; demand; teachers' lists", () => {
  const preferences = {
    1: { mon: ["10", "11"], tue: ["20"], thu: ["30", "20"] },
    2: { mon: ["10", "11"] },
    3: { mon: ["10", "11"] },
    4: { mon: ["10"], thu: ["20", "30"] },
  };
  const teacherLists = { 11: ["2", "4"] };
  const r = allocateWeek({ students, clubs, preferences, teacherLists, lottery });
  const logByDay = Object.fromEntries(Object.entries(r.days).map(([d, v]) => [d, v.log]));
  const st = allocationStats(r, { students, clubs, preferences, teacherLists }, logByDay);
  // Monday: 1 gets Ρομποτική (1st); 2 and 3 Ζωγραφική (2nd); 4 nothing
  assert.deepEqual(st.byDay.mon, { 1: 1, 2: 2, 3: 0, "4+": 0, none: 1 });
  // Tuesday: 1 gets Αντιγόνη (1st); Thursday: carried there, counted with Tuesday's rank (1st)
  assert.deepEqual(st.byDay.tue, { 1: 1, 2: 0, 3: 0, "4+": 0, none: 0 });
  // (1 put Αντιγόνη 2nd on Thursday, but it was decided on Tuesday as 1st;
  //  4 gets Χορωδία, 2nd on the list as typed)
  assert.deepEqual(st.byDay.thu, { 1: 1, 2: 1, 3: 0, "4+": 0, none: 0 });
  assert.deepEqual(st.byGrade.Α, { 1: 3, 2: 1, 3: 0, "4+": 0, none: 0 });
  assert.deepEqual(st.byGrade.Β, { 1: 0, 2: 2, 3: 0, "4+": 0, none: 1 });
  assert.deepEqual(st.histogram, [{ rank: 1, count: 3 }, { rank: 2, count: 3 }]);
  assert.equal(st.firstChoiceShare, 3 / 7);
  assert.equal(st.topThreeShare, 6 / 7);
  const robot = st.demand.find((c) => c.code === 10);
  assert.deepEqual([robot.first, robot.applicants, robot.placed, robot.missed, robot.pressure], [4, 4, 1, 3, 4]);
  const antigone = st.demand.find((c) => c.code === 20);
  assert.equal(antigone.day, "tue", "a multi-day club is measured on its first day");
  // Χορωδία on Thursday: 1 ranked it 1st but was already in Αντιγόνη — not turned away
  const choir = st.demand.find((c) => c.code === 30);
  assert.deepEqual([choir.first, choir.applicants, choir.placed, choir.missed], [1, 2, 1, 0]);
  assert.equal(allocationStats(r, { students, clubs, preferences, teacherLists }).demand[0].missed, null, "unknown without the log");
  assert.deepEqual(st.teacherLists, [{ code: 11, name: "Ζωγραφική", listed: 2, ranked: 1, placed: 1, elsewhere: 0, notRanked: 1 }]);
});
