import { test } from "node:test";
import assert from "node:assert/strict";
import { allocateWeek } from "../src/algorithm/index.js";
import { describeEvent, gapReasons, storiesByStudent } from "../src/export/story.js";

const clubs = [
  { code: 10, name: "Ρομποτική", days: ["mon"], grades: ["Α"], capacity: 1 },
  { code: 11, name: "Ζωγραφική", days: ["mon"], grades: ["Α"], capacity: 5 },
  { code: 20, name: "Αντιγόνη", days: ["mon", "thu"], grades: ["Α"], capacity: 5 },
  { code: 30, name: "Άλγεβρα", days: ["thu"], grades: ["Β"], capacity: 5 },
];
const students = [{ am: "1", grade: "Α" }, { am: "2", grade: "Α" }, { am: "3", grade: "Α" }];
const lottery = new Map([["1", 1], ["2", 2], ["3", 3]]);

test("each student's story, in words, per day", () => {
  const r = allocateWeek({ students, clubs, preferences: { 1: { mon: ["10", "11"] }, 2: { mon: ["10", "20"] } }, lottery });
  const nameOf = (c) => clubs.find((x) => String(x.code) === String(c)).name;
  const daysOf = (c) => clubs.find((x) => String(x.code) === String(c)).days;
  const logByDay = Object.fromEntries(Object.entries(r.days).map(([d, v]) => [d, v.log]));
  const stories = storiesByStudent(logByDay, nameOf, daysOf);
  assert.deepEqual(stories["2"].mon.map((s) => s.text), [
    "Αίτηση στον όμιλο «Ρομποτική» (1η επιλογή).",
    "Δεν χώρεσε στον όμιλο «Ρομποτική»: γέμισε με μαθητές υψηλότερης προτεραιότητας (λίστα εκπαιδευτικού, τάξη με υποχρεωτική ένταξη ή καλύτερος αριθμός κλήρωσης).",
    "Αίτηση στον όμιλο «Αντιγόνη» (2η επιλογή).",
    "Προσωρινή θέση στον όμιλο «Αντιγόνη».",
  ]);
  assert.deepEqual(stories["2"].thu.map((s) => s.text), ["Θέση στον όμιλο «Αντιγόνη», γιατί τοποθετήθηκε σε αυτόν τη Δευτέρα (όμιλος πολλών ημερών)."]);
  assert.match(describeEvent({ event: "CLUB_DROPPED", club: "20", reason: "day_conflict" }, nameOf), /ήδη όμιλο/);
});

test("why a student has no club on a day", () => {
  const r = allocateWeek({
    students, clubs: clubs.map((c) => (c.code === 11 ? { ...c, capacity: 0 } : c)),
    preferences: { 1: { mon: ["10"] }, 2: { mon: ["10", "11"] } }, lottery,
  });
  const gaps = gapReasons(r, students, clubs);
  assert.equal(gaps["1"]?.mon, undefined, "placed");
  assert.equal(gaps["2"].mon, "all_rejected");
  assert.equal(gaps["3"].mon, "no_preferences");
  assert.equal(gaps["1"].thu, "no_preferences", "Αντιγόνη runs Thursday for grade Α");
  assert.equal(gaps["1"].tue, "not_offered");
  assert.equal(gaps["1"].fri, "not_offered", "no club at all that day");
});
