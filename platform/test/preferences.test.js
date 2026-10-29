import { test } from "node:test";
import assert from "node:assert/strict";
import { preferenceSheets } from "../src/export/preferences.js";
import { placedRanks, rosterRows } from "../src/export/rosters.js";

const students = [
  { am: "2", grade: "Β", surname: "ΑΛΦΑ", name: "ΓΙΩΡΓΟΣ", father: "ΝΙΚΟΣ", mother: "ΜΑΡΙΑ", section: "Β1" },
  { am: "1", grade: "Α", surname: "ΒΗΤΑ", name: "ΕΛΕΝΗ", father: "ΠΕΤΡΟΣ", mother: "ΑΝΝΑ", section: "" },
  { am: "3", grade: "Α", surname: "ΓΑΜΜΑ", name: "ΝΙΚΟΣ", father: "", mother: "" },
];
const clubs = [
  { code: 20, name: "Φυσική", days: ["mon"], grades: ["Β"], capacity: 5 },
  { code: 10, name: "Ρομποτική", days: ["mon"], grades: ["Α", "Β"], capacity: 5 },
  { code: 30, name: "Αντιγόνη", days: ["mon", "thu"], grades: ["Α", "Β"], capacity: 3 },
  { code: 40, name: "Χορωδία", days: ["tue"], grades: ["Α"], capacity: 9 },
];
const preferences = { 2: { mon: ["20", "30", "10"] }, 1: { mon: ["10", "30"], tue: ["40"] } };

test("preferences: a sheet per day, a column per club ranked that day, the preference number in each cell", () => {
  const sheets = preferenceSheets(students, clubs, preferences);
  assert.deepEqual(sheets.map((s) => s.name), ["Δευτέρα", "Τρίτη"], "no clubs are ranked on the other days");
  const [mon] = sheets;
  assert.deepEqual(mon.rows[0], ["ΑΜ", "Τάξη", "Τμήμα", "Επώνυμο", "Όνομα", "Πατρώνυμο", "Μητρώνυμο", "Δήλωση",
    "Ρομποτική [10]", "Φυσική [20]", "Αντιγόνη (Δευτέρα + Πέμπτη) [30]"]);
  assert.deepEqual(mon.rows.slice(1), [
    [1, "Α", "", "ΒΗΤΑ", "ΕΛΕΝΗ", "ΠΕΤΡΟΣ", "ΑΝΝΑ", "✓", 1, "", 2],
    [3, "Α", "", "ΓΑΜΜΑ", "ΝΙΚΟΣ", "", "", "—", "", "", ""],
    [2, "Β", "Β1", "ΑΛΦΑ", "ΓΙΩΡΓΟΣ", "ΝΙΚΟΣ", "ΜΑΡΙΑ", "✓", 3, 1, 2],
  ]);
  assert.equal(mon.cols.length, mon.rows[0].length);
  assert.equal(mon.filter, true);
  assert.deepEqual(sheets[1].rows[1].slice(-2), ["✓", 1]);
});

test("preferences: without sections, no «Τμήμα» column", () => {
  const [mon] = preferenceSheets(students.map(({ section, ...s }) => s), clubs, preferences);
  assert.equal(mon.rows[0].includes("Τμήμα"), false);
});

test("rosters: the «Προτίμηση» column only when asked, from the allocation's ranks", () => {
  const ranks = placedRanks({
    mon: { assignments: [{ am: "1", club: "30", rank: 1, originalRank: 2, via: "da" }, { am: "2", club: "30", rank: 2, originalRank: 2, via: "da" }] },
    thu: { assignments: [{ am: "1", club: "30", rank: null, originalRank: null, via: "carried" }] },
  });
  assert.deepEqual(ranks, { 1: { mon: 2 }, 2: { mon: 2 } });
  const members = [students[1]];
  assert.deepEqual(rosterRows(clubs[2], members, [], false).at(-1), [1, 1, "ΒΗΤΑ", "ΕΛΕΝΗ", "ΠΕΤΡΟΣ", "Α"]);
  const rows = rosterRows(clubs[2], members, [], false, (am) => ranks[am]?.mon ?? null);
  assert.equal(rows.at(-2).at(-1), "Προτίμηση");
  assert.equal(rows.at(-1).at(-1), "2η");
  assert.equal(rosterRows(clubs[2], [students[2]], [], false, () => null).at(-1).at(-1), "");
});
