import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { importLegacyResponses } from "../src/import/legacy.js";
import { decodeCsv, parseCsv } from "../src/import/csv.js";

const clubs = [
  { code: 101, name: "Tallinn", days: ["mon"], grades: ["Α", "Β"], capacity: 10 },
  { code: 102, name: "Παρίσι", days: ["mon", "thu"], grades: ["Α", "Β"], capacity: 10 },
  { code: 103, name: "London", days: ["mon"], grades: ["Β"], capacity: 10 },
  { code: 104, name: "Βερολίνο", days: ["thu"], grades: ["Α", "Β"], capacity: 10 },
];
const students = [{ am: "7205", grade: "Α" }, { am: "7206", grade: "Β" }];

test("last year's layout: ranks → ordered lists, partial rankings accepted", () => {
  const rows = [
    ["RegistryNr", "Surname", "Name", "Tallinn", "Παρίσι", "LONDON"],
    [7205, "ΑΓΕΛΟΠΟΥΛΟΣ", "ΘΕΟΚΛΗΤΟΣ", 2, 1, ""],
    ["7206", "ΑΗΔΟΝΟΠΟΥΛΟΣ", "ΚΛΕΑΡΧΟΣ", "", "3", "1"],
    ["", "", "", "", "", ""],
  ];
  const r = importLegacyResponses(rows, { day: "mon", clubs, students });
  assert.deepEqual(r.problems, []);
  assert.deepEqual(r.preferences, { 7205: ["102", "101"], 7206: ["103", "102"] });
});

test("club columns by code; accents and case ignored", () => {
  const r = importLegacyResponses([["ΑΜ", "Επώνυμο", "Όνομα", "104", "παρισι"], [7205, "Α", "Β", 1, 2]], { day: "thu", clubs, students });
  assert.deepEqual(r.problems, []);
  assert.deepEqual(r.preferences, { 7205: ["104", "102"] }, "a later day of a multi-day club is kept; the allocation drops it");
});

test("unknown clubs and clubs of another day are errors", () => {
  let r = importLegacyResponses([["RegistryNr", "Tallinn", "Lisbon"], [7205, 1, 2]], { day: "mon", clubs, students });
  assert.match(r.problems[0].message, /«Lisbon»/);
  r = importLegacyResponses([["RegistryNr", "Tallinn", "Βερολίνο"], [7205, 1, 2]], { day: "mon", clubs, students });
  assert.match(r.problems[0].message, /δεν γίνονται Δευτέρα: «Βερολίνο»/);
});

test("students missing from the list: error, or added as trial students", () => {
  const rows = [["RegistryNr", "Surname", "Name", "Tallinn"], [9999, "ΝΕΟΣ", "ΜΑΘΗΤΗΣ", 1]];
  let r = importLegacyResponses(rows, { day: "mon", clubs, students });
  assert.match(r.problems[0].message, /1 ΑΜ δεν υπάρχουν στον κατάλογο \(π\.χ\. 9999\)/);
  r = importLegacyResponses(rows, { day: "mon", clubs, students, addMissingGrade: "Β" });
  assert.deepEqual(r.problems, []);
  assert.deepEqual(r.newStudents.map((s) => [s.am, s.grade, s.surname, s.trial]), [["9999", "Β", "ΝΕΟΣ", true]]);
  assert.deepEqual(r.preferences, { 9999: ["101"] });
});

test("clubs not for the student's grade are dropped with a warning", () => {
  const r = importLegacyResponses([["RegistryNr", "London", "Tallinn"], [7205, 1, 2]], { day: "mon", clubs, students });
  assert.deepEqual(r.preferences, { 7205: ["101"] });
  assert.match(r.problems.map((p) => p.message).join(" "), /Παραλείφθηκαν 1 επιλογές/);
});

test("the repository's dailyresponses.csv reads as CSV, UTF-8 and Windows-1253", () => {
  const bytes = readFileSync(new URL("../../archive/dailyresponses.csv", import.meta.url));
  const rows = parseCsv(decodeCsv(new Uint8Array(bytes)));
  assert.deepEqual(rows[0].slice(0, 4), ["RegistryNr", "Surname", "Name", "Tallinn"]);
  assert.equal(rows[1][1], "ΑΓΕΛΟΠΟΥΛΟΣ");
  // Greek Excel: Windows-1253 with semicolons
  const win = new Uint8Array([0xC1, 0xCC, 0x3B, 0xD0, 0xE1, 0xF1, 0xDF, 0xF3, 0xE9, 0x0D, 0x0A, 0x37, 0x3B, 0x31]); // "ΑΜ;Παρίσι\r\n7;1"
  assert.deepEqual(parseCsv(decodeCsv(win)), [["ΑΜ", "Παρίσι"], ["7", "1"]]);
  assert.deepEqual(parseCsv('a,"b ""c"", d"\n1,2'), [["a", 'b "c", d'], ["1", "2"]]);
});

test("same club name on different days: the club of the file's day", () => {
  const many = [
    { code: 101, name: "Άλγεβρα", days: ["mon"], grades: ["Α"], capacity: 10 },
    { code: 114, name: "Άλγεβρα", days: ["tue"], grades: ["Α"], capacity: 10 },
    { code: 156, name: "Άλγεβρα", days: ["fri"], grades: ["Α"], capacity: 10 },
    { code: 100, name: "Γερμανικά", days: ["mon", "thu"], grades: ["Α"], capacity: 10 },
    { code: 145, name: "Χημεία", days: ["thu"], grades: ["Α"], capacity: 10 },
    { code: 146, name: "Χημεία", days: ["thu"], grades: ["Α"], capacity: 10 },
  ];
  const st = [{ am: "1", grade: "Α" }];
  let r = importLegacyResponses([["RegistryNr", "Άλγεβρα", "Γερμανικά"], [1, 1, 2]], { day: "mon", clubs: many, students: st });
  assert.deepEqual(r.problems, []);
  assert.deepEqual(r.preferences, { 1: ["101", "100"] });
  r = importLegacyResponses([["RegistryNr", "Άλγεβρα"], [1, 1]], { day: "fri", clubs: many, students: st });
  assert.deepEqual(r.preferences, { 1: ["156"] });
  r = importLegacyResponses([["RegistryNr", "Γερμανικά"], [1, 1]], { day: "thu", clubs: many, students: st });
  assert.deepEqual(r.preferences, { 1: ["100"] }, "a multi-day club on its later day");
  r = importLegacyResponses([["RegistryNr", "Άλγεβρα"], [1, 1]], { day: "wed", clubs: many, students: st });
  assert.match(r.problems[0].message, /δεν γίνονται Τετάρτη: «Άλγεβρα»/);
  r = importLegacyResponses([["RegistryNr", "Χημεία"], [1, 1]], { day: "thu", clubs: many, students: st });
  assert.match(r.problems[0].message, /«Χημεία» \(κωδικοί 145, 146\).*κωδικό/);
  r = importLegacyResponses([["RegistryNr", "145", "146"], [1, 2, 1]], { day: "thu", clubs: many, students: st });
  assert.deepEqual(r.preferences, { 1: ["146", "145"] }, "codes resolve the ambiguity");
});
