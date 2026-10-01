// The data package the platform exports for the offline R run (R/run_week.R).
// Plain UTF-8 CSV files, one fact per row, so they are easy to read in R
// or Excel:
//   students.csv       RegistryNr, Surname, Name, grade
//   clubs.csv          code, name, days ("mon;thu"), grades ("Α;Β"), capacity, similar (group word or empty)
//   preferences.csv    RegistryNr, day, rank, club_code
//   teacher_lists.csv  club_code, position, RegistryNr
//   mandatory_grades.csv grade (grades placed first, after the teacher's list)
//   lottery.csv        RegistryNr, lottery_number
//   seed.txt           the published seed
// Parents' names are not included: the allocation does not need them.

import { DAYS } from "../algorithm/days.js";

const csvCell = (v) => {
  const s = v === null || v === undefined ? "" : String(v);
  return /[",\n\r]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s;
};
export const toCsv = (header, rows) => [header, ...rows].map((r) => r.map(csvCell).join(",")).join("\n") + "\n";

/**
 * @returns {Record<string, string>} file name → contents
 */
export function buildRPackage({ students, clubs, preferences, teacherLists = {}, lottery, seed, mandatoryGrades = [] }) {
  const files = {};
  files["students.csv"] = toCsv(["RegistryNr", "Surname", "Name", "grade"],
    students.map((s) => [s.am, s.surname ?? "", s.name ?? "", s.grade]));
  files["clubs.csv"] = toCsv(["code", "name", "days", "grades", "capacity", "similar"],
    clubs.map((c) => [c.code, c.name, c.days.join(";"), c.grades.join(";"), c.capacity, c.similar ?? ""]));
  const prefRows = [];
  for (const s of students) {
    for (const day of DAYS) {
      (preferences[s.am]?.[day] ?? []).forEach((code, i) => prefRows.push([s.am, day, i + 1, code]));
    }
  }
  files["preferences.csv"] = toCsv(["RegistryNr", "day", "rank", "club_code"], prefRows);
  files["teacher_lists.csv"] = toCsv(["club_code", "position", "RegistryNr"],
    Object.entries(teacherLists).flatMap(([code, ams]) => ams.map((am, i) => [code, i + 1, am])));
  files["mandatory_grades.csv"] = toCsv(["grade"], mandatoryGrades.map((g) => [g]));
  if (lottery) {
    files["lottery.csv"] = toCsv(["RegistryNr", "lottery_number"], [...lottery].map(([am, n]) => [am, n]));
  }
  if (seed !== undefined) files["seed.txt"] = `${seed}\n`;
  return files;
}
