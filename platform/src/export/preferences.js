// The parents' declarations as a table for the admin: one sheet per day,
// one row per student, one column per club ranked that day (multi-day clubs
// on their first day, where they are ranked), holding the student's
// preference number for it. Pure: the server gives the rows, the browser
// writes the .xlsx.

import { DAYS, DAY_LABELS } from "../algorithm/days.js";
import { byGradeAndName, hasSections } from "./rosters.js";

/** «Θεατρικό (Δευτέρα + Πέμπτη)» for a multi-day club, the name alone otherwise. */
const clubLabel = (club) => (club.days.length > 1 ? `${club.name} (${club.days.map((d) => DAY_LABELS[d]).join(" + ")})` : club.name);

/**
 * @param {{am: string, grade: string, surname: string, name: string, father?: string, mother?: string, section?: string}[]} students
 * @param {{code: number|string, name: string, days: string[]}[]} clubs
 * @param {Record<string, Record<string, (string|number)[]>>} preferences ΑΜ → day → club codes, best first
 * @returns {{name: string, rows: unknown[][], cols: number[], filter: boolean}[]} one sheet per day with clubs
 */
export function preferenceSheets(students, clubs, preferences) {
  const withSection = hasSections(students);
  const sorted = [...students].sort(byGradeAndName);
  const sheets = [];
  for (const day of DAYS) {
    const dayClubs = clubs.filter((c) => c.days[0] === day).sort((a, b) => Number(a.code) - Number(b.code));
    if (dayClubs.length === 0) continue;
    const header = ["ΑΜ", "Τάξη", ...(withSection ? ["Τμήμα"] : []), "Επώνυμο", "Όνομα", "Πατρώνυμο", "Μητρώνυμο", "Δήλωση",
      ...dayClubs.map((c) => `${clubLabel(c)} [${c.code}]`)];
    const rows = sorted.map((s) => {
      const list = (preferences[s.am]?.[day] ?? []).map(String);
      return [Number(s.am) || s.am, s.grade, ...(withSection ? [s.section ?? ""] : []), s.surname, s.name, s.father ?? "", s.mother ?? "",
        preferences[s.am] ? "✓" : "—",
        ...dayClubs.map((c) => {
          const i = list.indexOf(String(c.code));
          return i < 0 ? "" : i + 1;
        })];
    });
    sheets.push({
      name: DAY_LABELS[day],
      rows: [header, ...rows],
      cols: [8, 5, ...(withSection ? [7] : []), 20, 16, 14, 14, 7, ...dayClubs.map(() => 14)],
      filter: true,
    });
  }
  return sheets;
}
