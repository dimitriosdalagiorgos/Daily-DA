// The allocation as tables for the admin: the whole week per student (one
// column per day) and each club's roster (its members, for the teacher to
// keep attendance). Pure: shown on screen and written to Excel in the browser.

import { DAYS, DAY_LABELS } from "../algorithm/days.js";

const el = (a, b) => String(a ?? "").localeCompare(String(b ?? ""), "el");

/** Grade, surname, name, father's name, mother's name. */
export const byGradeAndName = (a, b) =>
  el(a.grade, b.grade) || el(a.surname, b.surname) || el(a.name, b.name) || el(a.father, b.father) || el(a.mother, b.mother);

/** Surname, name, father's name (a roster lists the club's members alphabetically). */
const byName = (a, b) => el(a.surname, b.surname) || el(a.name, b.name) || el(a.father, b.father);

const GAP_TEXT = { all_rejected: "— δεν χώρεσε", no_preferences: "— χωρίς προτιμήσεις" };

/** The «Τμήμα» column appears once the sections file has been uploaded (any student has one). */
export const hasSections = (students) => students.some((s) => s.section);

/** Header of weekRows(): with «Τμήμα» after «Τάξη» when there are sections. */
export const weekHeader = (students) =>
  ["ΑΜ", "Τάξη", ...(hasSections(students) ? ["Τμήμα"] : []), "Επώνυμο", "Όνομα", "Πατρώνυμο", "Μητρώνυμο", ...DAYS.map((d) => DAY_LABELS[d])];

/**
 * One row per student: their details, then the club of each day (or why none).
 * @param {{am: string, grade: string, surname: string, name: string, father?: string, mother?: string, section?: string}[]} students
 * @param {{byStudent: Record<string, Record<string, string>>, gaps?: Record<string, Record<string, string>>}} results
 * @param {{code: number|string, name: string}[]} clubs
 * @returns {unknown[][]} data rows (without the header), sorted by grade and name
 */
export function weekRows(students, results, clubs) {
  const nameOf = new Map(clubs.map((c) => [String(c.code), c.name]));
  const cell = (am, day) => {
    const code = results.byStudent[am]?.[day];
    if (code) return nameOf.get(String(code)) ?? String(code);
    return GAP_TEXT[results.gaps?.[am]?.[day]] ?? "";
  };
  const withSection = hasSections(students);
  return [...students].sort(byGradeAndName).map((s) =>
    [Number(s.am) || s.am, s.grade, ...(withSection ? [s.section ?? ""] : []), s.surname, s.name, s.father ?? "", s.mother ?? "", ...DAYS.map((d) => cell(s.am, d))]);
}

/** The students placed in a club (on its first day, where its seats are decided), alphabetically. */
export function clubMembers(club, students, results) {
  const code = String(club.code);
  const day = club.days[0];
  return students.filter((s) => String(results.byStudent[s.am]?.[day] ?? "") === code).sort(byName);
}

export const ROSTER_SHEET = "Παρουσιολόγιο";
const ROSTER_HEADER = ["Α/Α", "ΑΜ", "Επώνυμο", "Όνομα", "Πατρώνυμο", "Τάξη"];

/**
 * A club's roster for its teacher: the club on top, then its members
 * (with their section, when the sections file has been uploaded).
 * @param {{code: number|string, name: string, days: string[], capacity: number}} club
 * @param {object[]} members from clubMembers()
 * @param {string[]} [teacherNames]
 * @param {boolean} [withSection] add «Τμήμα» (pass hasSections(allStudents), so every roster has the same columns)
 * @returns {unknown[][]} rows for a worksheet
 */
export function rosterRows(club, members, teacherNames = [], withSection = hasSections(members)) {
  return [
    ["Όμιλος", `${club.name} (${club.code})`],
    ["Ημέρες", club.days.map((d) => DAY_LABELS[d]).join(" + ")],
    ...(teacherNames.length ? [["Εκπαιδευτικοί", teacherNames.join(", ")]] : []),
    ["Μαθητές", `${members.length} (θέσεις ${club.capacity})`],
    [],
    [...ROSTER_HEADER, ...(withSection ? ["Τμήμα"] : [])],
    ...members.map((s, i) => [i + 1, Number(s.am) || s.am, s.surname, s.name, s.father ?? "", s.grade, ...(withSection ? [s.section ?? ""] : [])]),
  ];
}

/** «parousiologio_102_Θεατρική_παράσταση.xlsx» */
export function rosterFileName(club) {
  const safe = String(club.name).replace(/[\\/:*?"<>|«»]/g, "").trim().replace(/\s+/g, "_").slice(0, 40);
  return `parousiologio_${club.code}_${safe || "omilos"}.xlsx`;
}
