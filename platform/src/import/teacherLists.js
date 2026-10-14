// Teachers' lists from files (admin bulk import), two layouts:
//
//  1. The per-club file of src/export/teacherListFile.js: «Κωδικός ομίλου»
//     on top, then the students with an «Επιλογή» column. Marked rows are
//     the list; if nothing is marked, the rows left in the file are (the
//     teacher deleted the others) — unless every eligible student is still
//     there, which means the file came back untouched.
//  2. One table for many clubs: columns «Κωδικός ομίλου» and «ΑΜ», one row
//     per chosen student (an «Επιλογή» column, if present, filters rows).
//
// Order does not matter: a list holds at most as many students as seats,
// so every listed student who applies fits.
//
// Exclusions: «ΟΧΙ» (or «-», «⛔») in «Επιλογή» excludes the student (⛔),
// any other mark lists them (⭐). In layout 1 a «Κλειστή λίστα | ΝΑΙ» row
// on top makes the list closed (everyone not marked is excluded); without
// that row (older files) the closed setting stays as it was.

import { validateTeacherList } from "../algorithm/validate.js";
import { cellInteger, cellText, findHeader, headerKey } from "./table.js";

const FIELDS = {
  code: ["Κωδικός ομίλου", "Κωδικός"],
  pick: ["Επιλογή"],
  am: ["ΑΜ", "Α.Μ.", "Αριθμός Μητρώου", "Αρ. Μητρώου", "RegistryNr"],
};
const CODE_LABEL = headerKey("Κωδικός ομίλου");
const CLOSED_LABEL = headerKey("Κλειστή λίστα");
const BLOCK_MARKS = new Set(["ΟΧΙ", "OXI", "NO", "-", "–", "⛔", "Ο", "O", "0"].map(headerKey));
const YES = new Set(["ΝΑΙ", "NAI", "YES", "Χ", "X", "✓", "1"].map(headerKey));
/** «ΟΧΙ» and the like: the student is excluded. */
export const isBlockMark = (value) => BLOCK_MARKS.has(headerKey(value));

/**
 * @param {unknown[][]} rows the sheet
 * @returns {{lists: {code: number, ams: string[], blocked?: string[], closed?: boolean}[], problems: string[], warnings: string[]}}
 */
export function readTeacherListRows(rows, { clubs, students }) {
  const problems = [];
  const warnings = [];
  const amHeader = findHeader(rows, FIELDS, ["am"]);
  if (!("columns" in amHeader)) return { lists: [], problems: ["Δεν βρέθηκε στήλη «ΑΜ»."], warnings };
  const { headerRow, columns } = amHeader;
  const body = rows.slice(headerRow + 1).filter((r) => cellText(r?.[columns.am]) !== "");
  const isMarked = (r) => "pick" in columns && cellText(r[columns.pick]) !== "";
  const isBlocked = (r) => isMarked(r) && isBlockMark(r[columns.pick]);
  const amOf = (r) => String(cellInteger(r[columns.am]) ?? cellText(r[columns.am]));

  // Layout 2: a club code on every row
  if ("code" in columns) {
    const chosen = body.some(isMarked) ? body.filter(isMarked) : body;
    const byCode = new Map();
    for (const r of chosen) {
      const code = cellInteger(r[columns.code]);
      if (code === null) { problems.push(`Μη έγκυρος κωδικός ομίλου «${cellText(r[columns.code])}».`); continue; }
      const entry = byCode.get(code) ?? byCode.set(code, { ams: [], blocked: [] }).get(code);
      (isBlocked(r) ? entry.blocked : entry.ams).push(amOf(r));
    }
    return { lists: [...byCode].map(([code, { ams, blocked }]) => ({ code, ams, ...(blocked.length ? { blocked } : {}) })), problems, warnings };
  }

  // Layout 1: the code in a «Κωδικός ομίλου | 102» row above the table
  const codeRow = rows.slice(0, headerRow).find((r) => headerKey(r?.[0]) === CODE_LABEL);
  const code = codeRow ? cellInteger(codeRow[1]) : null;
  if (code === null) return { lists: [], problems: ["Δεν βρέθηκε ο κωδικός ομίλου στην κορυφή του αρχείου («Κωδικός ομίλου»)."], warnings };
  const club = clubs.find((c) => c.code === code);
  const closedRow = rows.slice(0, headerRow).find((r) => headerKey(r?.[0]) === CLOSED_LABEL);
  const closed = closedRow ? YES.has(headerKey(closedRow[1])) : undefined;
  let chosen;
  let blocked = [];
  if (body.some(isMarked)) {
    chosen = body.filter((r) => isMarked(r) && !isBlocked(r));
    blocked = body.filter(isBlocked);
    if (closed && blocked.length) warnings.push("Κλειστή λίστα: τα «ΟΧΙ» δεν χρειάζονται, αποκλείονται όλοι όσοι δεν έχουν Χ.");
  } else {
    const eligible = students.filter((s) => club?.grades.includes(s.grade)).length;
    if (club && body.length >= eligible) {
      warnings.push("Δεν σημειώθηκε κανένας μαθητής· η λίστα του ομίλου θα είναι κενή.");
      chosen = [];
    } else {
      chosen = body; // the teacher deleted the rows they did not want
    }
  }
  if (closed && !chosen.length) warnings.push("Κλειστή λίστα χωρίς μαθητές: ο όμιλος δεν θα δεχτεί κανέναν.");
  return {
    lists: [{ code, ams: chosen.map(amOf), blocked: closed ? [] : blocked.map(amOf), ...(closed !== undefined ? { closed } : {}) }],
    problems, warnings,
  };
}

/**
 * Several files at once → one report per club, ready to save.
 * @param {{fileName: string, rows: unknown[][]}[]} files
 * @param {{clubs: object[], students: object[], lists: Record<string, {ams: string[]}>}} data
 *   clubs with their effective capacity; lists = current teachers' lists
 */
export function importTeacherLists(files, { clubs, students, lists = {} }) {
  const studentsByAm = new Map(students.map((s) => [s.am, s]));
  const results = [];
  const seen = new Map();
  for (const { fileName, rows } of files) {
    const read = readTeacherListRows(rows ?? [], { clubs, students });
    if (read.problems.length && !read.lists.length) {
      results.push({ fileName, code: null, ams: [], problems: read.problems, warnings: read.warnings });
      continue;
    }
    for (const { code, ams, blocked, closed } of read.lists) {
      const club = clubs.find((c) => c.code === code);
      const problems = [...read.problems];
      const warnings = [...read.warnings];
      if (!club) problems.push(`Άγνωστος όμιλος ${code}.`);
      else problems.push(...validateTeacherList(club, ams, studentsByAm, blocked ?? []));
      if (seen.has(code)) problems.push(`Ο όμιλος ${code} υπάρχει και στο αρχείο «${seen.get(code)}».`);
      seen.set(code, fileName);
      const existing = lists[code]?.ams ?? [];
      if (club && existing.length && problems.length === 0) {
        warnings.push(`Ο όμιλος έχει ήδη λίστα ${existing.length} μαθητών${lists[code].updatedBy && lists[code].updatedBy !== "admin" ? ` (από ${lists[code].updatedBy})` : ""}· θα αντικατασταθεί.`);
      }
      results.push({ fileName, code, clubName: club?.name ?? "", capacity: club?.capacity ?? null, ams, ...(blocked ? { blocked } : {}), ...(closed !== undefined ? { closed } : {}), problems, warnings });
    }
  }
  return results;
}
