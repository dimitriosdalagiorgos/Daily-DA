// Trial import of last year's per-day responses (the Google Form export used
// with the R scripts, e.g. dailyresponses.csv):
//   RegistryNr | Surname | Name | <one column per club, value = rank>
// One file per day. Club columns are matched to the clubs of the template by
// name (accents/case ignored) or by code. Unlike real submissions, partial
// rankings are accepted (last year parents ranked 6–7 of 12 clubs).

import { DAY_LABELS } from "../algorithm/days.js";
import { normalizeName } from "./names.js";
import { cellInteger, cellText, excelRow } from "./table.js";

const ID_HEADERS = new Set(["REGISTRYNR", "ΑΜ", "ΑΡΙΘΜΟΣ ΜΗΤΡΩΟΥ"].map((h) => normalizeName(h)));
const NAME_HEADERS = new Set(["SURNAME", "NAME", "ΕΠΩΝΥΜΟ", "ΟΝΟΜΑ", "ΤΑΞΗ", "GRADE", "TIMESTAMP", "ΧΡΟΝΙΚΗ ΣΗΜΑΝΣΗ", "EMAIL"].map((h) => normalizeName(h)));

/**
 * @param {unknown[][]} rows the sheet (first non-empty row = header)
 * @param {{day: string, clubs: object[], students: object[], addMissingGrade?: string|null}} opts
 * @returns {{preferences: Record<string, string[]>, newStudents: object[], problems: object[], summary: object}}
 */
export function importLegacyResponses(rows, { day, clubs, students, addMissingGrade = null }) {
  const problems = [];
  const headerIndex = rows.findIndex((r) => (r ?? []).some((c) => cellText(c) !== ""));
  const header = (rows[headerIndex] ?? []).map((h) => cellText(h));
  const amCol = header.findIndex((h) => ID_HEADERS.has(normalizeName(h)));
  if (amCol < 0) {
    problems.push({ level: "error", message: "Δεν βρέθηκε στήλη αριθμού μητρώου (RegistryNr / ΑΜ)." });
    return { preferences: {}, newStudents: [], problems, summary: { rows: 0 } };
  }
  const col = (names) => header.findIndex((h) => names.map(normalizeName).includes(normalizeName(h)));
  const surnameCol = col(["SURNAME", "ΕΠΩΝΥΜΟ"]);
  const nameCol = col(["NAME", "ΟΝΟΜΑ"]);

  // Club columns → clubs of the template (by code or by name)
  const byName = new Map(clubs.map((c) => [normalizeName(c.name), c]));
  const byCode = new Map(clubs.map((c) => [String(c.code), c]));
  const clubCols = [];
  const unknown = [];
  header.forEach((h, i) => {
    if (i === amCol || h === "" || NAME_HEADERS.has(normalizeName(h))) return;
    const club = byCode.get(h.trim()) ?? byName.get(normalizeName(h));
    if (club) clubCols.push({ i, club });
    else unknown.push(h);
  });
  if (unknown.length) {
    problems.push({ level: "error", message: `Στήλες που δεν αντιστοιχούν σε όμιλο του προτύπου: ${unknown.map((u) => `«${u}»`).join(", ")}. Διορθώστε το όνομα (όπως στο πρότυπο) ή γράψτε τον κωδικό του ομίλου ως επικεφαλίδα.` });
  }
  const notToday = clubCols.filter(({ club }) => !club.days.includes(day));
  if (notToday.length) {
    problems.push({ level: "error", message: `Όμιλοι που δεν γίνονται ${DAY_LABELS[day]}: ${notToday.map(({ club }) => `«${club.name}»`).join(", ")}.` });
  }
  if (problems.some((p) => p.level === "error")) return { preferences: {}, newStudents: [], problems, summary: { rows: 0 } };

  const studentsByAm = new Map(students.map((s) => [s.am, s]));
  const preferences = {};
  const newStudents = [];
  const missing = [];
  let empty = 0;
  let dropped = 0;
  for (let r = headerIndex + 1; r < rows.length; r++) {
    const row = rows[r] ?? [];
    if (row.every((c) => cellText(c) === "")) continue;
    const at = { row: excelRow(r) };
    const am = cellInteger(row[amCol]);
    if (am === null) {
      problems.push({ level: "error", ...at, message: `Μη έγκυρος αριθμός μητρώου «${cellText(row[amCol])}».` });
      continue;
    }
    const key = String(am);
    if (preferences[key]) {
      problems.push({ level: "warning", ...at, message: `Ο ΑΜ ${key} υπάρχει δύο φορές· κρατήθηκε η τελευταία γραμμή.` });
    }
    let student = studentsByAm.get(key);
    if (!student) {
      if (!addMissingGrade) {
        missing.push(key);
        continue;
      }
      student = { am: key, grade: addMissingGrade, surname: cellText(row[surnameCol]) || key, name: cellText(row[nameCol]) || "-", father: "", mother: "", loginException: false, trial: true };
      studentsByAm.set(key, student);
      newStudents.push(student);
    }
    // Ranked clubs, in rank order (ties keep the column order)
    const ranked = clubCols
      .map(({ i, club }) => ({ club, rank: cellInteger(row[i]) }))
      .filter((x) => x.rank !== null && x.rank > 0)
      .sort((a, b) => a.rank - b.rank);
    // Later days of multi-day clubs are kept: the allocation drops them.
    const list = ranked.filter(({ club }) => club.grades.includes(student.grade)).map(({ club }) => String(club.code));
    dropped += ranked.length - list.length;
    if (list.length === 0) {
      empty++;
      continue;
    }
    preferences[key] = list;
  }
  if (missing.length) {
    problems.push({ level: "error", message: `${missing.length} ΑΜ δεν υπάρχουν στον κατάλογο (π.χ. ${missing.slice(0, 5).join(", ")}). Ανεβάστε τον αντίστοιχο κατάλογο ή επιλέξτε να προστεθούν ως δοκιμαστικοί μαθητές.` });
  }
  if (dropped) problems.push({ level: "warning", message: `Παραλείφθηκαν ${dropped} επιλογές ομίλων που δεν αφορούν την τάξη του μαθητή.` });
  if (empty) problems.push({ level: "warning", message: `${empty} γραμμές χωρίς καμία έγκυρη επιλογή αγνοήθηκαν.` });
  const summary = { rows: Object.keys(preferences).length, newStudents: newStudents.length, clubs: clubCols.length };
  return { preferences, newStudents, problems, summary };
}
