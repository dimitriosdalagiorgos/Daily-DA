// Clubs & teachers from our template (templates/omiloi_protypo.xlsx):
//   «Όμιλοι»:        Κωδικός | Όνομα ομίλου | Ημέρα 1 | Ημέρα 2 | Ημέρα 3 | Τάξεις | Χωρητικότητα | Ώρες | Περιγραφή
//   «Εκπαιδευτικοί»: Κωδικός ομίλου | Επώνυμο | Όνομα | Email | Όμιλος (έλεγχος)
// «Ώρες» and «Όμιλος (έλεγχος)» are formulas in the template and are ignored.

import { dayFromLabel } from "../algorithm/days.js";
import { GRADES, validateClubs } from "../algorithm/validate.js";
import { normalizeName } from "./names.js";
import { cellInteger, cellText, excelRow, findHeader, isBlankRow } from "./table.js";

const CLUB_FIELDS = {
  code: ["Κωδικός"],
  name: ["Όνομα ομίλου"],
  day1: ["Ημέρα 1"],
  day2: ["Ημέρα 2"],
  day3: ["Ημέρα 3"],
  grades: ["Τάξεις"],
  capacity: ["Χωρητικότητα"],
  description: ["Περιγραφή"],
};
const TEACHER_FIELDS = {
  code: ["Κωδικός ομίλου"],
  surname: ["Επώνυμο"],
  name: ["Όνομα"],
  email: ["Email", "E-mail", "Ηλεκτρονικό ταχυδρομείο"],
};

const DAY_BY_KEY = new Map(["Δευτέρα", "Τρίτη", "Τετάρτη", "Πέμπτη", "Παρασκευή"].map((l) => [normalizeName(l), dayFromLabel(l)]));
const parseDay = (value) => DAY_BY_KEY.get(normalizeName(value)) ?? null;

/** "Β-Γ", "Β, Γ", "Α Β Γ" → ["Β", "Γ"] …; null if any part is unknown. */
function parseGrades(value) {
  const parts = normalizeName(value).split(/[\s,;/]+/).filter(Boolean);
  if (parts.length === 0 || parts.some((p) => !GRADES.includes(p))) return null;
  return GRADES.filter((g) => parts.includes(g));
}

const EMAIL = /^[a-z0-9._%+-]+@sch\.gr$/;

function missingColumns(sheet, header, labels) {
  return {
    level: "error",
    sheet,
    message: `Φύλλο «${sheet}»: δεν βρέθηκαν οι στήλες ${header.missing.map((f) => labels[f]).join(", ")}. Χρησιμοποιήστε το πρότυπο της πλατφόρμας.`,
  };
}

/**
 * @param {{clubs: unknown[][], teachers: unknown[][]}} sheets rows of the two sheets
 * @returns {{clubs: object[], teachers: object[], problems: object[], summary: object}}
 */
export function importClubs(sheets) {
  const problems = [];
  const clubs = [];
  const clubLabels = Object.fromEntries(Object.entries(CLUB_FIELDS).map(([f, [l]]) => [f, `«${l}»`]));
  const ch = findHeader(sheets.clubs ?? [], CLUB_FIELDS, ["code", "name", "day1", "grades", "capacity"]);
  if (!("columns" in ch)) {
    problems.push(missingColumns("Όμιλοι", ch, clubLabels));
    return { clubs, teachers: [], problems, summary: summarize(clubs, []) };
  }

  const rowOfCode = new Map();
  for (let i = ch.headerRow + 1; i < sheets.clubs.length; i++) {
    const row = sheets.clubs[i];
    const get = (f) => (f in ch.columns ? row[ch.columns[f]] : "");
    if (cellText(get("code")) === "" && cellText(get("name")) === "") continue; // empty template row
    const at = { sheet: "Όμιλοι", row: excelRow(i) };
    const rowProblems = [];

    const code = cellInteger(get("code"));
    if (code === null || code <= 0) rowProblems.push({ ...at, field: "code", message: `Μη έγκυρος κωδικός «${cellText(get("code"))}».` });

    const dayCells = ["day1", "day2", "day3"].map((f) => cellText(get(f)));
    const days = [];
    dayCells.forEach((text, k) => {
      if (text === "") return;
      const day = parseDay(text);
      if (!day) rowProblems.push({ ...at, field: `day${k + 1}`, message: `Άγνωστη ημέρα «${text}».` });
      else days.push(day);
    });
    if (dayCells.some((t, k) => t !== "" && dayCells.slice(0, k).some((prev) => prev === ""))) {
      rowProblems.push({ ...at, field: "days", message: "Οι ημέρες συμπληρώνονται με τη σειρά: Ημέρα 1, μετά Ημέρα 2, μετά Ημέρα 3." });
    }

    const grades = parseGrades(get("grades"));
    if (cellText(get("grades")) !== "" && !grades) {
      rowProblems.push({ ...at, field: "grades", message: `Άγνωστες τάξεις «${cellText(get("grades"))}».` });
    }
    const capacityCell = get("capacity");
    const capacity = cellInteger(capacityCell);
    if (cellText(capacityCell) !== "" && capacity === null) {
      rowProblems.push({ ...at, field: "capacity", message: `Μη έγκυρη χωρητικότητα «${cellText(capacityCell)}».` });
    }

    const club = {
      code: code ?? NaN,
      name: cellText(get("name")),
      days,
      grades: grades ?? [],
      capacity: capacity ?? NaN,
      description: cellText(get("description")),
    };
    // Shared rules (also used when clubs are edited on the platform); skip
    // fields already reported above in more detail.
    const reported = new Set(rowProblems.map((p) => (p.field.startsWith("day") ? "days" : p.field)));
    for (const p of validateClubs([club])) {
      if (!reported.has(p.field)) rowProblems.push({ ...at, field: p.field, message: p.message.replace(/^Όμιλος \S+: /, "") });
    }
    if (code !== null && rowOfCode.has(code)) {
      rowProblems.push({ ...at, field: "code", message: `Ο κωδικός ${code} υπάρχει ήδη στη γραμμή ${rowOfCode.get(code)}.` });
    }

    problems.push(...rowProblems.map((p) => ({ level: "error", ...p })));
    if (rowProblems.length === 0) {
      rowOfCode.set(code, at.row);
      clubs.push(club);
    }
  }
  if (clubs.length === 0 && problems.length === 0) {
    problems.push({ level: "error", sheet: "Όμιλοι", message: "Δεν υπάρχει κανένας όμιλος." });
  }

  const teachers = importTeachers(sheets.teachers ?? [], clubs, rowOfCode, problems);
  for (const club of clubs) {
    if (!teachers.some((t) => t.clubs.includes(club.code))) {
      problems.push({ level: "warning", sheet: "Εκπαιδευτικοί", message: `Ο όμιλος ${club.code} «${club.name}» δεν έχει εκπαιδευτικό.` });
    }
  }
  return { clubs, teachers, problems, summary: summarize(clubs, teachers) };
}

function importTeachers(rows, clubs, rowOfCode, problems) {
  const labels = Object.fromEntries(Object.entries(TEACHER_FIELDS).map(([f, [l]]) => [f, `«${l}»`]));
  const th = findHeader(rows, TEACHER_FIELDS, Object.keys(TEACHER_FIELDS));
  if (!("columns" in th)) {
    problems.push(missingColumns("Εκπαιδευτικοί", th, labels));
    return [];
  }
  const byEmail = new Map();
  const seenPair = new Map();
  for (let i = th.headerRow + 1; i < rows.length; i++) {
    const row = rows[i];
    if (isBlankRow(row, th.columns)) continue;
    const get = (f) => row[th.columns[f]];
    const at = { sheet: "Εκπαιδευτικοί", row: excelRow(i) };
    const rowProblems = [];

    const code = cellInteger(get("code"));
    if (code === null) rowProblems.push({ ...at, field: "code", message: `Μη έγκυρος κωδικός ομίλου «${cellText(get("code"))}».` });
    else if (!rowOfCode.has(code)) rowProblems.push({ ...at, field: "code", message: `Ο κωδικός ${code} δεν υπάρχει στο φύλλο «Όμιλοι».` });
    for (const f of ["surname", "name"]) {
      if (!cellText(get(f))) rowProblems.push({ ...at, field: f, message: `Λείπει: ${labels[f]}.` });
    }
    const email = cellText(get("email")).toLowerCase();
    if (!EMAIL.test(email)) rowProblems.push({ ...at, field: "email", message: `Μη έγκυρο email «${cellText(get("email"))}» (μόνο διευθύνσεις @sch.gr).` });

    if (rowProblems.length) {
      problems.push(...rowProblems.map((p) => ({ level: "error", ...p })));
      continue;
    }
    const pair = `${email}|${code}`;
    if (seenPair.has(pair)) {
      problems.push({ level: "warning", ...at, message: `Διπλή γραμμή: ο/η ${email} είναι ήδη στον όμιλο ${code} (γραμμή ${seenPair.get(pair)}).` });
      continue;
    }
    seenPair.set(pair, at.row);

    const teacher = byEmail.get(email);
    if (!teacher) {
      byEmail.set(email, { email, surname: cellText(get("surname")), name: cellText(get("name")), clubs: [code], row: at.row });
    } else {
      if (normalizeName(teacher.surname) !== normalizeName(get("surname")) || normalizeName(teacher.name) !== normalizeName(get("name"))) {
        problems.push({ level: "warning", ...at, message: `Το email ${email} έχει άλλο ονοματεπώνυμο στη γραμμή ${teacher.row}.` });
      }
      teacher.clubs.push(code);
    }
  }
  return [...byEmail.values()].map(({ row, ...t }) => t);
}

function summarize(clubs, teachers) {
  return {
    clubs: clubs.length,
    multiDay: clubs.filter((c) => c.days.length > 1).length,
    teachers: teachers.length,
  };
}
