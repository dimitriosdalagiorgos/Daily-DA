// Student list as exported by myschool («Κατάλογος Μαθητών», one sheet):
//   Τάξη | Αριθμός Μητρώου | Επώνυμο | Όνομα | Όνομα πατέρα | Όνομα μητέρας
// Problems are "error" (the file cannot be used until fixed) or
// "warning" (usable; the admin should look).

import { GRADES } from "../algorithm/validate.js";
import { normalizeName } from "./names.js";
import { cellInteger, cellText, excelRow, findHeader, isBlankRow } from "./table.js";

const FIELDS = {
  grade: ["Τάξη"],
  am: ["Αριθμός Μητρώου", "ΑΜ", "Α.Μ.", "Αρ. Μητρώου"],
  surname: ["Επώνυμο"],
  name: ["Όνομα"],
  father: ["Όνομα πατέρα", "Πατρώνυμο"],
  mother: ["Όνομα μητέρας", "Μητρώνυμο"],
};
const LABELS = {
  grade: "Τάξη", am: "Αριθμός Μητρώου", surname: "Επώνυμο", name: "Όνομα",
  father: "Όνομα πατέρα", mother: "Όνομα μητέρας",
};

/** "Α", "α", "A" (Latin), "Α΄", "Α τάξη" → "Α"; otherwise null. */
function parseGrade(value) {
  const g = normalizeName(value).replace(/[^Α-Ω]/g, "").replace(/ΤΑΞΗ$/, "");
  return GRADES.includes(g) ? g : null;
}

/**
 * @param {unknown[][]} rows the sheet as rows of cells
 * @returns {{students: object[], problems: object[], summary: object}}
 */
export function importStudents(rows) {
  const problems = [];
  const header = findHeader(rows, FIELDS, Object.keys(FIELDS));
  if (!("columns" in header)) {
    problems.push({
      level: "error",
      message: `Δεν βρέθηκαν οι στήλες: ${header.missing.map((f) => LABELS[f]).join(", ")}. Ανεβάστε τον «Κατάλογο Μαθητών» του myschool.`,
    });
    return { students: [], problems, summary: { count: 0, byGrade: {} } };
  }

  const { headerRow, columns } = header;
  const students = [];
  const rowOfAm = new Map();
  for (let i = headerRow + 1; i < rows.length; i++) {
    const row = rows[i];
    if (isBlankRow(row, columns)) continue;
    const at = { row: excelRow(i) };
    const get = (f) => row[columns[f]];
    const rowProblems = [];

    const am = cellInteger(get("am"));
    if (am === null || am <= 0) rowProblems.push({ level: "error", ...at, field: "am", message: `Μη έγκυρος αριθμός μητρώου «${cellText(get("am"))}».` });
    const grade = parseGrade(get("grade"));
    if (!grade) rowProblems.push({ level: "error", ...at, field: "grade", message: `Άγνωστη τάξη «${cellText(get("grade"))}».` });
    for (const f of ["surname", "name"]) {
      if (!cellText(get(f))) rowProblems.push({ level: "error", ...at, field: f, message: `Λείπει: ${LABELS[f]}.` });
    }
    for (const f of ["father", "mother"]) {
      if (!cellText(get(f))) {
        rowProblems.push({
          level: "warning", ...at, field: f,
          message: `Λείπει: ${LABELS[f]}. Ο γονέας δεν θα μπορεί να συνδεθεί χωρίς εξαίρεση «μόνο ΑΜ + επώνυμο».`,
        });
      }
    }
    problems.push(...rowProblems);
    if (rowProblems.some((p) => p.level === "error")) continue;

    const key = String(am);
    if (rowOfAm.has(key)) {
      problems.push({ level: "error", ...at, field: "am", message: `Ο αριθμός μητρώου ${key} υπάρχει ήδη στη γραμμή ${rowOfAm.get(key)}.` });
      continue;
    }
    rowOfAm.set(key, at.row);
    students.push({
      am: key,
      grade,
      surname: cellText(get("surname")),
      name: cellText(get("name")),
      father: cellText(get("father")),
      mother: cellText(get("mother")),
    });
  }

  if (students.length === 0 && !problems.some((p) => p.level === "error")) {
    problems.push({ level: "error", message: "Το αρχείο δεν περιέχει μαθητές." });
  }
  const byGrade = Object.fromEntries(GRADES.map((g) => [g, students.filter((s) => s.grade === g).length]));
  return { students, problems, summary: { count: students.length, byGrade } };
}
