// Students' sections (τμήματα) from two myschool reports, as CSV:
//   «Γενικά Στοιχεία Τμημάτων»: Α/Α | Τμήμα | Τάξη | Τύπος τμήματος | Μάθημα
//   «Τμήματα μαθητών»: per grade a «Τάξη Εγγραφής:» line, then
//     Α/Α | Αριθμός μητρώου | Επώνυμο μαθητή | Όνομα μαθητή | Όνομα πατέρα | Τμήματα
// Only the general-education section (Γενικής Παιδείας, e.g. «Α1») is kept:
// it is shown next to the student in the admin's tables and rosters. The
// type of each section comes from the first report, not from its name.
// Ported from the school's make_student_sections.py.

import { GRADES } from "../algorithm/validate.js";
import { cellInteger, cellText, headerKey } from "./table.js";

// Latin capitals that look like Greek ones (myschool has e.g. «A1» with a
// Latin A). Applied to section names only, never to students' details.
const LOOKALIKES = { A: "Α", B: "Β", E: "Ε", H: "Η", I: "Ι", K: "Κ", M: "Μ", N: "Ν", O: "Ο", P: "Ρ", T: "Τ", X: "Χ", Y: "Υ", Z: "Ζ" };

/** «A1» (Latin A) → «Α1»; trimmed, upper case. */
export const sectionName = (value) => cellText(value).toUpperCase().replace(/[A-Z]/g, (ch) => LOOKALIKES[ch] ?? ch);

/** Matching key: also without spaces and dashes («Β-ΓΕΡΜ 1» = «ΒΓΕΡΜ1»). */
const sectionKey = (value) => sectionName(value).replace(/[\s\-–—]/g, "");

const GENERAL_TYPE = headerKey("Γενικής Παιδείας Λυκείου");

const DEFINITION_HEADER = ["Α/Α", "Τμήμα", "Τάξη", "Τύπος τμήματος", "Μάθημα"].map(headerKey);
const STUDENT_HEADER = ["Α/Α", "Αριθμός μητρώου", "Επώνυμο μαθητή", "Όνομα μαθητή", "Όνομα πατέρα", "Τμήματα"].map(headerKey);
const startsWith = (row, header) => header.every((h, i) => headerKey(row?.[i]) === h);

/**
 * Which of the two reports a file is, from its header rows.
 * @param {unknown[][]} rows
 * @returns {"definitions"|"students"|null}
 */
export function sectionsFileKind(rows) {
  if (rows.some((r) => startsWith(r, DEFINITION_HEADER))) return "definitions";
  if (rows.some((r) => startsWith(r, STUDENT_HEADER))) return "students";
  return null;
}

/** «Α», «Α΄», «Α΄ Λυκείου», «A» (Latin) → «Α»; otherwise null. */
function parseGrade(value) {
  const m = sectionName(value).match(/^([ΑΒΓ])\s*[΄'’`ʹ]?\s*(ΛΥΚΕ[ΙΊ]ΟΥ)?$/);
  return m && GRADES.includes(m[1]) ? m[1] : null;
}

/**
 * @param {unknown[][]} definitionRows «Γενικά Στοιχεία Τμημάτων»
 * @param {unknown[][]} studentRows «Τμήματα μαθητών»
 * @returns {{sections: Record<string, {grade: string, section: string}>, problems: object[], summary: object}}
 *   sections: ΑΜ → grade and general-education section
 */
export function importSections(definitionRows, studentRows) {
  const problems = [];
  const definitions = new Map(); // key → { name, general }
  const defStart = definitionRows.findIndex((r) => startsWith(r, DEFINITION_HEADER));
  for (const row of defStart < 0 ? [] : definitionRows.slice(defStart + 1)) {
    if (!/^\d+$/.test(cellText(row[0])) || !cellText(row[1])) continue;
    definitions.set(sectionKey(row[1]), { name: sectionName(row[1]), general: headerKey(row[3]) === GENERAL_TYPE });
  }
  if (!definitions.size) {
    problems.push({ level: "error", message: "Δεν βρέθηκε ο πίνακας «Γενικά στοιχεία τμημάτων» (Α/Α, Τμήμα, Τάξη, Τύπος τμήματος, Μάθημα)." });
  }

  const sections = {};
  const unknown = new Set();
  const withoutGeneral = [];
  let grade = null;
  let inTable = false;
  studentRows.forEach((row, i) => {
    const first = cellText(row[0]);
    const at = { row: i + 1 };
    if (headerKey(first).startsWith(headerKey("Τάξη Εγγραφής"))) {
      // The grade is in the same cell or in a later one
      grade = [first.replace(/^[^:]*:/, ""), ...row.slice(1)].map(parseGrade).find(Boolean) ?? null;
      inTable = false;
      if (!grade) problems.push({ level: "error", ...at, message: `Δεν αναγνωρίστηκε η τάξη στη γραμμή «Τάξη Εγγραφής» (αναμενόταν Α, Β ή Γ).` });
      return;
    }
    if (startsWith(row, STUDENT_HEADER)) {
      inTable = true;
      return;
    }
    if (!inTable || !grade || !/^\d+$/.test(first)) return;
    const am = cellInteger(row[1]);
    if (am === null || am <= 0) {
      problems.push({ level: "error", ...at, message: `Μη έγκυρος αριθμός μητρώου «${cellText(row[1])}».` });
      return;
    }
    const key = String(am);
    if (sections[key]) {
      problems.push({ level: "error", ...at, message: `Ο αριθμός μητρώου ${key} υπάρχει δύο φορές.` });
      return;
    }
    const general = [];
    for (const part of cellText(row[5]).split(",").map((p) => p.trim()).filter(Boolean)) {
      const def = definitions.get(sectionKey(part));
      if (!def) unknown.add(part);
      else if (def.general) general.push(def.name);
    }
    if (!general.length) withoutGeneral.push(key);
    sections[key] = { grade, section: general.join(", ") };
  });

  const count = Object.keys(sections).length;
  if (!count && !problems.some((p) => p.level === "error")) {
    problems.push({ level: "error", message: "Δεν βρέθηκαν μαθητές στο αρχείο «Τμήματα μαθητών»." });
  }
  if (unknown.size && definitions.size) {
    problems.push({ level: "warning", message: `Τμήματα που δεν υπάρχουν στα «Γενικά Στοιχεία Τμημάτων» (αγνοήθηκαν): ${[...unknown].join(", ")}. Μήπως τα δύο αρχεία είναι από διαφορετική ημερομηνία;` });
  }
  if (withoutGeneral.length) {
    problems.push({ level: "warning", message: `${withoutGeneral.length} μαθητές χωρίς τμήμα Γενικής Παιδείας: ΑΜ ${withoutGeneral.slice(0, 10).join(", ")}${withoutGeneral.length > 10 ? "…" : ""}.` });
  }
  const byGrade = Object.fromEntries(GRADES.map((g) => [g, Object.values(sections).filter((s) => s.grade === g).length]));
  return { sections, problems, summary: { count, byGrade } };
}
