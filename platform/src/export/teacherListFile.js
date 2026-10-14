// The per-club file a teacher fills in (one sheet, «Λίστα»): the club on
// top, then every student of the club's grades with an empty «Επιλογή»
// column. The teacher marks the students they prefer (any mark, e.g. Χ),
// up to the club's seats, and «ΟΧΙ» for students the club does not accept;
// «Κλειστή λίστα: ΝΑΙ» accepts only the marked students. Read back by
// src/import/teacherLists.js.

export const LIST_SHEET = "Λίστα";
export const LIST_HEADER = ["Επιλογή", "ΑΜ", "Επώνυμο", "Όνομα", "Τάξη"];

/**
 * @param {{code: number, name: string, grades: string[], capacity: number, days: string[]}} club
 * @param {{am: string, surname: string, name: string, grade: string}[]} students
 * @param {string[]} [chosen] ΑΜ already on the club's list (pre-marked Χ)
 * @param {{blocked?: string[], closed?: boolean}} [list] the rest of the saved list (pre-marked ΟΧΙ)
 * @returns {unknown[][]} rows for a worksheet
 */
export function teacherListRows(club, students, chosen = [], { blocked = [], closed = false } = {}) {
  const marked = new Set(chosen.map(String));
  const out = new Set(closed ? [] : blocked.map(String));
  const eligible = students
    .filter((s) => club.grades.includes(s.grade))
    .sort((a, b) => a.grade.localeCompare(b.grade, "el") || a.surname.localeCompare(b.surname, "el") || a.name.localeCompare(b.name, "el"));
  return [
    ["Κωδικός ομίλου", club.code],
    ["Όμιλος", club.name],
    ["Θέσεις", club.capacity],
    ["Οδηγίες", `Βάλτε Χ στη στήλη «Επιλογή» δίπλα στους μαθητές που προτιμάτε — έως ${club.capacity} — και ΟΧΙ σε όσους ο όμιλος δεν δέχεται. Με «Κλειστή λίστα: ΝΑΙ» ο όμιλος δέχεται μόνο όσους έχουν Χ. Μην αλλάζετε τον κωδικό ομίλου ούτε τους ΑΜ.`],
    ["Κλειστή λίστα", closed ? "ΝΑΙ" : "ΟΧΙ"],
    LIST_HEADER,
    ...eligible.map((s) => [marked.has(s.am) ? "Χ" : out.has(s.am) ? "ΟΧΙ" : "", Number(s.am) || s.am, s.surname, s.name, s.grade]),
  ];
}

/** «102_Θεατρική_παράσταση.xlsx»: code first, so the files sort by club. */
export function teacherListFileName(club) {
  const safe = String(club.name).replace(/[\\/:*?"<>|«»]/g, "").trim().replace(/\s+/g, "_").slice(0, 40);
  return `${club.code}_${safe || "omilos"}.xlsx`;
}
