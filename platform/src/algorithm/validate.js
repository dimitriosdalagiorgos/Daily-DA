// Checks on what parents, teachers and the administrator submit.
// Each function returns a list of problems in Greek (empty = valid);
// validateClubs returns {code, field, message} objects.

import { DAYS, DAY_LABELS } from "./days.js";

export const GRADES = ["Α", "Β", "Γ"];
export const MAX_CLUB_DAYS = 3;

/**
 * Clubs checked against the template's rules.
 * @returns {{code: unknown, field: string, message: string}[]}
 */
export function validateClubs(clubs) {
  const problems = [];
  const seen = new Set();
  for (const club of clubs) {
    const code = club.code;
    const add = (field, message) => problems.push({ code, field, message: `Όμιλος ${code}: ${message}` });
    if (!Number.isInteger(code) || code <= 0) add("code", "ο κωδικός πρέπει να είναι θετικός ακέραιος.");
    if (seen.has(code)) add("code", "ο κωδικός υπάρχει ήδη.");
    seen.add(code);
    if (!String(club.name ?? "").trim()) add("name", "λείπει το όνομα.");

    const days = club.days ?? [];
    if (days.length === 0) add("days", "λείπει η ημέρα.");
    if (days.length > MAX_CLUB_DAYS) add("days", `έως ${MAX_CLUB_DAYS} ημέρες (6 ώρες).`);
    if (days.some((d) => !DAYS.includes(d))) {
      add("days", "άγνωστη ημέρα.");
    } else if (days.some((d, i) => i > 0 && DAYS.indexOf(d) <= DAYS.indexOf(days[i - 1]))) {
      add("days", "οι ημέρες πρέπει να είναι διαφορετικές και με τη σειρά της εβδομάδας.");
    }

    const grades = club.grades ?? [];
    if (grades.length === 0) add("grades", "λείπουν οι τάξεις.");
    if (grades.some((g) => !GRADES.includes(g))) add("grades", "άγνωστη τάξη.");
    if (!Number.isInteger(club.capacity) || club.capacity <= 0) {
      add("capacity", "η χωρητικότητα πρέπει να είναι θετικός ακέραιος.");
    }
  }
  return problems;
}

/**
 * Clubs a student ranks on a given day: clubs whose allocation happens that
 * day (multi-day clubs only on their first day) and which admit the grade.
 */
export function clubsToRank(clubs, grade, day) {
  return clubs.filter((c) => c.days[0] === day && c.grades.includes(grade)).map((c) => String(c.code));
}

/**
 * A parent's submission must rank, for every day, exactly the clubs from
 * clubsToRank() — each once. Multi-day clubs are ranked on their first day
 * only; on their later days the form shows them locked, since that
 * allocation has already happened (see SPEC).
 */
export function validateSubmission(clubs, student, preferences) {
  const problems = [];
  for (const day of DAYS) {
    const expected = clubsToRank(clubs, student.grade, day);
    const given = (preferences?.[day] ?? []).map(String);
    const label = DAY_LABELS[day];
    if (new Set(given).size !== given.length) problems.push(`${label}: κάποιος όμιλος εμφανίζεται δύο φορές.`);
    const missing = expected.filter((c) => !given.includes(c));
    const extra = given.filter((c) => !expected.includes(c));
    if (missing.length) problems.push(`${label}: λείπουν όμιλοι από την ιεράρχηση (${missing.join(", ")}).`);
    if (extra.length) problems.push(`${label}: όμιλοι που δεν επιτρέπονται (${extra.join(", ")}).`);
  }
  return problems;
}

/** A teacher's list: known students of the club's grades, no repeats, up to capacity. */
export function validateTeacherList(club, ams, studentsByAm) {
  const problems = [];
  const list = ams.map(String);
  if (list.length > club.capacity) {
    problems.push(`Η λίστα έχει ${list.length} μαθητές· η χωρητικότητα είναι ${club.capacity}.`);
  }
  if (new Set(list).size !== list.length) problems.push("Κάποιος μαθητής εμφανίζεται δύο φορές.");
  for (const am of list) {
    const student = studentsByAm.get(am);
    if (!student) problems.push(`Άγνωστος ΑΜ ${am}.`);
    else if (!club.grades.includes(student.grade)) {
      problems.push(`Ο ΑΜ ${am} είναι στην τάξη ${student.grade}, που δεν αφορά τον όμιλο.`);
    }
  }
  return problems;
}
