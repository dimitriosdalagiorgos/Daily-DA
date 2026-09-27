// Checks that need both files: students per grade against the clubs on offer.
//
// Seats for grades where joining a club is mandatory (settings.mandatoryGrades):
// for every day and every group of those grades, the clubs open to at least
// one grade of the group must have at least as many seats as the group has
// students. Checking each grade alone is not enough: a club open to Α and Β
// would be counted twice. When all groups pass, a placement for everyone
// exists (Hall's theorem); when one fails, some students surely stay out.
// Failures are errors (declarations cannot open); for other grades the same
// shortfalls are only warnings.

import { DAYS, DAY_LABELS } from "../algorithm/days.js";
import { GRADES } from "../algorithm/validate.js";

/** All non-empty groups of the given grades, smallest first. */
function groups(grades) {
  const out = [];
  for (let mask = 1; mask < 1 << grades.length; mask++) out.push(grades.filter((_, i) => mask & (1 << i)));
  return out.sort((a, b) => a.length - b.length);
}

const seatsText = (n) => `${n} ${n === 1 ? "θέση" : "θέσεις"}`;
const list = (grades) => (grades.length === 1 ? `την ${grades[0]} τάξη` : `τις τάξεις ${grades.join(" + ")}`);

/**
 * @param {{am: string, grade: string}[]} students
 * @param {{code: number, days: string[], grades: string[], capacity: number}[]} clubs
 * @param {{mandatoryGrades?: string[]}} options
 * @returns {{level: "error"|"warning", message: string, day?: string, grades?: string[], seats?: number, students?: number}[]}
 */
export function checkReadiness(students, clubs, { mandatoryGrades = [] } = {}) {
  const problems = [];
  const perGrade = Object.fromEntries(GRADES.map((g) => [g, students.filter((s) => s.grade === g).length]));
  const present = GRADES.filter((g) => perGrade[g] > 0);
  const mandatory = present.filter((g) => mandatoryGrades.includes(g));
  const optional = present.filter((g) => !mandatoryGrades.includes(g));

  for (const day of DAYS) {
    const running = clubs.filter((c) => c.days.includes(day));
    if (running.length === 0) continue; // no clubs at all that day
    const label = DAY_LABELS[day];

    for (const grade of present) {
      if (!running.some((c) => c.grades.includes(grade))) {
        problems.push({
          level: mandatoryGrades.includes(grade) ? "error" : "warning", day, grades: [grade],
          message: `${label}: δεν υπάρχει όμιλος για την ${grade} τάξη${mandatoryGrades.includes(grade) ? " (υποχρεωτική ένταξη)" : ""}.`,
        });
      }
    }

    const shortfall = (group) => {
      const seats = running.filter((c) => c.grades.some((g) => group.includes(g))).reduce((sum, c) => sum + c.capacity, 0);
      const need = group.reduce((sum, g) => sum + perGrade[g], 0);
      return { seats, need };
    };

    // Mandatory grades: every group (errors)
    for (const group of groups(mandatory)) {
      const { seats, need } = shortfall(group);
      if (seats === 0 && group.length === 1) continue; // reported above as "no club"
      if (seats < need) {
        problems.push({
          level: "error", day, grades: group, seats, students: need,
          message: `${label}: οι όμιλοι για ${list(group)} έχουν ${seatsText(seats)} για ${need} μαθητές (υποχρεωτική ένταξη) — λείπουν ${seatsText(need - seats)}.`,
        });
      }
    }

    // Other grades: each grade alone (warnings)
    for (const grade of optional) {
      const { seats, need } = shortfall([grade]);
      if (seats > 0 && seats < need) {
        problems.push({
          level: "warning", day, grades: [grade], seats, students: need,
          message: `${label}: οι όμιλοι για την ${grade} τάξη έχουν ${seatsText(seats)} για ${need} μαθητές — κάποιοι θα μείνουν χωρίς όμιλο.`,
        });
      }
    }
  }
  return problems;
}
