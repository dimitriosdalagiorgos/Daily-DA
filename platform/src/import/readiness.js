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
//
// Teachers' exclusions (⛔, closed lists) are checked on top, as warnings
// only (exclusions win over mandatory placement, SPEC): students no club
// accepts on a day, and groups whose seats fall short once a club counts at
// most as many seats as it has accepted students of the group.

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
export function checkReadiness(students, clubs, { mandatoryGrades = [] } = {}, exclusions = {}) {
  const problems = [];
  const perGrade = Object.fromEntries(GRADES.map((g) => [g, students.filter((s) => s.grade === g).length]));
  const present = GRADES.filter((g) => perGrade[g] > 0);
  const mandatory = present.filter((g) => mandatoryGrades.includes(g));
  const optional = present.filter((g) => !mandatoryGrades.includes(g));

  // A grade with no club on any day: its parents would log in to an empty
  // form. Reported once for the week instead of once per day.
  const noClubAtAll = present.filter((g) => !clubs.some((c) => c.grades.includes(g)));
  for (const grade of noClubAtAll) {
    const isMandatory = mandatoryGrades.includes(grade);
    problems.push({
      level: isMandatory ? "error" : "warning", grades: [grade], students: perGrade[grade],
      message: `Η ${grade} τάξη (${perGrade[grade]} μαθητές) δεν έχει κανέναν όμιλο σε καμία ημέρα${isMandatory ? " (υποχρεωτική ένταξη)" : ""}: οι γονείς της θα συνδέονται αλλά δεν θα βλέπουν ομίλους για δήλωση. Ελέγξτε τη στήλη «Τάξεις» του αρχείου ομίλων.`,
    });
  }

  for (const day of DAYS) {
    const running = clubs.filter((c) => c.days.includes(day));
    if (running.length === 0) continue; // no clubs at all that day
    const label = DAY_LABELS[day];

    for (const grade of present) {
      if (noClubAtAll.includes(grade)) continue; // reported once above
      if (!running.some((c) => c.grades.includes(grade))) {
        problems.push({
          level: mandatoryGrades.includes(grade) ? "error" : "warning", day, grades: [grade],
          message: `${label}: δεν υπάρχει όμιλος για την ${grade} τάξη${mandatoryGrades.includes(grade) ? " (υποχρεωτική ένταξη)" : ""}.`,
        });
      }
    }

    const isOut = (c, am) => (exclusions[String(c.code)] ?? []).includes(String(am));
    const shortfall = (group, withExclusions = false) => {
      const seats = running.filter((c) => c.grades.some((g) => group.includes(g))).reduce((sum, c) => {
        if (!withExclusions) return sum + c.capacity;
        const accepted = students.filter((s) => group.includes(s.grade) && c.grades.includes(s.grade) && !isOut(c, s.am)).length;
        return sum + Math.min(c.capacity, accepted);
      }, 0);
      const need = group.reduce((sum, g) => sum + perGrade[g], 0);
      return { seats, need };
    };
    const failed = new Set();

    // Mandatory grades: every group (errors)
    for (const group of groups(mandatory)) {
      const { seats, need } = shortfall(group);
      if (seats === 0 && group.length === 1) continue; // reported above as "no club"
      if (group.length === 1 && noClubAtAll.includes(group[0])) continue;
      if (seats < need) {
        failed.add(group.join());
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
        failed.add(grade);
        problems.push({
          level: "warning", day, grades: [grade], seats, students: need,
          message: `${label}: οι όμιλοι για την ${grade} τάξη έχουν ${seatsText(seats)} για ${need} μαθητές — κάποιοι θα μείνουν χωρίς όμιλο.`,
        });
      }
    }

    if (!Object.keys(exclusions).length) continue;
    // Students that every club of the day for their grade excludes
    const orphans = students.filter((s) => {
      const theirs = running.filter((c) => c.grades.includes(s.grade));
      return theirs.length > 0 && theirs.every((c) => isOut(c, s.am));
    });
    if (orphans.length) {
      problems.push({
        level: "warning", day, students: orphans.length, ams: orphans.map((s) => s.am),
        message: `${label}: ${orphans.length} ${orphans.length === 1 ? "μαθητής δεν έχει" : "μαθητές δεν έχουν"} κανέναν όμιλο που να ${orphans.length === 1 ? "τον" : "τους"} δέχεται (αποκλεισμοί εκπαιδευτικών) — θα ${orphans.length === 1 ? "μείνει" : "μείνουν"} χωρίς όμιλο: ΑΜ ${orphans.slice(0, 10).map((s) => s.am).join(", ")}${orphans.length > 10 ? "…" : ""}.`,
      });
    }
    // Seats the exclusions leave out (where the plain check passed)
    for (const group of groups(present)) {
      if (failed.has(group.join()) || group.some((g) => noClubAtAll.includes(g))) continue;
      const { seats, need } = shortfall(group, true);
      const plain = shortfall(group).seats;
      if (seats < need && seats < plain) {
        problems.push({
          level: "warning", day, grades: group, seats, students: need,
          message: `${label}: με τους αποκλεισμούς των εκπαιδευτικών, οι όμιλοι για ${list(group)} δέχονται ${seatsText(seats)} για ${need} μαθητές — κάποιοι θα μείνουν χωρίς όμιλο.`,
        });
        break; // one warning per day is enough
      }
    }
  }
  return problems;
}
