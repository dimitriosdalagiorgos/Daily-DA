// Checks that need both files: students per grade against the clubs on offer.
// All are warnings — the school decides (SPEC: no limits beyond capacity).

import { DAYS, DAY_LABELS } from "../algorithm/days.js";
import { GRADES } from "../algorithm/validate.js";

export function checkReadiness(students, clubs) {
  const problems = [];
  const perGrade = Object.fromEntries(GRADES.map((g) => [g, students.filter((s) => s.grade === g).length]));

  for (const day of DAYS) {
    const running = clubs.filter((c) => c.days.includes(day));
    if (running.length === 0) continue; // no clubs that day at all
    const label = DAY_LABELS[day];

    for (const grade of GRADES) {
      if (perGrade[grade] === 0) continue;
      const open = running.filter((c) => c.grades.includes(grade));
      if (open.length === 0) {
        problems.push({ level: "warning", message: `${label}: δεν υπάρχει όμιλος για την ${grade} τάξη.` });
        continue;
      }
      const seats = open.reduce((sum, c) => sum + c.capacity, 0);
      if (seats < perGrade[grade]) {
        problems.push({
          level: "warning",
          message: `${label}: οι όμιλοι για την ${grade} τάξη έχουν ${seats} θέσεις για ${perGrade[grade]} μαθητές — κάποιοι θα μείνουν χωρίς όμιλο.`,
        });
      }
    }

    const grades = GRADES.filter((g) => running.some((c) => c.grades.includes(g)));
    const total = grades.reduce((sum, g) => sum + perGrade[g], 0);
    const seats = running.reduce((sum, c) => sum + c.capacity, 0);
    if (seats < total) {
      problems.push({
        level: "warning",
        message: `${label}: συνολικά ${seats} θέσεις για ${total} μαθητές (τάξεις ${grades.join(", ")}) — κάποιοι θα μείνουν χωρίς όμιλο.`,
      });
    }
  }
  return problems;
}
