// A random school for property tests: students, clubs (some multi-day,
// some similar), preferences, teacher lists, mandatory grades, lottery.

import { drawLottery, DAYS } from "../src/algorithm/index.js";

const club = (code, days, capacity, grades = ["Α", "Β", "Γ"]) => ({ code, name: `Όμιλος ${code}`, days, grades, capacity });

export function randomInstance(rng) {
  const pick = (arr) => arr[Math.floor(rng() * arr.length)];
  const shuffle = (arr) => {
    const a = [...arr];
    for (let i = a.length - 1; i > 0; i--) {
      const j = Math.floor(rng() * (i + 1));
      [a[i], a[j]] = [a[j], a[i]];
    }
    return a;
  };
  const grades = ["Α", "Β", "Γ"];
  const students = Array.from({ length: 20 + Math.floor(rng() * 40) }, (_, i) => ({ am: String(1000 + i), grade: pick(grades) }));
  const clubs = [];
  for (let code = 1; code <= 6 + Math.floor(rng() * 10); code++) {
    const len = rng() < 0.6 ? 1 : rng() < 0.7 ? 2 : 3;
    const days = shuffle(DAYS).slice(0, len).sort((a, b) => DAYS.indexOf(a) - DAYS.indexOf(b));
    const g = shuffle(grades).slice(0, 1 + Math.floor(rng() * 3));
    clubs.push(club(code, days, 1 + Math.floor(rng() * 6), g));
  }
  const preferences = {};
  for (const s of students) {
    preferences[s.am] = {};
    for (const day of DAYS) {
      const eligible = clubs.filter((c) => c.days.includes(day) && c.grades.includes(s.grade)).map((c) => c.code);
      if (rng() < 0.05) continue; // some parents skip a day
      preferences[s.am][day] = shuffle(eligible);
    }
  }
  const teacherLists = {};
  for (const c of clubs) {
    if (rng() < 0.3) {
      const pool = students.filter((s) => c.grades.includes(s.grade)).map((s) => s.am);
      teacherLists[c.code] = shuffle(pool).slice(0, Math.min(c.capacity, Math.floor(rng() * 4)));
    }
  }
  const lottery = drawLottery(students.map((s) => s.am), String(rng()));
  const mandatoryGrades = grades.filter(() => rng() < 0.4);
  // A few groups of similar clubs
  for (const c of clubs) if (rng() < 0.4) c.similar = pick(["Α1", "Α2", "Α3"]);
  return { students, clubs, preferences, teacherLists, lottery, mandatoryGrades };
}
