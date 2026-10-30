// Capacity planning around the allocation: how many seats each grade is
// sure of before declarations open, and which seats to add when students of
// mandatory grades did not fit. Neither changes the allocation itself: it
// stays pure Gale–Shapley, only the capacities change (no change in R).
//
// Why clubs shared by grades need this (raised 30/10/2026): with lottery
// priorities, students of one grade may fill a shared club, and the students
// of the other grade they push out find no club of their own with room,
// while unpopular clubs of the first grade keep empty seats. Every stable
// allocation leaves the same students out (rural hospitals theorem: McVitie
// & Wilson 1970; Roth 1986), so only seats, eligibility or priorities can
// change that. Hall's condition (readiness.js) says a placement for everyone
// exists, not that a stable one does.

import { allocateWeek, EVENTS, UNASSIGNED_REASONS } from "../algorithm/allocate.js";
import { DAYS } from "../algorithm/days.js";
import { GRADES } from "../algorithm/validate.js";

// Ceiling for suggested increases (decided 30/10/2026): the regulation
// allows 10–22 students per club, and up to 30 in a «σύνολο». The platform
// does not know which clubs are «σύνολα», so a club the clubs file already
// gave more than 22 seats counts as one.
export const CLUB_MAX = 22;
export const ENSEMBLE_MAX = 30;
export const capacityCeiling = (fileCapacity) => (fileCapacity > CLUB_MAX ? ENSEMBLE_MAX : CLUB_MAX);

/**
 * Seats each grade gets whatever students rank and the lottery draws, per
 * day: its own clubs, plus the seats of shared clubs that students of other
 * grades could not fill even if they all went there. Assumes everyone ranks
 * every club open to them (as the parents' form requires) and ignores
 * teachers' exclusions. At most `students - guaranteed` of the grade can be
 * left out: an unplaced student means every club open to them is full.
 * @returns {{day: string, grade: string, students: number, own: number, shared: number, others: number, guaranteed: number, atRisk: number}[]}
 */
export function guaranteedSeats(students, clubs) {
  const perGrade = Object.fromEntries(GRADES.map((g) => [g, students.filter((s) => s.grade === g).length]));
  const rows = [];
  for (const day of DAYS) {
    const running = clubs.filter((c) => c.days.includes(day));
    for (const grade of GRADES) {
      const theirs = running.filter((c) => c.grades.includes(grade));
      if (!perGrade[grade] || !theirs.length) continue;
      const own = theirs.filter((c) => c.grades.every((g) => g === grade)).reduce((n, c) => n + c.capacity, 0);
      const sharedClubs = theirs.filter((c) => c.grades.some((g) => g !== grade));
      const shared = sharedClubs.reduce((n, c) => n + c.capacity, 0);
      const otherGrades = new Set(sharedClubs.flatMap((c) => c.grades).filter((g) => g !== grade));
      const others = [...otherGrades].reduce((n, g) => n + perGrade[g], 0);
      const guaranteed = own + Math.max(0, shared - others);
      rows.push({ day, grade, students: perGrade[grade], own, shared, others, guaranteed, atRisk: Math.max(0, perGrade[grade] - guaranteed) });
    }
  }
  return rows;
}

/** Students of mandatory grades who declared but did not fit, per day. */
function unplaced(result, gradeOf, mandatoryGrades) {
  return DAYS.flatMap((day) => result.days[day].unassigned
    .filter((u) => u.reason === UNASSIGNED_REASONS.ALL_REJECTED && mandatoryGrades.includes(gradeOf.get(String(u.am))))
    .map((u) => ({ am: String(u.am), day })));
}

/**
 * Seats to add so that students of mandatory grades who did not fit get a
 * club, within each club's ceiling. Adding a seat where a student was first
 * turned away is not enough: in a shared club the seat may go to a student
 * of another grade, who leaves an empty seat behind in a club the unplaced
 * student cannot join. So the search tries seats and reruns the allocation.
 *
 * Greedy: at each step, among the full clubs running on a day someone is
 * left out, the fewest extra seats that place at least one more student
 * (one seat in each club, then two, …). The cheapest wins; then the one
 * placing more students; then a club open to fewer grades (its seats cannot
 * go to other grades, so shared clubs do not swell); then the lower code. Finally each increase is cut back as far as
 * it can go without leaving anyone out again.
 *
 * Each step reruns the week's allocation many times (some seconds for a
 * large school with many students left out), so the search stops after
 * `timeLimitMs` with what it found so far (`complete: false`).
 *
 * @param {object} input        allocateWeek's input, capacities as they stand
 * @param {Record<string, number>} fileCapacity  club code → capacity in the clubs file
 * @returns {{
 *   before: {am: string, day: string}[], after: {am: string, day: string}[],
 *   increases: {code: string, from: number, to: number, ceiling: number}[], complete: boolean,
 * }}
 */
export function suggestCapacityIncreases(input, fileCapacity, { timeLimitMs = Infinity, now = Date.now } = {}) {
  const stopAt = now() + timeLimitMs;
  let complete = true;
  const gradeOf = new Map(input.students.map((s) => [String(s.am), s.grade]));
  const mandatory = input.mandatoryGrades ?? [];
  const capacity = new Map(input.clubs.map((c) => [String(c.code), c.capacity]));
  const ceiling = new Map(input.clubs.map((c) => [String(c.code), capacityCeiling(fileCapacity[c.code] ?? c.capacity)]));
  const allocate = () => allocateWeek({ ...input, clubs: input.clubs.map((c) => ({ ...c, capacity: capacity.get(String(c.code)) })) });
  const run = () => unplaced(allocate(), gradeOf, mandatory);
  const tryWith = (code, value) => {
    const old = capacity.get(code);
    capacity.set(code, value);
    const left = run();
    capacity.set(code, old);
    return left;
  };
  // A seat more changes something only in a club that turned someone away
  // (it is then full); the others are not worth an allocation run.
  const turnedAway = (result) => new Set(DAYS.flatMap((day) => result.days[day].log
    .filter((e) => e.event === EVENTS.REJECTED || e.event === EVENTS.DISPLACED).map((e) => String(e.club))));

  let result = allocate();
  const before = unplaced(result, gradeOf, mandatory);
  let left = before;
  while (left.length && complete) {
    const days = new Set(left.map((u) => u.day));
    const full = turnedAway(result);
    const candidates = input.clubs.filter((c) => full.has(String(c.code)) && c.days.some((d) => days.has(d)));
    let best = null;
    // Fewest seats first: one more seat in every candidate, then two, …
    for (let add = 1; !best && candidates.length && now() <= stopAt; add++) {
      const open = candidates.filter((c) => capacity.get(String(c.code)) + add <= ceiling.get(String(c.code)));
      if (!open.length) break;
      for (const club of open) {
        if (now() > stopAt) break;
        const code = String(club.code);
        const after = tryWith(code, capacity.get(code) + add);
        if (after.length >= left.length) continue;
        const candidate = { code, add, after, placed: left.length - after.length, grades: club.grades.length };
        if (!best || compare(candidate, best) < 0) best = candidate;
      }
    }
    if (now() > stopAt) complete = false;
    if (!best) break; // no club can place anyone more within its ceiling, or out of time
    capacity.set(best.code, capacity.get(best.code) + best.add);
    result = allocate();
    left = best.after;
  }

  // An early increase may have become unnecessary after later ones.
  const original = new Map(input.clubs.map((c) => [String(c.code), c.capacity]));
  for (const code of [...capacity.keys()].reverse()) {
    while (capacity.get(code) > original.get(code) && tryWith(code, capacity.get(code) - 1).length <= left.length) {
      capacity.set(code, capacity.get(code) - 1);
    }
  }
  left = run();

  return {
    before,
    after: left,
    complete,
    increases: input.clubs
      .map((c) => String(c.code))
      .filter((code) => capacity.get(code) !== original.get(code))
      .map((code) => ({ code, from: original.get(code), to: capacity.get(code), ceiling: ceiling.get(code) })),
  };
}

function compare(a, b) {
  return a.add - b.add || b.placed - a.placed || a.grades - b.grades || Number(a.code) - Number(b.code) || a.code.localeCompare(b.code);
}
