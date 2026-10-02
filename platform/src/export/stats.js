// Statistics of an allocation, for the admin's «Στατιστικά» tab and the
// results workbook. Pure: results + the allocation's input.
//
// - choices: which choice each student got, per day and per grade
//   (a later day of a multi-day club counts with its first day's rank)
// - demand: per club, on its first day (where its seats are decided)
// - teacher lists: what became of the students on them

import { DAYS } from "../algorithm/days.js";

/** Buckets of the "which choice" bars; histograms use the exact rank. */
export const CHOICE_BUCKETS = ["1", "2", "3", "4+"];
const bucketOf = (rank) => (rank <= 3 ? String(rank) : "4+");

/**
 * @param {{byStudent: object, days: Record<string, {unassigned: {am: string, reason: string}[]}>}} results
 * @param {{students: {am: string, grade: string}[], clubs: object[], preferences: Record<string, Record<string, string[]>>,
 *          teacherLists?: Record<string, string[]>}} input
 * @param {Record<string, object[]>} [logByDay] the allocation's log: who was turned away by each club
 */
export function allocationStats(results, { students, clubs, preferences, teacherLists = {} }, logByDay = null) {
  // Students a club finally turned away (last word REJECTED or DISPLACED), per day
  const turnedAway = new Map(); // `${day}|${code}` → Set of ΑΜ
  for (const [day, log] of Object.entries(logByDay ?? {})) {
    const last = new Map();
    for (const e of log ?? []) if (e.am && e.club !== undefined && ["ACCEPTED", "RETAINED", "REJECTED", "DISPLACED"].includes(e.event)) last.set(`${e.am}|${e.club}`, e.event);
    for (const [key, event] of last) {
      if (event !== "REJECTED" && event !== "DISPLACED") continue;
      const [am, code] = key.split("|");
      const k = `${day}|${code}`;
      if (!turnedAway.has(k)) turnedAway.set(k, new Set());
      turnedAway.get(k).add(am);
    }
  }
  const clubBy = new Map(clubs.map((c) => [String(c.code), c]));
  const grades = [...new Set(students.map((s) => s.grade))].sort();
  const prefs = (am, day) => (preferences[am]?.[day] ?? []).map(String);
  /** 1-based rank of a club in the student's list (first day for a carried day), or null */
  const rankOf = (am, day, code) => {
    const first = clubBy.get(code)?.days[0] ?? day;
    let i = prefs(am, first).indexOf(code); // the day its seats were decided
    if (i < 0) i = prefs(am, day).indexOf(code);
    return i < 0 ? null : i + 1;
  };

  const emptyBuckets = () => Object.fromEntries([...CHOICE_BUCKETS, "none"].map((b) => [b, 0]));
  const byDay = Object.fromEntries(DAYS.map((d) => [d, emptyBuckets()]));
  const byGrade = Object.fromEntries(grades.map((g) => [g, emptyBuckets()]));
  const histogram = {}; // exact rank → student-days
  const overall = emptyBuckets();
  for (const day of DAYS) {
    const rejected = new Set((results.days[day]?.unassigned ?? []).filter((u) => u.reason === "all_rejected").map((u) => u.am));
    for (const s of students) {
      const code = results.byStudent[s.am]?.[day];
      let bucket = null;
      if (code) {
        const rank = rankOf(s.am, day, String(code));
        if (rank === null) continue;
        histogram[rank] = (histogram[rank] ?? 0) + 1;
        bucket = bucketOf(rank);
      } else if (rejected.has(s.am)) bucket = "none";
      if (!bucket) continue; // no wishes that day: not part of these figures
      byDay[day][bucket]++;
      byGrade[s.grade][bucket]++;
      overall[bucket]++;
    }
  }
  const total = (b) => Object.values(b).reduce((a, n) => a + n, 0);
  const share = (b, keys) => (total(b) ? keys.reduce((a, k) => a + b[k], 0) / total(b) : null);

  // Demand per club, on its first day
  const demand = clubs.map((c) => {
    const code = String(c.code);
    const day = c.days[0];
    let first = 0;
    let applicants = 0;
    let placed = 0;
    let missed = 0;
    for (const s of students) {
      const list = prefs(s.am, day);
      const at = list.indexOf(code);
      if (at < 0) continue;
      applicants++;
      if (at === 0) first++;
      if (String(results.byStudent[s.am]?.[day]) === code) placed++;
    }
    // Applied and did not fit (from the log; it also knows who never got to
    // apply, e.g. already placed by a multi-day club)
    missed = logByDay ? (turnedAway.get(`${day}|${code}`)?.size ?? 0) : null;
    return { code: c.code, name: c.name, day, days: c.days, capacity: c.capacity, first, applicants, placed, missed, fill: c.capacity ? placed / c.capacity : null, pressure: c.capacity ? first / c.capacity : null };
  });

  // Teachers' lists
  const lists = Object.entries(teacherLists).filter(([, ams]) => ams?.length).map(([code, ams]) => {
    const c = clubBy.get(String(code));
    const day = c?.days[0];
    let ranked = 0;
    let placed = 0;
    let elsewhere = 0;
    for (const am of ams) {
      const list = prefs(am, day);
      const at = list.indexOf(String(code));
      if (at < 0) continue;
      ranked++;
      const got = results.byStudent[am]?.[day];
      if (String(got) === String(code)) placed++;
      else if (got && list.indexOf(String(got)) >= 0 && list.indexOf(String(got)) < at) elsewhere++;
    }
    return { code: Number(code), name: c?.name ?? String(code), listed: ams.length, ranked, placed, elsewhere, notRanked: ams.length - ranked };
  });

  return {
    overall,
    firstChoiceShare: share(overall, ["1"]),
    topThreeShare: share(overall, ["1", "2", "3"]),
    byDay,
    byGrade,
    histogram: Object.entries(histogram).map(([rank, count]) => ({ rank: Number(rank), count })).sort((a, b) => a.rank - b.rank),
    demand,
    teacherLists: lists,
  };
}
