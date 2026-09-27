// Weekly club allocation: student-proposing Deferred Acceptance per day,
// Monday → Friday (see platform/SPEC.md, «Αλγόριθμος»).
//
// Club priority over students (lower tuple wins):
//   1. position in the teacher's list for the club (not listed = ∞)
//   2. students of grades with mandatory placement before the others
//      (only when mandatoryGrades is given)
//   3. the student's rank for the club on that day (after renumbering)
//   4. the student's lottery number
//
// Multi-day clubs (2 or 3 days) take part only in the allocation of their
// first day. Students placed there keep the seat on the club's later days
// and sit out those days' allocations. Everyone else simply loses the club
// from their later-day lists, and the remaining clubs are renumbered.
//
// Input shapes:
//   students:     [{ am, grade }]
//   clubs:        [{ code, name, days: ["mon", "thu"], grades: ["Α", "Β"], capacity }]
//   preferences:  { [am]: { [day]: [clubCode, ...] } }   best first
//   teacherLists: { [clubCode]: [am, ...] }              teacher's order
//   lottery:      Map<am, number>                        from drawLottery()
//   mandatoryGrades: ["Α", ...]                          grades placed first

import { DAYS, dayIndex } from "./days.js";

/** Event names written to the audit log. */
export const EVENTS = {
  ROUND_START: "ROUND_START",
  PROPOSAL: "PROPOSAL",
  ACCEPTED: "ACCEPTED", // new proposal kept
  RETAINED: "RETAINED", // seat from an earlier round kept
  REJECTED: "REJECTED", // new proposal turned away
  DISPLACED: "DISPLACED", // seat from an earlier round lost to higher priority
  CARRIED: "CARRIED", // seat on a later day of a multi-day club
  CLUB_DROPPED: "CLUB_DROPPED", // multi-day club removed from a later-day list
  NO_MORE_PROPOSALS: "NO_MORE_PROPOSALS",
};

export const UNASSIGNED_REASONS = {
  NO_PREFERENCES: "no_preferences",
  ALL_REJECTED: "all_rejected",
};

function indexClubs(clubs) {
  const byCode = new Map();
  for (const club of clubs) {
    const code = String(club.code);
    if (byCode.has(code)) throw new Error(`Διπλός κωδικός ομίλου: ${code}`);
    if (!Array.isArray(club.days) || club.days.length === 0) {
      throw new Error(`Ο όμιλος ${code} δεν έχει ημέρες.`);
    }
    const idx = club.days.map(dayIndex);
    for (let i = 1; i < idx.length; i++) {
      if (idx[i] <= idx[i - 1]) {
        throw new Error(`Ο όμιλος ${code}: οι ημέρες πρέπει να είναι διαφορετικές και με τη σειρά της εβδομάδας.`);
      }
    }
    if (!Number.isInteger(club.capacity) || club.capacity < 0) {
      throw new Error(`Ο όμιλος ${code}: μη έγκυρη χωρητικότητα.`);
    }
    byCode.set(code, { ...club, code, grades: new Set(club.grades) });
  }
  return byCode;
}

function comparePriority(a, b) {
  return a.teacherPos - b.teacherPos || a.group - b.group || a.rank - b.rank || a.lottery - b.lottery;
}

/**
 * Deferred Acceptance for one day.
 * @param {Map<string, {code: string, capacity: number}>} dayClubs clubs in today's allocation
 * @param {Map<string, string[]>} lists am → today's list (already filtered, best first)
 * @param {(am: string, code: string, rank: number) => object} priorityOf
 * @param {(entry: object) => void} log
 * @returns {Map<string, {code: string, rank: number}>} am → seat
 */
function deferredAcceptance(dayClubs, lists, priorityOf, log) {
  const held = new Map([...dayClubs.keys()].map((code) => [code, []]));
  const next = new Map([...lists.keys()].map((am) => [am, 0]));
  let free = [...lists.keys()];

  for (let round = 1; ; round++) {
    const proposers = free.filter((am) => next.get(am) < lists.get(am).length);
    if (proposers.length === 0) {
      if (free.length > 0) {
        log({ round, event: EVENTS.NO_MORE_PROPOSALS, count: free.length });
      }
      break;
    }
    log({ round, event: EVENTS.ROUND_START, count: proposers.length });

    const incoming = new Map();
    for (const am of proposers) {
      const i = next.get(am);
      next.set(am, i + 1);
      const code = lists.get(am)[i];
      const rank = i + 1;
      log({ round, event: EVENTS.PROPOSAL, am, club: code, rank });
      if (!incoming.has(code)) incoming.set(code, []);
      incoming.get(code).push(priorityOf(am, code, rank));
    }

    const proposed = new Set(proposers);
    free = free.filter((am) => !proposed.has(am));
    for (const [code, fresh] of incoming) {
      const previous = held.get(code);
      const all = [...previous.map((c) => ({ ...c, isNew: false })), ...fresh.map((c) => ({ ...c, isNew: true }))];
      all.sort(comparePriority);
      const capacity = dayClubs.get(code).capacity;
      const kept = all.slice(0, capacity);
      const out = all.slice(capacity);
      for (const c of kept) {
        log({ round, event: c.isNew ? EVENTS.ACCEPTED : EVENTS.RETAINED, am: c.am, club: code, rank: c.rank });
      }
      for (const c of out) {
        log({ round, event: c.isNew ? EVENTS.REJECTED : EVENTS.DISPLACED, am: c.am, club: code, rank: c.rank });
        free.push(c.am);
      }
      held.set(code, kept.map(({ isNew, ...c }) => c));
    }
  }

  const seats = new Map();
  for (const [code, list] of held) {
    for (const c of list) seats.set(c.am, { code, rank: c.rank });
  }
  return seats;
}

/**
 * Run the allocation for the whole week.
 * @returns {{
 *   days: Record<string, {
 *     assignments: {am: string, club: string, rank: number|null, originalRank: number|null, via: "da"|"carried"}[],
 *     unassigned: {am: string, reason: string}[],
 *     log: object[],
 *   }>,
 *   byStudent: Record<string, Record<string, string|null>>,
 * }}
 */
export function allocateWeek({ students, clubs, preferences, teacherLists = {}, lottery, mandatoryGrades = [] }) {
  const clubsByCode = indexClubs(clubs);
  const studentsByAm = new Map();
  for (const s of students) {
    const am = String(s.am);
    if (studentsByAm.has(am)) throw new Error(`Διπλός ΑΜ: ${am}`);
    if (!lottery.has(am)) throw new Error(`Ο ΑΜ ${am} δεν έχει αριθμό κλήρωσης.`);
    studentsByAm.set(am, { ...s, am });
  }

  const teacherPos = new Map();
  for (const [code, ams] of Object.entries(teacherLists)) {
    if (!clubsByCode.has(String(code))) throw new Error(`Λίστα εκπαιδευτικού για άγνωστο όμιλο ${code}.`);
    teacherPos.set(String(code), new Map(ams.map((am, i) => [String(am), i + 1])));
  }

  // am → Map(day → code) for seats already fixed by multi-day clubs
  const committed = new Map([...studentsByAm.keys()].map((am) => [am, new Map()]));
  const byStudent = Object.fromEntries([...studentsByAm.keys()].map((am) => [am, Object.fromEntries(DAYS.map((d) => [d, null]))]));
  const result = { days: {}, byStudent };

  for (const day of DAYS) {
    const dayLog = [];
    const log = (entry) => dayLog.push({ day, ...entry });
    const assignments = [];
    const unassigned = [];

    // Clubs whose allocation happens today = clubs whose first day is today.
    const dayClubs = new Map([...clubsByCode].filter(([, c]) => c.days[0] === day));

    // Seats carried over from the first day of multi-day clubs.
    for (const [am, days] of committed) {
      const code = days.get(day);
      if (code === undefined) continue;
      log({ round: 0, event: EVENTS.CARRIED, am, club: code });
      assignments.push({ am, club: code, rank: null, originalRank: null, via: "carried" });
      byStudent[am][day] = code;
    }

    // Today's lists for everyone not already seated today.
    const lists = new Map();
    for (const [am, student] of studentsByAm) {
      if (committed.get(am).has(day)) continue;
      const original = (preferences[am]?.[day] ?? []).map(String);
      if (new Set(original).size !== original.length) {
        throw new Error(`Ο ΑΜ ${am} έχει τον ίδιο όμιλο δύο φορές την ημέρα ${day}.`);
      }
      const list = [];
      for (const code of original) {
        const club = clubsByCode.get(code);
        if (!club) throw new Error(`Ο ΑΜ ${am} δήλωσε άγνωστο όμιλο ${code}.`);
        if (!club.days.includes(day)) throw new Error(`Ο όμιλος ${code} δεν γίνεται την ημέρα ${day} (ΑΜ ${am}).`);
        if (!club.grades.has(student.grade)) {
          throw new Error(`Ο όμιλος ${code} δεν απευθύνεται στην τάξη ${student.grade} (ΑΜ ${am}).`);
        }
        if (club.days[0] !== day) {
          // Later day of a multi-day club: its allocation already happened.
          log({ round: 0, event: EVENTS.CLUB_DROPPED, am, club: code, reason: "later_day" });
          continue;
        }
        if (club.days.some((d) => committed.get(am).has(d))) {
          // The club would clash with a seat the student already holds.
          log({ round: 0, event: EVENTS.CLUB_DROPPED, am, club: code, reason: "day_conflict" });
          continue;
        }
        list.push(code);
      }
      lists.set(am, list);
    }

    const priorityOf = (am, code, rank) => ({
      am,
      rank,
      group: mandatoryGrades.includes(studentsByAm.get(am).grade) ? 0 : 1,
      teacherPos: teacherPos.get(code)?.get(am) ?? Infinity,
      lottery: lottery.get(am),
    });
    const seats = deferredAcceptance(dayClubs, lists, priorityOf, log);

    for (const [am, list] of lists) {
      const seat = seats.get(am);
      if (!seat) {
        unassigned.push({
          am,
          reason: list.length === 0 ? UNASSIGNED_REASONS.NO_PREFERENCES : UNASSIGNED_REASONS.ALL_REJECTED,
        });
        continue;
      }
      const originalRank = (preferences[am]?.[day] ?? []).map(String).indexOf(seat.code) + 1;
      assignments.push({ am, club: seat.code, rank: seat.rank, originalRank, via: "da" });
      byStudent[am][day] = seat.code;
      for (const d of clubsByCode.get(seat.code).days.slice(1)) committed.get(am).set(d, seat.code);
    }

    result.days[day] = { assignments, unassigned, log: dayLog };
  }
  return result;
}
