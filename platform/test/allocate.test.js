import { test } from "node:test";
import assert from "node:assert/strict";
import { allocateWeek, drawLottery, seededRandom, DAYS, EVENTS, UNASSIGNED_REASONS } from "../src/algorithm/index.js";

// Lottery from an explicit order: first listed wins ties.
const lotteryOf = (...ams) => new Map(ams.map((am, i) => [am, i + 1]));
const club = (code, days, capacity, grades = ["Α", "Β", "Γ"]) => ({ code, name: `Όμιλος ${code}`, days, grades, capacity });
const seatOf = (result, day, am) => result.byStudent[am][day];
const events = (result, day, am) => result.days[day].log.filter((e) => e.am === am).map((e) => e.event);

test("equal rank: the better lottery number wins", () => {
  const r = allocateWeek({
    students: [{ am: "1", grade: "Α" }, { am: "2", grade: "Α" }],
    clubs: [club(10, ["mon"], 1), club(11, ["mon"], 5)],
    preferences: { 1: { mon: [10, 11] }, 2: { mon: [10, 11] } },
    lottery: lotteryOf("2", "1"),
  });
  assert.equal(seatOf(r, "mon", "2"), "10");
  assert.equal(seatOf(r, "mon", "1"), "11");
});

test("the student's rank gives no priority: the lottery decides (pure Gale–Shapley)", () => {
  // Student 1 ranks club 10 second but has the better lottery number;
  // student 2 ranks it first, holds it in round 1 and loses it in round 2.
  const r = allocateWeek({
    students: [{ am: "1", grade: "Α" }, { am: "2", grade: "Α" }],
    clubs: [club(10, ["mon"], 1), club(11, ["mon"], 0), club(12, ["mon"], 5)],
    preferences: { 1: { mon: [11, 10, 12] }, 2: { mon: [10, 11, 12] } },
    lottery: lotteryOf("1", "2"),
  });
  assert.equal(seatOf(r, "mon", "1"), "10");
  assert.equal(seatOf(r, "mon", "2"), "12");
  assert.deepEqual(events(r, "mon", "2"), [EVENTS.PROPOSAL, EVENTS.ACCEPTED, EVENTS.DISPLACED, EVENTS.PROPOSAL, EVENTS.REJECTED, EVENTS.PROPOSAL, EVENTS.ACCEPTED]);
});

test("Γιώργος, Σοφία and Νίκος: the best lottery number chooses first", () => {
  // The example of docs/algorithm.md: one seat in each club.
  const r = allocateWeek({
    students: [{ am: "Νίκος", grade: "Α" }, { am: "Γιώργος", grade: "Α" }, { am: "Σοφία", grade: "Α" }],
    clubs: [club("Ρομποτική", ["mon"], 1), club("Σκάκι", ["mon"], 1)],
    preferences: { "Νίκος": { mon: ["Ρομποτική", "Σκάκι"] }, "Γιώργος": { mon: ["Ρομποτική", "Σκάκι"] }, "Σοφία": { mon: ["Σκάκι", "Ρομποτική"] } },
    lottery: lotteryOf("Νίκος", "Γιώργος", "Σοφία"),
  });
  assert.equal(seatOf(r, "mon", "Νίκος"), "Ρομποτική");
  assert.equal(seatOf(r, "mon", "Γιώργος"), "Σκάκι");
  assert.equal(seatOf(r, "mon", "Σοφία"), null);
});

test("teacher's choice beats the student's rank, in the teacher's order", () => {
  const r = allocateWeek({
    students: [{ am: "1", grade: "Α" }, { am: "2", grade: "Α" }, { am: "3", grade: "Α" }],
    clubs: [club(10, ["mon"], 1), club(11, ["mon"], 5)],
    preferences: { 1: { mon: [10, 11] }, 2: { mon: [11, 10] }, 3: { mon: [10, 11] } },
    teacherLists: { 10: ["2"] },
    lottery: lotteryOf("1", "3", "2"),
  });
  // Student 2 prefers club 11 and gets it: a teacher's choice is priority,
  // not a forced placement.
  assert.equal(seatOf(r, "mon", "2"), "11");
  assert.equal(seatOf(r, "mon", "1"), "10");

  const r2 = allocateWeek({
    students: [{ am: "1", grade: "Α" }, { am: "2", grade: "Α" }],
    clubs: [club(10, ["mon"], 1), club(11, ["mon"], 5)],
    preferences: { 1: { mon: [10, 11] }, 2: { mon: [11, 10] } },
    teacherLists: { 10: ["2"] },
    lottery: lotteryOf("1", "2"),
  });
  // Club 11 has room, so the teacher's pick for club 10 is never needed.
  assert.equal(seatOf(r2, "mon", "1"), "10");
});

test("teacher's pick displaces a student held from an earlier round", () => {
  const r = allocateWeek({
    students: [{ am: "1", grade: "Α" }, { am: "2", grade: "Α" }],
    clubs: [club(10, ["mon"], 1), club(11, ["mon"], 0), club(12, ["mon"], 5)],
    preferences: { 1: { mon: [10, 12] }, 2: { mon: [11, 10, 12] } },
    teacherLists: { 10: ["2"] },
    lottery: lotteryOf("1", "2"),
  });
  assert.equal(seatOf(r, "mon", "2"), "10");
  assert.equal(seatOf(r, "mon", "1"), "12");
  assert.deepEqual(events(r, "mon", "1"), [EVENTS.PROPOSAL, EVENTS.ACCEPTED, EVENTS.DISPLACED, EVENTS.PROPOSAL, EVENTS.ACCEPTED]);
});

test("fixed lottery: a held seat is kept against an equal-rank newcomer with a worse number", () => {
  // The old R script redrew the tie-break every round, so this could flip.
  // Round 1: 1, 3, 4 → club 11 (3 wins); 2 → club 13.
  // Round 2: 1 → club 10 (rank 2, held); 4 → club 13, teacher's pick, displaces 2.
  // Round 3: 2 → club 10 (rank 2 too, worse lottery than 1) → rejected, 1 retained.
  const r = allocateWeek({
    students: ["1", "2", "3", "4"].map((am) => ({ am, grade: "Α" })),
    clubs: [club(10, ["mon"], 1), club(11, ["mon"], 1), club(12, ["mon"], 5), club(13, ["mon"], 1)],
    preferences: { 1: { mon: [11, 10, 12] }, 2: { mon: [13, 10, 12] }, 3: { mon: [11] }, 4: { mon: [11, 13] } },
    teacherLists: { 13: ["4"] },
    lottery: lotteryOf("3", "1", "2", "4"),
  });
  assert.deepEqual(r.byStudent["1"].mon, "10");
  assert.deepEqual(r.byStudent["2"].mon, "12");
  assert.deepEqual(r.byStudent["3"].mon, "11");
  assert.deepEqual(r.byStudent["4"].mon, "13");
  const round3 = r.days.mon.log.filter((e) => e.round === 3 && e.club === "10").map((e) => [e.am, e.event]);
  assert.deepEqual(round3, [["2", EVENTS.PROPOSAL], ["1", EVENTS.RETAINED], ["2", EVENTS.REJECTED]]);
});

// The example from the discussion of 27/9/2026:
// Αντιγόνη (double, Monday + Thursday), Ρομποτική (Monday), Άλγεβρα (Thursday).
test("double club: Papadopoulos placed Monday sits out Thursday; Christoforidis keeps Algebra as 1st", () => {
  const ANTIGONI = 100, ROBOTICS = 101, ALGEBRA = 102, OTHER_MON = 103, OTHER_THU = 104;
  const clubs = [
    club(ANTIGONI, ["mon", "thu"], 1),
    club(ROBOTICS, ["mon"], 5),
    club(ALGEBRA, ["thu"], 5),
    club(OTHER_MON, ["mon"], 0), // Papadopoulos' 1st choice, full
    club(OTHER_THU, ["thu"], 5),
  ];
  const r = allocateWeek({
    students: [{ am: "papadopoulos", grade: "Β" }, { am: "christoforidis", grade: "Β" }],
    clubs,
    // Lists as a parent fills them in the old way: Αντιγόνη at the same
    // position on both days.
    preferences: {
      papadopoulos: { mon: [OTHER_MON, ANTIGONI, ROBOTICS], thu: [OTHER_THU, ANTIGONI, ALGEBRA] },
      christoforidis: { mon: [ANTIGONI, ROBOTICS, OTHER_MON], thu: [ANTIGONI, ALGEBRA, OTHER_THU] },
    },
    teacherLists: { [ANTIGONI]: ["papadopoulos"] },
    lottery: lotteryOf("christoforidis", "papadopoulos"),
  });

  assert.equal(seatOf(r, "mon", "papadopoulos"), String(ANTIGONI));
  assert.equal(seatOf(r, "thu", "papadopoulos"), String(ANTIGONI));
  const papThu = r.days.thu.assignments.find((a) => a.am === "papadopoulos");
  assert.equal(papThu.via, "carried");
  assert.ok(!r.days.thu.log.some((e) => e.am === "papadopoulos" && e.event === EVENTS.PROPOSAL));

  assert.equal(seatOf(r, "mon", "christoforidis"), String(ROBOTICS));
  const chrThu = r.days.thu.assignments.find((a) => a.am === "christoforidis");
  assert.equal(chrThu.club, String(ALGEBRA));
  assert.equal(chrThu.rank, 1, "Άλγεβρα becomes his 1st choice on Thursday");
  assert.equal(chrThu.originalRank, 2);
});

test("renumbered rank is what counts as priority on the later day", () => {
  // Club 20 (Mon+Thu) was student 1's first choice on Thursday; he did not get
  // it, so club 21 moves from 2nd to 1st and ties with student 2, who ranked
  // 21 first all along. The lottery (student 1 better) decides.
  const r = allocateWeek({
    students: [{ am: "1", grade: "Α" }, { am: "2", grade: "Α" }, { am: "3", grade: "Α" }],
    clubs: [club(20, ["mon", "thu"], 1), club(22, ["mon"], 5), club(21, ["thu"], 1), club(23, ["thu"], 5)],
    preferences: {
      1: { mon: [20, 22], thu: [20, 21, 23] },
      2: { mon: [22, 20], thu: [21, 23] },
      3: { mon: [20, 22], thu: [20, 21, 23] },
    },
    lottery: lotteryOf("3", "1", "2"),
  });
  assert.equal(seatOf(r, "mon", "3"), "20");
  assert.equal(seatOf(r, "thu", "1"), "21");
  assert.equal(seatOf(r, "thu", "2"), "23");
});

test("a club's list in the new form (multi-day clubs only on their first day) gives the same result", () => {
  const base = {
    students: [{ am: "1", grade: "Α" }, { am: "2", grade: "Α" }, { am: "3", grade: "Α" }],
    clubs: [club(20, ["mon", "thu"], 1), club(22, ["mon"], 5), club(21, ["thu"], 1), club(23, ["thu"], 5)],
    lottery: lotteryOf("3", "1", "2"),
  };
  const oldForm = allocateWeek({
    ...base,
    preferences: { 1: { mon: [20, 22], thu: [20, 21, 23] }, 2: { mon: [22, 20], thu: [21, 23] }, 3: { mon: [20, 22], thu: [20, 21, 23] } },
  });
  const newForm = allocateWeek({
    ...base,
    preferences: { 1: { mon: [20, 22], thu: [21, 23] }, 2: { mon: [22, 20], thu: [21, 23] }, 3: { mon: [20, 22], thu: [21, 23] } },
  });
  assert.deepEqual(oldForm.byStudent, newForm.byStudent);
});

test("triple club: placed on Tuesday, carried to Wednesday and Friday", () => {
  const r = allocateWeek({
    students: [{ am: "1", grade: "Γ" }],
    clubs: [club(30, ["tue", "wed", "fri"], 1), club(31, ["wed"], 5), club(32, ["fri"], 5)],
    preferences: { 1: { tue: [30], wed: [31], fri: [32] } },
    lottery: lotteryOf("1"),
  });
  assert.deepEqual(r.byStudent["1"], { mon: null, tue: "30", wed: "30", thu: null, fri: "30" });
  assert.deepEqual(r.days.wed.unassigned, []);
});

test("two multi-day clubs sharing a later day never both go to one student", () => {
  // A = Mon+Thu, B = Tue+Thu. Placed in A on Monday, the student already
  // has Thursday taken, so B is dropped from Tuesday's list.
  const r = allocateWeek({
    students: [{ am: "1", grade: "Α" }],
    clubs: [club(40, ["mon", "thu"], 5), club(41, ["tue", "thu"], 5), club(42, ["tue"], 5)],
    preferences: { 1: { mon: [40], tue: [41, 42] } },
    lottery: lotteryOf("1"),
  });
  assert.equal(seatOf(r, "tue", "1"), "42");
  assert.equal(r.days.tue.assignments[0].rank, 1);
  assert.ok(r.days.tue.log.some((e) => e.event === EVENTS.CLUB_DROPPED && e.reason === "day_conflict"));
  assert.equal(seatOf(r, "thu", "1"), "40");
});

test("unassigned students carry a reason", () => {
  const r = allocateWeek({
    students: [{ am: "1", grade: "Α" }, { am: "2", grade: "Α" }, { am: "3", grade: "Α" }],
    clubs: [club(10, ["mon"], 1)],
    preferences: { 1: { mon: [10] }, 2: { mon: [10] } },
    lottery: lotteryOf("1", "2", "3"),
  });
  const reasons = Object.fromEntries(r.days.mon.unassigned.map((u) => [u.am, u.reason]));
  assert.deepEqual(reasons, { 2: UNASSIGNED_REASONS.ALL_REJECTED, 3: UNASSIGNED_REASONS.NO_PREFERENCES });
});

test("bad input is refused, not silently fixed", () => {
  const base = { students: [{ am: "1", grade: "Α" }], lottery: lotteryOf("1") };
  assert.throws(() => allocateWeek({ ...base, clubs: [club(10, ["mon"], 1, ["Β"])], preferences: { 1: { mon: [10] } } }), /τάξη/);
  assert.throws(() => allocateWeek({ ...base, clubs: [club(10, ["mon"], 1)], preferences: { 1: { mon: [99] } } }), /άγνωστο/);
  assert.throws(() => allocateWeek({ ...base, clubs: [club(10, ["mon"], 1)], preferences: { 1: { tue: [10] } } }), /δεν γίνεται/);
  assert.throws(() => allocateWeek({ ...base, clubs: [club(10, ["mon"], 1)], preferences: { 1: { mon: [10, 10] } } }), /δύο φορές/);
  assert.throws(() => allocateWeek({ ...base, clubs: [club(10, ["thu", "mon"], 1)], preferences: {} }), /σειρά/);
  assert.throws(() => allocateWeek({ ...base, clubs: [club(10, ["mon"], 1)], preferences: {}, lottery: new Map() }), /κλήρωσης/);
});

test("mandatory grades go before the others, but after the teacher's choice", () => {
  // Student 2 (Α, mandatory) ranks club 10 second, student 3 (Γ) ranks it
  // first with the better lottery number: the mandatory grade wins.
  const r = allocateWeek({
    students: [{ am: "2", grade: "Α" }, { am: "3", grade: "Γ" }],
    clubs: [club(10, ["mon"], 1), club(11, ["mon"], 0), club(12, ["mon"], 5)],
    preferences: { 2: { mon: [11, 10, 12] }, 3: { mon: [10, 11, 12] } },
    lottery: lotteryOf("3", "2"),
    mandatoryGrades: ["Α"],
  });
  assert.equal(seatOf(r, "mon", "2"), "10");
  assert.equal(seatOf(r, "mon", "3"), "12");

  // The teacher's pick (Γ) still goes before the mandatory grade.
  const t = allocateWeek({
    students: [{ am: "1", grade: "Γ" }, { am: "2", grade: "Α" }],
    clubs: [club(10, ["mon"], 1), club(12, ["mon"], 5)],
    preferences: { 1: { mon: [10, 12] }, 2: { mon: [10, 12] } },
    teacherLists: { 10: ["1"] },
    lottery: lotteryOf("2", "1"),
    mandatoryGrades: ["Α"],
  });
  assert.equal(seatOf(t, "mon", "1"), "10");
  assert.equal(seatOf(t, "mon", "2"), "12");
});

// ---------- Property test: stability on random instances ----------

function priorityKey(input, am, code, rank) {
  const pos = (input.teacherLists[code] ?? []).indexOf(am);
  const grade = input.students.find((s) => s.am === am).grade;
  return [pos < 0 ? Infinity : pos + 1, input.mandatoryGrades.includes(grade) ? 0 : 1, input.lottery.get(am)];
}
const better = (a, b) => a[0] - b[0] || a[1] - b[1] || a[2] - b[2];

function randomInstance(rng) {
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
  return { students, clubs, preferences, teacherLists, lottery, mandatoryGrades };
}

test("random instances: capacities respected, no multi-day clash, and every day's result is stable", () => {
  const rng = seededRandom("stability");
  for (let n = 0; n < 300; n++) {
    const input = randomInstance(rng);
    const r = allocateWeek(input);
    const byCode = new Map(input.clubs.map((c) => [String(c.code), c]));

    for (const day of DAYS) {
      const { assignments, log } = r.days[day];
      // capacity
      const count = new Map();
      for (const a of assignments) count.set(a.club, (count.get(a.club) ?? 0) + 1);
      for (const [code, k] of count) assert.ok(k <= byCode.get(code).capacity, `capacity ${code} on ${day}`);
      // a carried seat means the same club was assigned on the club's first day
      for (const a of assignments.filter((x) => x.via === "carried")) {
        assert.equal(r.byStudent[a.am][byCode.get(a.club).days[0]], a.club);
      }

      // Stability within today's allocation. Reconstruct each participant's
      // effective list: today's list minus dropped clubs.
      const dropped = new Set(log.filter((e) => e.event === EVENTS.CLUB_DROPPED).map((e) => `${e.am}|${e.club}`));
      const seated = new Map(assignments.filter((a) => a.via === "da").map((a) => [a.am, a]));
      const members = new Map();
      for (const a of seated.values()) {
        if (!members.has(a.club)) members.set(a.club, []);
        members.get(a.club).push(a);
      }
      const carried = new Set(assignments.filter((a) => a.via === "carried").map((a) => a.am));
      for (const s of input.students) {
        if (carried.has(s.am)) continue;
        const list = (input.preferences[s.am]?.[day] ?? []).map(String).filter((c) => !dropped.has(`${s.am}|${c}`));
        const mine = seated.get(s.am);
        const limit = mine ? mine.rank - 1 : list.length;
        for (let i = 0; i < limit; i++) {
          const code = list[i];
          const cap = byCode.get(code).capacity;
          const inClub = members.get(code) ?? [];
          if (inClub.length < cap) assert.fail(`${s.am} would take a free seat in ${code} on ${day}`);
          const myKey = priorityKey(input, s.am, code, i + 1);
          for (const m of inClub) {
            const theirKey = priorityKey(input, m.am, code, m.rank);
            assert.ok(better(theirKey, myKey) < 0, `blocking pair ${s.am}–${code} on ${day}`);
          }
        }
      }
    }

    // A student never holds two clubs on the same day (byStudent is a single
    // value per day, so check it agrees with the day's assignment lists).
    for (const day of DAYS) {
      const ams = r.days[day].assignments.map((a) => a.am);
      assert.equal(new Set(ams).size, ams.length, `double seat on ${day}`);
    }
  }
});

// ---------- The order of proposals does not matter ----------

// Every club on its first day only, and the lists cut accordingly.
function singleDays(input) {
  input.clubs = input.clubs.map((c) => ({ ...c, days: [c.days[0]] }));
  const on = new Map(input.clubs.map((c) => [String(c.code), c.days[0]]));
  for (const days of Object.values(input.preferences)) {
    for (const day of Object.keys(days)) days[day] = days[day].filter((c) => on.get(String(c)) === day);
  }
}
//
// The same result whatever the order in which applications are handled
// (McVitie & Wilson, 1970): here one application at a time, in a random
// order, against allocateWeek's simultaneous rounds.
function oneAtATime(input, day, rng) {
  const code = (c) => String(c);
  const cap = new Map(input.clubs.filter((c) => c.days.length === 1 && c.days[0] === day).map((c) => [code(c.code), c.capacity]));
  const key = (am, c) => priorityKey(input, am, c, 0);
  const lists = new Map(input.students.map((s) => [s.am, (input.preferences[s.am]?.[day] ?? []).map(code).filter((c) => cap.has(c))]));
  const next = new Map([...lists.keys()].map((am) => [am, 0]));
  const held = new Map([...cap.keys()].map((c) => [c, []]));
  const free = [...lists.keys()];
  while (true) {
    const ready = free.filter((am) => next.get(am) < lists.get(am).length);
    if (!ready.length) break;
    const am = ready[Math.floor(rng() * ready.length)];
    free.splice(free.indexOf(am), 1);
    const c = lists.get(am)[next.get(am)];
    next.set(am, next.get(am) + 1);
    const h = held.get(c);
    h.push(am);
    h.sort((x, y) => better(key(x, c), key(y, c)));
    if (h.length > cap.get(c)) free.push(h.pop());
  }
  return Object.fromEntries([...held].flatMap(([c, ams]) => ams.map((am) => [am, c])));
}

test("random instances: handling applications one at a time, in any order, gives the same result", () => {
  const rng = seededRandom("order-independence");
  for (let n = 0; n < 200; n++) {
    const input = randomInstance(rng);
    singleDays(input); // no carry-over between days
    const r = allocateWeek(input);
    for (const day of DAYS) {
      const rounds = Object.fromEntries(r.days[day].assignments.map((a) => [a.am, a.club]));
      for (let k = 0; k < 3; k++) assert.deepEqual(oneAtATime(input, day, rng), rounds, `instance ${n}, ${day}`);
    }
  }
});

test("without teacher lists and mandatory grades, DA = choosing in lottery order", () => {
  // Random serial dictatorship: by lottery number, each student takes the
  // best club on their list that still has a seat.
  const rng = seededRandom("serial-dictatorship");
  for (let n = 0; n < 200; n++) {
    const input = { ...randomInstance(rng), teacherLists: {}, mandatoryGrades: [] };
    singleDays(input);
    const r = allocateWeek(input);
    for (const day of DAYS) {
      const left = new Map(input.clubs.filter((c) => c.days[0] === day).map((c) => [String(c.code), c.capacity]));
      const got = {};
      for (const s of [...input.students].sort((a, b) => input.lottery.get(a.am) - input.lottery.get(b.am))) {
        const c = (input.preferences[s.am]?.[day] ?? []).map(String).find((x) => left.get(x) > 0);
        if (c) { left.set(c, left.get(c) - 1); got[s.am] = c; }
      }
      assert.deepEqual(Object.fromEntries(r.days[day].assignments.map((a) => [a.am, a.club])), got, `instance ${n}, ${day}`);
    }
  }
});
