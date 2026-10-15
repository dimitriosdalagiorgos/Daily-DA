// «Επαλήθευση κατανομής»: the allocation laid out so that a person can check
// it by hand, as the R scripts' reports and the simulation pages do for a
// small example. Sheets:
//   - Οδηγίες: seed, legend, the automatic checks and how to check by hand
//   - Βάσεις: per club, on the day its seats are decided, the last student
//     admitted and the best one left out (like university entry cut-offs)
//   - Μαθητές: per student and day, every choice and what became of it,
//     the rounds, and the check «no club above the seat would have taken them»
//   - Γύροι: per day, round and club: who held a seat, who applied, who stays
//     and who leaves, with priorities — the paper-and-pencil replay
//   - Κλήρωση: the lottery, by number
//
// The checks are computed from the inputs and the results only, not with
// allocate.js: the student's list for the day is rebuilt from the rules
// (multi-day clubs, exclusions, similar clubs) and each seat is tested for
// stability (no student prefers a club that had room for them or admitted a
// student of lower priority). So they are an independent check.
// Pure: input of the allocation + stored results + log.

import { DAYS, DAY_LABELS } from "../algorithm/days.js";

/** Why a club left a student's list for the day (log reason CLUB_DROPPED). */
export const DROP_REASONS = {
  later_day: "κρίθηκε την 1η ημέρα του ομίλου",
  excluded: "αποκλεισμός από τον εκπαιδευτικό",
  day_conflict: "σύγκρουση με όμιλο πολλών ημερών",
  similar: "έχει ήδη παρεμφερή όμιλο",
};

/**
 * A club's priority over students, as in allocate.js: teacher's list (⭐),
 * then grades with mandatory placement, then the lottery (lower first).
 * The ⭐ students' order matters only when a list is longer than the club's
 * capacity (a later clubs file may lower it); only then is it shown.
 * @returns {{ key: (am: string, code: string) => object, compare: (a: object, b: object) => number,
 *            text: (k: object) => string, category: (k: object) => string }}
 */
export function priorities({ students, clubs = [], teacherLists = {}, mandatoryGrades = [], lottery }) {
  const gradeOf = new Map(students.map((s) => [String(s.am), s.grade]));
  const pos = new Map(Object.entries(teacherLists).map(([code, ams]) => [String(code), new Map(ams.map((am, i) => [String(am), i + 1]))]));
  const capacity = new Map(clubs.map((c) => [String(c.code), c.capacity]));
  const ordered = new Set(Object.entries(teacherLists).filter(([code, ams]) => capacity.has(String(code)) && ams.length > capacity.get(String(code))).map(([code]) => String(code)));
  const mandatory = (am) => mandatoryGrades.includes(gradeOf.get(am));
  // When all grades (or none) are mandatory, the grade decides nothing: not shown
  const gradeMatters = students.some((s) => mandatory(String(s.am))) && students.some((s) => !mandatory(String(s.am)));
  const key = (am, code) => {
    const p = pos.get(String(code))?.get(String(am));
    // Within the capacity every ⭐ gets in: their order is left out, so the lottery orders them
    const inOrder = ordered.has(String(code));
    return { am: String(am), star: p !== undefined, pos: p === undefined ? Infinity : inOrder ? p : 1, ordered: inOrder, mandatory: mandatory(String(am)), lottery: lottery.get(String(am)) };
  };
  // Infinity - Infinity is NaN, which || skips like 0
  const compare = (a, b) => a.pos - b.pos || (a.mandatory === b.mandatory ? 0 : a.mandatory ? -1 : 1) || a.lottery - b.lottery;
  const star = (k) => (k.ordered ? `⭐ ${k.pos}ος στη λίστα` : "⭐");
  const text = (k) => [k.star ? star(k) : null, gradeMatters && k.mandatory ? "υποχρ." : null, `κλ. ${k.lottery}`].filter(Boolean).join(" · ");
  const category = (k) => (k.star ? (k.ordered ? `⭐ ${k.pos}ος στη λίστα εκπαιδευτικού` : "⭐ λίστα εκπαιδευτικού") : gradeMatters && k.mandatory ? "τάξη με υποχρεωτική ένταξη" : "—");
  return { key, compare, text, category };
}

const ordinal = (n) => `${n}η`;

/**
 * @param {{ students: {am: string, grade: string, surname?: string, name?: string}[], clubs: object[],
 *           preferences: Record<string, Record<string, string[]>>, teacherLists?: Record<string, string[]>,
 *           exclusions?: Record<string, string[]>, lottery: Map<string, number>, mandatoryGrades?: string[] }} input
 * @param {{ seed: string, at?: string, byStudent: Record<string, Record<string, string|null>> }} results
 * @param {Record<string, object[]>} logByDay
 * @returns {{ problems: number, checks: {label: string, problems: string[]}[], sheets: {name: string, rows: any[][], cols: number[], filter?: boolean}[] }}
 */
export function buildVerification(input, results, logByDay = {}) {
  const { students, clubs, preferences, exclusions = {}, lottery } = input;
  const prio = priorities(input);
  const clubBy = new Map(clubs.map((c) => [String(c.code), { ...c, code: String(c.code) }]));
  const nameOf = (code) => clubBy.get(String(code))?.name ?? String(code);
  const studentBy = new Map(students.map((s) => [String(s.am), s]));
  const seatOf = (am, day) => results.byStudent[am]?.[day] ?? null;
  const prefs = (am, day) => (preferences[am]?.[day] ?? []).map(String);
  const excluded = new Map(Object.entries(exclusions).map(([code, ams]) => [String(code), new Set(ams.map(String))]));
  const sorted = [...students].sort((a, b) => String(a.grade).localeCompare(String(b.grade)) || String(a.surname ?? "").localeCompare(String(b.surname ?? ""), "el") || String(a.name ?? "").localeCompare(String(b.name ?? ""), "el") || String(a.am).localeCompare(String(b.am)));

  const checks = {
    capacity: { label: "Κανένας όμιλος δεν έχει περισσότερους μαθητές από τις θέσεις του", problems: [] },
    excluded: { label: "Κανένας μαθητής με ⛔ δεν μπήκε στον όμιλο που τον αποκλείει", problems: [] },
    multiDay: { label: "Οι όμιλοι πολλών ημερών έχουν τους ίδιους μαθητές όλες τις ημέρες τους", problems: [] },
    similar: { label: "Κανένας μαθητής δεν έχει δύο παρεμφερείς ομίλους", problems: [] },
    listed: { label: "Κάθε μαθητής μπήκε σε όμιλο της λίστας του για εκείνη την ημέρα", problems: [] },
    dropped: { label: "Οι όμιλοι που βγήκαν από τις λίστες της ημέρας είναι αυτοί που ορίζουν οι κανόνες (ίδιοι με το audit log)", problems: [] },
    stable: { label: "Κανένας μαθητής δεν αδικήθηκε: κάθε όμιλος που έβαλε πιο ψηλά ήταν γεμάτος με μαθητές καλύτερης προτεραιότητας", problems: [] },
  };
  const who = (am) => `ΑΜ ${am}`;

  // --- Seats per day and club, and the rules that hold across days
  const seated = new Map(); // `${day}|${code}` → [am]
  for (const s of students) {
    const am = String(s.am);
    for (const day of DAYS) {
      const code = seatOf(am, day);
      if (code === null) continue;
      const k = `${day}|${code}`;
      if (!seated.has(k)) seated.set(k, []);
      seated.get(k).push(am);
      const club = clubBy.get(code);
      if (!club) continue;
      if (excluded.get(code)?.has(am)) checks.excluded.problems.push(`${who(am)}: ${DAY_LABELS[day]}, «${club.name}»`);
      if (day === club.days[0]) {
        for (const d of club.days.slice(1)) {
          if (seatOf(am, d) !== code) checks.multiDay.problems.push(`${who(am)}: «${club.name}» τη ${DAY_LABELS[club.days[0]]} αλλά όχι τη ${DAY_LABELS[d]}`);
        }
      } else if (seatOf(am, club.days[0]) !== code) {
        checks.multiDay.problems.push(`${who(am)}: «${club.name}» τη ${DAY_LABELS[day]} χωρίς την 1η του ημέρα`);
      }
    }
    const groups = new Map();
    for (const code of new Set(DAYS.map((d) => seatOf(am, d)).filter((c) => c !== null))) {
      const word = clubBy.get(code)?.similar;
      if (!word) continue;
      if (groups.has(word)) checks.similar.problems.push(`${who(am)}: «${nameOf(groups.get(word))}» και «${nameOf(code)}»`);
      else groups.set(word, code);
    }
  }
  for (const club of clubBy.values()) {
    for (const day of club.days) {
      const n = seated.get(`${day}|${club.code}`)?.length ?? 0;
      if (n > club.capacity) checks.capacity.problems.push(`«${club.name}» ${DAY_LABELS[day]}: ${n} μαθητές, ${club.capacity} θέσεις`);
    }
  }

  // --- Cut-offs: per club, on its first day
  const lastIn = new Map(); // code → key of the lowest-priority student admitted
  const full = new Map();
  for (const club of clubBy.values()) {
    const ams = seated.get(`${club.days[0]}|${club.code}`) ?? [];
    const keys = ams.map((am) => prio.key(am, club.code)).sort(prio.compare);
    lastIn.set(club.code, keys.at(-1) ?? null);
    full.set(club.code, ams.length >= club.capacity);
  }

  // --- The log, read per day: rounds per club, and each student's path
  const proposers = new Map(); // `${day}|${code}` → Set of am
  const rounds = []; // rows of «Γύροι»
  const paths = new Map(); // `${am}|${day}` → [text]
  const dropped = new Map(); // `${am}|${day}|${code}` → reason (as logged)
  for (const day of DAYS) {
    const prefsRank = (am, code) => prefs(am, day).indexOf(String(code)) + 1;
    const byRound = new Map(); // round → Map(code → {held, fresh, stay, out})
    for (const e of logByDay[day] ?? []) {
      const am = e.am === undefined ? undefined : String(e.am);
      const code = e.club === undefined ? undefined : String(e.club);
      if (e.event === "CLUB_DROPPED") { dropped.set(`${am}|${day}|${code}`, e.reason); continue; }
      if (!am || code === undefined || !e.round) continue;
      if (!byRound.has(e.round)) byRound.set(e.round, new Map());
      const r = byRound.get(e.round);
      if (!r.has(code)) r.set(code, { held: [], fresh: [], stay: [], out: [] });
      const c = r.get(code);
      const path = (paths.get(`${am}|${day}`) ?? paths.set(`${am}|${day}`, []).get(`${am}|${day}`));
      switch (e.event) {
        case "PROPOSAL":
          c.fresh.push(am);
          if (!proposers.has(`${day}|${code}`)) proposers.set(`${day}|${code}`, new Set());
          proposers.get(`${day}|${code}`).add(am);
          path.push(`Γ${e.round}: αίτηση στον όμιλο «${nameOf(code)}» (${ordinal(prefsRank(am, code))} επιλογή)`);
          break;
        case "ACCEPTED": c.stay.push(am); path[path.length - 1] += " → προσωρινή θέση"; break;
        case "REJECTED": c.out.push(am); path[path.length - 1] += " → δεν χώρεσε"; break;
        case "RETAINED": c.held.push(am); c.stay.push(am); break;
        case "DISPLACED": c.held.push(am); c.out.push(am); path.push(`Γ${e.round}: έχασε τη θέση στον όμιλο «${nameOf(code)}» από μαθητή καλύτερης προτεραιότητας`); break;
      }
    }
    const token = (code, withRank) => (am) => `${am} (${prio.text(prio.key(am, code))}${withRank ? `, ${ordinal(prefsRank(am, code))} επιλ.` : ""})`;
    const ordered = (code, ams) => ams.map((am) => prio.key(am, code)).sort(prio.compare).map((k) => k.am);
    for (const [round, byClub] of [...byRound].sort((a, b) => a[0] - b[0])) {
      for (const [code, c] of [...byClub].sort((a, b) => nameOf(a[0]).localeCompare(nameOf(b[0]), "el"))) {
        rounds.push([DAY_LABELS[day], round, code, nameOf(code), clubBy.get(code)?.capacity ?? "",
          ordered(code, c.held).map(token(code)).join(", "),
          ordered(code, c.fresh).map(token(code, true)).join(", "),
          ordered(code, c.stay).map(token(code)).join(", "),
          ordered(code, c.out).map(token(code)).join(", ")]);
      }
    }
  }

  // --- «Μαθητές»: one row per student and day with a list (or a carried seat)
  const groupsBefore = (am, day) => {
    const words = new Set();
    for (const d of DAYS.slice(0, DAYS.indexOf(day))) {
      const word = clubBy.get(seatOf(am, d))?.similar;
      if (word) words.add(word);
    }
    return words;
  };
  /** The seat on a later day of a multi-day club (decided on its first day), or null */
  const carriedOn = (am, day) => {
    const code = seatOf(am, day);
    const club = clubBy.get(code);
    return club && club.days[0] !== day ? code : null;
  };
  const studentRows = [];
  let widest = 0;
  for (const s of sorted) {
    const am = String(s.am);
    for (const day of DAYS) {
      const list = prefs(am, day);
      const carried = carriedOn(am, day);
      if (!list.length && !carried) continue;
      const seat = seatOf(am, day);
      const cells = [];
      const problems = [];
      if (carried) {
        for (const code of list) cells.push(code === carried ? `✔ ${nameOf(code)}: θέση από ${DAY_LABELS[clubBy.get(code).days[0]]}` : `${nameOf(code)}: δεν χρειάστηκε`);
      } else {
        // The day's list, rebuilt from the rules (as allocate.js applies them)
        const taken = groupsBefore(am, day);
        // Days already held by a multi-day club decided on an earlier day
        const committed = new Set(DAYS.filter((d) => {
          const c = clubBy.get(seatOf(am, d));
          return c && c.days[0] !== d && DAYS.indexOf(c.days[0]) < DAYS.indexOf(day);
        }));
        const effective = [];
        for (const code of list) {
          const club = clubBy.get(code);
          let reason = null;
          if (!club) reason = "άγνωστος όμιλος";
          else if (club.days[0] !== day) reason = "later_day";
          else if (excluded.get(code)?.has(am)) reason = "excluded";
          else if (club.days.some((d) => committed.has(d))) reason = "day_conflict";
          else if (club.similar && taken.has(club.similar)) reason = "similar";
          const logged = dropped.get(`${am}|${day}|${code}`) ?? null;
          if (logged !== reason) checks.dropped.problems.push(`${who(am)} ${DAY_LABELS[day]}: «${nameOf(code)}» (κανόνες: ${DROP_REASONS[reason] ?? reason ?? "μένει"}, audit log: ${DROP_REASONS[logged] ?? logged ?? "μένει"})`);
          if (reason) cells.push({ code, reason });
          else { cells.push({ code }); effective.push(code); }
        }
        const seatAt = seat === null ? effective.length : effective.indexOf(seat);
        if (seat !== null && seatAt < 0) {
          problems.push(`μπήκε στον όμιλο «${nameOf(seat)}», που δεν είναι στη λίστα της ημέρας`);
          checks.listed.problems.push(`${who(am)} ${DAY_LABELS[day]}: «${nameOf(seat)}»`);
        }
        for (let i = 0; i < cells.length; i++) {
          const { code, reason } = cells[i];
          if (reason) { cells[i] = `${nameOf(code)}: αφαιρέθηκε (${DROP_REASONS[reason] ?? reason})`; continue; }
          const at = effective.indexOf(code);
          if (code === seat) { cells[i] = `✔ ${nameOf(code)}: ΘΕΣΗ`; continue; }
          if (seatAt >= 0 && at > seatAt) { cells[i] = `${nameOf(code)}: δεν χρειάστηκε`; continue; }
          // A club the student preferred to the seat: it must be full with
          // students of better priority (stability)
          const mine = prio.key(am, code);
          const last = lastIn.get(code);
          const fair = full.get(code) && last && prio.compare(last, mine) < 0;
          const detail = last ? `τελευταίος που μπήκε: ${prio.text(last)} · μαθητής: ${prio.text(mine)}` : `κανείς δεν μπήκε · μαθητής: ${prio.text(mine)}`;
          if (fair) cells[i] = `✗ ${nameOf(code)}: γεμάτος (${detail})`;
          else {
            const why = !full.get(code) ? "είχε κενή θέση" : "είχε καλύτερη προτεραιότητα από τον τελευταίο που μπήκε";
            cells[i] = `⚠ ${nameOf(code)}: ${why} (${detail})`;
            problems.push(`«${nameOf(code)}» ${why}`);
            checks.stable.problems.push(`${who(am)} ${DAY_LABELS[day]}: «${nameOf(code)}» ${why}`);
          }
        }
      }
      widest = Math.max(widest, cells.length);
      const rank = seat === null ? null : list.indexOf(seat) + 1;
      const outcome = carried ? `${nameOf(carried)} (από ${DAY_LABELS[clubBy.get(carried).days[0]]})` : seat === null ? "— χωρίς όμιλο" : `${nameOf(seat)} (${ordinal(rank)} επιλογή)`;
      const path = carried
        ? `Όμιλος πολλών ημερών: η θέση κρίθηκε τη ${DAY_LABELS[clubBy.get(carried).days[0]]}`
        : [...(paths.get(`${am}|${day}`) ?? []), seat === null ? "Τέλος: χωρίς όμιλο" : `Τέλος: οριστική θέση στον όμιλο «${nameOf(seat)}»`].join(" · ");
      studentRows.push({ cells, row: [am, s.surname ?? "", s.name ?? "", s.grade, lottery.get(am), DAY_LABELS[day], outcome, problems.length ? `⚠ ${problems.join("· ")}` : "✓"], path });
    }
  }

  // --- «Βάσεις»: per club on its first day
  const cutRows = [];
  for (const day of DAYS) {
    for (const club of [...clubBy.values()].filter((c) => c.days[0] === day).sort((a, b) => a.name.localeCompare(b.name, "el"))) {
      const ams = seated.get(`${day}|${club.code}`) ?? [];
      const applied = proposers.get(`${day}|${club.code}`) ?? new Set();
      const outKeys = [...applied].filter((am) => !ams.includes(am)).map((am) => prio.key(am, club.code)).sort(prio.compare);
      const last = lastIn.get(club.code);
      const first = outKeys[0] ?? null;
      const ok = ams.length <= club.capacity && (!first || (full.get(club.code) && last && prio.compare(last, first) < 0));
      cutRows.push([DAY_LABELS[day], club.code, club.name, club.days.map((d) => DAY_LABELS[d]).join(", "), club.capacity, applied.size, ams.length,
        full.get(club.code) ? "Ναι" : "Όχι",
        last && full.get(club.code) ? last.am : "", last && full.get(club.code) ? prio.text(last) : full.get(club.code) ? "" : "χωράει όποιος κάνει αίτηση",
        first ? first.am : "", first ? prio.text(first) : "",
        ok ? "✓" : "⚠"]);
    }
  }

  // --- Sheets
  const list = Object.values(checks);
  const problems = list.reduce((n, c) => n + c.problems.length, 0);
  const choiceHead = Array.from({ length: widest }, (_, i) => `${ordinal(i + 1)} επιλογή`);
  const guide = [
    ["Επαλήθευση κατανομής ομίλων"],
    [],
    ["Seed κλήρωσης", results.seed ?? ""],
    ["Κατανομή", results.at ?? ""],
    [],
    ["Αυτόματοι έλεγχοι", problems === 0 ? "✓ Όλοι οι έλεγχοι πέρασαν" : `⚠ ${problems} προβλήματα`],
    ...list.flatMap((c) => [[c.problems.length ? "⚠" : "✓", c.label], ...c.problems.slice(0, 20).map((p) => ["", `   ${p}`]), ...(c.problems.length > 20 ? [["", `   … και ${c.problems.length - 20} ακόμα`]] : [])]),
    [],
    ["Προτεραιότητα ομίλου"],
    ["1.", "⭐ = ο μαθητής είναι στους προτιμώμενους του εκπαιδευτικού για τον όμιλο"],
    ["2.", "υποχρ. = τάξη με υποχρεωτική ένταξη (φαίνεται μόνο όταν δεν είναι όλες οι τάξεις υποχρεωτικές)"],
    ["3.", "κλ. = αριθμός κλήρωσης: ένας για κάθε μαθητή, ίδιος όλη την εβδομάδα· ο μικρότερος προηγείται"],
    ["", "Η σειρά των επιλογών του μαθητή δεν είναι κριτήριο προτεραιότητας· ορίζει μόνο πού και με ποια σειρά κάνει αίτηση."],
    [],
    ["Έλεγχος με το χέρι"],
    ["Βήμα 1", "Φύλλο «Βάσεις»: για κάθε όμιλο, ο τελευταίος που μπήκε (η «βάση») έχει καλύτερη προτεραιότητα από τον πρώτο που έμεινε έξω. Αν ο όμιλος δεν γέμισε, δεν έμεινε κανείς έξω."],
    ["Βήμα 2", "Φύλλο «Μαθητές»: μια γραμμή ανά μαθητή και ημέρα, με τις επιλογές του με τη σειρά που τις έβαλε ο γονέας. ✔ = η θέση του. ✗ = όμιλος που ήθελε περισσότερο, αλλά γέμισε με μαθητές καλύτερης προτεραιότητας: συγκρίνετε «τελευταίος που μπήκε» με «μαθητής». «δεν χρειάστηκε» = επιλογές μετά τη θέση του. «αφαιρέθηκε» = όμιλος που βγήκε από τη λίστα της ημέρας, με τον λόγο."],
    ["Βήμα 3", "Η στήλη «Πορεία» δείχνει τις αιτήσεις γύρο-γύρο (Γ1, Γ2, …): πού έκανε αίτηση, αν χώρεσε, αν έχασε τη θέση του αργότερα και πού πήγε μετά."],
    ["Βήμα 4", "Φύλλο «Γύροι»: ανά ημέρα, γύρο και όμιλο, ποιοι είχαν θέση, ποιοι έκαναν αίτηση, ποιοι μένουν (με σειρά προτεραιότητας) και ποιοι βγαίνουν. Όσοι βγαίνουν κάνουν αίτηση στην επόμενη επιλογή τους στον επόμενο γύρο. Έτσι ξαναπαίζεται η κατανομή με χαρτί και μολύβι."],
    ["", "Όμιλοι πολλών ημερών κρίνονται την 1η τους ημέρα· τις επόμενες ημέρες οι μαθητές τους κρατούν τη θέση και δεν κάνουν αιτήσεις."],
  ];
  const studentHead = ["ΑΜ", "Επώνυμο", "Όνομα", "Τάξη", "Κλήρωση", "Ημέρα", "Αποτέλεσμα", "Έλεγχος", ...choiceHead, "Πορεία (γύροι)"];
  const byNumber = [...lottery].sort((a, b) => a[1] - b[1]);
  return {
    problems,
    checks: list,
    sheets: [
      { name: "Οδηγίες", rows: guide, cols: [18, 110] },
      { name: "Βάσεις", filter: true, cols: [11, 9, 28, 24, 8, 9, 8, 8, 12, 22, 12, 22, 8],
        rows: [["Ημέρα", "Κωδικός", "Όμιλος", "Ημέρες ομίλου", "Θέσεις", "Αιτήσεις", "Μπήκαν", "Γέμισε", "Τελευταίος που μπήκε (ΑΜ)", "Προτεραιότητά του", "Πρώτος που έμεινε έξω (ΑΜ)", "Προτεραιότητά του", "Έλεγχος"], ...cutRows] },
      { name: "Μαθητές", filter: true, cols: [8, 16, 14, 5, 8, 11, 28, 10, ...choiceHead.map(() => 40), 120],
        rows: [studentHead, ...studentRows.map(({ row, cells, path }) => [...row, ...Array.from({ length: widest }, (_, i) => cells[i] ?? ""), path])] },
      { name: "Γύροι", filter: true, cols: [11, 6, 9, 28, 7, 50, 50, 50, 50],
        rows: [["Ημέρα", "Γύρος", "Κωδικός", "Όμιλος", "Θέσεις", "Είχαν θέση από πριν", "Νέες αιτήσεις", "Μένουν (με σειρά προτεραιότητας)", "Βγαίνουν"], ...rounds] },
      { name: "Κλήρωση", filter: true, cols: [10, 8, 18, 16, 6],
        rows: [["Αριθμός κλήρωσης", "ΑΜ", "Επώνυμο", "Όνομα", "Τάξη"], ...byNumber.map(([am, n]) => [n, am, studentBy.get(am)?.surname ?? "", studentBy.get(am)?.name ?? "", studentBy.get(am)?.grade ?? ""])] },
    ],
  };
}
