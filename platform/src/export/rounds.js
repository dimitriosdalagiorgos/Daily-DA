// The allocation round by round, for the offline page docs/gyroi-katanomis.html
// (built by scripts/build-gyroi.mjs). Pure: the page reads the R data
// package, reruns allocateWeek() and turns its log into one table per club
// and round, with a note on why each student is where they are.
//
// A round of a club lists the students it holds from earlier rounds and the
// new applications of the round, in the club's priority order (teacher's ⭐
// list, then grades with mandatory placement, then the lottery number). The
// first `capacity` stay, for now; the others are turned away and apply to
// their next club in the next round. Clubs without new applications are not
// reconsidered in a round (the log has no entries for them).

import { DAYS, DAY_LABELS } from "../algorithm/days.js";
import { decodeCsv, parseCsv } from "../import/csv.js";

const ordinal = (n) => `${n}η`;

/** Rows of a CSV file as objects keyed by the header. */
function records(text) {
  const [header, ...rows] = parseCsv(text ?? "");
  if (!header) return [];
  return rows.filter((r) => r.some((c) => c.trim() !== "")).map((r) => Object.fromEntries(header.map((h, i) => [h.trim(), (r[i] ?? "").trim()])));
}

/**
 * Files of a stored or deflated ZIP (the platform's R data package is stored).
 * @param {Uint8Array} bytes
 * @returns {Promise<Record<string, Uint8Array>>} name (without folders) → contents
 */
export async function readZip(bytes) {
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  let end = bytes.length - 22;
  while (end >= 0 && view.getUint32(end, true) !== 0x06054b50) end--;
  if (end < 0) throw new Error("Δεν είναι αρχείο zip.");
  const count = view.getUint16(end + 10, true);
  let p = view.getUint32(end + 16, true);
  const files = {};
  for (let i = 0; i < count; i++) {
    if (view.getUint32(p, true) !== 0x02014b50) throw new Error("Χαλασμένο αρχείο zip.");
    const method = view.getUint16(p + 10, true);
    const size = view.getUint32(p + 20, true);
    const nameLen = view.getUint16(p + 28, true);
    const extraLen = view.getUint16(p + 30, true);
    const commentLen = view.getUint16(p + 32, true);
    const local = view.getUint32(p + 42, true);
    const name = new TextDecoder().decode(bytes.subarray(p + 46, p + 46 + nameLen));
    p += 46 + nameLen + extraLen + commentLen;
    if (name.endsWith("/")) continue;
    const start = local + 30 + view.getUint16(local + 26, true) + view.getUint16(local + 28, true);
    const data = bytes.subarray(start, start + size);
    let content;
    if (method === 0) content = data;
    else if (method === 8) {
      const stream = new Blob([data]).stream().pipeThrough(new DecompressionStream("deflate-raw"));
      content = new Uint8Array(await new Response(stream).arrayBuffer());
    } else throw new Error(`Το αρχείο ${name} του zip έχει άγνωστη συμπίεση.`);
    files[name.split("/").pop()] = content;
  }
  return files;
}

/**
 * The R data package (src/export/rPackage.js) → input of allocateWeek().
 * @param {Record<string, string|Uint8Array>} files file name → contents
 * @param {(ams: string[], seed: string) => Map<string, number>} [drawLottery] used when lottery.csv is missing
 */
export function readRPackage(files, drawLottery) {
  const text = (name) => {
    const f = files[name];
    return f === undefined ? undefined : typeof f === "string" ? f : decodeCsv(f);
  };
  for (const name of ["students.csv", "clubs.csv", "preferences.csv"]) {
    if (text(name) === undefined) throw new Error(`Λείπει το ${name}: χρειάζεται το zip «Δεδομένα για R (.zip)» της διαχείρισης.`);
  }
  const students = records(text("students.csv")).map((r) => ({ am: r.RegistryNr, grade: r.grade, surname: r.Surname ?? "", name: r.Name ?? "" }));
  const clubs = records(text("clubs.csv")).map((r) => ({
    code: r.code, name: r.name, days: r.days.split(";").filter(Boolean), grades: r.grades.split(";").filter(Boolean),
    capacity: Number(r.capacity), ...(r.similar ? { similar: r.similar } : {}),
  }));
  const preferences = {};
  const byRank = records(text("preferences.csv")).sort((a, b) => Number(a.rank) - Number(b.rank));
  for (const r of byRank) ((preferences[r.RegistryNr] ??= {})[r.day] ??= []).push(r.club_code);
  const teacherLists = {};
  for (const r of records(text("teacher_lists.csv")).sort((a, b) => Number(a.position) - Number(b.position))) (teacherLists[r.club_code] ??= []).push(r.RegistryNr);
  const exclusions = {};
  for (const r of records(text("exclusions.csv"))) (exclusions[r.club_code] ??= []).push(r.RegistryNr);
  const mandatoryGrades = records(text("mandatory_grades.csv")).map((r) => r.grade);
  const seed = text("seed.txt")?.trim() || undefined;
  let lottery;
  if (text("lottery.csv") !== undefined) lottery = new Map(records(text("lottery.csv")).map((r) => [r.RegistryNr, Number(r.lottery_number)]));
  else if (seed && drawLottery) lottery = drawLottery(students.map((s) => s.am), seed);
  else throw new Error("Λείπουν το lottery.csv και το seed.txt: δεν υπάρχει κλήρωση.");
  return { students, clubs, preferences, teacherLists, exclusions, mandatoryGrades, lottery, seed };
}

export const DROP_REASONS = {
  later_day: "η κατανομή του γίνεται την πρώτη του ημέρα",
  excluded: "ο εκπαιδευτικός δεν δέχεται τον μαθητή (⛔)",
  day_conflict: "γίνεται και σε ημέρα όπου ο μαθητής έχει ήδη όμιλο",
  similar: "έχει ήδη παρεμφερή όμιλο σε προηγούμενη ημέρα",
};

export const UNASSIGNED_LABELS = {
  all_rejected: "δεν χώρεσε σε κανέναν όμιλο της λίστας του",
  no_preferences: "δεν έχει ομίλους να δηλώσει αυτή την ημέρα",
};

/**
 * Round-by-round view of a week's allocation.
 * @param {object} input the input of allocateWeek()
 * @param {object} result its output
 */
export function buildRounds(input, result) {
  const { students, clubs, preferences, teacherLists = {}, mandatoryGrades = [], lottery } = input;
  const student = new Map(students.map((s) => [String(s.am), s]));
  const club = new Map(clubs.map((c) => [String(c.code), c]));
  const clubName = (code) => club.get(String(code))?.name ?? String(code);
  const listed = new Map(Object.entries(teacherLists).map(([code, ams]) => [String(code), new Set(ams.map(String))]));
  const anyOptional = students.some((s) => !mandatoryGrades.includes(s.grade));
  const isMandatory = (am) => anyOptional && mandatoryGrades.includes(student.get(am)?.grade);
  const choiceOf = (am, day, code) => (preferences[am]?.[day] ?? []).map(String).indexOf(String(code)) + 1;

  const days = [];
  for (const day of DAYS) {
    const dayResult = result.days[day];
    const dayClubs = clubs.filter((c) => c.days[0] === day).map((c) => ({ code: String(c.code), name: c.name, capacity: c.capacity, days: c.days }));
    if (!dayResult || dayClubs.length === 0) continue;
    const log = dayResult.log;

    // Before the first round: seats carried from multi-day clubs, and clubs
    // that left students' lists.
    const carried = log.filter((e) => e.event === "CARRIED").map((e) => ({ am: e.am, club: e.club }));
    const dropped = log.filter((e) => e.event === "CLUB_DROPPED" && e.reason !== "later_day").map((e) => ({ am: e.am, club: e.club, reason: e.reason, text: DROP_REASONS[e.reason] ?? e.reason }));

    const proposalOf = new Map(); // `${round}|${am}` → club code
    for (const e of log) if (e.event === "PROPOSAL") proposalOf.set(`${e.round}|${e.am}`, e.club);
    const lastOutcome = new Map(); // am → { round, club, event } (latest)
    const heldSince = new Map(); // am → round of the application that got the seat

    const rounds = [];
    const maxRound = Math.max(0, ...log.map((e) => e.round));
    for (let round = 1; round <= maxRound; round++) {
      const events = log.filter((e) => e.round === round && ["ACCEPTED", "RETAINED", "REJECTED", "DISPLACED"].includes(e.event));
      if (events.length === 0) continue;
      const roundClubs = [];
      for (const c of dayClubs) {
        const own = events.filter((e) => e.club === c.code);
        if (own.length === 0) {
          // Not reconsidered: same as at the end of its last round.
          const holding = new Set();
          for (const e of log) {
            if (e.round >= round || e.club !== c.code) continue;
            if (e.event === "ACCEPTED" || e.event === "RETAINED") holding.add(e.am);
            if (e.event === "DISPLACED") holding.delete(e.am);
          }
          roundClubs.push({ ...c, reconsidered: false, held: holding.size, rows: [] });
          continue;
        }
        const prio = (am) => ({
          am,
          teacher: listed.get(c.code)?.has(am) ? 0 : 1,
          group: mandatoryGrades.includes(student.get(am)?.grade) ? 0 : 1,
          lottery: lottery.get(am),
        });
        const cand = own.map((e) => ({ ...prio(e.am), event: e.event, rank: e.rank }));
        // Same order as allocate.js (teacher's list position, then group, then lottery).
        const pos = (am) => (teacherLists[c.code] ?? []).map(String).indexOf(am);
        cand.sort((a, b) => (a.teacher - b.teacher) || (a.teacher === 0 ? pos(a.am) - pos(b.am) : 0) || a.group - b.group || a.lottery - b.lottery);
        const kept = cand.filter((x) => x.event === "ACCEPTED" || x.event === "RETAINED");
        const out = cand.filter((x) => x.event === "REJECTED" || x.event === "DISPLACED");
        const overflow = out.length > 0;
        const lastIn = kept[kept.length - 1];
        const firstOut = out[0];
        const fresh = cand.filter((x) => x.event === "ACCEPTED" || x.event === "REJECTED").length;

        const rows = cand.map((x, i) => {
          const s = student.get(x.am) ?? {};
          const parts = [];
          const prev = lastOutcome.get(x.am);
          if (x.event === "RETAINED") parts.push(`Μένει από τον γύρο ${heldSince.get(x.am)}.`);
          if (x.event === "ACCEPTED" && round > 1 && prev) {
            parts.push(`Έρχεται από τον όμιλο «${clubName(prev.club)}», όπου ${prev.event === "DISPLACED" ? "εκτοπίστηκε" : "δεν χώρεσε"} στον γύρο ${prev.round}.`);
          }
          if (x.event === "ACCEPTED" || x.event === "RETAINED") {
            if (x.teacher === 0) parts.push("⭐ Στη λίστα του εκπαιδευτικού: προηγείται όσων δεν είναι στη λίστα.");
            else if (overflow && x.group === 0 && firstOut && firstOut.group === 1 && firstOut.teacher === 1 && isMandatory(x.am)) parts.push(`Τάξη ${s.grade} (υποχρεωτική ένταξη): προηγείται των άλλων τάξεων.`);
            else if (overflow) parts.push("Χωράει με τον λαχνό του.");
            else if (x.event === "ACCEPTED") parts.push("Υπάρχει ελεύθερη θέση.");
          } else {
            if (x.event === "DISPLACED") {
              const by = kept.filter((k) => k.event === "ACCEPTED");
              parts.push(`Είχε θέση από τον γύρο ${heldSince.get(x.am)}, αλλά ${by.length === 1 ? "ήρθε 1 νέος" : `ήρθαν ${by.length} νέοι`} με μεγαλύτερη προτεραιότητα.`);
            }
            if (!lastIn) parts.push("Ο όμιλος δεν έχει θέσεις.");
            else if (lastIn.teacher < x.teacher) parts.push("Οι θέσεις πήγαν σε μαθητές της λίστας ⭐ του εκπαιδευτικού.");
            else if (lastIn.group < x.group && isMandatory(lastIn.am)) parts.push("Οι θέσεις πήγαν σε μαθητές τάξεων με υποχρεωτική ένταξη.");
            else parts.push(`Λαχνός ${x.lottery}, μετά τον ${lastIn.lottery} του τελευταίου που χωράει.`);
            const next = proposalOf.get(`${round + 1}|${x.am}`);
            parts.push(next !== undefined
              ? `Στον γύρο ${round + 1} κάνει αίτηση στον όμιλο «${clubName(next)}» (${ordinal(choiceOf(x.am, day, next))} επιλογή).`
              : "Δεν έχει άλλη επιλογή: μένει χωρίς όμιλο αυτή την ημέρα.");
          }
          return {
            pos: i + 1, am: x.am, lottery: x.lottery, name: [s.surname, s.name].filter(Boolean).join(" "), grade: s.grade ?? "",
            choice: choiceOf(x.am, day, c.code), status: x.event, starred: x.teacher === 0, mandatory: isMandatory(x.am),
            note: parts.join(" "),
          };
        });
        for (const x of cand) {
          if (x.event === "ACCEPTED") heldSince.set(x.am, round);
          if (x.event === "REJECTED" || x.event === "DISPLACED") { heldSince.delete(x.am); lastOutcome.set(x.am, { round, club: c.code, event: x.event }); }
        }
        let base = null;
        if (overflow && lastIn) {
          base = lastIn.teacher === 0 ? "λίστα ⭐ του εκπαιδευτικού"
            : isMandatory(lastIn.am) && firstOut && firstOut.group === 1 ? `τάξη με υποχρεωτική ένταξη (λαχνός ${lastIn.lottery})`
              : `λαχνός ${lastIn.lottery}${isMandatory(lastIn.am) ? " (τάξη με υποχρεωτική ένταξη)" : ""}`;
        }
        roundClubs.push({ ...c, reconsidered: true, applicants: cand.length, fresh, kept: kept.length, base, rows });
      }
      rounds.push({ round, applicants: events.filter((e) => e.event === "ACCEPTED" || e.event === "REJECTED").length, clubs: roundClubs });
    }

    const counts = Object.fromEntries(dayClubs.map((c) => [c.code, 0]));
    for (const a of dayResult.assignments) if (a.via === "da" && a.club in counts) counts[a.club]++;
    days.push({
      day, label: DAY_LABELS[day], clubs: dayClubs, carried, dropped, rounds,
      final: {
        counts,
        unassigned: dayResult.unassigned.map((u) => ({ am: u.am, reason: u.reason, text: UNASSIGNED_LABELS[u.reason] ?? u.reason })),
      },
    });
  }
  return days;
}
