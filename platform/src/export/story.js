// The allocation's audit log in plain Greek: one sentence per event, and
// the "story" of one student per day (what the R scripts wrote in
// <day>_audit_log.csv and <day>_report_<student>.txt).

import { DAYS, DAY_LABELS } from "../algorithm/days.js";

const ordinal = (n) => `${n}η`;

/** Where a student ended up on a day with no club: why. */
export const GAP_REASONS = {
  all_rejected: "δεν χώρεσε",
  no_preferences: "χωρίς προτιμήσεις",
  not_offered: "δεν υπάρχει όμιλος για την τάξη του",
};

/**
 * One event as a sentence.
 * @param {object} e an entry of allocateWeek()'s log
 * @param {(code: string) => string} nameOf club code → name
 * @param {(code: string) => string[]} daysOf club code → its days
 */
export function describeEvent(e, nameOf, daysOf = () => []) {
  const club = e.club !== undefined ? `«${nameOf(e.club)}»` : "";
  switch (e.event) {
    case "ROUND_START":
      return `Γύρος ${e.round}: ${e.count} μαθητές κάνουν αίτηση.`;
    case "PROPOSAL":
      return `Αίτηση στον όμιλο ${club} (${ordinal(e.rank)} επιλογή).`;
    case "ACCEPTED":
      return `Προσωρινή θέση στον όμιλο ${club}.`;
    case "RETAINED":
      return `Κράτησε τη θέση στον όμιλο ${club}: έκαναν αίτηση και άλλοι μαθητές, αλλά είχε υψηλότερη προτεραιότητα από όσους δεν χώρεσαν.`;
    case "REJECTED":
      return `Δεν χώρεσε στον όμιλο ${club}: γέμισε με μαθητές υψηλότερης προτεραιότητας (λίστα εκπαιδευτικού, τάξη με υποχρεωτική ένταξη ή καλύτερος αριθμός κλήρωσης).`;
    case "DISPLACED":
      return `Έχασε τη θέση στον όμιλο ${club} από μαθητή υψηλότερης προτεραιότητας (λίστα εκπαιδευτικού, τάξη με υποχρεωτική ένταξη ή καλύτερος αριθμός κλήρωσης) που έκανε αίτηση αργότερα.`;
    case "CARRIED": {
      const first = daysOf(e.club)[0];
      return `Θέση στον όμιλο ${club}, γιατί τοποθετήθηκε σε αυτόν ${first ? `τη ${DAY_LABELS[first]}` : "νωρίτερα"} (όμιλος πολλών ημερών).`;
    }
    case "CLUB_DROPPED":
      if (e.reason === "day_conflict") return `Ο όμιλος ${club} αφαιρέθηκε από τη λίστα: γίνεται και σε ημέρα όπου έχει ήδη όμιλο.`;
      if (e.reason === "similar") return `Ο όμιλος ${club} αφαιρέθηκε από τη λίστα: έχει ήδη παρεμφερή όμιλο σε προηγούμενη ημέρα.`;
      return `Ο όμιλος ${club} αφαιρέθηκε από τη λίστα αυτής της ημέρας: η κατανομή του έγινε την πρώτη του ημέρα.`;
    case "FINAL":
      return `Η θέση στον όμιλο ${club} έγινε οριστική: η κατανομή της ημέρας τελείωσε και κανείς δεν την πήρε.`;
    case "NO_MORE_PROPOSALS":
      return `Τέλος: ${e.count} μαθητές δεν έχουν άλλους ομίλους για αίτηση.`;
    default:
      return e.event;
  }
}

/**
 * Per-student story from the whole week's log. The log records a held seat
 * only when the club is reconsidered (new applicants); the story adds the
 * end of the day — a final step for whoever still holds a seat — and says
 * «Κράτησε τη θέση» once for several rounds in a row.
 * @returns {Record<string, Record<string, {round: number, event: string, club?: string, rank?: number, text: string}[]>>}
 *   am → day → events
 */
export function storiesByStudent(logByDay, nameOf, daysOf) {
  const out = {};
  const entry = (e) => ({
    round: e.round, event: e.event, ...(e.club !== undefined ? { club: e.club } : {}), ...(e.rank !== undefined ? { rank: e.rank } : {}),
    text: describeEvent(e, nameOf, daysOf),
  });
  for (const day of DAYS) {
    let lastRound = 0;
    for (const e of logByDay[day] ?? []) {
      lastRound = Math.max(lastRound, e.round ?? 0);
      if (!e.am) continue;
      const story = ((out[e.am] ??= {})[day] ??= []);
      const prev = story.at(-1);
      if (e.event === "RETAINED" && prev?.event === "RETAINED" && prev.club === e.club) continue;
      story.push(entry(e));
    }
    for (const byDay of Object.values(out)) {
      const last = byDay[day]?.at(-1);
      if (last && (last.event === "ACCEPTED" || last.event === "RETAINED")) byDay[day].push(entry({ round: lastRound, event: "FINAL", club: last.club }));
    }
  }
  return out;
}

/**
 * Why each student has no club on a day: all_rejected / no_preferences /
 * not_offered (no club for their grade that day).
 * @returns {Record<string, Record<string, string>>} am → day → reason
 */
export function gapReasons(results, students, clubs) {
  const offered = (day, grade) => clubs.some((c) => c.days.includes(day) && c.grades.includes(grade));
  const reasons = {};
  for (const day of DAYS) {
    const unassigned = new Map((results.days[day]?.unassigned ?? []).map((u) => [u.am, u.reason]));
    for (const s of students) {
      if (results.byStudent[s.am]?.[day]) continue;
      const reason = !offered(day, s.grade) ? "not_offered" : unassigned.get(s.am) ?? "no_preferences";
      (reasons[s.am] ??= {})[day] = reason;
    }
  }
  return reasons;
}
