// How much the lottery rule matters, on a real data package. Reads an R data
// package (the platform's «Δεδομένα για R (.zip)», as a .zip or unzipped
// into a folder), reruns the week with many seeds under each rule and
// prints aggregate counts only: no registry numbers or names, so the output
// can be shared. Nothing is sent anywhere.
//
//   node platform/scripts/lottery-compare.mjs <folder or .zip> [--runs N] [--json <file>]
//
// Rules compared (the platform uses the first):
//   weekly       one lottery number per student for the whole week
//   daily        a new, independent number each day
//   alternating  one number, with the order reversed on Tue and Thu
//                (number n becomes N+1−n), as in "snake" drafts
// None of the numbers depends on what students declared, so each day's
// Deferred Acceptance stays stable and strategy-proof under every rule.
//
// --runs  seeds per rule (default 200). Seeds are `<package seed>#<i>`; the
//         daily rule draws `<seed>|<day>` for each day. Reproducible.
// --json  also writes the full aggregate report (for the written study):
//         distributions with 90% ranges, rank distributions, inequality
//         indices, results by grade, correlation between days, each
//         student's risk across runs (as a histogram over students), and
//         demand per club. Still no registry numbers or names.
//
// Per student, a "decision" is a day the student takes part in that day's
// allocation with a non-empty list (multi-day clubs count once, on their
// first day). A decision is a "1st choice" when the student gets the first
// club of the list that reached the allocation (clubs removed beforehand,
// e.g. ⛔ or a similar club already held, are not lottery losses), and a
// "miss" otherwise (a lower club, or none).

import { writeFileSync } from "node:fs";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { DAYS } from "../src/algorithm/days.js";
import { drawLottery } from "../src/algorithm/lottery.js";
import { allocateWeek } from "../src/algorithm/allocate.js";
import { readRPackage } from "../src/export/rounds.js";
import { readPackage } from "./propose.mjs";

export const RULES = {
  weekly: (ams, seed) => ({ lottery: drawLottery(ams, seed) }),
  daily: (ams, seed) => {
    const lotteryByDay = Object.fromEntries(DAYS.map((d) => [d, drawLottery(ams, `${seed}|${d}`)]));
    return { lottery: lotteryByDay.mon, lotteryByDay };
  },
  alternating: (ams, seed) => {
    const lottery = drawLottery(ams, seed);
    const reversed = new Map([...lottery].map(([am, n]) => [am, ams.length + 1 - n]));
    return { lottery, lotteryByDay: Object.fromEntries(DAYS.map((d, i) => [d, i % 2 ? reversed : lottery])) };
  },
};

const RULE_LABELS = { actual: "Πραγματική", weekly: "Ένας αριθμός", daily: "Ανά ημέρα", alternating: "Αντιστροφή" };
const RANK_KEYS = ["1", "2", "3", "4", "5", "6+", "none"];

/** Per-student outcome of one allocation, plus decision-level counts. */
export function measure(result, gradeOf = () => "") {
  const perStudent = new Map();
  const ranks = Object.fromEntries(RANK_KEYS.map((k) => [k, 0]));
  const byDay = Object.fromEntries(DAYS.map((d) => [d, { decisions: 0, firsts: 0 }]));
  const get = (am) => {
    if (!perStudent.has(am)) perStudent.set(am, { decisions: 0, misses: 0, unplaced: 0, rankLoss: 0, hits: {} });
    return perStudent.get(am);
  };
  for (const day of DAYS) {
    for (const a of result.days[day].assignments) {
      if (a.via !== "da") continue;
      const s = get(a.am);
      s.decisions++;
      s.rankLoss += a.rank - 1;
      s.hits[day] = a.rank === 1 ? 1 : 0;
      if (a.rank !== 1) s.misses++;
      ranks[a.rank >= 6 ? "6+" : String(a.rank)]++;
      byDay[day].decisions++;
      if (a.rank === 1) byDay[day].firsts++;
    }
    for (const u of result.days[day].unassigned) {
      if (u.reason !== "all_rejected") continue;
      const s = get(u.am);
      s.decisions++;
      s.misses++;
      s.unplaced++;
      s.hits[day] = 0;
      ranks.none++;
      byDay[day].decisions++;
    }
  }
  return summarizeRun(perStudent, ranks, byDay, gradeOf);
}

function gini(values) {
  const v = [...values].sort((a, b) => a - b);
  const n = v.length;
  const sum = v.reduce((x, y) => x + y, 0);
  if (!n || !sum) return 0;
  let acc = 0;
  v.forEach((x, i) => { acc += (2 * (i + 1) - n - 1) * x; });
  return acc / (n * sum);
}

function pearson(pairs) {
  const n = pairs.length;
  if (n < 3) return null;
  const mx = pairs.reduce((s, [x]) => s + x, 0) / n;
  const my = pairs.reduce((s, [, y]) => s + y, 0) / n;
  let sxy = 0, sxx = 0, syy = 0;
  for (const [x, y] of pairs) { sxy += (x - mx) * (y - my); sxx += (x - mx) ** 2; syy += (y - my) ** 2; }
  return sxx && syy ? sxy / Math.sqrt(sxx * syy) : null;
}

function summarizeRun(perStudent, ranks, byDay, gradeOf) {
  const students = [...perStudent.entries()];
  const several = students.filter(([, s]) => s.decisions >= 2);
  const missHistogram = [0, 0, 0, 0, 0, 0];
  for (const [, s] of students) missHistogram[Math.min(s.misses, 5)]++;
  const misses = students.map(([, s]) => s.misses);
  const mean = misses.reduce((x, y) => x + y, 0) / (misses.length || 1);

  // Correlation of "got the 1st choice" between pairs of days
  let corrSum = 0, corrWeight = 0;
  for (let i = 0; i < DAYS.length; i++) {
    for (let j = i + 1; j < DAYS.length; j++) {
      const pairs = students.filter(([, s]) => s.hits[DAYS[i]] !== undefined && s.hits[DAYS[j]] !== undefined)
        .map(([, s]) => [s.hits[DAYS[i]], s.hits[DAYS[j]]]);
      const r = pearson(pairs);
      if (r !== null) { corrSum += r * pairs.length; corrWeight += pairs.length; }
    }
  }

  const byGrade = {};
  for (const [am, s] of students) {
    const g = (byGrade[gradeOf(am)] ??= { students: 0, misses: 0, threePlus: 0, perfect: 0, several: 0 });
    g.students++;
    g.misses += s.misses;
    if (s.misses >= 3) g.threePlus++;
    if (s.decisions >= 2) { g.several++; if (s.misses === 0) g.perfect++; }
  }

  return {
    perStudent,
    students: students.length,
    several: several.length,
    decisions: students.reduce((x, [, s]) => x + s.decisions, 0),
    firsts: ranks["1"],
    meanRank: (() => {
      const placed = RANK_KEYS.slice(0, 6).reduce((x, k) => x + ranks[k], 0);
      return placed ? (students.reduce((x, [, s]) => x + s.rankLoss, 0) + placed) / placed : 0;
    })(),
    unplaced: ranks.none,
    noFirstAtAll: several.filter(([, s]) => s.misses === s.decisions).length,
    firstEveryDay: several.filter(([, s]) => s.misses === 0).length,
    missedAtLeast3: students.filter(([, s]) => s.misses >= 3).length,
    missedAtLeast4: students.filter(([, s]) => s.misses >= 4).length,
    unplacedAtLeast2: students.filter(([, s]) => s.unplaced >= 2).length,
    totalMisses: misses.reduce((x, y) => x + y, 0),
    squaredMisses: misses.reduce((x, y) => x + y * y, 0),
    rankLoss: students.reduce((x, [, s]) => x + s.rankLoss, 0),
    worstMisses: Math.max(0, ...misses),
    sdMisses: Math.sqrt(misses.reduce((x, y) => x + (y - mean) ** 2, 0) / (misses.length || 1)),
    giniMisses: gini(misses),
    dayCorrelation: corrWeight ? corrSum / corrWeight : null,
    missHistogram,
    ranks,
    byDay,
    byGrade,
  };
}

const SCALARS = ["firsts", "meanRank", "unplaced", "noFirstAtAll", "firstEveryDay", "missedAtLeast3", "missedAtLeast4",
  "unplacedAtLeast2", "totalMisses", "squaredMisses", "rankLoss", "worstMisses", "sdMisses", "giniMisses", "dayCorrelation"];

function stats(values) {
  const v = values.filter((x) => x !== null && x !== undefined).sort((a, b) => a - b);
  if (!v.length) return null;
  const at = (q) => v[Math.min(v.length - 1, Math.floor(q * v.length))];
  return { mean: v.reduce((x, y) => x + y, 0) / v.length, p5: at(0.05), p95: at(0.95), min: v[0], max: v[v.length - 1] };
}

/** Means and 90% ranges of every measure over the runs of one rule. */
function aggregate(runs) {
  const out = {};
  for (const key of SCALARS) out[key] = stats(runs.map((r) => r[key]));
  out.missHistogram = [0, 1, 2, 3, 4, 5].map((k) => stats(runs.map((r) => r.missHistogram[k])));
  out.ranks = Object.fromEntries(RANK_KEYS.map((k) => [k, stats(runs.map((r) => r.ranks[k]))]));
  out.byDay = Object.fromEntries(DAYS.map((d) => [d, {
    decisions: stats(runs.map((r) => r.byDay[d].decisions)),
    firsts: stats(runs.map((r) => r.byDay[d].firsts)),
  }]));
  const grades = [...new Set(runs.flatMap((r) => Object.keys(r.byGrade)))].sort();
  out.byGrade = Object.fromEntries(grades.map((g) => [g, Object.fromEntries(["students", "misses", "threePlus", "perfect", "several"]
    .map((k) => [k, stats(runs.map((r) => r.byGrade[g]?.[k] ?? 0))]))]));
  return out;
}

/** Each student's risk over the runs of one rule, as histograms over students. */
function exAnte(runs) {
  const per = new Map();
  for (const r of runs) {
    for (const [am, s] of r.perStudent) {
      if (!per.has(am)) per.set(am, []);
      per.get(am).push(s);
    }
  }
  const rows = [...per.values()].map((list) => {
    const n = runs.length;
    // A run where the student had no decision counts as no miss.
    const m = list.map((s) => s.misses);
    while (m.length < n) m.push(0);
    const mean = m.reduce((x, y) => x + y, 0) / n;
    // The number of decisions can vary between runs (a multi-day club won
    // in some lotteries and not in others), so "a perfect week" is measured
    // over the runs where the student had 2+ decisions, whichever they are.
    const several = list.filter((s) => s.decisions >= 2);
    return {
      several: several.length > 0,
      expectedMisses: mean,
      sd: Math.sqrt(m.reduce((x, y) => x + (y - mean) ** 2, 0) / n),
      pPerfect: several.length ? several.filter((s) => s.misses === 0).length / several.length : 0,
      pThreePlus: list.filter((s) => s.misses >= 3).length / n,
      pFourPlus: list.filter((s) => s.misses >= 4).length / n,
    };
  });
  const bins = (values, edges) => edges.slice(0, -1).map((lo, i) => ({
    from: lo, to: edges[i + 1],
    students: values.filter((v) => v >= lo && (i === edges.length - 2 ? v <= edges[i + 1] : v < edges[i + 1])).length,
  }));
  const pEdges = [0, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 1];
  return {
    students: rows.length,
    meanSd: rows.reduce((x, r) => x + r.sd, 0) / (rows.length || 1),
    giniExpectedMisses: gini(rows.map((r) => r.expectedMisses)),
    expectedMisses: bins(rows.map((r) => r.expectedMisses), [0, 0.25, 0.5, 1, 1.5, 2, 3, 5]),
    pThreePlus: bins(rows.map((r) => r.pThreePlus), pEdges),
    pFourPlus: bins(rows.map((r) => r.pFourPlus), pEdges),
    pPerfect: bins(rows.filter((r) => r.several).map((r) => r.pPerfect), [0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1]),
    sd: bins(rows.map((r) => r.sd), [0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 3]),
  };
}

/**
 * Demand per club on its first day, from the declarations, and what the
 * teachers' ⭐ did in the actual allocation (counts only, no students).
 * `actualResult` must come from allocateWeek with the audit log.
 *
 * Each ⭐ ends in one of: placed (the student got the club), higherChoice
 * (got a club ranked higher that day), notListed (declared, but not this
 * club), dropped (listed, but removed before the allocation: similar club
 * or day clash), noDeclaration, or rejected (should not happen: a list
 * never exceeds the capacity). A placed ⭐ "made the difference" when the
 * club turned away a student without ⭐ who had better priority without
 * stars (mandatory grade, then lottery): without the ⭐, that student
 * would have come first. Checked against rerunning the whole week without
 * that ⭐: the same answer in 788 of 788 cases on random schools.
 */
export function demand(input, actualResult) {
  const clubs = new Map(input.clubs.map((c) => [String(c.code), c]));
  const excluded = new Map(Object.entries(input.exclusions ?? {}).map(([c, ams]) => [String(c), new Set(ams.map(String))]));
  const stars = new Map(Object.entries(input.teacherLists ?? {}).map(([c, ams]) => [String(c), ams.length]));
  const rows = new Map(input.clubs.map((c) => [String(c.code), {
    code: String(c.code), name: c.name, days: c.days, grades: c.grades, capacity: c.capacity,
    stars: stars.get(String(c.code)) ?? 0, excluded: excluded.get(String(c.code))?.size ?? 0,
    firstChoice: 0, applicants: 0, placed: 0, rejected: 0,
    star: { placed: 0, decisive: 0, higherChoice: 0, notListed: 0, dropped: 0, noDeclaration: 0, rejected: 0 },
    placedWithoutStar: { contested: 0, uncontested: 0 },
  }]));
  const lists = new Map(); // `${am}|${day}` → today's list, as in allocateWeek
  for (const s of input.students) {
    const am = String(s.am);
    const held = new Map(); // day → code (actual allocation)
    for (const d of DAYS) { const code = actualResult.byStudent[am]?.[d]; if (code) held.set(d, code); }
    for (const day of DAYS) {
      // As in allocateWeek: seats decided on earlier days (multi-day clubs
      // carry over), and similar clubs already held, shape today's list.
      const earlier = DAYS.slice(0, DAYS.indexOf(day))
        .map((d) => clubs.get(held.get(d)))
        .filter((c, i, all) => c && c.days[0] === DAYS[i] && all.indexOf(c) === i);
      const committedDays = new Set(earlier.flatMap((c) => c.days));
      if (committedDays.has(day)) continue; // seat carried over: no decision today
      const similar = new Set(earlier.map((c) => c.similar).filter(Boolean));
      const list = (input.preferences[am]?.[day] ?? []).map(String).filter((code) => {
        const c = clubs.get(code);
        if (!c || c.days[0] !== day || excluded.get(code)?.has(am)) return false;
        if (c.days.some((d) => committedDays.has(d))) return false;
        return !(c.similar && similar.has(c.similar));
      });
      lists.set(`${am}|${day}`, list);
      if (!list.length) continue;
      rows.get(list[0]).firstChoice++;
      const seat = held.get(day);
      const seatIndex = seat && clubs.get(seat).days[0] === day ? list.indexOf(seat) : -1;
      list.forEach((code, i) => {
        const row = rows.get(code);
        row.applicants++;
        if (i === seatIndex) row.placed++;
        else if (seatIndex === -1 || i < seatIndex) row.rejected++;
      });
    }
  }

  // What the ⭐ did
  const student = new Map(input.students.map((s) => [String(s.am), s]));
  const mandatory = new Set(input.mandatoryGrades ?? []);
  const priority = (am) => [mandatory.has(student.get(am)?.grade) ? 0 : 1, input.lottery.get(am)];
  const better = (a, b) => a[0] - b[0] || a[1] - b[1];
  const turnedAway = new Map(); // club → Set(am) rejected or displaced
  for (const day of DAYS) {
    for (const e of actualResult.days[day].log) {
      if (e.event !== "REJECTED" && e.event !== "DISPLACED") continue;
      if (!turnedAway.has(e.club)) turnedAway.set(e.club, new Set());
      turnedAway.get(e.club).add(e.am);
    }
  }
  const starred = new Map(Object.entries(input.teacherLists ?? {}).map(([c, ams]) => [String(c), new Set(ams.map(String))]));
  for (const [code, row] of rows) {
    const c = clubs.get(code);
    const day = c.days[0];
    const stars = starred.get(code) ?? new Set();
    const away = [...(turnedAway.get(code) ?? [])].filter((am) => !stars.has(am));
    for (const am of stars) {
      const r = row.star;
      if (!input.preferences[am]) { r.noDeclaration++; continue; }
      if (!(input.preferences[am][day] ?? []).map(String).includes(code)) { r.notListed++; continue; }
      const list = lists.get(`${am}|${day}`) ?? [];
      const i = list.indexOf(code);
      if (i === -1) { r.dropped++; continue; }
      const seat = actualResult.byStudent[am]?.[day];
      if (seat === code) {
        r.placed++;
        if (away.some((t) => better(priority(t), priority(am)) < 0)) r.decisive++;
        continue;
      }
      const seatIndex = seat ? list.indexOf(seat) : -1;
      if (seatIndex !== -1 && seatIndex < i) r.higherChoice++;
      else r.rejected++;
    }
    const others = row.placed - row.star.placed;
    if (turnedAway.get(code)?.size) row.placedWithoutStar.contested = others;
    else row.placedWithoutStar.uncontested = others;
  }
  return [...rows.values()];
}

/** Totals of the ⭐ outcomes over all clubs. */
export function starTotals(rows) {
  const sum = (f) => rows.reduce((x, r) => x + f(r), 0);
  return {
    given: sum((r) => r.stars),
    star: Object.fromEntries(Object.keys(rows[0]?.star ?? {}).map((k) => [k, sum((r) => r.star[k])])),
    placedWithoutStarContested: sum((r) => r.placedWithoutStar.contested),
    placedWithoutStarUncontested: sum((r) => r.placedWithoutStar.uncontested),
    seats: sum((r) => r.capacity),
    placed: sum((r) => r.placed),
  };
}

/**
 * Runs every rule and returns the report.
 * @param {Record<string, string|Uint8Array>} files
 */
export function compareLotteries(files, { runs = 200, onProgress = () => {} } = {}) {
  const input = readRPackage(files, drawLottery);
  const ams = input.students.map((s) => String(s.am));
  const gradeOf = ((m) => (am) => m.get(am) ?? "")(new Map(input.students.map((s) => [String(s.am), s.grade])));
  const base = input.seed ?? "omiloi";
  const actualResult = allocateWeek({ ...input }); // with the audit log, for the ⭐ outcomes
  const actual = measure(actualResult, gradeOf);

  const byRule = Object.fromEntries(Object.keys(RULES).map((r) => [r, []]));
  for (let i = 1; i <= runs; i++) {
    for (const [rule, make] of Object.entries(RULES)) {
      byRule[rule].push(measure(allocateWeek({ ...input, ...make(ams, `${base}#${i}`), audit: false }), gradeOf));
    }
    onProgress(i, runs);
  }
  const { perStudent, ...actualSummary } = actual;
  const demandRows = demand(input, actualResult);
  const report = {
    generated: "lottery-compare.mjs",
    runs,
    students: input.students.length,
    declared: Object.keys(input.preferences).length,
    clubs: input.clubs.length,
    mandatoryGrades: input.mandatoryGrades,
    starsTotal: Object.values(input.teacherLists ?? {}).reduce((x, l) => x + l.length, 0),
    exclusionsTotal: Object.values(input.exclusions ?? {}).reduce((x, l) => x + l.length, 0),
    actual: actualSummary,
    rules: Object.fromEntries(Object.entries(byRule).map(([r, list]) => [r, { ...aggregate(list), exAnte: exAnte(list) }])),
    demand: demandRows,
    starTotals: starTotals(demandRows),
  };
  return { actual, weekly: byRule.weekly, daily: byRule.daily, alternating: byRule.alternating, report, text: textReport(report) };
}

const fmt = (key, v) => (v === null || v === undefined ? "—" : ["meanRank", "giniMisses", "sdMisses", "dayCorrelation"].includes(key) ? v.toFixed(2) : Number.isInteger(v) ? String(v) : v.toFixed(1));

const TEXT_METRICS = [
  ["firsts", "Αποφάσεις με 1η επιλογή (σύνολο)"],
  ["meanRank", "Μέση θέση του ομίλου που πήραν"],
  ["unplaced", "Αποφάσεις χωρίς όμιλο (σύνολο)"],
  ["noFirstAtAll", "Δεν πήραν ποτέ την 1η τους επιλογή*"],
  ["firstEveryDay", "Πήραν την 1η τους επιλογή κάθε ημέρα*"],
  ["missedAtLeast3", "Έχασαν την 1η επιλογή σε 3+ ημέρες"],
  ["missedAtLeast4", "Έχασαν την 1η επιλογή σε 4+ ημέρες"],
  ["unplacedAtLeast2", "Χωρίς όμιλο σε 2+ ημέρες"],
  ["squaredMisses", "Δείκτης ποινής (άθροισμα τετραγώνων)"],
  ["giniMisses", "Gini των χαμένων ημερών"],
  ["dayCorrelation", "Συσχέτιση επιτυχίας μεταξύ ημερών"],
];

function textReport(report) {
  const rules = Object.keys(RULES);
  const lines = [];
  lines.push(`Μαθητές: ${report.students}, όμιλοι: ${report.clubs}, δηλώσεις: ${report.declared}`);
  lines.push(`Αποφάσεις (μαθητής × ημέρα κατανομής): ${report.actual.decisions}, μαθητές με 2+ αποφάσεις: ${report.actual.several}`);
  lines.push(`Επαναλήψεις ανά κανόνα: ${report.runs} (μέσος όρος· σε αγκύλες το 90% των επαναλήψεων)`);
  lines.push(`Κανόνες: Ένας αριθμός = όπως η πλατφόρμα· Ανά ημέρα = νέος αριθμός κάθε ημέρα· Αντιστροφή = ένας αριθμός, ανάποδα Τρίτη και Πέμπτη`);
  lines.push("");
  const w = 44, c = 22;
  lines.push(`${"".padEnd(w)}${RULE_LABELS.actual.padEnd(12)}${rules.map((r) => RULE_LABELS[r].padEnd(c)).join("")}`);
  for (const [key, label] of TEXT_METRICS) {
    const cells = rules.map((r) => {
      const s = report.rules[r][key];
      return (s ? `${fmt(key, s.mean)} [${fmt(key, s.p5)}–${fmt(key, s.p95)}]` : "—").padEnd(c);
    });
    lines.push(`${label.padEnd(w)}${fmt(key, report.actual[key]).padEnd(12)}${cells.join("")}`);
  }
  lines.push(`* μόνο μαθητές με 2+ αποφάσεις`);
  lines.push("");
  lines.push("Πόσες φορές έχασε ο καθένας την 1η επιλογή του (μέσος αριθμός μαθητών):");
  lines.push(`${"".padEnd(14)}${RULE_LABELS.actual.padEnd(12)}${rules.map((r) => RULE_LABELS[r].padEnd(14)).join("")}`);
  for (let k = 0; k <= 5; k++) {
    const label = k === 1 ? "1 φορά" : `${k} φορές`;
    lines.push(`${label.padEnd(14)}${String(report.actual.missHistogram[k]).padEnd(12)}${rules.map((r) => report.rules[r].missHistogram[k].mean.toFixed(1).padEnd(14)).join("")}`);
  }
  lines.push("");
  const t = report.starTotals;
  lines.push(`Τα ⭐ των εκπαιδευτικών στην πραγματική κατανομή (${t.seats} θέσεις, ${t.placed} τοποθετήσεις):`);
  lines.push(`  δόθηκαν ${t.given} ⭐`);
  lines.push(`  ο μαθητής πήρε τον όμιλο: ${t.star.placed}`);
  lines.push(`    και το ⭐ έκανε διαφορά (έμεινε έξω κάποιος χωρίς ⭐ με καλύτερο αριθμό): ${t.star.decisive}`);
  lines.push(`  πήρε όμιλο που είχε βάλει ψηλότερα: ${t.star.higherChoice}`);
  lines.push(`  δεν έβαλε τον όμιλο στη λίστα του: ${t.star.notListed}`);
  lines.push(`  τον είχε στη λίστα, αλλά έφυγε πριν από την κατανομή (παρεμφερής, σύγκρουση ημερών): ${t.star.dropped}`);
  lines.push(`  δεν έκανε δήλωση: ${t.star.noDeclaration}`);
  if (t.star.rejected) lines.push(`  δεν χώρεσε παρά το ⭐: ${t.star.rejected}`);
  lines.push(`  θέσεις χωρίς ⭐: ${t.placedWithoutStarContested} σε ομίλους που απέρριψαν μαθητές (κλήρωση), ${t.placedWithoutStarUncontested} σε ομίλους που χώρεσαν όλους`);
  lines.push("");
  lines.push("Η «Πραγματική» είναι με το seed του πακέτου και τον ένα αριθμό. Οι άλλες στήλες είναι μέσοι όροι από νέες");
  lines.push("κληρώσεις, για να φανεί τι κάνει ο κανόνας και όχι η τύχη μιας κλήρωσης. Δεν εμφανίζονται ΑΜ ή ονόματα.");
  return lines.join("\n");
}

async function main(args) {
  const usage = "Χρήση: node platform/scripts/lottery-compare.mjs <φάκελος ή .zip> [--runs N] [--json <αρχείο>]";
  let path;
  let runs = 200;
  let json;
  for (let i = 0; i < args.length; i++) {
    if (args[i] === "--runs") runs = Number(args[++i]);
    else if (args[i] === "--json") json = args[++i];
    else if (!path) path = args[i];
    else throw new Error(usage);
  }
  if (!path || !(Number.isInteger(runs) && runs > 0) || (args.includes("--json") && !json)) throw new Error(usage);
  const files = await readPackage(resolve(path));
  const { text, report } = compareLotteries(files, {
    runs,
    onProgress: (i, n) => { if (process.stderr.isTTY) process.stderr.write(`\r${i}/${n}`); },
  });
  if (process.stderr.isTTY) process.stderr.write("\r          \r");
  console.log(text);
  if (json) {
    writeFileSync(json, JSON.stringify(report, null, 1));
    console.log(`\nΠλήρης αναφορά (μόνο αθροίσματα): ${resolve(json)}`);
  }
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  main(process.argv.slice(2)).catch((err) => {
    console.error(err.message);
    process.exit(1);
  });
}
