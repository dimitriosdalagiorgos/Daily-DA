// The platform's backend: one function handle(Request) → Response, used by
// the local dev server and by Netlify Functions alike.
//
// Phases (settings.phase), in order:
//   setup      the admin uploads students and clubs
//   teachers   teachers pick (optionally) their preferred students
//   parents    parents submit rankings until the deadline; clubs and teacher
//              lists are locked, students can only be added
//   closed     no more submissions
//   allocated  the allocation has run (can be re-run while not published)
//   published  parents see the result

import { DAYS, DAY_LABELS } from "../algorithm/days.js";
import { GRADES } from "../algorithm/validate.js";
import { drawLottery } from "../algorithm/lottery.js";
import { allocateWeek } from "../algorithm/allocate.js";
import { clubsToRank, validateSubmission, validateTeacherList } from "../algorithm/validate.js";
import { importStudents } from "../import/students.js";
import { importClubs, normalizePersonalId, normalizeSimilar } from "../import/clubs.js";
import { importTeacherLists } from "../import/teacherLists.js";
import { checkReadiness } from "../import/readiness.js";
import { importLegacyResponses } from "../import/legacy.js";
import { givenNameMatches, sameName } from "../import/names.js";
import { buildRPackage, toCsv } from "../export/rPackage.js";
import { GAP_REASONS, describeEvent, gapReasons, storiesByStudent } from "../export/story.js";
import { allocationStats } from "../export/stats.js";
import { createHash } from "node:crypto";
import { makeZip } from "./zip.js";
import { createRateLimiter, hashPassword, personalIdHash, safeEqual, signToken, verifyPassword, verifyToken, withIdHashes } from "./auth.js";

export const PHASES = ["setup", "teachers", "parents", "closed", "allocated", "published"];
const phaseAtLeast = (phase, min) => PHASES.indexOf(phase) >= PHASES.indexOf(min);

const SESSION_TTL = { admin: 8 * 3600, teacher: 12 * 3600, parent: 2 * 3600 };
const MAGIC_TTL = 20 * 60;
// Links the admin passes on by hand (no e-mail service): valid a week.
const ADMIN_LINK_TTL = 7 * 24 * 3600;

/** Short code printed on the parent's receipt; changes with every change. */
export function receiptCode(am, submittedAt, preferences) {
  const h = createHash("sha256").update(JSON.stringify([am, submittedAt, preferences])).digest("hex").toUpperCase();
  return `${h.slice(0, 4)}-${h.slice(4, 8)}`;
}
const EMAIL = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
const GENERIC_LOGIN_ERROR = "Τα στοιχεία δεν ταιριάζουν με τον κατάλογο του σχολείου. Ελέγξτε τα και δοκιμάστε ξανά.";

class HttpError extends Error {
  constructor(status, message, extra = {}) {
    super(message);
    this.status = status;
    this.extra = extra;
  }
}

const json = (status, body, headers = {}) =>
  new Response(JSON.stringify(body), { status, headers: { "content-type": "application/json; charset=utf-8", ...headers } });

/**
 * @param {{store: object, env: {SESSION_SECRET: string, ADMIN_PASSWORD: string, BASE_URL?: string, DEV?: boolean},
 *          now?: () => number, sendMail?: (mail: object) => Promise<void>}} deps
 */
export function createApp({ store, env, now = () => Date.now(), sendMail }) {
  if (!env.SESSION_SECRET || env.SESSION_SECRET.length < 16) throw new Error("SESSION_SECRET (≥16 χαρακτήρες) δεν έχει οριστεί.");
  if (!env.ADMIN_PASSWORD) throw new Error("ADMIN_PASSWORD δεν έχει οριστεί.");

  const loginByIp = createRateLimiter({ limit: 30, windowMs: 10 * 60 * 1000 });
  const loginByAm = createRateLimiter({ limit: 10, windowMs: 10 * 60 * 1000 });
  const adminByIp = createRateLimiter({ limit: 10, windowMs: 10 * 60 * 1000 });
  const teacherCodeByIp = createRateLimiter({ limit: 10, windowMs: 10 * 60 * 1000 });
  // The teachers' page (its hidden address, see paths.js) for login links
  const teacherPage = `${env.BASE_URL ?? ""}${env.TEACHER_PATH ? `/${env.TEACHER_PATH}/` : "/teacher.html"}`;

  // ---------- data helpers ----------

  // mandatoryGrades: grades where every student must get a club (default: all)
  const getSettings = async () => ({ phase: "setup", contact: "", deadline: null, mandatoryGrades: [...GRADES], ...(await store.get("settings")) });
  const getStudents = async () => (await store.get("students")) ?? [];
  const getTeachers = async () => (await store.get("teachers")) ?? [];
  const getTeacherLists = async () => Object.fromEntries((await store.list("teacherList:")).map(({ key, value }) => [key.slice(12), value]));
  /** Clubs with the teacher-set capacity applied. */
  const getClubs = async () => {
    const clubs = (await store.get("clubs")) ?? [];
    const lists = await getTeacherLists();
    return clubs.map((c) => ({ ...c, capacity: lists[c.code]?.capacity ?? c.capacity }));
  };
  const getSubmissions = async () => Object.fromEntries((await store.list("submission:")).map(({ key, value }) => [key.slice(11), value]));
  /**
   * Clubs stay fixed once parents have declared, even if the admin goes back
   * a phase: a new or changed club would leave saved rankings incomplete.
   * (Trial imports of last year's responses do not count.)
   */
  const assertClubsChangeable = async (settings) => {
    if (phaseAtLeast(settings.phase, "parents")) throw new HttpError(409, "Οι όμιλοι κλείδωσαν με το άνοιγμα των δηλώσεων.");
    const real = Object.values(await getSubmissions()).filter((s) => !s.imported).length;
    if (real) throw new HttpError(409, `Υπάρχουν ήδη ${real} δηλώσεις γονέων, οπότε οι όμιλοι δεν αλλάζουν (οι δηλώσεις θα έμεναν ελλιπείς). Για νέα αρχή: «Επαναφορά πλατφόρμας».`);
  };

  const logEvent = (who, what, detail = {}) => store.append("events", { at: new Date(now()).toISOString(), who, what, ...detail });

  /** What has been uploaded, for the admin's «Αρχεία» overview. */
  const recordUpload = (path, info) => store.update("uploads", (u = {}) => {
    const next = structuredClone(u);
    let node = next;
    for (const k of path.slice(0, -1)) node = node[k] ??= {};
    node[path.at(-1)] = { at: new Date(now()).toISOString(), ...info };
    return next;
  });
  const fileNameOf = (name) => (typeof name === "string" ? name.slice(0, 200) : "");

  /**
   * Send (if a mail service is configured) and record in the outbox with the
   * outcome. A failed mail never fails the action that caused it. With
   * REDACT_OUTBOX the stored copy hides login links, so whoever reads the
   * outbox cannot use them.
   */
  const mail = async (to, subject, text) => {
    const message = { to, subject, text, at: new Date(now()).toISOString() };
    let status = "not_sent";
    let error;
    if (sendMail) {
      try {
        await sendMail(message);
        status = "sent";
      } catch (err) {
        status = "failed";
        error = String(err.message ?? err).slice(0, 300);
        console.error("mail failed", err);
        await store.append("events", { at: message.at, who: "system", what: "mail_failed", to, error });
      }
    }
    const stored = env.REDACT_OUTBOX ? { ...message, text: text.replace(/#token=\S+/g, "#token=[κρυφό]") } : message;
    await store.append("outbox", { ...stored, status, ...(error ? { error } : {}) });
    return { status, error };
  };

  const deadlinePassed = (settings) => settings.deadline && now() > Date.parse(settings.deadline);

  // ---------- auth ----------

  const session = (request, role) => {
    const header = request.headers.get("authorization") ?? "";
    const payload = verifyToken(env.SESSION_SECRET, header.replace(/^Bearer\s+/i, ""), now());
    if (!payload || payload.role !== role) throw new HttpError(401, "Η σύνδεση έληξε. Συνδεθείτε ξανά.");
    return payload;
  };
  const issue = (role, fields) => signToken(env.SESSION_SECRET, { role, ...fields }, SESSION_TTL[role], now());
  // Netlify's own header; behind Apache/nginx the proxy appends the real
  // address as the last X-Forwarded-For entry (earlier ones can be forged).
  const clientIp = (request) => request.headers.get("x-nf-client-connection-ip") ?? request.headers.get("x-forwarded-for")?.split(",").at(-1).trim() ?? "local";

  const body = async (request) => {
    try {
      return await request.json();
    } catch {
      throw new HttpError(400, "Μη έγκυρο αίτημα.");
    }
  };

  // ---------- routes ----------

  const routes = [];
  const route = (method, pattern, fn) => {
    const keys = [];
    const re = new RegExp(`^${pattern.replace(/:(\w+)/g, (_, k) => (keys.push(k), "([^/]+)"))}$`);
    routes.push({ method, re, keys, fn });
  };

  // --- public ---

  route("GET", "/api/public", async () => {
    const s = await getSettings();
    return json(200, {
      phase: s.phase, deadline: s.deadline, contact: s.contact, schoolName: s.schoolName ?? "", mailEnabled: Boolean(sendMail),
      teacherCode: Boolean(s.teacherPasswordHash),
    });
  });

  // --- admin ---

  route("POST", "/api/admin/login", async (req) => {
    if (!adminByIp.hit(clientIp(req), now())) throw new HttpError(429, "Πολλές προσπάθειες. Δοκιμάστε σε λίγα λεπτά.");
    const { password } = await body(req);
    if (!safeEqual(password, env.ADMIN_PASSWORD)) throw new HttpError(401, "Λάθος κωδικός.");
    await logEvent("admin", "login");
    return json(200, { token: issue("admin", {}) });
  });

  route("GET", "/api/admin/state", async (req) => {
    session(req, "admin");
    const [settings, students, clubs, teachers, lists, submissions, results, uploads] = await Promise.all([
      getSettings(), getStudents(), getClubs(), getTeachers(), getTeacherLists(), getSubmissions(), store.get("results"), store.get("uploads"),
    ]);
    const { parentPasswordHash, teacherPasswordHash, ...publicSettings } = settings;
    return json(200, {
      settings: { ...publicSettings, parentPasswordSet: Boolean(parentPasswordHash), teacherPasswordSet: Boolean(teacherPasswordHash) },
      teacherPath: env.TEACHER_PATH ? `/${env.TEACHER_PATH}/` : null,
      students: students.map(({ am, grade, surname, name, loginException }) => ({ am, grade, surname, name, loginException: Boolean(loginException) })),
      clubs,
      teachers: teachers.map(({ idHash, ...t }) => ({ ...t, hasPersonalId: Boolean(idHash) })),
      teacherLists: lists,
      submissions: Object.fromEntries(Object.entries(submissions).map(([am, s]) => [am, { submittedAt: s.submittedAt, parentEmail: s.parent?.email, parentName: s.parent?.name, changes: s.history?.length ?? 1, imported: Boolean(s.imported), days: Object.keys(s.preferences ?? {}) }])),
      uploads: uploads ?? {},
      readiness: students.length && clubs.length ? checkReadiness(students, clubs, settings) : [],
      results: results ? { seed: results.seed, at: results.at } : null,
    });
  });

  route("PUT", "/api/admin/students", async (req) => {
    session(req, "admin");
    const { rows, fileName } = await body(req);
    const report = importStudents(Array.isArray(rows) ? rows : []);
    if (report.problems.some((p) => p.level === "error")) throw new HttpError(422, "Το αρχείο έχει σφάλματα.", { report });

    const settings = await getSettings();
    const existing = await getStudents();
    const byAm = new Map(existing.map((s) => [s.am, s]));
    let students;
    const notes = [];
    if (phaseAtLeast(settings.phase, "allocated")) {
      throw new HttpError(409, "Η κατανομή έχει γίνει: οι νέοι μαθητές δεν θα είχαν αριθμό κλήρωσης ούτε αποτέλεσμα. Για να προσθέσετε μαθητές, πατήστε «Επιστροφή» μέχρι τη φάση «Κλειστές δηλώσεις», ανεβάστε το αρχείο και ξανατρέξτε την κατανομή.");
    }
    if (phaseAtLeast(settings.phase, "parents")) {
      // Declarations are open: only additions (SPEC).
      const incoming = new Map(report.students.map((s) => [s.am, s]));
      const added = report.students.filter((s) => !byAm.has(s.am));
      const removed = existing.filter((s) => !incoming.has(s.am));
      const changed = existing.filter((s) => {
        const n = incoming.get(s.am);
        return n && ["grade", "surname", "name", "father", "mother"].some((f) => n[f] !== s[f]);
      });
      if (removed.length) notes.push({ level: "warning", message: `${removed.length} μαθητές λείπουν από το νέο αρχείο· παραμένουν (οι δηλώσεις είναι ανοιχτές).` });
      if (changed.length) notes.push({ level: "warning", message: `${changed.length} μαθητές έχουν αλλαγές· δεν εφαρμόστηκαν (οι δηλώσεις είναι ανοιχτές): ΑΜ ${changed.map((s) => s.am).join(", ")}.` });
      students = [...existing, ...added];
      notes.push({ level: "info", message: `Προστέθηκαν ${added.length} μαθητές.` });
      // An earlier allocation (after going back a phase) no longer covers everyone
      if (added.length && (await store.get("results"))) {
        await store.delete("results");
        await store.delete("resultsLog");
        notes.push({ level: "warning", message: "Η προηγούμενη κατανομή ακυρώθηκε, γιατί δεν περιλάμβανε τους νέους μαθητές· ξανατρέξτε την." });
      }
    } else {
      students = report.students.map((s) => ({ ...s, loginException: byAm.get(s.am)?.loginException ?? false }));
    }
    await store.set("students", students);
    await logEvent("admin", "students_uploaded", { count: students.length, fileName: fileNameOf(fileName) });
    await recordUpload(["students"], { fileName: fileNameOf(fileName), count: students.length, byGrade: Object.fromEntries(["Α", "Β", "Γ"].map((g) => [g, students.filter((s) => s.grade === g).length])) });
    return json(200, { report: { ...report, problems: [...report.problems, ...notes] }, count: students.length });
  });

  route("PATCH", "/api/admin/students/:am", async (req, { am }) => {
    session(req, "admin");
    const { loginException } = await body(req);
    let found = false;
    await store.update("students", (list = []) =>
      list.map((s) => (s.am === am ? ((found = true), { ...s, loginException: Boolean(loginException) }) : s)));
    if (!found) throw new HttpError(404, "Άγνωστος ΑΜ.");
    await logEvent("admin", "login_exception", { am, loginException: Boolean(loginException) });
    return json(200, { ok: true });
  });

  route("PUT", "/api/admin/clubs", async (req) => {
    session(req, "admin");
    await assertClubsChangeable(await getSettings());
    const { clubs: clubRows, teachers: teacherRows, fileName } = await body(req);
    const imported = importClubs({ clubs: clubRows ?? [], teachers: teacherRows ?? [] });
    // The teachers' ΑΜ/ΑΦΜ are neither kept nor sent back: only a keyed hash, for the login.
    const report = { ...imported, teachers: withIdHashes(env.SESSION_SECRET, imported.teachers) };
    const reply = { ...report, teachers: report.teachers.map(({ idHash, ...t }) => ({ ...t, hasPersonalId: Boolean(idHash) })) };
    if (report.problems.some((p) => p.level === "error")) throw new HttpError(422, "Το αρχείο έχει σφάλματα.", { report: reply });
    // Declarations already in (trial imports) that the new clubs no longer fit
    const stale = staleSubmissions(await getSubmissions(), report.clubs, await getStudents());
    if (stale.length) {
      reply.problems = [...reply.problems, { level: "warning", message: `${staleText(stale)} Πριν από την κατανομή, ξαναεισαγάγετε τα περσινά αρχεία κάθε ημέρας (ή «Επαναφορά πλατφόρμας»).` }];
    }
    await store.set("clubs", report.clubs);
    await store.set("teachers", report.teachers);
    // Teacher lists: dropped for clubs that no longer exist; for the others
    // the file's capacity wins (an earlier «Διόρθωση» no longer applies) and
    // students no longer in the club's grades leave the list.
    const byCode = new Map(report.clubs.map((c) => [String(c.code), c]));
    const gradeOf = new Map((await getStudents()).map((st) => [st.am, st.grade]));
    for (const { key, value } of await store.list("teacherList:")) {
      const club = byCode.get(key.slice(12));
      if (!club) {
        await store.delete(key);
        continue;
      }
      const ams = (value.ams ?? []).filter((am) => club.grades.includes(gradeOf.get(am)));
      const { capacity: _old, ...rest } = value;
      await store.set(key, { ...rest, ams });
      if (ams.length < (value.ams ?? []).length) {
        reply.problems = [...reply.problems, { level: "warning", message: `Λίστα εκπαιδευτικού «${club.name}»: ${value.ams.length - ams.length} μαθητές βγήκαν, γιατί δεν είναι πια στις τάξεις του ομίλου.` }];
      }
      if (ams.length > club.capacity) {
        reply.problems = [...reply.problems, { level: "warning", message: `Λίστα εκπαιδευτικού «${club.name}»: ${ams.length} μαθητές, περισσότεροι από τις ${club.capacity} θέσεις του νέου αρχείου. Διορθώστε τη λίστα.` }];
      }
    }
    await logEvent("admin", "clubs_uploaded", { clubs: report.clubs.length, teachers: report.teachers.length, fileName: fileNameOf(fileName) });
    await recordUpload(["clubs"], { fileName: fileNameOf(fileName), ...report.summary });
    return json(200, { report: reply });
  });

  // «Παρεμφερείς» of one club, until declarations open (empty = none)
  route("PUT", "/api/admin/clubs/:code/similar", async (req, { code }) => {
    session(req, "admin");
    await assertClubsChangeable(await getSettings());
    const { similar } = await body(req);
    const value = normalizeSimilar(similar ?? "");
    let found = null;
    await store.update("clubs", (clubs = []) => clubs.map((c) => {
      if (String(c.code) !== String(code)) return c;
      const { similar: _old, ...rest } = c;
      found = value ? { ...rest, similar: value } : rest;
      return found;
    }));
    if (!found) throw new HttpError(404, "Δεν υπάρχει τέτοιος όμιλος.");
    await logEvent("admin", "club_similar", { club: Number(code), similar: value });
    return json(200, { club: found });
  });

  route("PUT", "/api/admin/settings", async (req) => {
    session(req, "admin");
    const { parentPassword, teacherPassword, deadline, contact, schoolName, mandatoryGrades } = await body(req);
    const patch = {};
    if (parentPassword !== undefined) {
      if (String(parentPassword).length < 6) throw new HttpError(422, "Ο κωδικός γονέων πρέπει να έχει τουλάχιστον 6 χαρακτήρες.");
      patch.parentPasswordHash = hashPassword(parentPassword);
    }
    // A new teachers' code also signs out everyone who used the old one.
    if (teacherPassword !== undefined) {
      if (String(teacherPassword).length < 6) throw new HttpError(422, "Ο κωδικός εκπαιδευτικών πρέπει να έχει τουλάχιστον 6 χαρακτήρες.");
      patch.teacherPasswordHash = hashPassword(teacherPassword);
    }
    if (deadline !== undefined) {
      if (deadline !== null && Number.isNaN(Date.parse(deadline))) throw new HttpError(422, "Μη έγκυρη προθεσμία.");
      patch.deadline = deadline;
    }
    if (contact !== undefined) patch.contact = String(contact).slice(0, 300);
    if (schoolName !== undefined) patch.schoolName = String(schoolName).slice(0, 120);
    if (mandatoryGrades !== undefined) {
      if (!Array.isArray(mandatoryGrades) || mandatoryGrades.some((g) => !GRADES.includes(g))) throw new HttpError(422, "Άγνωστη τάξη.");
      const next = GRADES.filter((g) => mandatoryGrades.includes(g));
      const current = await getSettings();
      // The seat check runs when declarations open; after that the rule stays as checked
      if (phaseAtLeast(current.phase, "parents") && next.join() !== current.mandatoryGrades.join()) {
        throw new HttpError(409, "Οι τάξεις με υποχρεωτική ένταξη δεν αλλάζουν αφού ανοίξουν οι δηλώσεις (ο έλεγχος θέσεων έγινε με τις τωρινές). Για αλλαγή: «Επιστροφή» στη φάση «Εκπαιδευτικοί», αλλαγή, και ξανά άνοιγμα των δηλώσεων.");
      }
      patch.mandatoryGrades = next;
    }
    await store.update("settings", (s = {}) => ({ phase: "setup", ...s, ...patch }));
    await logEvent("admin", "settings", { fields: Object.keys(patch) });
    return json(200, { ok: true });
  });

  route("POST", "/api/admin/phase", async (req) => {
    session(req, "admin");
    const { phase } = await body(req);
    if (!PHASES.includes(phase)) throw new HttpError(422, "Άγνωστη φάση.");
    const [settings, students, clubs, results] = await Promise.all([getSettings(), getStudents(), getClubs(), store.get("results")]);
    // The checks apply moving forward, and whenever declarations (re)open:
    // going back from «closed» to «parents» must still find enough seats.
    // Other steps back need nothing (e.g. after a reset cut short).
    const check = PHASES.indexOf(phase) > PHASES.indexOf(settings.phase) || phase === "parents";
    const missing = [];
    if (check) {
      if (phaseAtLeast(phase, "teachers") && clubs.length === 0) missing.push("ομίλους");
      if (phaseAtLeast(phase, "parents")) {
        if (students.length === 0) missing.push("μαθητές");
        if (!settings.parentPasswordHash) missing.push("κωδικό γονέων");
        if (!settings.deadline) missing.push("προθεσμία");
      }
      if (phaseAtLeast(phase, "allocated") && !results) missing.push("εκτέλεση κατανομής");
    }
    if (missing.length) throw new HttpError(409, `Για αυτή τη φάση χρειάζονται: ${missing.join(", ")}.`);
    if (check && phase === "parents") {
      // Enough seats for the mandatory grades, or declarations do not open.
      const errors = checkReadiness(students, clubs, settings).filter((p) => p.level === "error");
      if (errors.length) {
        throw new HttpError(409, "Οι δηλώσεις δεν μπορούν να ανοίξουν: δεν υπάρχουν αρκετές θέσεις για τις τάξεις με υποχρεωτική ένταξη. Αυξήστε χωρητικότητες ή προσθέστε ομίλους.", { problems: errors.map((p) => p.message) });
      }
    }
    await store.update("settings", (s = {}) => ({ ...s, phase }));
    await logEvent("admin", "phase", { from: settings.phase, to: phase });
    return json(200, { phase });
  });

  route("PUT", "/api/admin/teacher-lists/:code", async (req, { code }) => {
    session(req, "admin");
    return saveTeacherList(req, code, "admin");
  });

  // Teachers' lists from files (Excel/CSV, read in the browser): preview
  // with `apply: false`, save with `apply: true` (only if every file is OK).
  route("POST", "/api/admin/teacher-lists/import", async (req) => {
    session(req, "admin");
    const settings = await getSettings();
    if (phaseAtLeast(settings.phase, "parents")) throw new HttpError(409, "Οι λίστες κλείδωσαν με το άνοιγμα των δηλώσεων.");
    const { files, apply } = await body(req);
    if (!Array.isArray(files) || files.length === 0) throw new HttpError(422, "Επιλέξτε αρχεία.");
    const [clubs, students, lists] = await Promise.all([getClubs(), getStudents(), getTeacherLists()]);
    const report = importTeacherLists(files.map((f) => ({ fileName: fileNameOf(f.fileName), rows: f.rows })), { clubs, students, lists });
    const ok = report.every((r) => r.problems.length === 0);
    if (apply) {
      if (!ok) throw new HttpError(422, "Κάποια αρχεία έχουν σφάλματα· διορθώστε τα και ξαναδοκιμάστε.", { report });
      for (const r of report) await writeTeacherList(r.code, r.ams, "admin-file");
      await logEvent("admin", "teacher_lists_imported", { clubs: report.map((r) => r.code), files: files.length });
    }
    return json(200, { report, ok, saved: Boolean(apply) });
  });

  // Trial only: last year's per-day responses (Google Form export) as
  // submissions, to try the allocation with real-looking data.
  route("POST", "/api/admin/import-legacy", async (req) => {
    session(req, "admin");
    const settings = await getSettings();
    if (phaseAtLeast(settings.phase, "allocated")) throw new HttpError(409, "Έχει ήδη γίνει κατανομή. Για να προσθέσετε δηλώσεις, πατήστε επάνω «Επιστροφή» μέχρι τη φάση «Κλειστές δηλώσεις», ανεβάστε το αρχείο και ξανατρέξτε την κατανομή.");
    const { day, rows, addMissingGrade, fileName } = await body(req);
    if (!DAYS.includes(day)) throw new HttpError(422, "Επιλέξτε ημέρα.");
    if (addMissingGrade && !["Α", "Β", "Γ"].includes(addMissingGrade)) throw new HttpError(422, "Άγνωστη τάξη.");
    const [clubs, students] = await Promise.all([getClubs(), getStudents()]);
    if (clubs.length === 0) throw new HttpError(409, "Ανεβάστε πρώτα το αρχείο ομίλων.");
    const report = importLegacyResponses(Array.isArray(rows) ? rows : [], { day, clubs, students, addMissingGrade: addMissingGrade || null });
    if (report.problems.some((p) => p.level === "error")) throw new HttpError(422, "Το αρχείο έχει σφάλματα.", { report });

    if (report.newStudents.length) await store.set("students", [...students, ...report.newStudents]);
    const at = new Date(now()).toISOString();
    const existing = await getSubmissions();
    await store.setMany(Object.entries(report.preferences).map(([am, list]) => {
      const old = existing[am];
      const preferences = { ...(old?.preferences ?? {}), [day]: list };
      return {
        key: `submission:${am}`,
        value: {
          preferences,
          parent: old?.parent ?? { name: "Εισαγωγή δοκιμής", email: "" },
          submittedAt: at,
          imported: true,
          history: [...(old?.history ?? []), { at, email: "(εισαγωγή δοκιμής)", preferences }],
        },
      };
    }));
    await logEvent("admin", "import_legacy", { day, rows: report.summary.rows, newStudents: report.newStudents.length, fileName: fileNameOf(fileName) });
    await recordUpload(["legacy", day], { fileName: fileNameOf(fileName), rows: report.summary.rows, newStudents: report.newStudents.length });
    return json(200, { report });
  });

  // Delete all school data (e.g. after a trial with last year's data).
  // Keeps only the session secret and, if asked, the school's name/contact.
  route("POST", "/api/admin/reset", async (req) => {
    session(req, "admin");
    const { confirm, keepSchoolInfo = true } = await body(req);
    if (confirm !== "ΔΙΑΓΡΑΦΗ") throw new HttpError(422, "Για επιβεβαίωση γράψτε ΔΙΑΓΡΑΦΗ (κεφαλαία).");
    const settings = await getSettings();
    // Back to «setup» first: if the deletions are cut short, the platform is
    // not left in a late phase without data, and running the reset again
    // finishes the job.
    await store.set("settings", {
      phase: "setup",
      ...(keepSchoolInfo ? { schoolName: settings.schoolName ?? "", contact: settings.contact ?? "" } : {}),
    });
    await Promise.all([
      ...["students", "clubs", "teachers", "results", "resultsLog", "uploads"].map((key) => store.delete(key)),
      ...["teacherList:", "submission:", "story:"].map((prefix) => store.deletePrefix(prefix)),
      store.deleteLog("outbox"),
      store.deleteLog("events"),
    ]);
    await logEvent("admin", "reset", { keepSchoolInfo: Boolean(keepSchoolInfo) });
    return json(200, { ok: true });
  });

  route("POST", "/api/admin/allocate", async (req) => {
    session(req, "admin");
    const settings = await getSettings();
    if (!["closed", "allocated"].includes(settings.phase)) throw new HttpError(409, "Η κατανομή γίνεται αφού κλείσουν οι δηλώσεις.");
    const { seed } = await body(req);
    if (typeof seed !== "string" || seed.trim() === "") throw new HttpError(422, "Δώστε το seed της κλήρωσης.");
    const input = await allocationInput(seed);
    const stale = staleSubmissions(await getSubmissions(), input.clubs, input.students);
    if (stale.length) {
      throw new HttpError(422, `Η κατανομή δεν μπόρεσε να γίνει: ${staleText(stale)} Για δοκιμή με περσινά στοιχεία: ξαναεισαγάγετε τα αρχεία κάθε ημέρας («Αρχεία» → «Δοκιμή: εισαγωγή περσινών δηλώσεων», μετά από «Επιστροφή» στη φάση «Κλειστές δηλώσεις» αν χρειάζεται), ή κάντε «Επαναφορά πλατφόρμας» και ξεκινήστε με το τελικό αρχείο ομίλων.`);
    }
    let result;
    try {
      result = allocateWeek(input);
    } catch (err) {
      throw new HttpError(422, `Η κατανομή δεν μπόρεσε να γίνει: ${err.message}`);
    }
    const results = {
      seed,
      at: new Date(now()).toISOString(),
      lottery: [...input.lottery],
      byStudent: result.byStudent,
      days: Object.fromEntries(DAYS.map((d) => [d, { assignments: result.days[d].assignments, unassigned: result.days[d].unassigned }])),
    };
    await store.set("results", results);
    const logByDay = Object.fromEntries(DAYS.map((d) => [d, result.days[d].log]));
    await store.set("resultsLog", logByDay);
    // Each student's story, stored per student so a parent reads only theirs.
    const byCode = new Map(input.clubs.map((c) => [String(c.code), c]));
    const stories = storiesByStudent(logByDay, (code) => byCode.get(String(code))?.name ?? code, (code) => byCode.get(String(code))?.days ?? []);
    for (const { key } of await store.list("story:")) if (!stories[key.slice(6)]) await store.delete(key);
    await store.setMany(Object.entries(stories).map(([am, story]) => ({ key: `story:${am}`, value: story })));
    await store.update("settings", (s = {}) => ({ ...s, phase: "allocated" }));
    await logEvent("admin", "allocated", { seed });
    return json(200, { results: summarizeResults(results, input, logByDay) });
  });

  route("GET", "/api/admin/results", async (req) => {
    session(req, "admin");
    const results = await store.get("results");
    if (!results) throw new HttpError(404, "Δεν έχει γίνει κατανομή.");
    return json(200, { results: summarizeResults(results, await allocationInput(results.seed), await store.get("resultsLog")) });
  });

  route("GET", "/api/admin/export/r-package.zip", async (req) => {
    session(req, "admin");
    const url = new URL(req.url);
    const results = await store.get("results");
    const seed = results?.seed ?? url.searchParams.get("seed");
    if (!seed) throw new HttpError(409, "Χρειάζεται seed (εκτελέστε την κατανομή ή δώστε ?seed=).");
    const input = await allocationInput(seed);
    const students = await getStudents();
    const files = buildRPackage({ ...input, students, seed });
    return new Response(makeZip(files, new Date(now())), {
      headers: { "content-type": "application/zip", "content-disposition": 'attachment; filename="omiloi_dedomena_R.zip"' },
    });
  });

  route("GET", "/api/admin/export/results.csv", async (req) => {
    session(req, "admin");
    const results = await store.get("results");
    if (!results) throw new HttpError(404, "Δεν έχει γίνει κατανομή.");
    const [students, clubs] = await Promise.all([getStudents(), getClubs()]);
    const nameOf = new Map(clubs.map((c) => [String(c.code), c.name]));
    const gaps = gapReasons(results, students, clubs);
    const cell = (s, d) => {
      const code = results.byStudent[s.am]?.[d];
      if (code) return nameOf.get(code) ?? code;
      return gaps[s.am]?.[d] === "not_offered" ? "" : `— ${GAP_REASONS[gaps[s.am]?.[d]] ?? ""}`;
    };
    const rows = [...students]
      .sort((a, b) => a.grade.localeCompare(b.grade) || a.surname.localeCompare(b.surname, "el") || a.name.localeCompare(b.name, "el"))
      .map((s) => [s.am, s.surname, s.name, s.grade, ...DAYS.map((d) => cell(s, d))]);
    const csv = "﻿" + toCsv(["ΑΜ", "Επώνυμο", "Όνομα", "Τάξη", ...DAYS.map((d) => DAY_LABELS[d])], rows);
    return new Response(csv, { headers: { "content-type": "text/csv; charset=utf-8", "content-disposition": 'attachment; filename="katanomi_omilon.csv"' } });
  });

  const csvResponse = (header, rows, filename) =>
    new Response("\ufeff" + toCsv(header, rows), { headers: { "content-type": "text/csv; charset=utf-8", "content-disposition": `attachment; filename="${filename}"` } });

  // Every step of the allocation, as the R scripts' <day>_audit_log.csv.
  route("GET", "/api/admin/export/audit_log.csv", async (req) => {
    session(req, "admin");
    const log = await store.get("resultsLog");
    if (!log) throw new HttpError(404, "Δεν έχει γίνει κατανομή.");
    const [students, clubs] = await Promise.all([getStudents(), getClubs()]);
    const byAm = new Map(students.map((s) => [s.am, s]));
    const byCode = new Map(clubs.map((c) => [String(c.code), c]));
    const nameOf = (code) => byCode.get(String(code))?.name ?? code;
    const daysOf = (code) => byCode.get(String(code))?.days ?? [];
    const rows = DAYS.flatMap((d) => (log[d] ?? []).map((e) => [
      DAY_LABELS[d], e.round, e.event, e.am ?? "", byAm.get(e.am)?.surname ?? "", byAm.get(e.am)?.name ?? "",
      e.club ?? "", e.club !== undefined ? nameOf(e.club) : "", e.rank ?? "", describeEvent(e, nameOf, daysOf),
    ]));
    return csvResponse(["Ημέρα", "Γύρος", "Γεγονός", "ΑΜ", "Επώνυμο", "Όνομα", "Κωδικός ομίλου", "Όμιλος", "Θέση προτίμησης", "Περιγραφή"], rows, "audit_log_katanomis.csv");
  });

  // The lottery alone: ΑΜ and number, by ΑΜ — no names, so it can be
  // announced, and two students are compared at a glance. From the
  // allocation, or for a given seed (preview).
  route("GET", "/api/admin/export/lottery.csv", async (req) => {
    session(req, "admin");
    const url = new URL(req.url);
    const results = await store.get("results");
    const seed = url.searchParams.get("seed");
    let lottery;
    if (results && !seed) lottery = new Map(results.lottery);
    else if (seed && seed.trim()) lottery = drawLottery((await getStudents()).map((s) => s.am), seed);
    else throw new HttpError(404, "Δεν έχει γίνει κατανομή· δώστε seed.");
    const byAm = (a, b) => Number(a[0]) - Number(b[0]) || String(a[0]).localeCompare(String(b[0]));
    const rows = [...lottery].sort(byAm).map(([am, n]) => [am, n]);
    return csvResponse(["ΑΜ", "Αριθμός κλήρωσης"], rows, "klirosi.csv");
  });

  // Per club and day, as the R scripts' <day>_club_reports.csv.
  route("GET", "/api/admin/export/club_summary.csv", async (req) => {
    session(req, "admin");
    const [results, log] = await Promise.all([store.get("results"), store.get("resultsLog")]);
    if (!results || !log) throw new HttpError(404, "Δεν έχει γίνει κατανομή.");
    const input = await allocationInput(results.seed);
    const rows = [];
    for (const c of input.clubs) {
      const code = String(c.code);
      for (const d of c.days) {
        const events = (log[d] ?? []).filter((e) => e.club === code);
        const count = (ev) => events.filter((e) => e.event === ev).length;
        const enrolled = results.days[d].assignments.filter((a) => a.club === code);
        const ranks = enrolled.filter((a) => a.rank).map((a) => a.rank);
        const rankedBy = Object.values(input.preferences).filter((p) => (p[d] ?? []).map(String).includes(code)).length;
        rows.push([
          code, c.name, DAY_LABELS[d], d === c.days[0] ? "κατανομή" : `από ${DAY_LABELS[c.days[0]]}`, c.capacity, enrolled.length,
          c.capacity ? Math.round((1000 * enrolled.length) / c.capacity) / 10 : "", rankedBy, count("PROPOSAL"), count("ACCEPTED"),
          count("REJECTED") + count("DISPLACED"), ranks.length ? (ranks.reduce((a, b) => a + b, 0) / ranks.length).toFixed(2) : "",
          (input.teacherLists[code] ?? []).length,
        ]);
      }
    }
    return csvResponse(["Κωδικός", "Όμιλος", "Ημέρα", "Τρόπος", "Χωρητικότητα", "Τοποθετήθηκαν", "Πληρότητα %", "Τον δήλωσαν", "Αιτήσεις", "Αποδοχές", "Απορρίψεις", "Μέση θέση προτίμησης", "Επιλογές εκπαιδευτικού"], rows, "synopsi_omilon.csv");
  });

  // One student's allocation, step by step (as the R <day>_report_<student>.txt).
  route("GET", "/api/admin/report/:am", async (req, { am }) => {
    session(req, "admin");
    const [results, story, students, clubs] = await Promise.all([store.get("results"), store.get(`story:${am}`), getStudents(), getClubs()]);
    if (!results) throw new HttpError(404, "Δεν έχει γίνει κατανομή.");
    const student = students.find((s) => s.am === am);
    if (!student) throw new HttpError(404, "Άγνωστος ΑΜ.");
    return json(200, studentReport(student, results, story ?? {}, clubs));
  });

  route("GET", "/api/admin/events", async (req) => {
    session(req, "admin");
    return json(200, { events: await store.readLog("events", 1000) });
  });

  route("GET", "/api/admin/outbox", async (req) => {
    session(req, "admin");
    if (!env.DEV && !env.SHOW_OUTBOX) throw new HttpError(404, "Δεν υπάρχει.");
    return json(200, { outbox: await store.readLog("outbox", 200), mailConfigured: Boolean(sendMail) && !env.DEV });
  });

  route("POST", "/api/admin/test-email", async (req) => {
    session(req, "admin");
    const { to } = await body(req);
    const address = String(to ?? "").trim();
    if (!EMAIL.test(address)) throw new HttpError(422, "Μη έγκυρο email.");
    const result = await mail(address, "Δοκιμαστικό μήνυμα — πλατφόρμα ομίλων",
      "Αυτό είναι δοκιμαστικό μήνυμα από την πλατφόρμα δήλωσης ομίλων.\nΑν το λάβατε (και όχι στα ανεπιθύμητα), η αποστολή email λειτουργεί.");
    if (result.status === "failed") throw new HttpError(502, `Η αποστολή απέτυχε: ${result.error}`);
    return json(200, result);
  });

  // --- teacher ---

  route("POST", "/api/admin/teacher-link", async (req) => {
    session(req, "admin");
    const { email } = await body(req);
    const teacher = (await getTeachers()).find((t) => t.email === String(email ?? "").trim().toLowerCase());
    if (!teacher) throw new HttpError(404, "Άγνωστος εκπαιδευτικός.");
    const token = signToken(env.SESSION_SECRET, { role: "magic", email: teacher.email }, ADMIN_LINK_TTL, now());
    await logEvent("admin", "teacher_link", { email: teacher.email });
    return json(200, {
      link: `${teacherPage}#token=${token}`,
      expiresAt: new Date(now() + ADMIN_LINK_TTL * 1000).toISOString(),
    });
  });

  route("POST", "/api/teacher/login", async (req) => {
    if (!sendMail) throw new HttpError(409, "Η αποστολή email δεν είναι ενεργή. Τον σύνδεσμο εισόδου σας θα σας τον δώσει η διαχείριση της πλατφόρμας.");
    const { email } = await body(req);
    const address = String(email ?? "").trim().toLowerCase();
    if (!loginByIp.hit(`t:${clientIp(req)}`, now())) throw new HttpError(429, "Πολλές προσπάθειες. Δοκιμάστε σε λίγα λεπτά.");
    const teacher = (await getTeachers()).find((t) => t.email === address);
    if (teacher) {
      const token = signToken(env.SESSION_SECRET, { role: "magic", email: address }, MAGIC_TTL, now());
      const link = `${teacherPage}#token=${token}`;
      await mail(address, "Σύνδεση στην πλατφόρμα ομίλων",
        `Καλημέρα ${teacher.name} ${teacher.surname},\n\nΓια να συνδεθείτε, ανοίξτε τον σύνδεσμο (ισχύει 20 λεπτά):\n${link}\n\nΑν δεν το ζητήσατε εσείς, αγνοήστε αυτό το μήνυμα.`);
    }
    // Same answer either way: do not reveal which addresses exist.
    return json(200, { message: "Αν η διεύθυνση είναι καταχωρισμένη, στάλθηκε σύνδεσμος σύνδεσης. Ελέγξτε το email σας." });
  });

  route("POST", "/api/teacher/session", async (req) => {
    const { token } = await body(req);
    const payload = verifyToken(env.SESSION_SECRET, token, now());
    if (!payload || payload.role !== "magic") throw new HttpError(401, "Ο σύνδεσμος έληξε ή δεν είναι έγκυρος. Ζητήστε νέο.");
    const teacher = (await getTeachers()).find((t) => t.email === payload.email);
    if (!teacher) throw new HttpError(401, "Ο λογαριασμός δεν υπάρχει πια.");
    await logEvent(teacher.email, "teacher_login");
    return json(200, { token: issue("teacher", { email: teacher.email }) });
  });

  // The teachers' common code (set by the admin, like the parents' code)
  // together with the teacher's own ΑΜ or ΑΦΜ (from the clubs file): the
  // teacher sees only their own clubs. The session carries a fingerprint of
  // the code, so a new code ends the old sessions.
  const codeFingerprint = (hash) => createHash("sha256").update(String(hash)).digest("base64url").slice(0, 12);

  route("POST", "/api/teacher/code-login", async (req) => {
    if (!teacherCodeByIp.hit(clientIp(req), now())) throw new HttpError(429, "Πολλές προσπάθειες. Δοκιμάστε σε λίγα λεπτά.");
    const { password, personalId } = await body(req);
    const settings = await getSettings();
    if (!settings.teacherPasswordHash) throw new HttpError(409, "Η είσοδος με κωδικό δεν έχει ενεργοποιηθεί. Επικοινωνήστε με τη διαχείριση.");
    const id = normalizePersonalId(personalId);
    const teacher = id ? (await getTeachers()).find((t) => t.idHash && t.idHash === personalIdHash(env.SESSION_SECRET, id)) : null;
    // One answer for a wrong code and an unknown ΑΜ/ΑΦΜ
    if (!verifyPassword(password, settings.teacherPasswordHash) || !teacher) {
      throw new HttpError(401, "Ο κωδικός ή ο ΑΜ/ΑΦΜ δεν είναι σωστός. Αν συνεχίζει, επικοινωνήστε με τη διαχείριση.");
    }
    await logEvent(teacher.email, "teacher_login", { via: "code" });
    return json(200, { token: issue("teacher", { email: teacher.email, code: codeFingerprint(settings.teacherPasswordHash) }) });
  });

  /** The logged-in teacher (by e-mail); a login with an old common code no longer counts. */
  const teacherSession = async (req) => {
    const payload = session(req, "teacher");
    if (payload.code) {
      const { teacherPasswordHash } = await getSettings();
      if (!teacherPasswordHash || payload.code !== codeFingerprint(teacherPasswordHash)) throw new HttpError(401, "Ο κωδικός άλλαξε. Συνδεθείτε ξανά.");
    }
    const teacher = (await getTeachers()).find((t) => t.email === payload.email);
    if (!teacher) throw new HttpError(401, "Ο λογαριασμός δεν υπάρχει πια.");
    return teacher;
  };

  route("GET", "/api/teacher/me", async (req) => {
    const teacher = await teacherSession(req);
    const { email } = teacher;
    const [settings, teachers, clubs, students, lists] = await Promise.all([getSettings(), getTeachers(), getClubs(), getStudents(), getTeacherLists()]);
    const mine = clubs.filter((c) => teacher.clubs.includes(c.code)).map((c) => ({
      ...c,
      list: lists[c.code]?.ams ?? [],
      updatedBy: lists[c.code]?.updatedBy ?? null,
      updatedAt: lists[c.code]?.updatedAt ?? null,
      coTeachers: teachers.filter((t) => t.email !== email && t.clubs.includes(c.code)).map((t) => `${t.name} ${t.surname}`),
      eligible: students.filter((s) => c.grades.includes(s.grade)).map(({ am, surname, name, grade }) => ({ am, surname, name, grade })),
    }));
    return json(200, {
      teacher: { name: teacher.name, surname: teacher.surname, email },
      phase: settings.phase, canEdit: settings.phase === "teachers", clubs: mine,
    });
  });

  route("PUT", "/api/teacher/clubs/:code", async (req, { code }) => {
    const teacher = await teacherSession(req);
    if (!teacher.clubs.includes(Number(code))) throw new HttpError(403, "Ο όμιλος δεν είναι δικός σας.");
    if ((await getSettings()).phase !== "teachers") throw new HttpError(409, "Οι αλλαγές από εκπαιδευτικούς γίνονται μόνο στη φάση «Εκπαιδευτικοί».");
    return saveTeacherList(req, code, teacher.email);
  });

  // The capacity is set by the admin only (the clubs file, or «Όμιλοι» →
  // «Διόρθωση»); teachers and the bulk import change only the list.
  async function saveTeacherList(req, code, who) {
    const { capacity, ams } = await body(req);
    const entry = await writeTeacherList(code, (Array.isArray(ams) ? ams : []).map(String), who, who === "admin" ? capacity : undefined);
    return json(200, { list: entry });
  }

  async function writeTeacherList(code, list, who, capacity) {
    const settings = await getSettings();
    if (phaseAtLeast(settings.phase, "parents")) throw new HttpError(409, "Οι λίστες κλείδωσαν με το άνοιγμα των δηλώσεων.");
    const club = (await getClubs()).find((c) => String(c.code) === String(code)); // current capacity applied
    if (!club) throw new HttpError(404, "Άγνωστος όμιλος.");
    // Only the admin's «Διόρθωση» stores a capacity (overriding the file's);
    // a list saved by a teacher or from a file keeps whatever applies.
    const previous = await store.get(`teacherList:${club.code}`);
    const override = capacity !== undefined ? capacity : previous?.capacity;
    if (capacity === undefined) capacity = club.capacity;
    if (!Number.isInteger(capacity) || capacity <= 0 || capacity > 500) throw new HttpError(422, "Η χωρητικότητα πρέπει να είναι θετικός ακέραιος.");
    const studentsByAm = new Map((await getStudents()).map((s) => [s.am, s]));
    const problems = validateTeacherList({ ...club, capacity }, list, studentsByAm);
    if (problems.length) throw new HttpError(422, problems.join(" "), { problems });
    const entry = { ...(override !== undefined ? { capacity } : {}), ams: list, updatedBy: who, updatedAt: new Date(now()).toISOString() };
    await store.set(`teacherList:${club.code}`, entry);
    await logEvent(who, "teacher_list", { club: club.code, capacity, count: list.length });
    // Tell the club's teachers (except the one who saved).
    for (const t of (await getTeachers()).filter((t) => t.clubs.includes(club.code) && t.email !== who)) {
      await mail(t.email, `Αλλαγή στον όμιλο «${club.name}»`,
        `Η λίστα μαθητών του ομίλου «${club.name}» άλλαξε από ${who === "admin" || who === "admin-file" ? "τον διαχειριστή" : who}.\nΘέσεις: ${capacity}, μαθητές στη λίστα: ${list.length}.`);
    }
    return entry;
  }

  // --- parent ---

  route("POST", "/api/parent/login", async (req) => {
    const ip = clientIp(req);
    if (!loginByIp.hit(`p:${ip}`, now())) throw new HttpError(429, "Πολλές προσπάθειες. Δοκιμάστε σε λίγα λεπτά.");
    const { password, am, surname, name, father, mother } = await body(req);
    const settings = await getSettings();
    if (!phaseAtLeast(settings.phase, "parents")) throw new HttpError(409, "Οι δηλώσεις δεν έχουν ανοίξει ακόμα.");
    const key = String(am ?? "").trim();
    if (!loginByAm.hit(`am:${key}`, now())) throw new HttpError(429, "Πολλές προσπάθειες για αυτόν τον αριθμό μητρώου. Δοκιμάστε σε λίγα λεπτά.");
    if (!verifyPassword(password, settings.parentPasswordHash)) throw new HttpError(401, GENERIC_LOGIN_ERROR);
    const student = (await getStudents()).find((s) => s.am === key);
    const ok = student && sameName(surname, student.surname) && (student.loginException ||
      (givenNameMatches(name, student.name) && givenNameMatches(father, student.father) && givenNameMatches(mother, student.mother)));
    if (!ok) throw new HttpError(401, GENERIC_LOGIN_ERROR);
    loginByAm.reset(`am:${key}`);
    await logEvent(`parent:${key}`, "parent_login");
    return json(200, { token: issue("parent", { am: key }) });
  });

  route("GET", "/api/parent/me", async (req) => {
    const { am } = session(req, "parent");
    const [settings, students, clubs, submission, results] = await Promise.all([
      getSettings(), getStudents(), getClubs(), store.get(`submission:${am}`), store.get("results"),
    ]);
    const story = settings.phase === "published" && results ? await store.get(`story:${am}`) : null;
    const student = students.find((s) => s.am === am);
    if (!student) throw new HttpError(401, "Ο μαθητής δεν υπάρχει πια στον κατάλογο.");
    const byCode = new Map(clubs.map((c) => [String(c.code), c]));
    const publicClub = (c) => ({ code: String(c.code), name: c.name, description: c.description ?? "", days: c.days, ...(c.similar ? { similar: c.similar } : {}) });
    const days = DAYS.map((day) => ({
      day,
      label: DAY_LABELS[day],
      clubs: clubsToRank(clubs, student.grade, day).map((code) => publicClub(byCode.get(code))),
      // Multi-day clubs ranked on an earlier day: shown locked.
      locked: clubs.filter((c) => c.days.indexOf(day) > 0 && c.grades.includes(student.grade))
        .map((c) => ({ ...publicClub(c), firstDay: c.days[0], firstDayLabel: DAY_LABELS[c.days[0]] })),
    })).filter((d) => d.clubs.length || d.locked.length);
    const published = settings.phase === "published" && results;
    return json(200, {
      student: { am, surname: student.surname, name: student.name, grade: student.grade },
      phase: settings.phase,
      deadline: settings.deadline,
      contact: settings.contact,
      canEdit: settings.phase === "parents" && !deadlinePassed(settings),
      days,
      submission: submission
        ? {
          preferences: submission.preferences,
          parent: submission.parent,
          submittedAt: submission.submittedAt,
          receipt: receiptCode(am, submission.submittedAt, submission.preferences),
          // Every save, so a parent can spot a change they did not make.
          history: (submission.history ?? []).map((h) => ({ at: h.at, email: h.email })),
        }
        : null,
      result: published
        ? Object.fromEntries(DAYS.map((d) => [d, results.byStudent[am]?.[d] ? publicClub(byCode.get(results.byStudent[am][d])) : null]))
        : null,
      report: published ? studentReport(student, results, story ?? {}, clubs) : null,
    });
  });

  route("PUT", "/api/parent/submission", async (req) => {
    const { am } = session(req, "parent");
    const settings = await getSettings();
    if (settings.phase !== "parents") throw new HttpError(409, "Οι δηλώσεις δεν είναι ανοιχτές.");
    if (deadlinePassed(settings)) throw new HttpError(409, "Η προθεσμία έληξε.");
    const { parent, preferences } = await body(req);
    const parentName = String(parent?.name ?? "").trim();
    const parentEmail = String(parent?.email ?? "").trim().toLowerCase();
    if (!parentName) throw new HttpError(422, "Συμπληρώστε το ονοματεπώνυμο του γονέα/κηδεμόνα.");
    if (!EMAIL.test(parentEmail)) throw new HttpError(422, "Συμπληρώστε έγκυρο email.");
    const [students, clubs] = await Promise.all([getStudents(), getClubs()]);
    const student = students.find((s) => s.am === am);
    if (!student) throw new HttpError(401, "Ο μαθητής δεν υπάρχει πια στον κατάλογο.");
    const clean = Object.fromEntries(DAYS.map((d) => [d, (preferences?.[d] ?? []).map(String)]).filter(([, l]) => l.length));
    const problems = validateSubmission(clubs, student, clean);
    if (problems.length) throw new HttpError(422, problems.join(" "), { problems });

    const at = new Date(now()).toISOString();
    let previousEmail = null;
    await store.update(`submission:${am}`, (old) => {
      previousEmail = old?.parent?.email ?? null;
      return {
        preferences: clean,
        parent: { name: parentName.slice(0, 120), email: parentEmail },
        submittedAt: at,
        history: [...(old?.history ?? []), { at, email: parentEmail, preferences: clean }],
      };
    });
    await logEvent(`parent:${am}`, "submission", { email: parentEmail });

    const byCode = new Map(clubs.map((c) => [String(c.code), c.name]));
    const summary = DAYS.filter((d) => clean[d]).map((d) => `${DAY_LABELS[d]}:\n${clean[d].map((c, i) => `  ${i + 1}. ${byCode.get(c)}`).join("\n")}`).join("\n\n");
    const text = `Καταχωρίστηκε η δήλωση ομίλων για τον/την μαθητή/τρια ${student.surname} ${student.name} (${at.slice(0, 16).replace("T", " ")}).\n\n${summary}\n\nΜπορείτε να την αλλάξετε μέχρι την προθεσμία.`;
    await mail(parentEmail, "Επιβεβαίωση δήλωσης ομίλων", text);
    if (previousEmail && previousEmail !== parentEmail) {
      await mail(previousEmail, "Αλλαγή δήλωσης ομίλων",
        `Η δήλωση ομίλων για τον/την μαθητή/τρια ${student.surname} ${student.name} άλλαξε από άλλη διεύθυνση email (${parentEmail}). Αν δεν το κάνατε εσείς, επικοινωνήστε με το σχολείο.`);
    }
    return json(200, { submittedAt: at, receipt: receiptCode(am, at, clean) });
  });

  // ---------- helpers using the routes' data ----------

  /**
   * Declarations that no longer match the clubs: a club removed, moved to
   * another day, or no longer for the student's grade. Happens only when a
   * new clubs file is uploaded after (trial) declarations were imported;
   * real declarations open after the clubs lock.
   */
  function staleSubmissions(submissions, clubs, students) {
    const byCode = new Map(clubs.map((c) => [String(c.code), c]));
    const gradeOf = new Map(students.map((st) => [st.am, st.grade]));
    const stale = [];
    for (const [am, sub] of Object.entries(submissions)) {
      for (const [day, list] of Object.entries(sub.preferences ?? {})) {
        for (const code of list ?? []) {
          const club = byCode.get(String(code));
          const why = !club ? "δεν υπάρχει πια" : !club.days.includes(day) ? `δεν γίνεται πια ${DAY_LABELS[day]}` : !club.grades.includes(gradeOf.get(am)) ? "δεν απευθύνεται πια στην τάξη του" : null;
          if (why) stale.push({ am, day, code: String(code), why });
        }
      }
    }
    return stale;
  }
  const staleText = (stale) => {
    const students = new Set(stale.map((x) => x.am)).size;
    const examples = stale.slice(0, 3).map((x) => `ΑΜ ${x.am}: ο όμιλος ${x.code} ${x.why}`).join("· ");
    return `${students} δηλώσεις αναφέρονται σε ομίλους που άλλαξαν μετά την καταχώρισή τους (${examples}${stale.length > 3 ? "…" : ""}). Συμβαίνει όταν ανεβαίνει νέο αρχείο ομίλων μετά την εισαγωγή δηλώσεων.`;
  };

  async function allocationInput(seed) {
    const [students, clubs, lists, submissions, settings] = await Promise.all([getStudents(), getClubs(), getTeacherLists(), getSubmissions(), getSettings()]);
    return {
      mandatoryGrades: settings.mandatoryGrades,
      students: students.map(({ am, grade, surname, name }) => ({ am, grade, surname, name })),
      clubs,
      preferences: Object.fromEntries(Object.entries(submissions).map(([am, s]) => [am, s.preferences])),
      teacherLists: Object.fromEntries(Object.entries(lists).filter(([, l]) => l.ams.length).map(([code, l]) => [code, l.ams])),
      lottery: drawLottery(students.map((s) => s.am), seed),
    };
  }

  /** Per day: club or reason, and the steps that led there. */
  function studentReport(student, results, story, clubs) {
    const gaps = gapReasons(results, [student], clubs)[student.am] ?? {};
    const lottery = new Map(results.lottery).get(student.am) ?? null;
    const nameOf = new Map(clubs.map((c) => [String(c.code), c.name]));
    return {
      am: student.am,
      lottery,
      lotteryOf: results.lottery.length,
      seed: results.seed,
      days: Object.fromEntries(DAYS.map((d) => {
        const code = results.byStudent[student.am]?.[d] ?? null;
        return [d, {
          club: code ? { code, name: nameOf.get(code) ?? code } : null,
          gap: code ? null : gaps[d] ?? null,
          gapText: code ? null : GAP_REASONS[gaps[d]] ?? null,
          steps: (story[d] ?? []).map((s) => s.text),
        }];
      })),
    };
  }

  function summarizeResults(results, input, logByDay) {
    const byDayClub = {};
    for (const d of DAYS) {
      const counts = {};
      for (const a of results.days[d].assignments) counts[a.club] = (counts[a.club] ?? 0) + 1;
      byDayClub[d] = counts;
    }
    const gaps = gapReasons(results, input.students, input.clubs);
    const gapCounts = Object.fromEntries(DAYS.map((d) => [d, { all_rejected: 0, no_preferences: 0, not_offered: 0 }]));
    for (const byDay of Object.values(gaps)) for (const [d, reason] of Object.entries(byDay)) gapCounts[d][reason]++;
    return {
      seed: results.seed,
      at: results.at,
      byStudent: results.byStudent,
      gaps,
      gapCounts,
      unassigned: Object.fromEntries(DAYS.map((d) => [d, results.days[d].unassigned])),
      enrolled: byDayClub,
      submitted: Object.keys(input.preferences).length,
      mandatoryGrades: input.mandatoryGrades,
      stats: allocationStats(results, input, logByDay),
      // Students of mandatory grades left without a club (per day)
      mandatoryGaps: Object.entries(gaps).flatMap(([am, byDay]) => {
        const grade = input.students.find((s) => s.am === am)?.grade;
        if (!input.mandatoryGrades.includes(grade)) return [];
        return Object.entries(byDay).filter(([, r]) => r !== "not_offered").map(([day, reason]) => ({ am, day, reason }));
      }),
    };
  }

  // ---------- dispatcher ----------

  return async function handle(request) {
    const url = new URL(request.url);
    try {
      for (const r of routes) {
        const m = r.method === request.method && url.pathname.match(r.re);
        if (m) return await r.fn(request, Object.fromEntries(r.keys.map((k, i) => [k, decodeURIComponent(m[i + 1])])));
      }
      return json(404, { error: "Δεν βρέθηκε." });
    } catch (err) {
      if (err instanceof HttpError) return json(err.status, { error: err.message, ...err.extra });
      console.error(err);
      return json(500, { error: "Σφάλμα διακομιστή." });
    }
  };
}
