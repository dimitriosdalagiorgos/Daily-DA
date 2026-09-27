// The platform's backend: one function handle(Request) → Response, used by
// the local dev server and by Netlify Functions alike.
//
// Phases (settings.phase), in order:
//   setup      the admin uploads students and clubs
//   teachers   teachers set capacity and (optionally) their preferred students
//   parents    parents submit rankings until the deadline; clubs and teacher
//              lists are locked, students can only be added
//   closed     no more submissions
//   allocated  the allocation has run (can be re-run while not published)
//   published  parents see the result

import { DAYS, DAY_LABELS } from "../algorithm/days.js";
import { drawLottery } from "../algorithm/lottery.js";
import { allocateWeek } from "../algorithm/allocate.js";
import { clubsToRank, validateSubmission, validateTeacherList } from "../algorithm/validate.js";
import { importStudents } from "../import/students.js";
import { importClubs } from "../import/clubs.js";
import { checkReadiness } from "../import/readiness.js";
import { importLegacyResponses } from "../import/legacy.js";
import { givenNameMatches, sameName } from "../import/names.js";
import { buildRPackage, toCsv } from "../export/rPackage.js";
import { createHash } from "node:crypto";
import { makeZip } from "./zip.js";
import { createRateLimiter, hashPassword, safeEqual, signToken, verifyPassword, verifyToken } from "./auth.js";

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

  // ---------- data helpers ----------

  const getSettings = async () => ({ phase: "setup", contact: "", deadline: null, ...(await store.get("settings")) });
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

  const logEvent = (who, what, detail = {}) => store.append("events", { at: new Date(now()).toISOString(), who, what, ...detail });

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
  const clientIp = (request) => request.headers.get("x-nf-client-connection-ip") ?? request.headers.get("x-forwarded-for")?.split(",")[0].trim() ?? "local";

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
    return json(200, { phase: s.phase, deadline: s.deadline, contact: s.contact, schoolName: s.schoolName ?? "", mailEnabled: Boolean(sendMail) });
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
    const [settings, students, clubs, teachers, lists, submissions, results] = await Promise.all([
      getSettings(), getStudents(), getClubs(), getTeachers(), getTeacherLists(), getSubmissions(), store.get("results"),
    ]);
    const { parentPasswordHash, ...publicSettings } = settings;
    return json(200, {
      settings: { ...publicSettings, parentPasswordSet: Boolean(parentPasswordHash) },
      students: students.map(({ am, grade, surname, name, loginException }) => ({ am, grade, surname, name, loginException: Boolean(loginException) })),
      clubs,
      teachers,
      teacherLists: lists,
      submissions: Object.fromEntries(Object.entries(submissions).map(([am, s]) => [am, { submittedAt: s.submittedAt, parentEmail: s.parent?.email, parentName: s.parent?.name, changes: s.history?.length ?? 1 }])),
      readiness: students.length && clubs.length ? checkReadiness(students, clubs) : [],
      results: results ? { seed: results.seed, at: results.at } : null,
    });
  });

  route("PUT", "/api/admin/students", async (req) => {
    session(req, "admin");
    const { rows } = await body(req);
    const report = importStudents(Array.isArray(rows) ? rows : []);
    if (report.problems.some((p) => p.level === "error")) throw new HttpError(422, "Το αρχείο έχει σφάλματα.", { report });

    const settings = await getSettings();
    const existing = await getStudents();
    const byAm = new Map(existing.map((s) => [s.am, s]));
    let students;
    const notes = [];
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
    } else {
      students = report.students.map((s) => ({ ...s, loginException: byAm.get(s.am)?.loginException ?? false }));
    }
    await store.set("students", students);
    await logEvent("admin", "students_uploaded", { count: students.length });
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
    const settings = await getSettings();
    if (phaseAtLeast(settings.phase, "parents")) throw new HttpError(409, "Οι όμιλοι κλείδωσαν με το άνοιγμα των δηλώσεων.");
    const { clubs: clubRows, teachers: teacherRows } = await body(req);
    const report = importClubs({ clubs: clubRows ?? [], teachers: teacherRows ?? [] });
    if (report.problems.some((p) => p.level === "error")) throw new HttpError(422, "Το αρχείο έχει σφάλματα.", { report });
    await store.set("clubs", report.clubs);
    await store.set("teachers", report.teachers);
    // Teacher lists of clubs that no longer exist are dropped.
    const codes = new Set(report.clubs.map((c) => String(c.code)));
    for (const { key } of await store.list("teacherList:")) if (!codes.has(key.slice(12))) await store.delete(key);
    await logEvent("admin", "clubs_uploaded", { clubs: report.clubs.length, teachers: report.teachers.length });
    return json(200, { report });
  });

  route("PUT", "/api/admin/settings", async (req) => {
    session(req, "admin");
    const { parentPassword, deadline, contact, schoolName } = await body(req);
    const patch = {};
    if (parentPassword !== undefined) {
      if (String(parentPassword).length < 6) throw new HttpError(422, "Ο κωδικός γονέων πρέπει να έχει τουλάχιστον 6 χαρακτήρες.");
      patch.parentPasswordHash = hashPassword(parentPassword);
    }
    if (deadline !== undefined) {
      if (deadline !== null && Number.isNaN(Date.parse(deadline))) throw new HttpError(422, "Μη έγκυρη προθεσμία.");
      patch.deadline = deadline;
    }
    if (contact !== undefined) patch.contact = String(contact).slice(0, 300);
    if (schoolName !== undefined) patch.schoolName = String(schoolName).slice(0, 120);
    await store.update("settings", (s = {}) => ({ phase: "setup", ...s, ...patch }));
    await logEvent("admin", "settings", { fields: Object.keys(patch) });
    return json(200, { ok: true });
  });

  route("POST", "/api/admin/phase", async (req) => {
    session(req, "admin");
    const { phase } = await body(req);
    if (!PHASES.includes(phase)) throw new HttpError(422, "Άγνωστη φάση.");
    const [settings, students, clubs, results] = await Promise.all([getSettings(), getStudents(), getClubs(), store.get("results")]);
    const missing = [];
    if (phaseAtLeast(phase, "teachers") && clubs.length === 0) missing.push("ομίλους");
    if (phaseAtLeast(phase, "parents")) {
      if (students.length === 0) missing.push("μαθητές");
      if (!settings.parentPasswordHash) missing.push("κωδικό γονέων");
      if (!settings.deadline) missing.push("προθεσμία");
    }
    if (phaseAtLeast(phase, "allocated") && !results) missing.push("εκτέλεση κατανομής");
    if (missing.length) throw new HttpError(409, `Για αυτή τη φάση χρειάζονται: ${missing.join(", ")}.`);
    await store.update("settings", (s = {}) => ({ ...s, phase }));
    await logEvent("admin", "phase", { from: settings.phase, to: phase });
    return json(200, { phase });
  });

  route("PUT", "/api/admin/teacher-lists/:code", async (req, { code }) => {
    session(req, "admin");
    return saveTeacherList(req, code, "admin");
  });

  // Trial only: last year's per-day responses (Google Form export) as
  // submissions, to try the allocation with real-looking data.
  route("POST", "/api/admin/import-legacy", async (req) => {
    session(req, "admin");
    const settings = await getSettings();
    if (phaseAtLeast(settings.phase, "allocated")) throw new HttpError(409, "Η εισαγωγή δοκιμής γίνεται πριν από την κατανομή.");
    const { day, rows, addMissingGrade } = await body(req);
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
    await logEvent("admin", "import_legacy", { day, rows: report.summary.rows, newStudents: report.newStudents.length });
    return json(200, { report });
  });

  // Delete all school data (e.g. after a trial with last year's data).
  // Keeps only the session secret and, if asked, the school's name/contact.
  route("POST", "/api/admin/reset", async (req) => {
    session(req, "admin");
    const { confirm, keepSchoolInfo = true } = await body(req);
    if (confirm !== "ΔΙΑΓΡΑΦΗ") throw new HttpError(422, "Για επιβεβαίωση γράψτε ΔΙΑΓΡΑΦΗ (κεφαλαία).");
    const settings = await getSettings();
    for (const key of ["students", "clubs", "teachers", "results", "resultsLog"]) await store.delete(key);
    for (const prefix of ["teacherList:", "submission:"]) {
      for (const { key } of await store.list(prefix)) await store.delete(key);
    }
    await store.deleteLog("outbox");
    await store.deleteLog("events");
    await store.set("settings", {
      phase: "setup",
      ...(keepSchoolInfo ? { schoolName: settings.schoolName ?? "", contact: settings.contact ?? "" } : {}),
    });
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
    await store.set("resultsLog", Object.fromEntries(DAYS.map((d) => [d, result.days[d].log])));
    await store.update("settings", (s = {}) => ({ ...s, phase: "allocated" }));
    await logEvent("admin", "allocated", { seed });
    return json(200, { results: summarizeResults(results, input) });
  });

  route("GET", "/api/admin/results", async (req) => {
    session(req, "admin");
    const results = await store.get("results");
    if (!results) throw new HttpError(404, "Δεν έχει γίνει κατανομή.");
    return json(200, { results: summarizeResults(results, await allocationInput(results.seed)) });
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
    const rows = [...students]
      .sort((a, b) => a.grade.localeCompare(b.grade) || a.surname.localeCompare(b.surname, "el") || a.name.localeCompare(b.name, "el"))
      .map((s) => [s.am, s.surname, s.name, s.grade, ...DAYS.map((d) => nameOf.get(results.byStudent[s.am]?.[d] ?? "") ?? "")]);
    const csv = "﻿" + toCsv(["ΑΜ", "Επώνυμο", "Όνομα", "Τάξη", ...DAYS.map((d) => DAY_LABELS[d])], rows);
    return new Response(csv, { headers: { "content-type": "text/csv; charset=utf-8", "content-disposition": 'attachment; filename="katanomi_omilon.csv"' } });
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
      link: `${env.BASE_URL ?? ""}/teacher.html#token=${token}`,
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
      const link = `${env.BASE_URL ?? ""}/teacher.html#token=${token}`;
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

  route("GET", "/api/teacher/me", async (req) => {
    const { email } = session(req, "teacher");
    const [settings, teachers, clubs, students, lists] = await Promise.all([getSettings(), getTeachers(), getClubs(), getStudents(), getTeacherLists()]);
    const teacher = teachers.find((t) => t.email === email);
    if (!teacher) throw new HttpError(401, "Ο λογαριασμός δεν υπάρχει πια.");
    const mine = clubs.filter((c) => teacher.clubs.includes(c.code)).map((c) => ({
      ...c,
      list: lists[c.code]?.ams ?? [],
      updatedBy: lists[c.code]?.updatedBy ?? null,
      updatedAt: lists[c.code]?.updatedAt ?? null,
      coTeachers: teachers.filter((t) => t.email !== email && t.clubs.includes(c.code)).map((t) => `${t.name} ${t.surname}`),
      eligible: students.filter((s) => c.grades.includes(s.grade)).map(({ am, surname, name, grade }) => ({ am, surname, name, grade })),
    }));
    return json(200, { teacher: { name: teacher.name, surname: teacher.surname, email }, phase: settings.phase, canEdit: settings.phase === "teachers", clubs: mine });
  });

  route("PUT", "/api/teacher/clubs/:code", async (req, { code }) => {
    const { email } = session(req, "teacher");
    const teacher = (await getTeachers()).find((t) => t.email === email);
    if (!teacher?.clubs.includes(Number(code))) throw new HttpError(403, "Ο όμιλος δεν είναι δικός σας.");
    if ((await getSettings()).phase !== "teachers") throw new HttpError(409, "Οι αλλαγές από εκπαιδευτικούς γίνονται μόνο στη φάση «Εκπαιδευτικοί».");
    return saveTeacherList(req, code, email);
  });

  async function saveTeacherList(req, code, who) {
    const settings = await getSettings();
    if (phaseAtLeast(settings.phase, "parents")) throw new HttpError(409, "Οι λίστες κλείδωσαν με το άνοιγμα των δηλώσεων.");
    const club = ((await store.get("clubs")) ?? []).find((c) => String(c.code) === String(code));
    if (!club) throw new HttpError(404, "Άγνωστος όμιλος.");
    const { capacity, ams } = await body(req);
    if (!Number.isInteger(capacity) || capacity <= 0 || capacity > 500) throw new HttpError(422, "Η χωρητικότητα πρέπει να είναι θετικός ακέραιος.");
    const list = (Array.isArray(ams) ? ams : []).map(String);
    const studentsByAm = new Map((await getStudents()).map((s) => [s.am, s]));
    const problems = validateTeacherList({ ...club, capacity }, list, studentsByAm);
    if (problems.length) throw new HttpError(422, problems.join(" "), { problems });
    const entry = { capacity, ams: list, updatedBy: who, updatedAt: new Date(now()).toISOString() };
    await store.set(`teacherList:${club.code}`, entry);
    await logEvent(who, "teacher_list", { club: club.code, capacity, count: list.length });
    // Tell the club's other teachers.
    for (const t of (await getTeachers()).filter((t) => t.clubs.includes(club.code) && t.email !== who)) {
      await mail(t.email, `Αλλαγή στον όμιλο «${club.name}»`,
        `Η χωρητικότητα και η λίστα μαθητών του ομίλου «${club.name}» άλλαξαν από ${who === "admin" ? "τον διαχειριστή" : who}.\nΧωρητικότητα: ${capacity}, μαθητές στη λίστα: ${list.length}.`);
    }
    return json(200, { list: entry });
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
    const student = students.find((s) => s.am === am);
    if (!student) throw new HttpError(401, "Ο μαθητής δεν υπάρχει πια στον κατάλογο.");
    const byCode = new Map(clubs.map((c) => [String(c.code), c]));
    const publicClub = (c) => ({ code: String(c.code), name: c.name, description: c.description ?? "", days: c.days });
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

  async function allocationInput(seed) {
    const [students, clubs, lists, submissions] = await Promise.all([getStudents(), getClubs(), getTeacherLists(), getSubmissions()]);
    return {
      students: students.map(({ am, grade, surname, name }) => ({ am, grade, surname, name })),
      clubs,
      preferences: Object.fromEntries(Object.entries(submissions).map(([am, s]) => [am, s.preferences])),
      teacherLists: Object.fromEntries(Object.entries(lists).filter(([, l]) => l.ams.length).map(([code, l]) => [code, l.ams])),
      lottery: drawLottery(students.map((s) => s.am), seed),
    };
  }

  function summarizeResults(results, input) {
    const byDayClub = {};
    for (const d of DAYS) {
      const counts = {};
      for (const a of results.days[d].assignments) counts[a.club] = (counts[a.club] ?? 0) + 1;
      byDayClub[d] = counts;
    }
    return {
      seed: results.seed,
      at: results.at,
      byStudent: results.byStudent,
      unassigned: Object.fromEntries(DAYS.map((d) => [d, results.days[d].unassigned])),
      enrolled: byDayClub,
      submitted: Object.keys(input.preferences).length,
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
