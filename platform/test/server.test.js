import { test } from "node:test";
import assert from "node:assert/strict";
import { createApp } from "../src/server/app.js";
import { createMemoryStore } from "../src/server/store.js";
import { signToken, verifyToken, hashPassword, verifyPassword } from "../src/server/auth.js";
import { makeZip, crc32 } from "../src/server/zip.js";
import { createSupabaseStore } from "../src/server/store-supabase.js";
import { createFakePostgrest } from "./fake-postgrest.js";

const STORES = {
  memory: () => createMemoryStore(),
  supabase: () => {
    const fake = createFakePostgrest();
    return createSupabaseStore({ url: "https://proj.supabase.co", key: fake.key, fetch: fake.fetch });
  },
};

const env = { SESSION_SECRET: "test-secret-0123456789", ADMIN_PASSWORD: "admin-pass", BASE_URL: "http://localhost", DEV: true };

function setup({ now = Date.parse("2026-10-01T09:00:00Z"), store = createMemoryStore() } = {}) {
  const clock = { now };
  const handle = createApp({ store, env, now: () => clock.now });
  const call = async (method, path, { token, body, ip = "1.2.3.4" } = {}) => {
    const res = await handle(new Request(`http://localhost${path}`, {
      method,
      headers: { "content-type": "application/json", "x-forwarded-for": ip, ...(token ? { authorization: `Bearer ${token}` } : {}) },
      body: body === undefined ? undefined : JSON.stringify(body),
    }));
    const type = res.headers.get("content-type") ?? "";
    return { status: res.status, data: type.includes("json") ? await res.json() : new Uint8Array(await res.arrayBuffer()), headers: res.headers };
  };
  return { store, clock, call };
}

// Fictional school: the myschool list and the clubs template as rows.
const STUDENT_ROWS = [
  ["Τάξη", "Αριθμός Μητρώου", "Επώνυμο", "Όνομα", "Όνομα πατέρα", "Όνομα μητέρας"],
  ["Β", 9001, "ΠΑΠΑΔΟΠΟΥΛΟΣ", "ΝΙΚΟΛΑΟΣ", "ΓΕΩΡΓΙΟΣ", "ΜΑΡΙΑ"],
  ["Β", 9002, "ΧΡΙΣΤΟΦΟΡΙΔΗΣ", "ΑΝΝΑ ΠΑΝΩΡΙΑ", "ΙΩΑΝΝΗΣ", "ΕΛΕΝΗ-ΜΑΡΙΑ"],
  ["Α", 9003, "ΔΗΜΟΥ", "ΣΟΦΙΑ", "ΠΕΤΡΟΣ", "ΒΑΪΑ"],
  ["Β", 9004, "ΝΤΟΚΑ", "ΑΡΜΠΕΡΑ", "ΑΡΜΠΕΝ", "ΛΙΝΤΙΑ"],
];
const CLUB_ROWS = [
  ["Κωδικός", "Όνομα ομίλου", "Ημέρα 1", "Ημέρα 2", "Ημέρα 3", "Τάξεις", "Χωρητικότητα", "Ώρες", "Περιγραφή"],
  [100, "Αντιγόνη", "Δευτέρα", "Πέμπτη", "", "Β", 1, "", "Θέατρο"],
  [101, "Ρομποτική", "Δευτέρα", "", "", "Α-Β", 5, "", ""],
  [102, "Άλγεβρα", "Πέμπτη", "", "", "Β", 5, "", ""],
  [103, "Χορωδία", "Πέμπτη", "", "", "Α", 5, "", ""],
];
const TEACHER_ROWS = [
  ["Κωδικός ομίλου", "Επώνυμο", "Όνομα", "Email", "Όμιλος (έλεγχος)"],
  [100, "ΘΕΑΤΡΙΚΟΥ", "ΕΛΕΝΗ", "etheatr@sch.gr", ""],
  [100, "ΣΚΗΝΙΚΟΥ", "ΝΙΚΟΣ", "nskin@sch.gr", ""],
  [101, "ΜΗΧΑΝΙΚΟΥ", "ΑΝΝΑ", "amix@sch.gr", ""],
  [102, "ΜΑΘΗΜΑΤΙΚΟΥ", "ΚΩΣΤΑΣ", "kmath@sch.gr", ""],
  [103, "ΜΟΥΣΙΚΟΥ", "ΜΑΡΙΑ", "mmous@sch.gr", ""],
];

async function adminLogin(call) {
  const r = await call("POST", "/api/admin/login", { body: { password: "admin-pass" } });
  assert.equal(r.status, 200);
  return r.data.token;
}

for (const [storeName, makeStore] of Object.entries(STORES)) test(`the whole year (${storeName} store): upload → teachers → parents → allocation → results`, async () => {
  const { call, store, clock } = setup({ store: makeStore() });
  assert.equal((await call("POST", "/api/admin/login", { body: { password: "wrong" } })).status, 401);
  let admin = await adminLogin(call);

  // Upload data
  let r = await call("PUT", "/api/admin/students", { token: admin, body: { rows: STUDENT_ROWS } });
  assert.equal(r.status, 200);
  assert.equal(r.data.count, 4);
  r = await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs: CLUB_ROWS, teachers: TEACHER_ROWS } });
  assert.equal(r.status, 200);
  assert.deepEqual(r.data.report.summary, { clubs: 4, multiDay: 1, teachers: 5 });

  // Parents cannot open before the password and deadline are set
  r = await call("POST", "/api/admin/phase", { token: admin, body: { phase: "parents" } });
  assert.equal(r.status, 409);
  assert.match(r.data.error, /κωδικό γονέων, προθεσμία/);

  // Teachers' phase: magic link → session → list
  assert.equal((await call("POST", "/api/admin/phase", { token: admin, body: { phase: "teachers" } })).status, 200);
  // No e-mail service: the teacher gets the login link from the admin.
  assert.equal((await call("GET", "/api/public")).data.mailEnabled, false);
  r = await call("POST", "/api/teacher/login", { body: { email: "etheatr@sch.gr" } });
  assert.equal(r.status, 409);
  assert.match(r.data.error, /θα σας τον δώσει η διαχείριση/);
  assert.equal((await call("POST", "/api/admin/teacher-link", { token: admin, body: { email: "nobody@sch.gr" } })).status, 404);
  r = await call("POST", "/api/admin/teacher-link", { token: admin, body: { email: "EThEatr@sch.gr " } });
  assert.equal(r.status, 200);
  assert.equal(Date.parse(r.data.expiresAt) - clock.now, 7 * 24 * 3600 * 1000, "valid a week");
  const magic = r.data.link.match(/^http:\/\/localhost\/teacher\.html#token=(\S+)$/)[1];
  r = await call("POST", "/api/teacher/session", { body: { token: magic } });
  assert.equal(r.status, 200);
  const teacher = r.data.token;
  r = await call("GET", "/api/teacher/me", { token: teacher });
  assert.deepEqual(r.data.clubs.map((c) => c.code), [100]);
  assert.deepEqual(r.data.clubs[0].coTeachers, ["ΝΙΚΟΣ ΣΚΗΝΙΚΟΥ"]);
  assert.deepEqual(r.data.clubs[0].eligible.map((s) => s.am), ["9001", "9002", "9004"], "only grade Β");
  r = await call("PUT", "/api/teacher/clubs/100", { token: teacher, body: { capacity: 2, ams: ["9003"] } });
  assert.equal(r.status, 422, "grade Α student refused");
  r = await call("PUT", "/api/teacher/clubs/100", { token: teacher, body: { capacity: 1, ams: ["9001"] } });
  assert.equal(r.status, 200);
  assert.equal((await store.readLog("outbox")).at(-1).to, "nskin@sch.gr", "co-teacher notified");
  assert.equal((await call("PUT", "/api/teacher/clubs/101", { token: teacher, body: { capacity: 1, ams: [] } })).status, 403);

  // Open declarations
  await call("PUT", "/api/admin/settings", { token: admin, body: { parentPassword: "omiloi2026", deadline: "2026-10-10T21:00:00Z", contact: "2310 000000" } });
  assert.equal((await call("POST", "/api/admin/phase", { token: admin, body: { phase: "parents" } })).status, 200);
  assert.equal((await call("PUT", "/api/teacher/clubs/100", { token: teacher, body: { capacity: 1, ams: [] } })).status, 409, "lists locked");
  assert.equal((await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs: CLUB_ROWS, teachers: TEACHER_ROWS } })).status, 409);

  // Parent logins
  const login = (fields) => call("POST", "/api/parent/login", { body: { password: "omiloi2026", ...fields } });
  r = await login({ am: "9002", surname: "Χριστοφορίδης", name: "Άννα", father: "Ιωάννης", mother: "Ελένη Μαρία" });
  assert.equal(r.status, 200, "compound names: part or whole, accents and hyphen ignored");
  const christoforidis = r.data.token;
  r = await login({ am: "9002", surname: "Χριστοφορίδης", name: "Άννα", father: "Γιάννης", mother: "Ελένη" });
  assert.equal(r.status, 401);
  assert.match(r.data.error, /δεν ταιριάζουν/);
  assert.equal((await call("POST", "/api/parent/login", { body: { password: "wrong", am: "9002" } })).status, 401);

  // Login exception: only ΑΜ + surname
  r = await login({ am: "9004", surname: "Ντόκα", name: "Arbera", father: "", mother: "" });
  assert.equal(r.status, 401);
  await call("PATCH", "/api/admin/students/9004", { token: admin, body: { loginException: true } });
  r = await login({ am: "9004", surname: "Ντόκα" });
  assert.equal(r.status, 200);
  const ntoka = r.data.token;

  // What the parent ranks: Αντιγόνη on Monday, locked on Thursday
  r = await call("GET", "/api/parent/me", { token: christoforidis });
  const mon = r.data.days.find((d) => d.day === "mon");
  const thu = r.data.days.find((d) => d.day === "thu");
  assert.deepEqual(mon.clubs.map((c) => c.code), ["100", "101"]);
  assert.deepEqual(thu.clubs.map((c) => c.code), ["102"]);
  assert.deepEqual(thu.locked.map((c) => [c.code, c.firstDayLabel]), [["100", "Δευτέρα"]]);
  assert.equal(r.data.canEdit, true);

  // Submissions
  const submit = (token, preferences, email = "parent@example.com") =>
    call("PUT", "/api/parent/submission", { token, body: { parent: { name: "Γονέας", email }, preferences } });
  r = await submit(christoforidis, { mon: ["100"], thu: ["102"] });
  assert.equal(r.status, 422, "all clubs of the day must be ranked");
  r = await submit(christoforidis, { mon: ["100", "101"], thu: ["102"] }, "first@example.com");
  assert.equal(r.status, 200);
  r = await submit(christoforidis, { mon: ["100", "101"], thu: ["102"] }, "second@example.com");
  const mails = (await store.readLog("outbox")).slice(-2).map((m) => m.to);
  assert.deepEqual(mails, ["second@example.com", "first@example.com"], "previous address told about the change (when mail works)");
  const receipt = r.data.receipt;
  assert.match(receipt, /^[0-9A-F]{4}-[0-9A-F]{4}$/);
  const mine = (await call("GET", "/api/parent/me", { token: christoforidis })).data.submission;
  assert.equal(mine.receipt, receipt, "the page shows the same code as the receipt");
  assert.deepEqual(mine.history.map((h) => h.email), ["first@example.com", "second@example.com"], "every change is visible to the parent");
  assert.equal((await submit(ntoka, { mon: ["101", "100"], thu: ["102"] })).status, 200);
  const pap = (await login({ am: "9001", surname: "ΠΑΠΑΔΟΠΟΥΛΟΣ", name: "ΝΙΚΟΛΑΟΣ", father: "ΓΕΩΡΓΙΟΣ", mother: "ΜΑΡΙΑ" })).data.token;
  assert.equal((await submit(pap, { mon: ["100", "101"], thu: ["102"] })).status, 200);

  // Deadline
  clock.now = Date.parse("2026-10-11T00:00:00Z");
  assert.equal((await call("GET", "/api/parent/me", { token: pap })).status, 401, "sessions expire");
  const pap2 = (await login({ am: "9001", surname: "ΠΑΠΑΔΟΠΟΥΛΟΣ", name: "ΝΙΚΟΛΑΟΣ", father: "ΓΕΩΡΓΙΟΣ", mother: "ΜΑΡΙΑ" })).data.token;
  assert.equal((await submit(pap2, { mon: ["101", "100"], thu: ["102"] })).status, 409);
  assert.equal((await call("GET", "/api/parent/me", { token: pap2 })).data.canEdit, false);
  admin = await adminLogin(call);

  // Allocation
  assert.equal((await call("POST", "/api/admin/allocate", { token: admin, body: { seed: "x" } })).status, 409, "only after closing");
  await call("POST", "/api/admin/phase", { token: admin, body: { phase: "closed" } });
  r = await call("POST", "/api/admin/allocate", { token: admin, body: { seed: "Κλήρωση 2026" } });
  assert.equal(r.status, 200);
  const by = r.data.results.byStudent;
  // Αντιγόνη has 1 seat; 9001 and 9002 both ranked it 1st and the
  // teacher chose 9001.
  assert.equal(by["9001"].mon, "100");
  assert.equal(by["9001"].thu, "100");
  assert.equal(by["9002"].mon, "101");
  assert.equal(by["9002"].thu, "102");
  assert.equal(by["9003"].mon, null, "no submission");
  assert.deepEqual(r.data.results.unassigned.mon.map((u) => [u.am, u.reason]), [["9003", "no_preferences"]]);

  // Exports
  r = await call("GET", "/api/admin/export/r-package.zip", { token: admin });
  assert.equal(r.status, 200);
  assert.equal(r.headers.get("content-type"), "application/zip");
  const zipText = new TextDecoder().decode(r.data);
  for (const f of ["students.csv", "clubs.csv", "preferences.csv", "teacher_lists.csv", "lottery.csv", "seed.txt", "Κλήρωση 2026"]) assert.ok(zipText.includes(f), f);
  assert.ok(!zipText.includes("ΓΕΩΡΓΙΟΣ"), "parents' names are not exported");
  r = await call("GET", "/api/admin/export/results.csv", { token: admin });
  const csv = new TextDecoder().decode(r.data);
  assert.match(csv, /9001,ΠΑΠΑΔΟΠΟΥΛΟΣ,ΝΙΚΟΛΑΟΣ,Β,Αντιγόνη,,,Αντιγόνη,/);

  // Publication
  const fresh = (await login({ am: "9002", surname: "Χριστοφορίδης", name: "Πανωρία", father: "Ιωάννης", mother: "Μαρία" })).data.token;
  r = await call("GET", "/api/parent/me", { token: fresh });
  assert.equal(r.status, 200);
  assert.equal(r.data.result, null, "not before publication");
  await call("POST", "/api/admin/phase", { token: admin, body: { phase: "published" } });
  r = await call("GET", "/api/parent/me", { token: fresh });
  assert.equal(r.data.result.mon.name, "Ρομποτική");
  assert.equal(r.data.result.thu.name, "Άλγεβρα");
});

test("students can only be added once declarations are open", async () => {
  const { call } = setup();
  const admin = await adminLogin(call);
  await call("PUT", "/api/admin/students", { token: admin, body: { rows: STUDENT_ROWS } });
  await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs: CLUB_ROWS, teachers: TEACHER_ROWS } });
  await call("PUT", "/api/admin/settings", { token: admin, body: { parentPassword: "omiloi2026", deadline: "2026-10-10T21:00:00Z" } });
  await call("POST", "/api/admin/phase", { token: admin, body: { phase: "parents" } });
  const rows = [STUDENT_ROWS[0], ["Γ", 9001, "ΑΛΛΑΓΜΕΝΟ", "ΟΝΟΜΑ", "Χ", "Ψ"], ["Α", 9010, "ΝΕΟΣ", "ΜΑΘΗΤΗΣ", "Χ", "Ψ"]];
  const r = await call("PUT", "/api/admin/students", { token: admin, body: { rows } });
  assert.equal(r.status, 200);
  assert.equal(r.data.count, 5);
  const messages = r.data.report.problems.map((p) => p.message).join(" | ");
  assert.match(messages, /3 μαθητές λείπουν/);
  assert.match(messages, /1 μαθητές έχουν αλλαγές.*9001/);
  const state = await call("GET", "/api/admin/state", { token: admin });
  assert.equal(state.data.students.find((s) => s.am === "9001").surname, "ΠΑΠΑΔΟΠΟΥΛΟΣ");
});

test("files with errors are refused with the report", async () => {
  const { call } = setup();
  const admin = await adminLogin(call);
  const r = await call("PUT", "/api/admin/students", { token: admin, body: { rows: [STUDENT_ROWS[0], ["Δ", 1, "Α", "Β", "Γ", "Δ"]] } });
  assert.equal(r.status, 422);
  assert.equal(r.data.report.problems[0].field, "grade");
  assert.equal((await call("GET", "/api/admin/state", { token: admin })).data.students.length, 0);
});

test("parent login is rate-limited per ΑΜ", async () => {
  const { call } = setup();
  const admin = await adminLogin(call);
  await call("PUT", "/api/admin/students", { token: admin, body: { rows: STUDENT_ROWS } });
  await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs: CLUB_ROWS, teachers: TEACHER_ROWS } });
  await call("PUT", "/api/admin/settings", { token: admin, body: { parentPassword: "omiloi2026", deadline: "2026-10-10T21:00:00Z" } });
  await call("POST", "/api/admin/phase", { token: admin, body: { phase: "parents" } });
  let last;
  for (let i = 0; i < 11; i++) {
    last = await call("POST", "/api/parent/login", { ip: `10.0.0.${i}`, body: { password: "omiloi2026", am: "9001", surname: "Χ" } });
  }
  assert.equal(last.status, 429);
});

test("endpoints need the right session", async () => {
  const { call } = setup();
  assert.equal((await call("GET", "/api/admin/state")).status, 401);
  const parentToken = signToken(env.SESSION_SECRET, { role: "parent", am: "1" }, 60);
  assert.equal((await call("GET", "/api/admin/state", { token: parentToken })).status, 401);
  const forged = signToken("another-secret-0123456789", { role: "admin" }, 60);
  assert.equal((await call("GET", "/api/admin/state", { token: forged })).status, 401);
});

test("tokens expire; passwords are hashed", () => {
  const t = signToken(env.SESSION_SECRET, { role: "admin" }, 60, 0);
  assert.equal(verifyToken(env.SESSION_SECRET, t, 30_000).role, "admin");
  assert.equal(verifyToken(env.SESSION_SECRET, t, 61_000), null);
  const h = hashPassword("omiloi2026");
  assert.ok(!h.includes("omiloi2026"));
  assert.ok(verifyPassword("omiloi2026", h));
  assert.ok(!verifyPassword("omiloi2027", h));
});

test("zip: standard CRC and structure", () => {
  assert.equal(crc32(new TextEncoder().encode("123456789")), 0xcbf43926);
  const zip = makeZip({ "a.txt": "hello", "β.csv": "x,y\n" }, new Date(2026, 9, 1));
  const view = new DataView(zip.buffer);
  assert.equal(view.getUint32(0, true), 0x04034b50);
  assert.equal(view.getUint32(zip.length - 22, true), 0x06054b50);
  assert.equal(view.getUint16(zip.length - 22 + 10, true), 2);
});

for (const [storeName, makeStore] of Object.entries(STORES)) test(`reset deletes all school data (${storeName} store)`, async () => {
  const store = makeStore();
  const { call } = setup({ store });
  const admin = await adminLogin(call);
  await call("PUT", "/api/admin/students", { token: admin, body: { rows: STUDENT_ROWS } });
  await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs: CLUB_ROWS, teachers: TEACHER_ROWS } });
  await call("PUT", "/api/admin/settings", { token: admin, body: { parentPassword: "omiloi2026", deadline: "2026-10-10T21:00:00Z", schoolName: "Γυμνάσιο", contact: "2310" } });
  await call("POST", "/api/admin/phase", { token: admin, body: { phase: "teachers" } });
  await call("PUT", "/api/admin/teacher-lists/100", { token: admin, body: { capacity: 1, ams: ["9001"] } });
  await call("POST", "/api/admin/phase", { token: admin, body: { phase: "parents" } });
  const parent = (await call("POST", "/api/parent/login", { body: { password: "omiloi2026", am: "9001", surname: "ΠΑΠΑΔΟΠΟΥΛΟΣ", name: "ΝΙΚΟΛΑΟΣ", father: "ΓΕΩΡΓΙΟΣ", mother: "ΜΑΡΙΑ" } })).data.token;
  await call("PUT", "/api/parent/submission", { token: parent, body: { parent: { name: "Γ", email: "g@example.com" }, preferences: { mon: ["100", "101"], thu: ["102"] } } });

  assert.equal((await call("POST", "/api/admin/reset", { token: admin, body: { confirm: "διαγραφη" } })).status, 422, "exact word needed");
  assert.equal((await call("POST", "/api/admin/reset", { body: { confirm: "ΔΙΑΓΡΑΦΗ" } })).status, 401, "admin only");
  const r = await call("POST", "/api/admin/reset", { token: admin, body: { confirm: "ΔΙΑΓΡΑΦΗ", keepSchoolInfo: true } });
  assert.equal(r.status, 200);

  const state = (await call("GET", "/api/admin/state", { token: admin })).data;
  assert.deepEqual([state.students.length, state.clubs.length, state.teachers.length, Object.keys(state.submissions).length, Object.keys(state.teacherLists).length], [0, 0, 0, 0, 0]);
  assert.equal(state.settings.phase, "setup");
  assert.equal(state.settings.parentPasswordSet, false);
  assert.equal(state.settings.deadline, null);
  assert.deepEqual([state.settings.schoolName, state.settings.contact], ["Γυμνάσιο", "2310"]);
  assert.deepEqual(await store.readLog("outbox"), []);
  assert.deepEqual((await store.readLog("events")).map((e) => e.what), ["reset"], "only the reset itself is logged");
  assert.equal((await call("GET", "/api/parent/me", { token: parent })).status, 401, "old parent sessions stop working");
});

test("trial import of last year's responses → allocation", async () => {
  const { call, store } = setup();
  const admin = await adminLogin(call);
  await call("PUT", "/api/admin/students", { token: admin, body: { rows: STUDENT_ROWS } });
  await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs: CLUB_ROWS, teachers: TEACHER_ROWS } });
  const mon = [["RegistryNr", "Surname", "Name", "Αντιγόνη", "Ρομποτική"], [9001, "", "", 1, 2], [9002, "", "", 2, 1], [8000, "ΠΕΡΣΙΝΟΣ", "ΜΑΘΗΤΗΣ", 1, ""]];
  let r = await call("POST", "/api/admin/import-legacy", { token: admin, body: { day: "mon", rows: mon } });
  assert.equal(r.status, 422, "unknown ΑΜ without the option");
  r = await call("POST", "/api/admin/import-legacy", { token: admin, body: { day: "mon", rows: mon, addMissingGrade: "Β" } });
  assert.equal(r.status, 200);
  assert.deepEqual(r.data.report.summary, { rows: 3, newStudents: 1, clubs: 2 });
  r = await call("POST", "/api/admin/import-legacy", { token: admin, body: { day: "thu", rows: [["RegistryNr", "Άλγεβρα"], [9001, 1], [8000, 1]] } });
  assert.equal(r.status, 200);
  const sub = await store.get("submission:9001");
  assert.deepEqual(sub.preferences, { mon: ["100", "101"], thu: ["102"] }, "days merge");
  assert.equal(sub.parent.name, "Εισαγωγή δοκιμής");

  await call("PUT", "/api/admin/settings", { token: admin, body: { parentPassword: "omiloi2026", deadline: "2026-10-10T21:00:00Z" } });
  for (const phase of ["teachers", "parents", "closed"]) await call("POST", "/api/admin/phase", { token: admin, body: { phase } });
  r = await call("POST", "/api/admin/allocate", { token: admin, body: { seed: "δοκιμή" } });
  assert.equal(r.status, 200);
  assert.equal(r.data.results.submitted, 3);
  // Αντιγόνη has one seat: 9001 and 8000 ranked it 1st (the lottery decides),
  // 9002 ranked it 2nd and gets Ρομποτική; 8000 ranked nothing else.
  assert.equal(r.data.results.byStudent["9002"].mon, "101");
  const winner = ["9001", "8000"].find((am) => r.data.results.byStudent[am].mon === "100");
  assert.ok(winner, "one of the two first-choice students gets Αντιγόνη");
  assert.equal(r.data.results.byStudent["8000"].mon, winner === "8000" ? "100" : null);
  assert.equal((await call("POST", "/api/admin/import-legacy", { token: admin, body: { day: "mon", rows: mon } })).status, 409, "not after the allocation");
});

test("reports: reasons, audit log, club summary, student story, uploads, events", async () => {
  const { call, store } = setup();
  const admin = await adminLogin(call);
  await call("PUT", "/api/admin/students", { token: admin, body: { rows: STUDENT_ROWS, fileName: "Katalogos_Mathiton.xls" } });
  await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs: CLUB_ROWS, teachers: TEACHER_ROWS, fileName: "omiloi.xlsx" } });
  await call("POST", "/api/admin/import-legacy", { token: admin, body: { day: "mon", fileName: "deutera.csv", rows: [["RegistryNr", "Αντιγόνη", "Ρομποτική"], [9001, 1, 2], [9002, 1, ""]] } });

  let state = (await call("GET", "/api/admin/state", { token: admin })).data;
  assert.deepEqual([state.uploads.students.fileName, state.uploads.students.count, state.uploads.students.byGrade], ["Katalogos_Mathiton.xls", 4, { Α: 1, Β: 3, Γ: 0 }]);
  assert.deepEqual([state.uploads.clubs.fileName, state.uploads.clubs.clubs, state.uploads.clubs.teachers], ["omiloi.xlsx", 4, 5]);
  assert.deepEqual([state.uploads.legacy.mon.fileName, state.uploads.legacy.mon.rows], ["deutera.csv", 2]);
  assert.deepEqual(state.submissions["9001"].days, ["mon"]);
  assert.equal(state.submissions["9001"].imported, true);

  await call("PUT", "/api/admin/settings", { token: admin, body: { parentPassword: "omiloi2026", deadline: "2026-10-10T21:00:00Z" } });
  for (const phase of ["teachers", "parents", "closed"]) await call("POST", "/api/admin/phase", { token: admin, body: { phase } });
  const res = (await call("POST", "/api/admin/allocate", { token: admin, body: { seed: "Σ" } })).data.results;
  // Αντιγόνη has 1 seat and both ranked it first; the loser ranked Ρομποτική
  // (9001) or nothing else (9002).
  const loser = res.byStudent["9001"].mon === "100" ? "9002" : "9001";
  assert.equal(res.gaps[loser]?.mon ?? null, loser === "9002" ? "all_rejected" : null);
  assert.equal(res.gaps["9003"].mon, "no_preferences");
  assert.equal(res.gaps["9001"].thu === undefined || res.gaps["9001"].thu === "no_preferences", true);
  assert.equal(res.gaps["9003"].thu, "no_preferences", "Χορωδία runs Thursday for Α");
  assert.equal(res.gaps["9003"].tue, "not_offered");
  assert.ok(res.gapCounts.mon.no_preferences >= 2);

  const csv = new TextDecoder().decode((await call("GET", "/api/admin/export/results.csv", { token: admin })).data);
  assert.match(csv, /9003,ΔΗΜΟΥ,ΣΟΦΙΑ,Α,— χωρίς προτιμήσεις,,,— χωρίς προτιμήσεις,/);

  const audit = new TextDecoder().decode((await call("GET", "/api/admin/export/audit_log.csv", { token: admin })).data);
  assert.match(audit, /^Ημέρα,Γύρος,Γεγονός,ΑΜ,Επώνυμο,Όνομα,Κωδικός ομίλου,Όμιλος,Θέση προτίμησης,Περιγραφή\n/);
  assert.match(audit, /Δευτέρα,1,PROPOSAL,9001,ΠΑΠΑΔΟΠΟΥΛΟΣ,ΝΙΚΟΛΑΟΣ,100,Αντιγόνη,1,Αίτηση στον όμιλο «Αντιγόνη» \(1η επιλογή\)\./);

  const summary = new TextDecoder().decode((await call("GET", "/api/admin/export/club_summary.csv", { token: admin })).data);
  assert.match(summary, /100,Αντιγόνη,Δευτέρα,κατανομή,1,1,100,2,2,/);
  assert.match(summary, /100,Αντιγόνη,Πέμπτη,από Δευτέρα,1,1,100,/);

  const report = (await call("GET", "/api/admin/report/9001", { token: admin })).data;
  assert.equal(report.lotteryOf, 4);
  assert.ok(report.lottery >= 1 && report.lottery <= 4);
  assert.ok(report.days.mon.steps.length >= 2);
  assert.equal(report.days.tue.gap, "not_offered");

  const events = (await call("GET", "/api/admin/events", { token: admin })).data.events.map((e) => e.what);
  for (const what of ["students_uploaded", "clubs_uploaded", "import_legacy", "phase", "allocated"]) assert.ok(events.includes(what), what);

  // Parents see the story only after publication
  await call("POST", "/api/admin/phase", { token: admin, body: { phase: "published" } });
  const parent = (await call("POST", "/api/parent/login", { body: { password: "omiloi2026", am: "9001", surname: "ΠΑΠΑΔΟΠΟΥΛΟΣ", name: "ΝΙΚΟΛΑΟΣ", father: "ΓΕΩΡΓΙΟΣ", mother: "ΜΑΡΙΑ" } })).data.token;
  const me = (await call("GET", "/api/parent/me", { token: parent })).data;
  assert.deepEqual(me.report.days.mon.steps, report.days.mon.steps);

  await call("POST", "/api/admin/reset", { token: admin, body: { confirm: "ΔΙΑΓΡΑΦΗ" } });
  state = (await call("GET", "/api/admin/state", { token: admin })).data;
  assert.deepEqual(state.uploads, {});
  assert.deepEqual(await store.list("story:"), []);
});

test("mandatory grades: declarations do not open without enough seats; gaps listed after the allocation", async () => {
  const { call } = setup();
  const admin = await adminLogin(call);
  await call("PUT", "/api/admin/students", { token: admin, body: { rows: STUDENT_ROWS } });
  // Monday: only Αντιγόνη (1 seat, grade Β) for the three Β students
  const clubs = [CLUB_ROWS[0], [100, "Αντιγόνη", "Δευτέρα", "", "", "Β", 1, "", ""], [101, "Ρομποτική", "Δευτέρα", "", "", "Α", 5, "", ""]];
  await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs, teachers: [TEACHER_ROWS[0], TEACHER_ROWS[1], TEACHER_ROWS[3]] } });
  await call("PUT", "/api/admin/settings", { token: admin, body: { parentPassword: "omiloi2026", deadline: "2026-10-10T21:00:00Z" } });
  let state = (await call("GET", "/api/admin/state", { token: admin })).data;
  assert.deepEqual(state.settings.mandatoryGrades, ["Α", "Β", "Γ"], "default: all grades");
  assert.deepEqual(state.readiness.filter((p) => p.level === "error").map((p) => p.message), ["Δευτέρα: οι όμιλοι για την Β τάξη έχουν 1 θέση για 3 μαθητές (υποχρεωτική ένταξη) — λείπουν 2 θέσεις."]);

  await call("POST", "/api/admin/phase", { token: admin, body: { phase: "teachers" } });
  let r = await call("POST", "/api/admin/phase", { token: admin, body: { phase: "parents" } });
  assert.equal(r.status, 409);
  assert.match(r.data.error, /υποχρεωτική ένταξη/);
  assert.equal(r.data.problems.length, 1);

  // Only Α mandatory (as last year): opens, and the Β shortfall is a warning
  assert.equal((await call("PUT", "/api/admin/settings", { token: admin, body: { mandatoryGrades: ["Δ"] } })).status, 422);
  await call("PUT", "/api/admin/settings", { token: admin, body: { mandatoryGrades: ["Α"] } });
  state = (await call("GET", "/api/admin/state", { token: admin })).data;
  assert.deepEqual(state.readiness.map((p) => p.level), ["warning"]);
  assert.equal((await call("POST", "/api/admin/phase", { token: admin, body: { phase: "parents" } })).status, 200);

  // 9003 (Α) submits nothing → a mandatory gap; Β students are not listed
  await call("POST", "/api/admin/phase", { token: admin, body: { phase: "closed" } });
  r = await call("POST", "/api/admin/allocate", { token: admin, body: { seed: "s" } });
  assert.deepEqual(r.data.results.mandatoryGrades, ["Α"]);
  assert.deepEqual(r.data.results.mandatoryGaps, [{ am: "9003", day: "mon", reason: "no_preferences" }]);
});

test("admin: «Παρεμφερείς» of a club can be set or cleared until declarations open", async () => {
  const { call } = setup();
  const admin = await adminLogin(call);
  await call("PUT", "/api/admin/students", { token: admin, body: { rows: STUDENT_ROWS } });
  await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs: CLUB_ROWS, teachers: TEACHER_ROWS } });
  let r = await call("PUT", "/api/admin/clubs/101/similar", { token: admin, body: { similar: " Ρομποτική " } });
  assert.equal(r.status, 200);
  assert.equal(r.data.club.similar, "ΡΟΜΠΟΤΙΚΗ");
  assert.equal((await call("PUT", "/api/admin/clubs/999/similar", { token: admin, body: { similar: "Χ" } })).status, 404);
  let clubs = (await call("GET", "/api/admin/state", { token: admin })).data.clubs;
  assert.equal(clubs.find((c) => c.code === 101).similar, "ΡΟΜΠΟΤΙΚΗ");
  r = await call("PUT", "/api/admin/clubs/101/similar", { token: admin, body: { similar: "" } });
  assert.equal("similar" in r.data.club, false);
  // locked once declarations are open
  await call("PUT", "/api/admin/settings", { token: admin, body: { parentPassword: "omiloi2026", deadline: "2026-10-10T21:00:00Z", mandatoryGrades: [] } });
  await call("POST", "/api/admin/phase", { token: admin, body: { phase: "teachers" } });
  assert.equal((await call("POST", "/api/admin/phase", { token: admin, body: { phase: "parents" } })).status, 200);
  assert.equal((await call("PUT", "/api/admin/clubs/101/similar", { token: admin, body: { similar: "Α" } })).status, 409);
  // R export carries the column
  r = await call("GET", "/api/admin/export/r-package.zip?seed=x", { token: admin });
  assert.equal(r.status, 200);
});
