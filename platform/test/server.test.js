import { test } from "node:test";
import assert from "node:assert/strict";
import { createApp } from "../src/server/app.js";
import { createMemoryStore } from "../src/server/store.js";
import { signToken, verifyToken, hashPassword, verifyPassword } from "../src/server/auth.js";
import { makeZip, crc32 } from "../src/server/zip.js";
import { createSupabaseStore } from "../src/server/store-supabase.js";
import { createFakePostgrest } from "./fake-postgrest.js";
import { drawLottery } from "../src/algorithm/lottery.js";

const STORES = {
  memory: () => createMemoryStore(),
  supabase: () => {
    const fake = createFakePostgrest();
    return createSupabaseStore({ url: "https://proj.supabase.co", key: fake.key, fetch: fake.fetch });
  },
};

const env = { SESSION_SECRET: "test-secret-0123456789", ADMIN_PASSWORD: "admin-pass", BASE_URL: "http://localhost", TEACHER_PATH: "e-test1234", DEV: true };

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
  const magic = r.data.link.match(/^http:\/\/localhost\/e-test1234\/#token=(\S+)$/)[1];
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
  // The capacity is the admin's: a teacher's value is ignored
  r = await call("PUT", "/api/teacher/clubs/100", { token: teacher, body: { capacity: 30, ams: ["9001"] } });
  assert.equal(r.data.list.capacity, undefined, "a teacher's save stores no capacity");
  assert.equal((await call("GET", "/api/teacher/me", { token: teacher })).data.clubs[0].capacity, 1, "capacity unchanged by the teacher");
  assert.equal((await call("PUT", "/api/teacher/clubs/100", { token: teacher, body: { ams: ["9001", "9002"] } })).status, 422, "more than the seats");

  // Bulk import by the admin: preview, then save (replaces the list)
  const file = [["Κωδικός ομίλου", 100], ["Όμιλος", "Αντιγόνη"], [], ["Επιλογή", "ΑΜ", "Επώνυμο", "Όνομα", "Τάξη"], ["", 9001, "", "", "Β"], ["Χ", 9002, "", "", "Β"], ["", 9004, "", "", "Β"]];
  r = await call("POST", "/api/admin/teacher-lists/import", { token: admin, body: { files: [{ fileName: "100.xlsx", rows: file }] } });
  assert.equal(r.status, 200);
  assert.equal(r.data.ok, true);
  assert.deepEqual(r.data.report[0].ams, ["9002"]);
  assert.match(r.data.report[0].warnings[0], /θα αντικατασταθεί/);
  assert.deepEqual((await call("GET", "/api/admin/state", { token: admin })).data.teacherLists[100].ams, ["9001"], "preview saves nothing");
  const bad = [["Κωδικός ομίλου", "ΑΜ"], [101, 9003], [101, 7777]];
  r = await call("POST", "/api/admin/teacher-lists/import", { token: admin, body: { files: [{ fileName: "100.xlsx", rows: file }, { fileName: "bad.csv", rows: bad }], apply: true } });
  assert.equal(r.status, 422, "nothing saved while a file has errors");
  r = await call("POST", "/api/admin/teacher-lists/import", { token: admin, body: { files: [{ fileName: "100.xlsx", rows: file }], apply: true } });
  assert.equal(r.data.saved, true);
  const saved = (await call("GET", "/api/admin/state", { token: admin })).data.teacherLists[100];
  assert.deepEqual([saved.ams, saved.capacity, saved.updatedBy], [["9002"], undefined, "admin-file"], "no capacity stored by the import");
  // back to the teacher's choice, for the rest of the year
  await call("PUT", "/api/teacher/clubs/100", { token: teacher, body: { ams: ["9001"] } });

  // Open declarations
  await call("PUT", "/api/admin/settings", { token: admin, body: { parentPassword: "omiloi2026", deadline: "2026-10-10T21:00:00Z", contact: "2310 000000" } });
  assert.equal((await call("POST", "/api/admin/phase", { token: admin, body: { phase: "parents" } })).status, 200);
  // Teachers' lists stay open while parents declare (same deadline)
  r = await call("PUT", "/api/teacher/clubs/100", { token: teacher, body: { ams: ["9001"] } });
  assert.equal(r.status, 200, "lists still open in the parents' phase");
  assert.equal((await call("GET", "/api/teacher/me", { token: teacher })).data.canEdit, true);
  // the admin may add seats then, but not remove them (the seat check was done)
  assert.equal((await call("PUT", "/api/admin/teacher-lists/101", { token: admin, body: { capacity: 4, ams: [] } })).status, 409, "fewer seats refused");
  assert.equal((await call("PUT", "/api/admin/teacher-lists/101", { token: admin, body: { capacity: 6, ams: [] } })).status, 200, "more seats fine");
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
  await login({ am: "123", surname: "Χ" });
  assert.deepEqual((await store.readLog("loginFailures")).filter((e) => e.what === "parent_login_failed").map(({ who, reason }) => [who, reason]),
    [["parent:9002", "names"], ["parent:9002", "code"], ["parent:?", "am"]], "why, and the ΑΜ only if it is the catalog's");
  r = await login({ password: "omiloi2026 ", am: "9002", surname: "Χριστοφορίδης", name: "Άννα", father: "Ιωάννης", mother: "Ελένη" });
  assert.equal(r.status, 200, "a space pasted with the code is forgiven");

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
  // …and the teachers' lists lock with the parents' deadline
  const teacherAgain = (await call("POST", "/api/teacher/session", { body: { token: (await call("POST", "/api/admin/teacher-link", { token: admin, body: { email: "etheatr@sch.gr" } })).data.link.split("#token=")[1] } })).data.token;
  assert.equal((await call("PUT", "/api/teacher/clubs/100", { token: teacherAgain, body: { ams: [] } })).status, 409, "lists locked after the deadline");
  assert.equal((await call("GET", "/api/teacher/me", { token: teacherAgain })).data.canEdit, false);

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

  // The lottery alone: ΑΜ and number, by ΑΜ, no names
  const lot = await call("GET", "/api/admin/export/lottery.csv", { token: admin });
  assert.equal(lot.status, 200);
  const lotRows = new TextDecoder().decode(lot.data).trim().split(/\r?\n/).map((l) => l.split(","));
  assert.deepEqual(lotRows[0], ["ΑΜ", "Αριθμός κλήρωσης"]);
  const expected = drawLottery(["9001", "9002", "9003", "9004"], "Κλήρωση 2026");
  assert.deepEqual(lotRows.slice(1), ["9001", "9002", "9003", "9004"].map((am) => [am, String(expected.get(am))]), "by ΑΜ");
  assert.ok(!new TextDecoder().decode(lot.data).includes("ΠΑΠΑΔΟΠΟΥΛΟΣ"), "no names");

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
  assert.match(audit, /^Ημέρα,Γύρος,Γεγονός,ΑΜ,Επώνυμο,Όνομα,Αριθμός κλήρωσης,Κωδικός ομίλου,Όμιλος,Θέση προτίμησης,Προτεραιότητα στον όμιλο,Περιγραφή\n/);
  assert.match(audit, /Δευτέρα,1,PROPOSAL,9001,ΠΑΠΑΔΟΠΟΥΛΟΣ,ΝΙΚΟΛΑΟΣ,\d+,100,Αντιγόνη,1,[^,]*,Αίτηση στον όμιλο «Αντιγόνη» \(1η επιλογή\)\./);

  // The workbook for checking by hand: the automatic checks pass, and
  // the student's row shows the lottery number and each choice's outcome.
  const ver = (await call("GET", "/api/admin/export/verification", { token: admin })).data;
  assert.equal(ver.problems, 0);
  assert.deepEqual(ver.sheets.map((s) => s.name), ["Οδηγίες", "Βάσεις", "Μαθητές", "Γύροι", "Κλήρωση"]);
  const rows = ver.sheets[2].rows;
  const lostRow = rows.find((r) => r[0] === loser && r[5] === "Δευτέρα");
  assert.match(lostRow.join("|"), /✗ Αντιγόνη: γεμάτος \(τελευταίος που μπήκε: .*κλ\. \d+ · μαθητής: .*κλ\. \d+\)/);
  assert.match(lostRow.at(-1), /Γ1: αίτηση στον όμιλο «Αντιγόνη» \(1η επιλογή\) → δεν χώρεσε/);
  assert.equal((await call("GET", "/api/admin/export/verification")).status, 401);

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
  // Lottery preview before the allocation: needs a seed
  assert.equal((await call("GET", "/api/admin/export/lottery.csv", { token: admin })).status, 404);
  const preview = await call("GET", "/api/admin/export/lottery.csv?seed=abc", { token: admin });
  assert.equal(preview.status, 200);
  assert.equal(new TextDecoder().decode(preview.data).trim().split(/\r?\n/).length, 5, "header + 4 students");
  // R export carries the column
  r = await call("GET", "/api/admin/export/r-package.zip?seed=x", { token: admin });
  assert.equal(r.status, 200);
});

// The teachers' sheet with the optional «ΑΜ ή ΑΦΜ» column (ΑΦΜ with its leading zero lost, as Excel does)
const TEACHER_ROWS_WITH_IDS = [
  [...TEACHER_ROWS[0], "ΑΜ ή ΑΦΜ"],
  [...TEACHER_ROWS[1], "612345"],
  [...TEACHER_ROWS[2], 12345678],
  [...TEACHER_ROWS[3], ""],
  [...TEACHER_ROWS[4], "700100"],
  [...TEACHER_ROWS[5], "700200"],
];

test("teachers' common code + own ΑΜ/ΑΦΜ: only their clubs; the number is not kept; a new code ends old sessions", async () => {
  const { call, store } = setup();
  const admin = await adminLogin(call);
  await call("PUT", "/api/admin/students", { token: admin, body: { rows: STUDENT_ROWS } });
  let r = await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs: CLUB_ROWS, teachers: TEACHER_ROWS_WITH_IDS } });
  assert.equal(r.status, 200);
  assert.match(r.data.report.problems.map((p) => p.message).join(" "), /Χωρίς ΑΜ\/ΑΦΜ: ΜΗΧΑΝΙΚΟΥ ΑΝΝΑ/);
  const stored = JSON.stringify(await store.get("teachers"));
  for (const id of ["612345", "12345678", "700100"]) {
    assert.ok(!stored.includes(id), `ΑΜ/ΑΦΜ ${id} must not be stored`);
    assert.ok(!JSON.stringify(r.data).includes(id), `ΑΜ/ΑΦΜ ${id} must not be sent back`);
  }
  let state = (await call("GET", "/api/admin/state", { token: admin })).data;
  assert.deepEqual(state.teachers.map((t) => t.hasPersonalId), [true, true, false, true, true]);
  assert.ok(!JSON.stringify(state).includes("idHash"));

  assert.equal((await call("GET", "/api/public")).data.teacherCode, false);
  r = await call("POST", "/api/teacher/code-login", { body: { password: "omiloi2026", personalId: "612345" } });
  assert.equal(r.status, 409, "not enabled yet");
  assert.equal((await call("PUT", "/api/admin/settings", { token: admin, body: { teacherPassword: "abc" } })).status, 422);
  await call("PUT", "/api/admin/settings", { token: admin, body: { teacherPassword: "omiloi2026" } });
  assert.equal((await call("GET", "/api/public")).data.teacherCode, true);
  state = (await call("GET", "/api/admin/state", { token: admin })).data;
  assert.equal(state.settings.teacherPasswordSet, true);
  assert.equal(state.settings.teacherPasswordHash, undefined, "the hash is not sent");
  assert.equal(state.teacherPath, "/e-test1234/");

  // Wrong code, unknown number, or a teacher without one: the same answer
  for (const body of [{ password: "wrong-code", personalId: "612345" }, { password: "omiloi2026", personalId: "999999" }, { password: "omiloi2026", personalId: "" }]) {
    r = await call("POST", "/api/teacher/code-login", { body });
    assert.equal(r.status, 401);
    assert.match(r.data.error, /κωδικός ή ο ΑΜ\/ΑΦΜ/);
  }
  // The history says why, never what was typed
  const failed = (await store.readLog("loginFailures")).filter((e) => e.what === "teacher_login_failed");
  assert.deepEqual(failed.map(({ who, reason }) => [who, reason]), [["etheatr@sch.gr", "code"], ["teacher:?", "id"], ["teacher:?", "id"]]);
  assert.ok(!JSON.stringify(failed).includes("wrong-code") && !JSON.stringify(failed).includes("999999"));
  // A keyboard left in Greek is noted; spaces pasted around the code are forgiven
  await call("POST", "/api/teacher/code-login", { body: { password: "ομιλοι2026", personalId: "612345" } });
  assert.equal((await store.readLog("loginFailures")).at(-1).greek, true);
  const history = (await call("GET", "/api/admin/events", { token: admin })).data.events;
  assert.ok(history.some((e) => e.what === "teacher_login_failed") && history.some((e) => e.what === "clubs_uploaded"), "failures shown with the events, kept apart");
  assert.equal((await call("POST", "/api/teacher/code-login", { body: { password: " omiloi2026\n", personalId: "612345" } })).status, 200);
  // ΑΦΜ typed with its leading zero matches the number Excel kept without it
  r = await call("POST", "/api/teacher/code-login", { body: { password: "omiloi2026", personalId: " 012345678 " } });
  assert.equal(r.status, 200);
  const skin = r.data.token;
  r = await call("GET", "/api/teacher/me", { token: skin });
  assert.equal(r.data.teacher.email, "nskin@sch.gr");
  assert.deepEqual(r.data.clubs.map((c) => c.code), [100], "only their own club");

  await call("POST", "/api/admin/phase", { token: admin, body: { phase: "teachers" } });
  assert.equal((await call("PUT", "/api/teacher/clubs/102", { token: skin, body: { ams: ["9001"] } })).status, 403, "not their club");
  r = await call("PUT", "/api/teacher/clubs/100", { token: skin, body: { ams: ["9001"], capacity: 99 } });
  assert.equal(r.status, 200);
  assert.equal(r.data.list.updatedBy, "nskin@sch.gr", "the history names the teacher");
  assert.equal(r.data.list.capacity, undefined, "the capacity stays the admin's (none stored)");
  const events = (await store.readLog("events")).filter((e) => e.who === "nskin@sch.gr").map((e) => e.what);
  assert.deepEqual(events, ["teacher_login", "teacher_list"]);

  // A new code signs out whoever used the old one
  await call("PUT", "/api/admin/settings", { token: admin, body: { teacherPassword: "neos-kodikos" } });
  r = await call("GET", "/api/teacher/me", { token: skin });
  assert.equal(r.status, 401);
  assert.match(r.data.error, /κωδικός άλλαξε/);
  assert.equal((await call("POST", "/api/teacher/code-login", { body: { password: "omiloi2026", personalId: "12345678" } })).status, 401);
  assert.equal((await call("POST", "/api/teacher/code-login", { body: { password: "neos-kodikos", personalId: "12345678" } })).status, 200);
});

test("teachers' common code is rate-limited per address", async () => {
  const { call } = setup();
  const admin = await adminLogin(call);
  await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs: CLUB_ROWS, teachers: TEACHER_ROWS_WITH_IDS } });
  await call("PUT", "/api/admin/settings", { token: admin, body: { teacherPassword: "omiloi2026" } });
  for (let i = 0; i < 10; i++) assert.equal((await call("POST", "/api/teacher/code-login", { body: { password: "omiloi2026", personalId: String(100000 + i) }, ip: "9.9.9.9" })).status, 401);
  assert.equal((await call("POST", "/api/teacher/code-login", { body: { password: "omiloi2026", personalId: "612345" }, ip: "9.9.9.9" })).status, 429);
  assert.equal((await call("POST", "/api/teacher/code-login", { body: { password: "omiloi2026", personalId: "612345" }, ip: "8.8.8.8" })).status, 200);
});

test("a new clubs file after the trial import: warned at upload, explained at the allocation", async () => {
  const { call } = setup();
  const admin = await adminLogin(call);
  await call("PUT", "/api/admin/students", { token: admin, body: { rows: STUDENT_ROWS } });
  await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs: CLUB_ROWS, teachers: TEACHER_ROWS } });
  await call("POST", "/api/admin/import-legacy", { token: admin, body: { day: "thu", rows: [["RegistryNr", "Άλγεβρα"], [9001, 1]] } });
  // Now «Άλγεβρα» (102) moves from Thursday to Friday
  const moved = CLUB_ROWS.map((row) => (row[0] === 102 ? [102, "Άλγεβρα", "Παρασκευή", "", "", "Β", 5, "", ""] : row));
  let r = await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs: moved, teachers: TEACHER_ROWS } });
  assert.equal(r.status, 200);
  assert.match(r.data.report.problems.map((p) => p.message).join(" "), /1 δηλώσεις .*ΑΜ 9001: ο όμιλος 102 δεν γίνεται πια Πέμπτη/);
  await call("PUT", "/api/admin/settings", { token: admin, body: { parentPassword: "omiloi2026", deadline: "2026-10-10T21:00:00Z" } });
  for (const phase of ["teachers", "parents", "closed"]) await call("POST", "/api/admin/phase", { token: admin, body: { phase } });
  r = await call("POST", "/api/admin/allocate", { token: admin, body: { seed: "δοκιμή" } });
  assert.equal(r.status, 422);
  assert.match(r.data.error, /ο όμιλος 102 δεν γίνεται πια Πέμπτη.*ξαναεισαγάγετε/);
});

test("going back a phase: clubs stay fixed once parents declared; mandatory grades frozen; no students after the allocation", async () => {
  const { call } = setup();
  const admin = await adminLogin(call);
  await call("PUT", "/api/admin/students", { token: admin, body: { rows: STUDENT_ROWS } });
  await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs: CLUB_ROWS, teachers: TEACHER_ROWS } });
  await call("PUT", "/api/admin/settings", { token: admin, body: { parentPassword: "omiloi2026", deadline: "2026-10-10T21:00:00Z", mandatoryGrades: [] } });
  for (const phase of ["teachers", "parents"]) await call("POST", "/api/admin/phase", { token: admin, body: { phase } });
  let r = await call("PUT", "/api/admin/settings", { token: admin, body: { mandatoryGrades: ["Β"] } });
  assert.equal(r.status, 409, "mandatory grades frozen while declarations are open");
  assert.equal((await call("PUT", "/api/admin/settings", { token: admin, body: { mandatoryGrades: [] } })).status, 200, "the same value is fine");
  // A parent declares
  const parent = (await call("POST", "/api/parent/login", { body: { password: "omiloi2026", am: "9001", surname: "ΠΑΠΑΔΟΠΟΥΛΟΣ", name: "ΝΙΚΟΛΑΟΣ", father: "ΓΕΩΡΓΙΟΣ", mother: "ΜΑΡΙΑ" } })).data.token;
  r = await call("PUT", "/api/parent/submission", { token: parent, body: { parent: { name: "Γ", email: "g@example.com" }, preferences: { mon: ["100", "101"], thu: ["102"] } } });
  assert.equal(r.status, 200, JSON.stringify(r.data));
  // Back to the teachers' phase: clubs still cannot change
  await call("POST", "/api/admin/phase", { token: admin, body: { phase: "teachers" } });
  r = await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs: CLUB_ROWS, teachers: TEACHER_ROWS } });
  assert.equal(r.status, 409);
  assert.match(r.data.error, /1 δηλώσεις γονέων/);
  assert.equal((await call("PUT", "/api/admin/clubs/101/similar", { token: admin, body: { similar: "Χ" } })).status, 409);
  // Allocation done: no new students
  for (const phase of ["parents", "closed"]) await call("POST", "/api/admin/phase", { token: admin, body: { phase } });
  assert.equal((await call("POST", "/api/admin/allocate", { token: admin, body: { seed: "1" } })).status, 200);
  const more = [...STUDENT_ROWS, ["Α", 9005, "ΝΕΟΣ", "ΜΑΘΗΤΗΣ", "Π", "Μ"]];
  r = await call("PUT", "/api/admin/students", { token: admin, body: { rows: more } });
  assert.equal(r.status, 409);
  // Back to «closed»: allowed, and the old allocation is dropped
  await call("POST", "/api/admin/phase", { token: admin, body: { phase: "closed" } });
  r = await call("PUT", "/api/admin/students", { token: admin, body: { rows: more } });
  assert.equal(r.status, 200);
  assert.match(r.data.report.problems.map((p) => p.message).join(" "), /ακυρώθηκε/);
  assert.equal((await call("GET", "/api/admin/results", { token: admin })).status, 404);
});

test("a new clubs file: its capacity wins over an earlier «Διόρθωση»; lists keep only eligible students", async () => {
  const { call } = setup();
  const admin = await adminLogin(call);
  await call("PUT", "/api/admin/students", { token: admin, body: { rows: STUDENT_ROWS } });
  await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs: CLUB_ROWS, teachers: TEACHER_ROWS } });
  await call("POST", "/api/admin/phase", { token: admin, body: { phase: "teachers" } });
  // Admin corrects Ρομποτική (101, grades Α-Β) to 3 seats, with a list of an Α and a Β student
  let r = await call("PUT", "/api/admin/teacher-lists/101", { token: admin, body: { capacity: 3, ams: ["9003", "9001"] } });
  assert.equal(r.status, 200);
  let state = (await call("GET", "/api/admin/state", { token: admin })).data;
  assert.equal(state.clubs.find((c) => c.code === 101).capacity, 3);
  // A list saved later (e.g. by a teacher) keeps the correction
  await call("PUT", "/api/admin/teacher-lists/101", { token: admin, body: { ams: ["9003"] } });
  state = (await call("GET", "/api/admin/state", { token: admin })).data;
  assert.equal(state.clubs.find((c) => c.code === 101).capacity, 3, "the correction stays");
  await call("PUT", "/api/admin/teacher-lists/101", { token: admin, body: { ams: ["9003", "9001"] } });
  // ⛔ for 9002 (Β) stays; none for Α students can stay once Α leaves the club
  await call("PUT", "/api/admin/teacher-lists/102", { token: admin, body: { ams: [], blocked: ["9002"] } });
  // New file: 101 now 4 seats and grade Β only
  const changed = CLUB_ROWS.map((row) => (row[0] === 101 ? [101, "Ρομποτική", "Δευτέρα", "", "", "Β", 4, "", ""] : row));
  r = await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs: changed, teachers: TEACHER_ROWS } });
  assert.equal(r.status, 200);
  assert.match(r.data.report.problems.map((p) => p.message).join(" "), /Ρομποτική.*1 μαθητές βγήκαν/);
  state = (await call("GET", "/api/admin/state", { token: admin })).data;
  assert.equal(state.clubs.find((c) => c.code === 101).capacity, 4, "the file's capacity");
  assert.deepEqual(state.teacherLists["101"].ams, ["9001"]);
  assert.deepEqual(state.teacherLists["102"].blocked, ["9002"]);
  // Algebra (Β) to Α only: its ⛔ for a Β student is dropped, not kept hidden
  const toA = CLUB_ROWS.map((row) => (row[0] === 102 ? [102, "Άλγεβρα", "Πέμπτη", "", "", "Α", 5, "", ""] : row));
  await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs: toA, teachers: TEACHER_ROWS } });
  state = (await call("GET", "/api/admin/state", { token: admin })).data;
  assert.deepEqual(state.teacherLists["102"].blocked, []);
});

test("reset with hundreds of declarations: a few requests to Supabase, and «setup» first", async () => {
  const fake = createFakePostgrest();
  const store = createSupabaseStore({ url: "https://proj.supabase.co", key: fake.key, fetch: fake.fetch });
  const { call } = setup({ store });
  const admin = await adminLogin(call);
  await call("PUT", "/api/admin/students", { token: admin, body: { rows: STUDENT_ROWS } });
  await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs: CLUB_ROWS, teachers: TEACHER_ROWS } });
  // 400 trial declarations and 400 stories, as after last year's data
  await store.setMany(Array.from({ length: 400 }, (_, i) => [
    { key: `submission:${10000 + i}`, value: { preferences: {}, imported: true } },
    { key: `story:${10000 + i}`, value: {} },
  ]).flat());
  await store.set("settings", { phase: "published" });
  const before = fake.requests.length;
  const r = await call("POST", "/api/admin/reset", { token: admin, body: { confirm: "ΔΙΑΓΡΑΦΗ" } });
  assert.equal(r.status, 200);
  const used = fake.requests.length - before;
  assert.ok(used < 40, `reset used ${used} requests`);
  assert.equal((await store.list("submission:")).length, 0);
  assert.equal((await store.list("story:")).length, 0);
  assert.equal((await store.get("settings")).phase, "setup");
  // the reset's first write is the phase, so a reset cut short leaves «setup»
  const writes = fake.requests.slice(before).filter((q) => q.method !== "GET");
  assert.match(writes[0].search, /key=eq\.settings/);
});

test("going back a phase: no checks, except when declarations reopen", async () => {
  const { call, store } = setup();
  const admin = await adminLogin(call);
  // A platform left in «published» without data (a reset cut short) can step back…
  await store.set("settings", { phase: "published" });
  for (const phase of ["allocated", "closed"]) {
    const r = await call("POST", "/api/admin/phase", { token: admin, body: { phase } });
    assert.equal(r.status, 200, `${phase}: ${r.data.error}`);
  }
  // …but reopening declarations checks again (here: nothing to declare on)
  let r = await call("POST", "/api/admin/phase", { token: admin, body: { phase: "parents" } });
  assert.equal(r.status, 409);
  assert.match(r.data.error, /ομίλους, μαθητές, κωδικό γονέων, προθεσμία/);
  // the reset brings it back to «setup»; forward still checks
  assert.equal((await call("POST", "/api/admin/reset", { token: admin, body: { confirm: "ΔΙΑΓΡΑΦΗ" } })).status, 200);
  assert.equal((await call("POST", "/api/admin/phase", { token: admin, body: { phase: "teachers" } })).status, 409);
});

test("reopening declarations from «closed» checks the seats again", async () => {
  const { call } = setup();
  const admin = await adminLogin(call);
  await call("PUT", "/api/admin/students", { token: admin, body: { rows: STUDENT_ROWS } });
  await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs: CLUB_ROWS, teachers: TEACHER_ROWS } });
  await call("PUT", "/api/admin/settings", { token: admin, body: { parentPassword: "omiloi2026", deadline: "2026-10-10T21:00:00Z", mandatoryGrades: ["Α"] } });
  for (const phase of ["teachers", "parents", "closed"]) assert.equal((await call("POST", "/api/admin/phase", { token: admin, body: { phase } })).status, 200, phase);
  // Grade Α has 5 + 5 Monday/Thursday seats (Ρομποτική, Χορωδία); 12 more Α students no longer fit
  const more = [...STUDENT_ROWS, ...Array.from({ length: 12 }, (_, i) => ["Α", 9100 + i, `ΝΕΟΣ${i}`, "ΜΑΘΗΤΗΣ", "Π", "Μ"])];
  assert.equal((await call("PUT", "/api/admin/students", { token: admin, body: { rows: more } })).status, 200);
  const r = await call("POST", "/api/admin/phase", { token: admin, body: { phase: "parents" } });
  assert.equal(r.status, 409);
  assert.match(r.data.error, /δεν υπάρχουν αρκετές θέσεις/);
});

test("students' sections: any phase; matched by ΑΜ and grade; kept over a new student list; cleared by the reset", async () => {
  const { call } = setup();
  const admin = await adminLogin(call);
  await call("PUT", "/api/admin/students", { token: admin, body: { rows: STUDENT_ROWS } });
  const definitions = [
    ["Α/Α", "Τμήμα", "Τάξη", "Τύπος τμήματος", "Μάθημα"],
    ["1", "A1", "Α", "Γενικής Παιδείας Λυκείου", "Ιστορία"],
    ["2", "Β2", "Β", "Γενικής Παιδείας Λυκείου", "Ιστορία"],
    ["3", "Β-ΘΕΤ1", "Β", "Θετικές Σπουδές", "Φυσική (ΠΡΣ)"],
  ];
  const students = [
    ["Τάξη Εγγραφής: ", "", "", "Β"],
    ["Α/Α", "Αριθμός μητρώου", "Επώνυμο μαθητή", "Όνομα μαθητή", "Όνομα πατέρα", "Τμήματα"],
    ["1", "9001", "ΠΑΠΑΔΟΠΟΥΛΟΣ", "ΝΙΚΟΛΑΟΣ", "ΓΕΩΡΓΙΟΣ", "Β-ΘΕΤ1, Β2"],
    ["2", "9003", "ΔΗΜΟΥ", "ΣΟΦΙΑ", "ΠΕΤΡΟΣ", "Β2"], // Α in the student list
    ["3", "9999", "ΑΛΛΟΣ", "ΜΑΘΗΤΗΣ", "ΠΑΤΕΡΑΣ", "Β2"],
  ];
  let r = await call("PUT", "/api/admin/sections", { token: admin, body: { definitions: [], students } });
  assert.equal(r.status, 422);
  r = await call("PUT", "/api/admin/sections", { token: admin, body: { definitions, students, fileNames: ["a.csv", "b.csv"] } });
  assert.equal(r.status, 200);
  assert.equal(r.data.count, 3);
  assert.equal(r.data.matched, 1, "only 9001: 9003 is in another grade, 9999 is not in the catalogue");
  assert.equal(r.data.students, 4);
  assert.equal(r.data.report.sections, undefined);
  assert.deepEqual(r.data.report.problems.map((p) => p.level), ["warning", "warning", "info"]);
  assert.match(r.data.report.problems[0].message, /ΑΜ 9003/);

  const sectionOf = async () => Object.fromEntries((await call("GET", "/api/admin/state", { token: admin })).data.students.map((s) => [s.am, s.section]));
  assert.deepEqual(await sectionOf(), { 9001: "Β2", 9002: "", 9003: "", 9004: "" });
  await call("PUT", "/api/admin/students", { token: admin, body: { rows: STUDENT_ROWS } });
  assert.equal((await sectionOf())[9001], "Β2");
  assert.equal((await call("GET", "/api/admin/state", { token: admin })).data.uploads.sections.count, 3);
  await call("POST", "/api/admin/reset", { token: admin, body: { confirm: "ΔΙΑΓΡΑΦΗ" } });
  await call("PUT", "/api/admin/students", { token: admin, body: { rows: STUDENT_ROWS } });
  assert.equal((await sectionOf())[9001], "");
});

test("deleting a declaration: the parent until the deadline, the admin until the allocation", async () => {
  const { call, store, clock } = setup();
  const admin = await adminLogin(call);
  await call("PUT", "/api/admin/students", { token: admin, body: { rows: STUDENT_ROWS } });
  await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs: CLUB_ROWS, teachers: TEACHER_ROWS } });
  await call("PUT", "/api/admin/settings", { token: admin, body: { parentPassword: "omiloi2026", deadline: "2026-10-10T21:00:00Z" } });
  for (const phase of ["teachers", "parents"]) await call("POST", "/api/admin/phase", { token: admin, body: { phase } });
  const login = (am, surname, name, father, mother) => call("POST", "/api/parent/login", { body: { password: "omiloi2026", am, surname, name, father, mother } }).then((r) => r.data.token);
  const parent = await login("9001", "ΠΑΠΑΔΟΠΟΥΛΟΣ", "ΝΙΚΟΛΑΟΣ", "ΓΕΩΡΓΙΟΣ", "ΜΑΡΙΑ");
  const declare = (token) => call("PUT", "/api/parent/submission", { token, body: { parent: { name: "Γ", email: "g@example.com" }, preferences: { mon: ["100", "101"], thu: ["102"] } } });

  // The parent deletes their own declaration
  assert.equal((await declare(parent)).status, 200);
  assert.equal((await call("DELETE", "/api/parent/submission")).status, 401, "parents only");
  assert.equal((await call("DELETE", "/api/parent/submission", { token: parent })).status, 200);
  assert.equal(await store.get("submission:9001"), undefined);
  assert.equal((await call("GET", "/api/parent/me", { token: parent })).data.submission, null, "the form shows as on the first login");
  assert.ok((await store.readLog("events")).some((e) => e.what === "submission_deleted" && e.who === "parent:9001"));
  assert.deepEqual(Object.values((await store.readLog("outbox")).at(-1)).slice(0, 2), ["g@example.com", "Διαγραφή δήλωσης ομίλων"], "the parent is told");
  // Not after the deadline
  assert.equal((await declare(parent)).status, 200);
  clock.now = Date.parse("2026-10-11T09:00:00Z");
  const late = await login("9001", "ΠΑΠΑΔΟΠΟΥΛΟΣ", "ΝΙΚΟΛΑΟΣ", "ΓΕΩΡΓΙΟΣ", "ΜΑΡΙΑ");
  assert.equal((await call("DELETE", "/api/parent/submission", { token: late })).status, 409);
  assert.ok(await store.get("submission:9001"));

  // The admin deletes one, until the allocation
  clock.now = Date.parse("2026-10-01T10:00:00Z");
  assert.equal((await call("DELETE", "/api/admin/submissions/9001")).status, 401, "admin only");
  assert.equal((await call("DELETE", "/api/admin/submissions/9002", { token: admin })).status, 404, "no declaration");
  await call("POST", "/api/admin/phase", { token: admin, body: { phase: "closed" } });
  assert.equal((await call("DELETE", "/api/admin/submissions/9001", { token: admin })).status, 200);
  assert.deepEqual(Object.keys((await call("GET", "/api/admin/state", { token: admin })).data.submissions), []);
  assert.match((await store.readLog("outbox")).at(-1).text, /διαγράφηκε από τη διαχείριση/, "the parent is told");
  // Back to declarations: the parent declares again; after the allocation nothing is deleted
  await call("POST", "/api/admin/phase", { token: admin, body: { phase: "parents" } });
  assert.equal((await declare(parent)).status, 200);
  await call("POST", "/api/admin/phase", { token: admin, body: { phase: "closed" } });
  assert.equal((await call("POST", "/api/admin/allocate", { token: admin, body: { seed: "1" } })).status, 200);
  assert.equal((await call("DELETE", "/api/admin/submissions/9001", { token: admin })).status, 409);
});

test("teachers' exclusions: ⛔ and closed lists, hidden from parents, added or lifted while they declare, used by the allocation", async () => {
  const { call } = setup();
  const admin = await adminLogin(call);
  await call("PUT", "/api/admin/students", { token: admin, body: { rows: STUDENT_ROWS } });
  await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs: CLUB_ROWS, teachers: TEACHER_ROWS } });
  await call("POST", "/api/admin/phase", { token: admin, body: { phase: "teachers" } });
  const link = (await call("POST", "/api/admin/teacher-link", { token: admin, body: { email: "amix@sch.gr" } })).data.link;
  const teacher = (await call("POST", "/api/teacher/session", { body: { token: link.split("#token=")[1] } })).data.token;
  const save = (body) => call("PUT", "/api/teacher/clubs/101", { token: teacher, body });

  let r = await save({ ams: ["9001"], blocked: ["9003"] });
  assert.equal(r.status, 200);
  let mine = (await call("GET", "/api/teacher/me", { token: teacher })).data.clubs[0];
  assert.deepEqual([mine.list, mine.blocked, mine.closed], [["9001"], ["9003"], false]);
  assert.equal((await save({ ams: ["9001"], blocked: ["9001"] })).status, 422, "listed and excluded at once");
  // Closed list: only 9001 and 9002; separate ⛔ are dropped
  r = await save({ ams: ["9001", "9002"], blocked: ["9003"], closed: true });
  assert.deepEqual([r.data.list.closed, r.data.list.blocked], [true, undefined]);
  // The admin's «Διόρθωση» (capacity and ⭐ only) keeps the closed setting
  await call("PUT", "/api/admin/teacher-lists/101", { token: admin, body: { capacity: 5, ams: ["9001", "9002"] } });
  let state = (await call("GET", "/api/admin/state", { token: admin })).data;
  assert.equal(state.teacherLists[101].closed, true);
  // 9003 (Α) has only Ρομποτική on Monday: a warning, not an error
  assert.ok(state.readiness.some((p) => p.level === "warning" && /Δευτέρα: 1 μαθητής δεν έχει κανέναν όμιλο.*ΑΜ 9003/.test(p.message)));

  await call("PUT", "/api/admin/settings", { token: admin, body: { parentPassword: "omiloi2026", deadline: "2026-10-10T21:00:00Z" } });
  assert.equal((await call("POST", "/api/admin/phase", { token: admin, body: { phase: "parents" } })).status, 200, "exclusions do not block the declarations");

  const login = (fields) => call("POST", "/api/parent/login", { body: { password: "omiloi2026", ...fields } });
  const dimou = (await login({ am: "9003", surname: "ΔΗΜΟΥ", name: "ΣΟΦΙΑ", father: "ΠΕΤΡΟΣ", mother: "ΒΑΪΑ" })).data.token;
  const ntoka = (await login({ am: "9004", surname: "ΝΤΟΚΑ", name: "ΑΡΜΠΕΡΑ", father: "ΑΡΜΠΕΝ", mother: "ΛΙΝΤΙΑ" })).data.token;
  r = await call("GET", "/api/parent/me", { token: dimou });
  assert.deepEqual(r.data.days.map((d) => [d.day, d.clubs.map((c) => c.code)]), [["thu", ["103"]]], "Monday has nothing left for 9003");
  r = await call("GET", "/api/parent/me", { token: ntoka });
  assert.deepEqual(r.data.days.find((d) => d.day === "mon").clubs.map((c) => c.code), ["100"], "Ρομποτική hidden");
  const submit = (token, preferences) => call("PUT", "/api/parent/submission", { token, body: { parent: { name: "Γονέας", email: "p@example.com" }, preferences } });
  assert.equal((await submit(ntoka, { mon: ["100", "101"], thu: ["102"] })).status, 422, "an excluded club cannot be ranked");
  assert.equal((await submit(ntoka, { mon: ["100"], thu: ["102"] })).status, 200);
  assert.equal((await submit(dimou, { thu: ["103"] })).status, 200);

  // While parents declare: an exclusion can be lifted…
  r = await save({ ams: ["9001", "9002", "9004"], closed: true });
  assert.equal(r.status, 200);
  assert.equal(r.data.list.liftedAfterSubmission, 1, "9004 had already declared without Ρομποτική");
  assert.deepEqual((await call("GET", "/api/parent/me", { token: ntoka })).data.days.find((d) => d.day === "mon").clubs.map((c) => c.code), ["100", "101"]);
  assert.equal((await submit(ntoka, { mon: ["101", "100"], thu: ["102"] })).status, 200, "Ρομποτική now 1st");
  // …and added again (decided 15/10/2026): the club leaves 9004's declaration
  const file = [["Κωδικός ομίλου", 101], ["Κλειστή λίστα", "ΟΧΙ"], ["Επιλογή", "ΑΜ"], ["Χ", 9001], ["Χ", 9002], ["", 9003], ["ΟΧΙ", 9004]];
  r = await call("POST", "/api/admin/teacher-lists/import", { token: admin, body: { files: [{ fileName: "101.xlsx", rows: file }] } });
  assert.equal(r.data.ok, true);
  assert.match(r.data.report[0].warnings.join(" "), /αποκλείει 1 μαθητή που έχει ήδη δηλώσει τον όμιλο/);
  r = await save({ ams: ["9001", "9002"], closed: true });
  assert.equal(r.status, 200);
  assert.deepEqual([r.data.list.excludedAfterSubmission, r.data.list.liftedAfterSubmission], [1, 0]);
  const mon = (await call("GET", "/api/parent/me", { token: ntoka })).data.days.find((d) => d.day === "mon");
  assert.deepEqual(mon.clubs.map((c) => c.code), ["100"]);
  assert.deepEqual(mon.withdrawn, [{ code: "101", name: "Ρομποτική" }], "the parent is told");
  // Lifted again: 9004's declaration still ranks Ρομποτική, so nothing for the parent to redo
  r = await save({ ams: ["9001", "9002", "9004"], closed: true });
  assert.deepEqual([r.data.list.excludedAfterSubmission, r.data.list.liftedAfterSubmission], [0, 0]);
  assert.deepEqual((await call("GET", "/api/parent/me", { token: ntoka })).data.days.find((d) => d.day === "mon").withdrawn, []);
  await save({ ams: ["9001", "9002"], closed: true });

  await call("POST", "/api/admin/phase", { token: admin, body: { phase: "closed" } });
  r = await call("POST", "/api/admin/allocate", { token: admin, body: { seed: "Κλήρωση 2026" } });
  assert.equal(r.status, 200);
  assert.equal(r.data.results.byStudent["9003"].mon, null);
  assert.equal(r.data.results.gaps["9003"].mon, "excluded");
  assert.equal(r.data.results.gapCounts.mon.excluded, 1);
  assert.equal(r.data.results.byStudent["9004"].mon, "100", "Ρομποτική skipped, the next club of the declaration");
  const zip = new TextDecoder().decode((await call("GET", "/api/admin/export/r-package.zip", { token: admin })).data);
  assert.match(zip, /exclusions\.csv/);
  assert.match(zip, /club_code,RegistryNr\n101,9003\n101,9004\n/);
  assert.match(new TextDecoder().decode((await call("GET", "/api/admin/export/results.csv", { token: admin })).data), /9003,ΔΗΜΟΥ,ΣΟΦΙΑ,Α,— αποκλεισμός/);
});

test("removing a club after declarations opened: out of every declaration, the parent told, until the allocation", async () => {
  const { call, store } = setup();
  const admin = await adminLogin(call);
  await call("PUT", "/api/admin/students", { token: admin, body: { rows: STUDENT_ROWS } });
  await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs: CLUB_ROWS, teachers: TEACHER_ROWS } });
  await call("PUT", "/api/admin/settings", { token: admin, body: { parentPassword: "omiloi2026", deadline: "2026-10-10T21:00:00Z", mandatoryGrades: ["Β"] } });
  for (const phase of ["teachers", "parents"]) await call("POST", "/api/admin/phase", { token: admin, body: { phase } });
  await call("PUT", "/api/admin/teacher-lists/101", { token: admin, body: { ams: ["9001"] } });
  const parent = (await call("POST", "/api/parent/login", { body: { password: "omiloi2026", am: "9001", surname: "ΠΑΠΑΔΟΠΟΥΛΟΣ", name: "ΝΙΚΟΛΑΟΣ", father: "ΓΕΩΡΓΙΟΣ", mother: "ΜΑΡΙΑ" } })).data.token;
  assert.equal((await call("PUT", "/api/parent/submission", { token: parent, body: { parent: { name: "Γ", email: "g@example.com" }, preferences: { mon: ["101", "100"], thu: ["102"] } } })).status, 200);
  const receipt = (await call("GET", "/api/parent/me", { token: parent })).data.submission.receipt;

  assert.equal((await call("DELETE", "/api/admin/clubs/101")).status, 401, "admin only");
  assert.equal((await call("DELETE", "/api/admin/clubs/999", { token: admin })).status, 404);
  let r = await call("DELETE", "/api/admin/clubs/101", { token: admin });
  assert.equal(r.status, 200);
  assert.deepEqual(r.data.removed, { code: "101", name: "Ρομποτική" });
  assert.equal(r.data.declarations, 1);
  // Gone: clubs, the teacher's clubs, the teacher's list
  const state = (await call("GET", "/api/admin/state", { token: admin })).data;
  assert.deepEqual(state.clubs.map((c) => c.code), [100, 102, 103]);
  assert.deepEqual(state.teachers.find((t) => t.email === "amix@sch.gr").clubs, []);
  assert.equal(state.teacherLists["101"], undefined);
  assert.deepEqual(state.submissions["9001"].clubs, ["100", "102"]);
  // The declaration: the next club moves up, the parent sees why
  const sub = await store.get("submission:9001");
  assert.deepEqual(sub.preferences, { mon: ["100"], thu: ["102"] });
  const me = (await call("GET", "/api/parent/me", { token: parent })).data;
  assert.deepEqual(me.submission.removedClubs, [{ code: "101", name: "Ρομποτική" }]);
  assert.notEqual(me.submission.receipt, receipt, "the receipt code shows the change");
  assert.match(me.submission.history.at(-1).email, /κατάργηση ομίλου «Ρομποτική»/);
  assert.ok(!me.days.flatMap((d) => d.clubs).some((c) => c.code === "101"), "not on the form");
  assert.ok((await store.readLog("events")).some((e) => e.what === "club_removed" && e.club === 101));
  // A new save clears the note
  assert.equal((await call("PUT", "/api/parent/submission", { token: parent, body: { parent: { name: "Γ", email: "g@example.com" }, preferences: { mon: ["100"], thu: ["102"] } } })).status, 200);
  assert.deepEqual((await call("GET", "/api/parent/me", { token: parent })).data.submission.removedClubs, []);

  // Fewer seats than the mandatory grade needs: removed, with a warning
  r = await call("DELETE", "/api/admin/clubs/102", { token: admin });
  assert.equal(r.status, 200);
  assert.ok(r.data.warnings.length, "the mandatory grade Β no longer fits on Thursday");

  // After the allocation: refused
  await call("POST", "/api/admin/phase", { token: admin, body: { phase: "closed" } });
  assert.equal((await call("POST", "/api/admin/allocate", { token: admin, body: { seed: "1" } })).status, 200);
  r = await call("DELETE", "/api/admin/clubs/103", { token: admin });
  assert.equal(r.status, 409);
  assert.match(r.data.error, /Επιστροφή/);
  // A step back: removed, and the old allocation is dropped
  await call("POST", "/api/admin/phase", { token: admin, body: { phase: "closed" } });
  assert.equal((await call("DELETE", "/api/admin/clubs/103", { token: admin })).status, 200);
  assert.equal(await store.get("results"), undefined);
  assert.equal((await call("POST", "/api/admin/allocate", { token: admin, body: { seed: "1" } })).status, 200);
});

test("removing a club while a parent saves: neither change is lost", async () => {
  const store = createMemoryStore();
  const { call } = setup({ store });
  const admin = await adminLogin(call);
  await call("PUT", "/api/admin/students", { token: admin, body: { rows: STUDENT_ROWS } });
  await call("PUT", "/api/admin/clubs", { token: admin, body: { clubs: CLUB_ROWS, teachers: TEACHER_ROWS } });
  await call("PUT", "/api/admin/settings", { token: admin, body: { parentPassword: "omiloi2026", deadline: "2026-10-10T21:00:00Z" } });
  for (const phase of ["teachers", "parents"]) await call("POST", "/api/admin/phase", { token: admin, body: { phase } });
  const parent = (await call("POST", "/api/parent/login", { body: { password: "omiloi2026", am: "9001", surname: "ΠΑΠΑΔΟΠΟΥΛΟΣ", name: "ΝΙΚΟΛΑΟΣ", father: "ΓΕΩΡΓΙΟΣ", mother: "ΜΑΡΙΑ" } })).data.token;
  const declare = (mon) => call("PUT", "/api/parent/submission", { token: parent, body: { parent: { name: "Γ", email: "g@example.com" }, preferences: { mon, thu: ["102"] } } });

  // The removal lands after the save was checked against the old clubs
  const append = store.append.bind(store);
  let race = true;
  store.append = async (kind, item) => {
    if (race && item.what === "submission") {
      race = false;
      assert.equal((await call("DELETE", "/api/admin/clubs/101", { token: admin })).status, 200);
    }
    return append(kind, item);
  };
  assert.equal((await declare(["101", "100"])).status, 200);
  let sub = await store.get("submission:9001");
  assert.deepEqual(sub.preferences, { mon: ["100"], thu: ["102"] }, "the removed club leaves the late save too");
  assert.deepEqual(sub.removedClubs, [{ code: "101", name: "Ρομποτική" }]);

  // The parent's newer save is not overwritten by the removal's older read
  const update = store.update.bind(store);
  race = false;
  let once = true;
  store.update = async (key, fn) => {
    if (once && key === "submission:9001") {
      once = false;
      const r = await call("PUT", "/api/parent/submission", { token: parent, body: { parent: { name: "Γ2", email: "g2@example.com" }, preferences: { mon: ["100"] } } });
      assert.equal(r.status, 200, JSON.stringify(r.data));
    }
    return update(key, fn);
  };
  await store.set("submission:9001", { ...sub, preferences: { mon: ["100"], thu: ["102"] } });
  assert.equal((await call("DELETE", "/api/admin/clubs/102", { token: admin })).status, 200);
  sub = await store.get("submission:9001");
  assert.deepEqual(sub.preferences, { mon: ["100"] }, "the parent's save kept (no longer ranking the club)");
  assert.deepEqual(sub.removedClubs, undefined, "a new save starts without notes");
  assert.equal(sub.parent.email, "g2@example.com", "the parent's newer details kept");
});
