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
  r = await call("POST", "/api/teacher/login", { body: { email: "EThEatr@sch.gr " } });
  assert.equal(r.status, 200);
  const unknown = await call("POST", "/api/teacher/login", { body: { email: "nobody@sch.gr" } });
  assert.equal(unknown.data.message, r.data.message, "same answer for unknown addresses");
  const link = (await store.readLog("outbox")).at(-1);
  assert.equal(link.to, "etheatr@sch.gr");
  const magic = link.text.match(/#token=(\S+)/)[1];
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
  assert.deepEqual(mails, ["second@example.com", "first@example.com"], "previous address told about the change");
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
