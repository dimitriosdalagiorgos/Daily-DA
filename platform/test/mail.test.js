import { test } from "node:test";
import assert from "node:assert/strict";
import { createBrevoMailer } from "../src/server/mail-brevo.js";
import { createApp } from "../src/server/app.js";
import { createMemoryStore } from "../src/server/store.js";

test("Brevo: request as documented; errors reported", async () => {
  const calls = [];
  const fetch = async (url, init) => {
    calls.push({ url, init });
    return calls.length === 1 ? new Response('{"messageId":"x"}', { status: 201 }) : new Response('{"message":"Key not found"}', { status: 401 });
  };
  const send = createBrevoMailer({ apiKey: "xkeysib-test", from: "gymnasio@sch.gr", fromName: "Γυμνάσιο", fetch });
  await send({ to: "parent@example.com", subject: "Θέμα", text: "Κείμενο" });
  assert.equal(calls[0].url, "https://api.brevo.com/v3/smtp/email");
  assert.equal(calls[0].init.headers["api-key"], "xkeysib-test");
  assert.deepEqual(JSON.parse(calls[0].init.body), {
    sender: { email: "gymnasio@sch.gr", name: "Γυμνάσιο" }, to: [{ email: "parent@example.com" }], subject: "Θέμα", textContent: "Κείμενο",
  });
  await assert.rejects(send({ to: "a@b.gr", subject: "s", text: "t" }), /Brevo 401: .*Key not found/);
  assert.throws(() => createBrevoMailer({ apiKey: "", from: "" }), /BREVO_API_KEY/);
});

function app({ sendMail, REDACT_OUTBOX } = {}) {
  const store = createMemoryStore();
  const handle = createApp({ store, env: { SESSION_SECRET: "test-secret-0123456789", ADMIN_PASSWORD: "admin-pass", BASE_URL: "https://x", SHOW_OUTBOX: true, REDACT_OUTBOX }, sendMail });
  const call = async (method, path, body, token) => {
    const res = await handle(new Request(`https://x${path}`, { method, headers: { "content-type": "application/json", ...(token ? { authorization: `Bearer ${token}` } : {}) }, body: body && JSON.stringify(body) }));
    return { status: res.status, data: await res.json() };
  };
  return { store, call };
}

test("test e-mail: sent, not configured, failed", async () => {
  const sent = [];
  let a = app({ sendMail: async (m) => { sent.push(m); } });
  let token = (await a.call("POST", "/api/admin/login", { password: "admin-pass" })).data.token;
  let r = await a.call("POST", "/api/admin/test-email", { to: "me@example.com" }, token);
  assert.deepEqual([r.status, r.data.status, sent.length], [200, "sent", 1]);
  assert.equal((await a.call("POST", "/api/admin/test-email", { to: "not-an-email" }, token)).status, 422);

  a = app();
  token = (await a.call("POST", "/api/admin/login", { password: "admin-pass" })).data.token;
  r = await a.call("POST", "/api/admin/test-email", { to: "me@example.com" }, token);
  assert.equal(r.data.status, "not_sent");
  assert.equal((await a.call("GET", "/api/admin/outbox", undefined, token)).data.mailConfigured, false);

  a = app({ sendMail: async () => { throw new Error("Brevo 401: bad key"); } });
  token = (await a.call("POST", "/api/admin/login", { password: "admin-pass" })).data.token;
  r = await a.call("POST", "/api/admin/test-email", { to: "me@example.com" }, token);
  assert.equal(r.status, 502);
  assert.match(r.data.error, /bad key/);
  const box = (await a.call("GET", "/api/admin/outbox", undefined, token)).data.outbox;
  assert.deepEqual([box[0].status, box[0].error], ["failed", "Brevo 401: bad key"]);
});

test("a failed e-mail does not fail the action; login links hidden when mail works", async () => {
  const teachers = { clubs: [["Κωδικός", "Όνομα ομίλου", "Ημέρα 1", "Ημέρα 2", "Ημέρα 3", "Τάξεις", "Χωρητικότητα", "Ώρες", "Περιγραφή"], [101, "Ρομποτική", "Δευτέρα", "", "", "Α", 5, "", ""]],
    teachers: [["Κωδικός ομίλου", "Επώνυμο", "Όνομα", "Email", "Όμιλος (έλεγχος)"], [101, "ΑΛΦΑ", "ΒΗΤΑ", "ab@sch.gr", ""]] };

  const broken = app({ sendMail: async () => { throw new Error("down"); } });
  let token = (await broken.call("POST", "/api/admin/login", { password: "admin-pass" })).data.token;
  await broken.call("PUT", "/api/admin/clubs", teachers, token);
  assert.equal((await broken.call("POST", "/api/teacher/login", { email: "ab@sch.gr" })).status, 200);
  const events = await broken.store.readLog("events");
  assert.ok(events.some((e) => e.what === "mail_failed"));

  const sent = [];
  const working = app({ sendMail: async (m) => { sent.push(m); }, REDACT_OUTBOX: true });
  token = (await working.call("POST", "/api/admin/login", { password: "admin-pass" })).data.token;
  await working.call("PUT", "/api/admin/clubs", teachers, token);
  await working.call("POST", "/api/teacher/login", { email: "ab@sch.gr" });
  assert.match(sent[0].text, /#token=[\w.-]{20,}/, "the teacher receives the real link");
  const [stored] = (await working.call("GET", "/api/admin/outbox", undefined, token)).data.outbox;
  assert.match(stored.text, /#token=\[κρυφό\]/);
  assert.doesNotMatch(stored.text, /#token=[\w.-]{20,}/, "the admin does not see it");
});
