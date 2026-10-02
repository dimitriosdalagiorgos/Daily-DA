import { test } from "node:test";
import assert from "node:assert/strict";
import { spawn, spawnSync } from "node:child_process";
import { mkdtempSync, rmSync, existsSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

const script = new URL("../server/start.mjs", import.meta.url).pathname;

function start(env) {
  const child = spawn(process.execPath, [script], { env: { ...process.env, ...env }, stdio: ["ignore", "pipe", "pipe"] });
  return new Promise((resolve, reject) => {
    child.stdout.on("data", (d) => { if (String(d).includes("Πλατφόρμα ομίλων")) resolve(child); });
    child.on("exit", (code) => reject(new Error(`exited ${code}`)));
  });
}
const stop = (child) => new Promise((r) => { child.removeAllListeners("exit"); child.on("exit", r); child.kill(); });

test("production server: refuses to start without its settings", () => {
  const r = spawnSync(process.execPath, [script], { env: { PATH: process.env.PATH }, encoding: "utf8" });
  assert.notEqual(r.status, 0);
  assert.match(r.stderr, /ADMIN_PASSWORD/);
  const r2 = spawnSync(process.execPath, [script], { env: { PATH: process.env.PATH, ADMIN_PASSWORD: "0123456789" }, encoding: "utf8" });
  assert.match(r2.stderr, /BASE_URL/);
  const r3 = spawnSync(process.execPath, [script], { env: { PATH: process.env.PATH, ADMIN_PASSWORD: "0123456789", BASE_URL: "https://x.gr" }, encoding: "utf8" });
  assert.match(r3.stderr, /TEACHER_PATH.*ADMIN_PATH/);
});

test("production server: pages, API, headers, and data kept across restarts", async () => {
  const dir = mkdtempSync(join(tmpdir(), "omiloi-prod-"));
  const port = String(18000 + Math.floor(Math.random() * 1000));
  const env = { ADMIN_PASSWORD: "δοκιμή-κωδικός-123", BASE_URL: "https://omiloi.example.gr/", DATA_DIR: dir, PORT: port, TEACHER_PATH: "e-prod1234", ADMIN_PATH: "d-prod1234" };
  const base = `http://127.0.0.1:${port}`;
  let child = await start(env);
  try {
    const page = await fetch(`${base}/`);
    assert.equal(page.status, 200);
    assert.equal(page.headers.get("x-frame-options"), "DENY");
    assert.match(await page.text(), /Δήλωση/);
    assert.equal((await fetch(`${base}/lib/server/app.js`)).status, 404); // server code is not served
    // Each role at its own address: the parents' page at the root, the
    // others only at their hidden paths, with no links between them
    const home = await (await fetch(`${base}/`)).text();
    assert.match(home, /parent\.js/);
    assert.doesNotMatch(home, /teacher|admin|e-prod1234|d-prod1234/);
    for (const old of ["/teacher.html", "/admin.html", "/parent.html"]) assert.equal((await fetch(`${base}${old}`)).status, 404, old);
    assert.match(await (await fetch(`${base}/e-prod1234/`)).text(), /teacher\.js/);
    assert.match(await (await fetch(`${base}/d-prod1234/`)).text(), /admin\.js/);
    const bare = await fetch(`${base}/d-prod1234`, { redirect: "manual" });
    assert.equal(bare.status, 301);
    assert.equal(bare.headers.get("location"), "/d-prod1234/");
    assert.doesNotMatch(await (await fetch(`${base}/help.html`)).text(), /teacher|admin/);
    assert.equal((await fetch(`${base}/api/public`)).status, 200);

    const { token } = await (await fetch(`${base}/api/admin/login`, { method: "POST", headers: { "content-type": "application/json" }, body: JSON.stringify({ password: env.ADMIN_PASSWORD }) })).json();
    assert.ok(token);
    const put = await fetch(`${base}/api/admin/settings`, { method: "PUT", headers: { "content-type": "application/json", authorization: `Bearer ${token}` }, body: JSON.stringify({ schoolName: "Σχολείο Δοκιμής" }) });
    assert.equal(put.status, 200);
    assert.ok(existsSync(join(dir, "store.json")));

    await stop(child);
    child = await start(env);
    assert.equal((await (await fetch(`${base}/api/public`)).json()).schoolName, "Σχολείο Δοκιμής");
    // the session secret was kept, so the old token still works
    assert.equal((await fetch(`${base}/api/admin/state`, { headers: { authorization: `Bearer ${token}` } })).status, 200);
  } finally {
    await stop(child);
    rmSync(dir, { recursive: true, force: true });
  }
});
