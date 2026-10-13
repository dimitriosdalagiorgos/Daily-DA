import { test } from "node:test";
import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { createServer } from "node:http";
import { existsSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { createSupabaseStore } from "../src/server/store-supabase.js";
import { createFileStore, createMemoryStore } from "../src/server/store.js";
import { personalIdHash } from "../src/server/auth.js";
import { createFakePostgrest } from "./fake-postgrest.js";

const script = new URL("../server/import-supabase.mjs", import.meta.url).pathname;

async function filledSupabase({ secret = true } = {}) {
  const fake = createFakePostgrest();
  const store = createSupabaseStore({ url: "https://proj.supabase.co", key: fake.key, fetch: fake.fetch });
  await store.set("settings", { phase: "parents", deadline: "2026-10-20T21:00" });
  if (secret) await store.set("sessionSecret", "s".repeat(43));
  await store.set("students", [{ am: "9001" }, { am: "9002" }]);
  await store.set("teachers", [{ email: "t@example.org", clubs: ["101"], idHash: personalIdHash("s".repeat(43), "123456") }]);
  await store.set("teacherList:101", { ams: ["9001"] });
  for (let i = 0; i < 7; i++) await store.set(`submission:${9001 + i}`, { preferences: { mon: ["101"] }, i });
  for (let i = 0; i < 5; i++) await store.append("events", { i });
  await store.append("outbox", { to: "p@example.org" });
  return { fake, store };
}

test("supabase exportAll: every key and log, in the file store's layout, across pages", async () => {
  const { store } = await filledSupabase();
  const data = await store.exportAll({ pageSize: 2 });
  assert.equal(Object.keys(data).filter((k) => k.startsWith("submission:")).length, 7);
  assert.deepEqual(data["teacherList:101"], { ams: ["9001"] });
  assert.deepEqual(data["log:events"], [0, 1, 2, 3, 4].map((i) => ({ i })));
  // The same answers from a store built on the export
  const mem = createMemoryStore(data);
  for (const k of ["settings", "students", "teachers", "sessionSecret"]) assert.deepEqual(await mem.get(k), await store.get(k), k);
  assert.deepEqual(await mem.list("submission:"), await store.list("submission:"));
  assert.deepEqual(await mem.readLog("events"), await store.readLog("events"));
  assert.deepEqual(await mem.readLog("outbox"), await store.readLog("outbox"));
});

// Supabase's REST API, served over HTTP from the in-memory fake
function serve(fake) {
  const server = createServer(async (req, res) => {
    let body = "";
    for await (const chunk of req) body += chunk;
    const r = await fake.fetch(`https://proj.supabase.co${req.url}`, { method: req.method, headers: req.headers, body: body || undefined });
    res.writeHead(r.status, { "content-type": "application/json" });
    res.end(await r.text());
  });
  return new Promise((resolve) => server.listen(0, "127.0.0.1", () => resolve(server)));
}

function run(args, env) {
  const child = spawn(process.execPath, [script, ...args], { env: { PATH: process.env.PATH, ...env }, stdio: ["ignore", "pipe", "pipe"] });
  let out = "";
  child.stdout.on("data", (d) => { out += d; });
  child.stderr.on("data", (d) => { out += d; });
  return new Promise((resolve) => child.on("exit", (status) => resolve({ status, out })));
}

test("import-supabase: dry run, the session secret checks, and the real import", async () => {
  const dir = mkdtempSync(join(tmpdir(), "omiloi-import-"));
  const { fake, store } = await filledSupabase({ secret: false });
  const server = await serve(fake);
  try {
    const dataDir = join(dir, "data");
    const target = join(dataDir, "store.json");
    const envFile = join(dir, "omiloi.env");
    const env = {
      OMILOI_ENV: envFile, OMILOI_BACKUP_DIR: join(dir, "backups"),
      SUPABASE_URL: `http://127.0.0.1:${server.address().port}`, SUPABASE_SECRET_KEY: fake.key,
    };

    // Secret neither in Supabase nor in the settings file: it is in Netlify
    writeFileSync(envFile, `ADMIN_PASSWORD=x\nDATA_DIR=${dataDir}\n`);
    let r = await run(["--dry-run"], env);
    assert.equal(r.status, 1, r.out);
    assert.match(r.out, /Δηλώσεις γονέων: 7/);
    assert.match(r.out, /SESSION_SECRET/);

    // In Supabase, but a different one in the settings file would win
    await store.set("sessionSecret", "s".repeat(43));
    writeFileSync(envFile, `ADMIN_PASSWORD=x\nDATA_DIR=${dataDir}\nSESSION_SECRET=${"o".repeat(43)}\n`);
    r = await run(["--dry-run"], env);
    assert.equal(r.status, 1, r.out);
    assert.match(r.out, /Σβήστε τη γραμμή SESSION_SECRET/);

    // Dry run writes nothing
    writeFileSync(envFile, `ADMIN_PASSWORD=x\nDATA_DIR=${dataDir}\n`);
    r = await run(["--dry-run"], env);
    assert.equal(r.status, 0, r.out);
    assert.match(r.out, /Δοκιμή/);
    assert.ok(!existsSync(target));

    // Wrong key: a clear message, nothing written
    r = await run(["--yes"], { ...env, SUPABASE_SECRET_KEY: "sb_secret_wrong" });
    assert.equal(r.status, 1, r.out);
    assert.match(r.out, /Supabase απέτυχε/);
    assert.ok(!existsSync(target));

    // The real import, over an existing (empty) installation
    const before = createFileStore(target);
    await before.set("sessionSecret", "v".repeat(43));
    r = await run(["--yes"], env);
    assert.equal(r.status, 0, r.out);
    assert.equal(JSON.parse(readFileSync(join(dir, "backups", "omiloi-before-import.json"), "utf8")).sessionSecret, "v".repeat(43));

    // Same secret as on Netlify, so teachers' ΑΜ/ΑΦΜ still match
    const moved = createFileStore(target);
    assert.equal((await moved.list("submission:")).length, 7);
    assert.equal((await moved.readLog("events")).length, 5);
    assert.equal((await moved.get("teachers"))[0].idHash, personalIdHash(await moved.get("sessionSecret"), "123456"));
  } finally {
    server.close();
    rmSync(dir, { recursive: true, force: true });
  }
});
