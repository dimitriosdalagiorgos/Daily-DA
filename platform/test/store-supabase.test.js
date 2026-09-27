import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { createSupabaseStore } from "../src/server/store-supabase.js";
import { createMemoryStore } from "../src/server/store.js";
import { createFakePostgrest } from "./fake-postgrest.js";

const make = (keyValue = "sb_secret_test") => {
  const fake = createFakePostgrest({ key: keyValue });
  return { fake, store: createSupabaseStore({ url: "https://proj.supabase.co/", key: keyValue, fetch: fake.fetch }) };
};

// The same behaviour is required from both stores.
for (const [name, factory] of [["memory", () => ({ store: createMemoryStore() })], ["supabase", () => make()]]) {
  test(`${name} store: get / set / update / list / delete / log`, async () => {
    const { store } = factory();
    assert.equal(await store.get("settings"), undefined);
    await store.set("settings", { phase: "setup" });
    assert.deepEqual(await store.get("settings"), { phase: "setup" });
    assert.deepEqual(await store.update("settings", (s) => ({ ...s, phase: "teachers" })), { phase: "teachers" });
    await store.set("submission:9001", { a: 1 });
    await store.set("submission:9002", { a: 2 });
    await store.set("submissionX", { a: 3 });
    await store.set("teacherList:101", { ams: [] });
    assert.deepEqual((await store.list("submission:")).map((r) => r.key).sort(), ["submission:9001", "submission:9002"]);
    await store.delete("submission:9001");
    assert.deepEqual((await store.list("submission:")).map((r) => r.value), [{ a: 2 }]);
    for (let i = 1; i <= 5; i++) await store.append("events", { i });
    assert.deepEqual(await store.readLog("events", 3), [{ i: 3 }, { i: 4 }, { i: 5 }]);
    assert.deepEqual(await store.readLog("outbox"), []);
    // Values come back as copies
    const s = await store.get("settings");
    s.phase = "changed";
    assert.equal((await store.get("settings")).phase, "teachers");
  });

  test(`${name} store: concurrent updates of one key lose nothing`, async () => {
    const { store } = factory();
    await Promise.all(Array.from({ length: 25 }, () => store.update("counter", (n = 0) => n + 1)));
    assert.equal(await store.get("counter"), 25);
    await Promise.all(Array.from({ length: 10 }, (_, i) => store.update("fresh", (list = []) => [...list, i])));
    assert.equal((await store.get("fresh")).length, 10, "concurrent first inserts");
    await Promise.all(Array.from({ length: 30 }, (_, i) => store.append("events", { i })));
    assert.equal((await store.readLog("events", 100)).length, 30);
  });
}

test("supabase store: key header by key type; SQL wildcards escaped in list", async () => {
  const { fake, store } = make();
  await store.get("x");
  assert.equal(fake.requests[0].headers.apikey, "sb_secret_test");
  assert.equal(fake.requests[0].headers.authorization, undefined, "new secret keys are not JWTs");
  const legacy = make("eyJhbGciOi.legacy.jwt");
  await legacy.store.get("x");
  assert.equal(legacy.fake.requests[0].headers.authorization, "Bearer eyJhbGciOi.legacy.jwt");

  await store.set("a_b:1", 1);
  await store.set("axb:1", 2);
  assert.deepEqual((await store.list("a_b:")).map((r) => r.key), ["a_b:1"]);
});

test("supabase store: errors are reported, not swallowed", async () => {
  const fake = createFakePostgrest({ key: "right" });
  const store = createSupabaseStore({ url: "https://proj.supabase.co", key: "wrong", fetch: fake.fetch });
  await assert.rejects(store.get("x"), /401/);
  assert.throws(() => createSupabaseStore({ url: "", key: "" }), /SUPABASE_URL/);
});

test("schema.sql creates the tables the store uses, with row-level security", () => {
  const sql = readFileSync(new URL("../supabase/schema.sql", import.meta.url), "utf8");
  for (const s of ["create table if not exists public.kv", "create table if not exists public.log", "version    integer",
    "alter table public.kv  enable row level security", "alter table public.log enable row level security"]) {
    assert.ok(sql.includes(s), s);
  }
});
