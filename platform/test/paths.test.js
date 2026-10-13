import { test } from "node:test";
import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { existsSync, readdirSync, readFileSync, rmSync } from "node:fs";
import { join } from "node:path";
import { rolePaths } from "../src/server/paths.js";

test("hidden addresses: checked, normalized, distinct", () => {
  assert.deepEqual(rolePaths({ TEACHER_PATH: "/e-7kq3m9xa/", ADMIN_PATH: "d-x82pfa_q" }), { teacher: "/e-7kq3m9xa/", admin: "/d-x82pfa_q/" });
  assert.throws(() => rolePaths({}), /TEACHER_PATH.*ADMIN_PATH/);
  assert.throws(() => rolePaths({ TEACHER_PATH: "short", ADMIN_PATH: "d-x82pfa_q" }), /TEACHER_PATH/);
  assert.throws(() => rolePaths({ TEACHER_PATH: "e-7kq/3m9xa", ADMIN_PATH: "d-x82pfa_q" }), /TEACHER_PATH/);
  assert.throws(() => rolePaths({ TEACHER_PATH: "templates", ADMIN_PATH: "d-x82pfa_q" }), /TEACHER_PATH/);
  assert.throws(() => rolePaths({ TEACHER_PATH: "Same-path1", ADMIN_PATH: "same-PATH1" }), /διαφέρουν/);
});

test("build: the role pages only at their hidden addresses; stops without them", () => {
  const root = new URL("..", import.meta.url).pathname;
  const script = join(root, "scripts", "build.mjs");
  const dist = join(root, "dist");
  const fail = spawnSync(process.execPath, [script], { env: { PATH: process.env.PATH }, encoding: "utf8" });
  assert.notEqual(fail.status, 0);
  assert.match(fail.stderr, /TEACHER_PATH/);
  const ok = spawnSync(process.execPath, [script], { env: { PATH: process.env.PATH, TEACHER_PATH: "e-build1234", ADMIN_PATH: "d-build1234" }, encoding: "utf8" });
  try {
    assert.equal(ok.status, 0, ok.stderr);
    assert.match(readFileSync(join(dist, "e-build1234", "index.html"), "utf8"), /teacher\.js/);
    assert.match(readFileSync(join(dist, "d-build1234", "index.html"), "utf8"), /admin\.js/);
    assert.match(readFileSync(join(dist, "index.html"), "utf8"), /parent\.js/);
    const top = readdirSync(dist);
    for (const f of ["teacher.html", "admin.html", "parent.html"]) assert.ok(!top.includes(f), f);
    // no page links to another role's page
    for (const f of top.filter((x) => x.endsWith(".html"))) assert.doesNotMatch(readFileSync(join(dist, f), "utf8"), /teacher|admin|build1234/, f);
  } finally {
    if (existsSync(dist)) rmSync(dist, { recursive: true, force: true });
  }
});

test("build: MOVED_TO redirects every address to the new server, and must be just an address", () => {
  const root = new URL("..", import.meta.url).pathname;
  const script = join(root, "scripts", "build.mjs");
  const dist = join(root, "dist");
  const env = { PATH: process.env.PATH, TEACHER_PATH: "e-build1234", ADMIN_PATH: "d-build1234" };
  try {
    const bad = spawnSync(process.execPath, [script], { env: { ...env, MOVED_TO: "https://omiloi.example.gr/x" }, encoding: "utf8" });
    assert.notEqual(bad.status, 0);
    assert.match(bad.stderr, /MOVED_TO/);
    const ok = spawnSync(process.execPath, [script], { env: { ...env, MOVED_TO: "https://omiloi.example.gr/" }, encoding: "utf8" });
    assert.equal(ok.status, 0, ok.stderr);
    assert.equal(readFileSync(join(dist, "_redirects"), "utf8"), "/*  https://omiloi.example.gr/:splat  301!\n");
    const plain = spawnSync(process.execPath, [script], { env, encoding: "utf8" });
    assert.equal(plain.status, 0, plain.stderr);
    assert.ok(!existsSync(join(dist, "_redirects")));
  } finally {
    if (existsSync(dist)) rmSync(dist, { recursive: true, force: true });
  }
});
