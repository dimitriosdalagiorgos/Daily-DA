import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtempSync, writeFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { buildRPackage } from "../src/export/rPackage.js";
import { propose, readPackage, withCapacities } from "../scripts/propose.mjs";

// Shared club 1 (Α+Β, 1 seat), club 2 only for Α (1 seat): the Α student
// takes the shared seat and the Β student is left out (as in capacity.test.js).
const sinkPackage = (mandatoryGrades = ["Α", "Β"]) => buildRPackage({
  students: [{ am: "1", grade: "Α", surname: "ΑΛΦΑ", name: "Α" }, { am: "2", grade: "Β", surname: "ΒΗΤΑ", name: "Β" }],
  clubs: [
    { code: "1", name: "Κοινός", days: ["mon"], grades: ["Α", "Β"], capacity: 1 },
    { code: "2", name: "Μόνο Α", days: ["mon"], grades: ["Α"], capacity: 1 },
  ],
  preferences: { 1: { mon: ["1", "2"] }, 2: { mon: ["1"] } },
  lottery: new Map([["1", 0.1], ["2", 0.9]]),
  seed: "test",
  mandatoryGrades,
});

test("propose script: suggests the shared seat, and writes it into clubs.csv", () => {
  const files = sinkPackage();
  const { suggestion, text } = propose(files);
  assert.deepEqual(suggestion.increases, [{ code: "1", from: 1, to: 2, ceiling: 22 }]);
  assert.match(text, /1 Κοινός \(Δευτέρα\): 1 → 2 θέσεις/);
  assert.match(text, /ΑΜ 2 ΒΗΤΑ Β \(Β\)/);
  assert.match(text, /Μένουν χωρίς όμιλο και μετά: 0/);

  const changed = withCapacities(files, suggestion.increases);
  assert.match(changed["clubs.csv"], /^1,Κοινός,mon,Α;Β,2,$/m);
  assert.match(changed["clubs.csv"], /^2,Μόνο Α,mon,Α,1,$/m);
  assert.equal(changed["preferences.csv"], files["preferences.csv"]);
  // The changed package needs nothing more
  assert.deepEqual(propose(changed).suggestion.before, []);
});

test("propose script: reads an unzipped package folder", async () => {
  const dir = mkdtempSync(join(tmpdir(), "propose-"));
  try {
    for (const [name, content] of Object.entries(sinkPackage())) writeFileSync(join(dir, name), content);
    const { suggestion } = propose(await readPackage(dir));
    assert.equal(suggestion.increases.length, 1);
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test("propose script: says so when no grade has mandatory placement", () => {
  const { suggestion, text } = propose(sinkPackage([]));
  assert.deepEqual(suggestion.increases, []);
  assert.match(text, /Καμία τάξη δεν έχει υποχρεωτική ένταξη/);
});
