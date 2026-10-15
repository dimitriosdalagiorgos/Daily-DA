import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { allocateWeek, drawLottery, seededRandom } from "../src/algorithm/index.js";
import { buildRPackage } from "../src/export/rPackage.js";
import { buildRounds, readRPackage, readZip } from "../src/export/rounds.js";
import { makeZip } from "../src/export/zip.js";
import { buildPage, samplePackage } from "../scripts/build-gyroi.mjs";
import { randomInstance } from "./random-instance.js";

const roundTrip = async (input) => readRPackage(await readZip(makeZip(buildRPackage({ ...input, seed: "s" }))), drawLottery);

test("gyroi page: docs/gyroi-katanomis.html is up to date", () => {
  const page = readFileSync(new URL("../../docs/gyroi-katanomis.html", import.meta.url), "utf8");
  assert.equal(page, buildPage(), "run: node scripts/build-gyroi.mjs");
});

test("gyroi page: the R package read back from the zip gives the same allocation", async () => {
  const rng = seededRandom("gyroi-zip");
  for (let i = 0; i < 10; i++) {
    const input = randomInstance(rng);
    const back = await roundTrip(input);
    assert.deepEqual(allocateWeek(back).byStudent, allocateWeek(input).byStudent);
    assert.deepEqual([...back.lottery], [...input.lottery].map(([am, n]) => [String(am), n]));
  }
});

test("gyroi page: without lottery.csv the lottery comes from the seed", () => {
  const files = samplePackage();
  const withLottery = readRPackage(files, drawLottery);
  const { "lottery.csv": _, ...rest } = files;
  assert.deepEqual([...readRPackage(rest, drawLottery).lottery], [...withLottery.lottery]);
  assert.throws(() => readRPackage({ "students.csv": "RegistryNr\n" }), /clubs\.csv/);
});

test("gyroi page: each round keeps at most the capacity, in priority order, and ends in the allocation", async () => {
  const rng = seededRandom("gyroi-rounds");
  for (let i = 0; i < 25; i++) {
    const input = await roundTrip(randomInstance(rng));
    const result = allocateWeek(input);
    const days = buildRounds(input, result);
    for (const d of days) {
      const finalSeats = new Map();
      for (const r of d.rounds) {
        for (const c of r.clubs) {
          if (!c.reconsidered) continue;
          const inside = c.rows.filter((x) => x.status === "ACCEPTED" || x.status === "RETAINED");
          assert.ok(inside.length <= c.capacity);
          // Those who stay come first; below the line, the others.
          assert.deepEqual(c.rows.slice(0, inside.length), inside);
          assert.equal(c.kept, inside.length);
          for (let k = 1; k < c.rows.length; k++) {
            const [a, b] = [c.rows[k - 1], c.rows[k]];
            if (a.starred === b.starred && a.mandatory === b.mandatory && !a.starred) assert.ok(a.lottery < b.lottery);
          }
          for (const x of c.rows) {
            assert.ok(x.note.length > 0);
            if (x.status === "REJECTED" || x.status === "DISPLACED") assert.match(x.note, /Στον γύρο \d+ κάνει αίτηση|μένει χωρίς όμιλο/);
          }
          finalSeats.set(c.code, inside.map((x) => x.am).sort());
        }
      }
      for (const c of d.clubs) {
        const expected = result.days[d.day].assignments.filter((a) => a.via === "da" && a.club === c.code).map((a) => a.am).sort();
        assert.deepEqual(finalSeats.get(c.code) ?? [], expected, `${d.day} ${c.code}`);
        assert.equal(d.final.counts[c.code], expected.length);
      }
    }
  }
});

test("gyroi page: the sample shows displaced students and clubs that leave lists", () => {
  const input = readRPackage(samplePackage(), drawLottery);
  const days = buildRounds(input, allocateWeek(input));
  const statuses = new Set(days.flatMap((d) => d.rounds.flatMap((r) => r.clubs.flatMap((c) => c.rows.map((x) => x.status)))));
  assert.deepEqual([...statuses].sort(), ["ACCEPTED", "DISPLACED", "REJECTED", "RETAINED"]);
  const reasons = new Set(days.flatMap((d) => d.dropped.map((x) => x.reason)));
  for (const r of ["excluded", "similar", "day_conflict"]) assert.ok(reasons.has(r), r);
});

test("gyroi page: a day with only carried seats is shown, and a declaration whose clubs all left the list counts as without a club", () => {
  const students = [{ am: "1", grade: "Α" }, { am: "2", grade: "Α" }];
  const clubs = [
    { code: "10", name: "Δύο ημέρες", days: ["mon", "thu"], grades: ["Α"], capacity: 5 },
    { code: "20", name: "Δευτέρα", days: ["mon"], grades: ["Α"], capacity: 5 },
  ];
  const input = {
    students, clubs, lottery: new Map([["1", 1], ["2", 2]]),
    preferences: { 1: { mon: ["10"] }, 2: { mon: ["20"] } },
    exclusions: { 20: ["2"] },
  };
  const days = buildRounds(input, allocateWeek(input));
  const thu = days.find((d) => d.day === "thu");
  assert.ok(thu, "Thursday is shown");
  assert.deepEqual(thu.rounds, []);
  assert.deepEqual(thu.carried, [{ am: "1", club: "10" }]);
  const mon = days.find((d) => d.day === "mon");
  assert.deepEqual(mon.final.unassigned.map((u) => [u.am, u.reason, u.declared]), [["2", "all_dropped", true]]);
});
