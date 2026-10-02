import { test } from "node:test";
import assert from "node:assert/strict";
import { drawLottery } from "../src/algorithm/index.js";

const ams = ["5742", "5414", "5743", "5597", "5598", "5731", "5416"];

test("same seed gives the same lottery", () => {
  assert.deepEqual([...drawLottery(ams, "2026-10-01")], [...drawLottery(ams, "2026-10-01")]);
});

test("lottery does not depend on the order the students are listed", () => {
  const a = drawLottery(ams, "seed");
  const b = drawLottery([...ams].reverse(), "seed");
  assert.deepEqual(Object.fromEntries(a), Object.fromEntries(b));
});

test("every student gets a distinct number 1..N", () => {
  const lottery = drawLottery(ams, "seed");
  assert.deepEqual([...lottery.values()].sort((x, y) => x - y), [1, 2, 3, 4, 5, 6, 7]);
  assert.deepEqual([...lottery.keys()].sort(), [...ams].sort());
});

test("different seeds give different lotteries", () => {
  const many = Array.from({ length: 100 }, (_, i) => String(5000 + i));
  assert.notDeepEqual([...drawLottery(many, "a")], [...drawLottery(many, "b")]);
});

test("known output stays fixed across versions", () => {
  // Guards the published procedure: changing hash, PRNG or shuffle breaks this.
  const lottery = drawLottery(["1", "2", "3", "4", "5"], "Daily-DA");
  assert.deepEqual(Object.fromEntries(lottery), { 1: 2, 2: 3, 3: 4, 4: 1, 5: 5 });
});

test("rejects an empty seed and duplicate ΑΜ", () => {
  assert.throws(() => drawLottery(ams, ""));
  assert.throws(() => drawLottery(["1", "1"], "seed"));
});
