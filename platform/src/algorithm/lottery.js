// Lottery: one fixed number per student, drawn once before the allocation
// from a published seed. Anyone with the seed and the list of registry
// numbers (ΑΜ) can reproduce it.
//
// Procedure:
//   1. Sort the ΑΜ numerically (ties impossible: ΑΜ is unique).
//   2. Seed a PRNG (sfc32) from the seed string via the cyrb128 hash.
//   3. Fisher–Yates shuffle the sorted list.
//   4. The student at position i (1-based) gets lottery number i.
// A lower number wins ties.

function cyrb128(str) {
  let h1 = 1779033703, h2 = 3144134277, h3 = 1013904242, h4 = 2773480762;
  for (let i = 0; i < str.length; i++) {
    const k = str.charCodeAt(i);
    h1 = h2 ^ Math.imul(h1 ^ k, 597399067);
    h2 = h3 ^ Math.imul(h2 ^ k, 2869860233);
    h3 = h4 ^ Math.imul(h3 ^ k, 951274213);
    h4 = h1 ^ Math.imul(h4 ^ k, 2716044179);
  }
  h1 = Math.imul(h3 ^ (h1 >>> 18), 597399067);
  h2 = Math.imul(h4 ^ (h2 >>> 22), 2869860233);
  h3 = Math.imul(h1 ^ (h3 >>> 17), 951274213);
  h4 = Math.imul(h2 ^ (h4 >>> 19), 2716044179);
  h1 ^= h2 ^ h3 ^ h4;
  h2 ^= h1;
  h3 ^= h1;
  h4 ^= h1;
  return [h1 >>> 0, h2 >>> 0, h3 >>> 0, h4 >>> 0];
}

function sfc32(a, b, c, d) {
  return function next() {
    a |= 0; b |= 0; c |= 0; d |= 0;
    const t = (((a + b) | 0) + d) | 0;
    d = (d + 1) | 0;
    a = b ^ (b >>> 9);
    b = (c + (c << 3)) | 0;
    c = (c << 21) | (c >>> 11);
    c = (c + t) | 0;
    return (t >>> 0) / 4294967296;
  };
}

/** Deterministic PRNG in [0, 1) from a seed string. */
export function seededRandom(seed) {
  if (typeof seed !== "string" || seed.trim() === "") {
    throw new Error("Η κλήρωση χρειάζεται μη κενό seed.");
  }
  const rng = sfc32(...cyrb128(seed));
  for (let i = 0; i < 16; i++) rng(); // discard warm-up outputs
  return rng;
}

function compareAm(a, b) {
  const na = Number(a), nb = Number(b);
  if (Number.isFinite(na) && Number.isFinite(nb) && na !== nb) return na - nb;
  return String(a).localeCompare(String(b));
}

/**
 * Draw the lottery.
 * @param {string[]} ams registry numbers of all students
 * @param {string} seed published seed
 * @returns {Map<string, number>} ΑΜ → lottery number (1..N, lower wins)
 */
export function drawLottery(ams, seed) {
  const list = [...new Set(ams.map(String))];
  if (list.length !== ams.length) throw new Error("Διπλότυπος ΑΜ στην κλήρωση.");
  list.sort(compareAm);
  const rng = seededRandom(seed);
  for (let i = list.length - 1; i > 0; i--) {
    const j = Math.floor(rng() * (i + 1));
    [list[i], list[j]] = [list[j], list[i]];
  }
  return new Map(list.map((am, i) => [am, i + 1]));
}
