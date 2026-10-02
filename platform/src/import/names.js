// Name normalization and matching (see SPEC, «Γονέας»).
//
// normalizeName: uppercase, no accents or diaeresis, Latin look-alikes
// turned into Greek (typing with an English keyboard), hyphens and runs of
// spaces turned into one space. "Μαρία - Ελένη" → "ΜΑΡΙΑ ΕΛΕΝΗ".

// Latin capitals that look identical to Greek capitals.
const LATIN_TO_GREEK = {
  A: "Α", B: "Β", E: "Ε", Z: "Ζ", H: "Η", I: "Ι", K: "Κ", M: "Μ",
  N: "Ν", O: "Ο", P: "Ρ", T: "Τ", Y: "Υ", X: "Χ",
};

export function normalizeName(value) {
  return String(value ?? "")
    .normalize("NFD")
    .replace(/\p{M}/gu, "") // accents, diaeresis
    .toUpperCase()
    .replace(/[A-Z]/g, (ch) => LATIN_TO_GREEK[ch] ?? ch)
    .replace(/[‐-―−-]/g, " ") // hyphen, dashes, minus
    .replace(/\s+/g, " ")
    .trim();
}

/** Parts of a compound name: "ΑΝΝΑ-ΠΑΝΩΡΙΑ" → ["ΑΝΝΑ", "ΠΑΝΩΡΙΑ"]. */
export function nameParts(value) {
  const n = normalizeName(value);
  return n ? n.split(" ") : [];
}

/**
 * Strict match of a whole name (surname).
 */
export function sameName(input, stored) {
  const a = normalizeName(input);
  return a !== "" && a === normalizeName(stored);
}

/**
 * First name, father's or mother's name: the whole name, or any one of its
 * parts ("ΑΝΝΑ ΠΑΝΩΡΙΑ" accepts "Άννα Πανωρία", "Άννα", "Πανωρία").
 */
export function givenNameMatches(input, stored) {
  const a = normalizeName(input);
  if (a === "") return false;
  return a === normalizeName(stored) || nameParts(stored).includes(a);
}
