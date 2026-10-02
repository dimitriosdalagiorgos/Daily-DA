import { test } from "node:test";
import assert from "node:assert/strict";
import { givenNameMatches, normalizeName, nameParts, sameName } from "../src/import/index.js";

test("normalization: case, accents, diaeresis, final sigma, hyphens, spaces", () => {
  assert.equal(normalizeName("  Αναστάσιος  "), "ΑΝΑΣΤΑΣΙΟΣ");
  assert.equal(normalizeName("Βαΐα"), "ΒΑΙΑ");
  assert.equal(normalizeName("ΒΑΪΑ"), "ΒΑΙΑ");
  assert.equal(normalizeName("Χουϊλίδου"), "ΧΟΥΙΛΙΔΟΥ");
  assert.equal(normalizeName("Μαρία - Ελένη"), "ΜΑΡΙΑ ΕΛΕΝΗ");
  assert.equal(normalizeName("ΜΑΡΙΑ-ΕΛΕΝΗ"), "ΜΑΡΙΑ ΕΛΕΝΗ");
  assert.equal(normalizeName("μαρία   ελένη"), "ΜΑΡΙΑ ΕΛΕΝΗ");
});

test("Latin look-alike letters typed on an English keyboard count as Greek", () => {
  // "ΜΑΡΙΑ" typed as Latin M, A, P, I, A
  assert.equal(normalizeName("MAPIA"), "ΜΑΡΙΑ");
  assert.ok(sameName("KOYTΣΙΚΑΣ", "ΚΟΥΤΣΙΚΑΣ"));
  // Letters with no Greek twin stay as they are, so they never match.
  assert.ok(!sameName("GEORGIOS", "ΓΕΩΡΓΙΟΣ"));
});

test("surname: whole name only", () => {
  assert.ok(sameName("παπαδοπούλου", "ΠΑΠΑΔΟΠΟΥΛΟΥ"));
  assert.ok(!sameName("ΠΑΠΑΔΟΠΟΥΛΟ", "ΠΑΠΑΔΟΠΟΥΛΟΥ"));
  assert.ok(!sameName("", ""));
});

test("given names: whole compound name or any one part", () => {
  const stored = "ΑΝΝΑ ΠΑΝΩΡΙΑ";
  for (const input of ["Άννα Πανωρία", "Άννα-Πανωρία", "άννα", "Πανωρία"]) {
    assert.ok(givenNameMatches(input, stored), input);
  }
  for (const input of ["Πανωρία Άννα", "Αννα Μαρία", "Ανν", ""]) {
    assert.ok(!givenNameMatches(input, stored), input);
  }
  assert.ok(givenNameMatches("Μαρία", "ΜΑΡΙΑ-ΜΑΡΓΑΡΙΤΑ"));
  assert.ok(givenNameMatches("ΓΕΩΡΓΙΟΣ", "ΓΕΩΡΓΙΟΣ"));
  assert.ok(!givenNameMatches("ΓΙΩΡΓΟΣ", "ΓΕΩΡΓΙΟΣ"));
});

test("name parts", () => {
  assert.deepEqual(nameParts("ΕΡΣΗ - ΕΛΕΝΗ"), ["ΕΡΣΗ", "ΕΛΕΝΗ"]);
  assert.deepEqual(nameParts(""), []);
});
