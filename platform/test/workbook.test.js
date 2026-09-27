// Real Excel files through SheetJS. SheetJS is not a dependency of the
// platform (the browser loads the official build from cdn.sheetjs.com);
// CI installs the official tarball and sets REQUIRE_SHEETJS=1 so these
// tests must run there.

import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { importClubsFile, importStudentsFile } from "../src/import/index.js";

const XLSX = await import("xlsx").catch(() => null);
if (!XLSX && process.env.REQUIRE_SHEETJS) throw new Error("SheetJS (xlsx) is required but not installed.");
const skip = XLSX ? false : "SheetJS not installed (runs in CI)";

const fixture = (name) => new Uint8Array(readFileSync(join(dirname(fileURLToPath(import.meta.url)), "fixtures", name)));

test("myschool .xls student list", { skip }, () => {
  const r = importStudentsFile(XLSX, fixture("myschool_katalogos_fictional.xls"));
  assert.deepEqual(r.summary, { count: 4, byGrade: { Α: 1, Β: 2, Γ: 1 } });
  assert.deepEqual(r.students[1], { am: "9002", grade: "Β", surname: "ΧΡΙΣΤΟΦΟΡΙΔΗΣ", name: "ΑΝΝΑ ΠΑΝΩΡΙΑ", father: "ΙΩΑΝΝΗΣ", mother: "ΕΛΕΝΗ-ΜΑΡΙΑ" });
  assert.deepEqual(r.problems.map((p) => [p.level, p.row, p.field]), [["warning", 5, "father"]]);
});

test("filled clubs template .xlsx (formula columns ignored)", { skip }, () => {
  const r = importClubsFile(XLSX, fixture("omiloi_filled_fictional.xlsx"));
  assert.deepEqual(r.problems, []);
  assert.deepEqual(r.summary, { clubs: 3, multiDay: 2, teachers: 3 });
  assert.deepEqual(r.clubs.map((c) => [c.code, c.days, c.grades, c.capacity]), [
    [101, ["mon"], ["Α"], 15],
    [102, ["mon", "thu"], ["Β", "Γ"], 20],
    [103, ["tue", "wed", "fri"], ["Α", "Β", "Γ"], 30],
  ]);
});

test("the empty template is recognised (no clubs yet)", { skip }, () => {
  const data = new Uint8Array(readFileSync(join(dirname(fileURLToPath(import.meta.url)), "..", "templates", "omiloi_protypo.xlsx")));
  const r = importClubsFile(XLSX, data);
  assert.deepEqual(r.problems.map((p) => p.message), ["Δεν υπάρχει κανένας όμιλος."]);
});

test("a student list uploaded as the clubs file is refused clearly", { skip }, () => {
  const r = importClubsFile(XLSX, fixture("myschool_katalogos_fictional.xls"));
  assert.match(r.problems[0].message, /Λείπει το φύλλο «Όμιλοι» και «Εκπαιδευτικοί»/);
});
