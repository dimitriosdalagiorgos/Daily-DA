// Reading .xls / .xlsx files with SheetJS, which the caller passes in
// (browser: the official build from cdn.sheetjs.com; Node: the official
// tarball). Everything after this step works on plain rows of cells.

import { importClubs } from "./clubs.js";
import { importStudents } from "./students.js";
import { headerKey } from "./table.js";

/**
 * @param {object} XLSX the SheetJS module
 * @param {ArrayBuffer|Uint8Array} data file contents
 * @returns {Record<string, unknown[][]>} sheet name → rows of cells
 */
export function readWorkbook(XLSX, data) {
  const wb = XLSX.read(data, { type: "array" });
  return Object.fromEntries(
    wb.SheetNames.map((name) => [name, XLSX.utils.sheet_to_json(wb.Sheets[name], { header: 1, raw: true, defval: "" })]),
  );
}

/** myschool student list: the first sheet. */
export function importStudentsFile(XLSX, data) {
  const sheets = readWorkbook(XLSX, data);
  const [first] = Object.values(sheets);
  return importStudents(first ?? []);
}

/** Our clubs template: sheets «Όμιλοι» and «Εκπαιδευτικοί». */
export function importClubsFile(XLSX, data) {
  const sheets = readWorkbook(XLSX, data);
  const find = (name) => Object.entries(sheets).find(([n]) => headerKey(n) === headerKey(name))?.[1];
  const clubs = find("Όμιλοι");
  const teachers = find("Εκπαιδευτικοί");
  if (!clubs || !teachers) {
    const missing = [!clubs && "«Όμιλοι»", !teachers && "«Εκπαιδευτικοί»"].filter(Boolean).join(" και ");
    return {
      clubs: [], teachers: [],
      problems: [{ level: "error", message: `Λείπει το φύλλο ${missing}. Χρησιμοποιήστε το πρότυπο της πλατφόρμας.` }],
      summary: { clubs: 0, multiDay: 0, teachers: 0 },
    };
  }
  return importClubs({ clubs, teachers });
}
