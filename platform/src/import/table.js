// Helpers for tables read from Excel: rows are arrays of cell values
// (numbers or strings), the first matching row is the header.

import { normalizeName } from "./names.js";

/** Header text compared without accents, case or extra spaces. */
export const headerKey = (value) => normalizeName(value).replace(/[.'΄’]/g, "").replace(/\s+/g, " ");

/**
 * Locate the header row and map each field to its column.
 * @param {unknown[][]} rows
 * @param {Record<string, string[]>} fields field → accepted header texts
 * @param {string[]} required fields that must be present
 * @returns {{headerRow: number, columns: Record<string, number>} | {missing: string[]}}
 */
export function findHeader(rows, fields, required) {
  const wanted = Object.fromEntries(Object.entries(fields).map(([f, names]) => [f, names.map(headerKey)]));
  let best = { missing: required };
  for (let r = 0; r < Math.min(rows.length, 15); r++) {
    const cells = (rows[r] ?? []).map(headerKey);
    const columns = {};
    for (const [field, names] of Object.entries(wanted)) {
      const c = cells.findIndex((cell) => names.includes(cell));
      if (c >= 0) columns[field] = c;
    }
    const missing = required.filter((f) => !(f in columns));
    if (missing.length === 0) return { headerRow: r, columns };
    if (missing.length < best.missing.length) best = { missing };
  }
  return best;
}

export const cellText = (value) => (value === null || value === undefined ? "" : String(value).trim());

export const isBlankRow = (row, columns) => Object.values(columns).every((c) => cellText(row?.[c]) === "");

/**
 * A whole number from a cell: 5742, 5742.0, "5742" → 5742; otherwise null.
 */
export function cellInteger(value) {
  if (typeof value === "number") return Number.isInteger(value) ? value : null;
  const text = cellText(value);
  return /^\d+(\.0+)?$/.test(text) ? Number.parseInt(text, 10) : null;
}

/** Excel row number (1-based) for a row index, as the admin sees it. */
export const excelRow = (index) => index + 1;
