// School days in the order the allocation runs (Monday → Friday).
export const DAYS = ["mon", "tue", "wed", "thu", "fri"];

export const DAY_LABELS = {
  mon: "Δευτέρα",
  tue: "Τρίτη",
  wed: "Τετάρτη",
  thu: "Πέμπτη",
  fri: "Παρασκευή",
};

const LABEL_TO_DAY = Object.fromEntries(
  Object.entries(DAY_LABELS).map(([day, label]) => [label, day]),
);

/** Greek day label (as in the clubs template) → day key, or null. */
export function dayFromLabel(label) {
  return LABEL_TO_DAY[String(label ?? "").trim()] ?? null;
}

export function dayIndex(day) {
  const i = DAYS.indexOf(day);
  if (i < 0) throw new Error(`Άγνωστη ημέρα: ${day}`);
  return i;
}
