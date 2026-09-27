// A student's allocation report, day by day (admin and parent pages).
import { el } from "./ui.js";

export const DAY_LABELS = { mon: "Δευτέρα", tue: "Τρίτη", wed: "Τετάρτη", thu: "Πέμπτη", fri: "Παρασκευή" };

export function reportView(report, { title = "Πώς προέκυψε η κατανομή" } = {}) {
  const days = Object.entries(report.days).filter(([, d]) => d.club || d.gap !== "not_offered");
  return el("div.report", {},
    title ? el("h3", {}, title) : null,
    el("p.small", {}, `Αριθμός κλήρωσης: ${report.lottery ?? "—"} από ${report.lotteryOf} (seed «${report.seed}»). Μικρότερος αριθμός προηγείται στις ισοβαθμίες.`),
    days.map(([day, d]) => el("div.summary-day", {},
      el("strong", {}, `${DAY_LABELS[day]}: `),
      d.club ? el("span", {}, d.club.name) : el("span", { style: "color:var(--warn)" }, `χωρίς όμιλο (${d.gapText})`),
      d.steps.length ? el("ol.small", {}, d.steps.map((s) => el("li", {}, s))) : null)));
}
