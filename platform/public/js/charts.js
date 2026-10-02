// Small HTML charts for the admin's statistics: 100% stacked bars, a column
// histogram, and bars with a target tick. Plain elements (no SVG), so text
// stays readable at phone width. Every mark has a hover/focus tooltip, every
// chart a legend (two or more series) and a table view underneath.
//
// Colours are CSS tokens (style.css, --viz-*): an ordinal blue ramp for
// "which choice" (validated light and dark) and a neutral gray for "no club".

import { el } from "./ui.js";

const pct = (x) => `${Math.round(x * 100)}%`;
const fmt = (n) => n.toLocaleString("el-GR");

// One tooltip for the page, following the pointer or the focused mark
let tip = null;
function tooltip() {
  if (!tip) {
    tip = el("div.viz-tip", { role: "tooltip", hidden: true });
    document.body.append(tip);
  }
  return tip;
}
function withTip(node, text) {
  node.dataset.tip = text;
  node.setAttribute("aria-label", text);
  node.tabIndex = 0;
  const showAt = (x, y) => {
    const t = tooltip();
    t.textContent = text;
    t.hidden = false;
    const w = t.offsetWidth;
    t.style.left = `${Math.max(8, Math.min(x + 12, window.innerWidth - w - 8))}px`;
    t.style.top = `${y + 14 + window.scrollY}px`;
  };
  node.addEventListener("pointermove", (e) => showAt(e.clientX, e.clientY));
  node.addEventListener("focus", () => { const r = node.getBoundingClientRect(); showAt(r.left, r.bottom - 10); });
  for (const ev of ["pointerleave", "blur"]) node.addEventListener(ev, () => { tooltip().hidden = true; });
  return node;
}

const legend = (series) => el("div.viz-legend", {}, series.map((s) => el("span", {}, el("i", { style: `background:var(${s.color})` }), s.label)));

/** The numbers behind a chart, collapsed under it. */
function tableView(head, rows) {
  return el("details.viz-table", {}, el("summary", {}, "Πίνακας"),
    el("div.table-wrap", {}, el("table", {},
      el("thead", {}, el("tr", {}, head.map((h, i) => el(i ? "th.num" : "th", {}, h)))),
      el("tbody", {}, rows.map((r) => el("tr", {}, r.map((v, i) => el(i ? "td.num" : "td", {}, v))))))));
}

/**
 * One 100% bar per row, split into series.
 * @param {{label: string, values: Record<string, number>}[]} rows
 * @param {{key: string, label: string, color: string}[]} series
 */
export function stackedBars(rows, series, { unit = "μαθητές" } = {}) {
  const lines = rows.map((r) => {
    const total = series.reduce((a, s) => a + (r.values[s.key] ?? 0), 0);
    const bar = el("div.viz-stack");
    for (const s of series) {
      const v = r.values[s.key] ?? 0;
      if (!v) continue;
      // Label ink chosen per fill (white on dark steps, ink on light ones)
      const seg = el("span.viz-seg", { style: `flex-grow:${v};background:var(${s.color});color:var(${s.color.replace("--viz-", "--viz-ink-")})` });
      // Label inside only when the segment is wide enough
      if (v / total >= 0.12) seg.append(el("b", {}, pct(v / total)));
      bar.append(withTip(seg, `${r.label} · ${s.label}: ${fmt(v)} ${unit} (${pct(v / total)})`));
    }
    return el("div.viz-row", {}, el("span.viz-label", {}, r.label), total ? bar : el("span.muted.small", {}, "—"), el("span.viz-total.small.muted", {}, total ? fmt(total) : ""));
  });
  return el("div.viz", {}, legend(series), el("div.viz-rows", {}, lines),
    tableView(["", ...series.map((s) => s.label), "Σύνολο"], rows.map((r) => {
      const total = series.reduce((a, s) => a + (r.values[s.key] ?? 0), 0);
      return [r.label, ...series.map((s) => (total ? `${fmt(r.values[s.key] ?? 0)} (${pct((r.values[s.key] ?? 0) / total)})` : "0")), fmt(total)];
    })));
}

/**
 * Columns: a histogram of counts.
 * @param {{label: string, value: number}[]} data
 */
export function columns(data, { unit = "", color = "--viz-1", caption = "" } = {}) {
  const max = Math.max(1, ...data.map((d) => d.value));
  const total = data.reduce((a, d) => a + d.value, 0);
  const cols = data.map((d) => el("div.viz-col", {},
    el("span.viz-cap.small", {}, d.value ? fmt(d.value) : ""),
    withTip(el("span.viz-colbar", { style: `height:${(d.value / max) * 100}%;background:var(${color})` }),
      `${d.label}: ${fmt(d.value)} ${unit}${total ? ` (${pct(d.value / total)})` : ""}`),
    el("span.viz-collabel.small", {}, d.label)));
  return el("div.viz", {}, caption ? el("p.small.muted", { style: "margin:0 0 6px" }, caption) : null,
    el("div.viz-cols", {}, cols),
    tableView(["", unit || "Πλήθος", "%"], data.map((d) => [d.label, fmt(d.value), total ? pct(d.value / total) : ""])));
}

/**
 * Horizontal bars (one series) on one shared scale, with a tick for a
 * target in the same unit (e.g. 1st-choice demand vs seats).
 * @param {{label: string, value: number, target: number, note?: string}[]} data
 */
export function barsWithTarget(data, { valueLabel, targetLabel, unit = "μαθητές" }) {
  const max = Math.max(1, ...data.flatMap((d) => [d.value, d.target]));
  const rows = data.map((d) => el("div.viz-row", {},
    el("span.viz-label", {}, d.label),
    el("div.viz-track", {},
      withTip(el("span.viz-hbar", { style: `width:${(d.value / max) * 100}%;background:var(--viz-1)` }),
        `${d.label}: ${valueLabel} ${fmt(d.value)} · ${targetLabel} ${fmt(d.target)}${d.note ? ` · ${d.note}` : ""}`),
      el("span.viz-target", { style: `left:${(d.target / max) * 100}%`, "aria-hidden": "true" })),
    el("span.viz-total.small.muted", {}, `${fmt(d.value)} / ${fmt(d.target)}`)));
  return el("div.viz", {},
    el("div.viz-legend", {}, el("span", {}, el("i", { style: "background:var(--viz-1)" }), valueLabel), el("span", {}, el("i.viz-tick-key"), targetLabel)),
    el("div.viz-rows", {}, rows),
    tableView(["", valueLabel, targetLabel, "Σχέση"], data.map((d) => [d.label, fmt(d.value), fmt(d.target), d.target ? `${(d.value / d.target).toLocaleString("el-GR", { maximumFractionDigits: 1 })}×` : ""])));
}

/** A number with its label (stat tile). */
export function statTile(label, value, note = "") {
  return el("div.viz-tile", {}, el("span.small.muted", {}, label), el("strong", {}, value), note ? el("span.small.muted", {}, note) : null);
}

export { pct, fmt };
