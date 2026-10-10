import { $, busy, createApi, el, formatDateTime, message, show } from "./ui.js";
import { readWorkbook } from "/lib/import/workbook.js";
import { headerKey } from "/lib/import/table.js";
import { decodeCsv, parseCsv } from "/lib/import/csv.js";
import { reportView as studentReportView } from "./report.js";
import { makeZip } from "/lib/export/zip.js";
import { LIST_SHEET, teacherListFileName, teacherListRows } from "/lib/export/teacherListFile.js";
import { helpTabs } from "./help.js";
import { barsWithTarget, columns, fmt, pct, stackedBars, statTile } from "./charts.js";
import { ROSTER_SHEET, WEEK_HEADER, clubMembers, rosterFileName, rosterRows, weekRows } from "/lib/export/rosters.js";

const api = createApi("admin");
const app = $("#app");
const logout = $("#logout");

const DAYS = ["mon", "tue", "wed", "thu", "fri"];
const DAY_LABELS = { mon: "Δευτέρα", tue: "Τρίτη", wed: "Τετάρτη", thu: "Πέμπτη", fri: "Παρασκευή" };
const PHASES = [
  ["setup", "Προετοιμασία"],
  ["teachers", "Εκπαιδευτικοί"],
  ["parents", "Δηλώσεις γονέων"],
  ["closed", "Κλειστές δηλώσεις"],
  ["allocated", "Κατανομή"],
  ["published", "Ανακοίνωση"],
];
const PHASE_HELP = {
  setup: "Ανεβάστε τον κατάλογο μαθητών και το αρχείο ομίλων.",
  teachers: "Οι εκπαιδευτικοί συνδέονται (με τον σύνδεσμο που τους στέλνετε από την καρτέλα «Όμιλοι») και ορίζουν χωρητικότητα και προτιμώμενους μαθητές.",
  parents: "Οι γονείς υποβάλλουν δηλώσεις μέχρι την προθεσμία. Όμιλοι και λίστες εκπαιδευτικών είναι κλειδωμένα· μαθητές μόνο προστίθενται.",
  closed: "Δεν γίνονται δεκτές δηλώσεις. Εκτελέστε την κατανομή.",
  allocated: "Η κατανομή έγινε. Ελέγξτε τα αποτελέσματα (μπορείτε να την ξανατρέξετε) και ανακοινώστε τα.",
  published: "Οι γονείς βλέπουν το αποτέλεσμα όταν συνδέονται.",
};

let state = null;
let tab = "overview";
let resultsTab = "fill"; // sub-tab of the allocation results
let devOutbox = false;

api.onExpired = () => { api.setToken(null); loginView(message("warn", "Η σύνδεση έληξε. Συνδεθείτε ξανά.")); };
logout.addEventListener("click", () => { api.setToken(null); loginView(); });

async function start() {
  if (api.hasToken()) {
    try {
      await refresh();
      return;
    } catch { api.setToken(null); }
  }
  loginView();
}

function loginView(notice) {
  logout.classList.add("hidden");
  const out = el("div");
  const pw = el("input", { id: "pw", type: "password", required: true, autocomplete: "current-password" });
  const form = el("form.card", {}, el("label", { for: "pw" }, "Κωδικός διαχείρισης", pw), out, el("div.actions", {}, el("button.primary", { type: "submit" }, "Σύνδεση")));
  form.addEventListener("submit", (e) => {
    e.preventDefault();
    busy(form.querySelector("button"), out, async () => {
      const { token } = await api("POST", "/api/admin/login", { password: pw.value });
      api.setToken(token);
      await refresh();
    });
  });
  show(app, el("h1", {}, "Διαχείριση"), notice, form);
}

async function refresh() {
  state = await api("GET", "/api/admin/state");
  devOutbox = await api("GET", "/api/admin/outbox").then(() => true, () => false);
  render();
}

function render() {
  logout.classList.remove("hidden");
  const tabs = [["overview", "Πορεία & ρυθμίσεις"], ["data", "Αρχεία"], ["students", "Μαθητές"], ["clubs", "Όμιλοι"], ["allocation", "Κατανομή"], ["history", "Ιστορικό"], ...(devOutbox ? [["outbox", "Εξερχόμενα email"]] : []), ["help", "Βοήθεια"]];
  const nav = el("nav.tabs", { role: "tablist" }, tabs.map(([id, label]) =>
    el("button", { type: "button", role: "tab", "aria-selected": String(tab === id), onclick: () => { tab = id; render(); } }, label)));
  const views = { overview, data, students, clubs, allocation, history, outbox, help: () => helpTabs(["admin", "teacher", "parent"]) };
  show(app, el("h1", {}, "Διαχείριση"), phaseBar(), nav, el("div", { role: "tabpanel" }, views[tab]()));
}

// ---------- Phase ----------

function phaseBar() {
  const phase = state.settings.phase;
  const idx = PHASES.findIndex(([p]) => p === phase);
  const out = el("div");
  const go = (target, confirmText) => async (e) => {
    if (confirmText && !confirm(confirmText)) return;
    await busy(e.currentTarget, out, async () => {
      await api("POST", "/api/admin/phase", { phase: target });
      await refresh();
    });
  };
  const next = PHASES[idx + 1];
  const prev = PHASES[idx - 1];
  const confirmNext = {
    parents: [
      ...(state.readiness ?? []).filter((p) => p.level === "warning" && !p.day).map((p) => `ΠΡΟΣΟΧΗ: ${p.message}`),
      "Άνοιγμα δηλώσεων: οι όμιλοι και οι λίστες των εκπαιδευτικών κλειδώνουν. Συνέχεια;",
    ].join("\n\n"),
    closed: "Κλείσιμο δηλώσεων: οι γονείς δεν θα μπορούν πια να αλλάξουν δηλώσεις. Συνέχεια;",
    published: "Ανακοίνωση: οι γονείς θα βλέπουν το αποτέλεσμα. Συνέχεια;",
  };
  return el("section.card", {},
    el("ol.steps", {}, PHASES.map(([p, label], i) => el(`li${i < idx ? ".done" : i === idx ? ".current" : ""}`, {}, label))),
    el("p", {}, PHASE_HELP[phase]),
    out,
    // Previous phase on the left, next phase on the right.
    el("div.actions.phase-actions", {},
      prev ? el("button", { type: "button", onclick: go(prev[0], `Επιστροφή στη φάση «${prev[1]}»;`) }, `← Επιστροφή: ${prev[1]}`) : null,
      el("span.spacer"),
      next && next[0] !== "allocated" ? el("button.primary", { type: "button", onclick: go(next[0], confirmNext[next[0]]) }, `Επόμενη φάση: ${next[1]} →`) : null,
      next && next[0] === "allocated" ? el("span.muted.small", {}, "Επόμενο βήμα: καρτέλα «Κατανομή».") : null));
}

// ---------- Overview & settings ----------

function overview() {
  const s = state.settings;
  const out = el("div");
  const toLocal = (iso) => (iso ? new Date(new Date(iso).getTime() - new Date(iso).getTimezoneOffset() * 60000).toISOString().slice(0, 16) : "");
  const f = {
    schoolName: el("input", { id: "school", type: "text", value: s.schoolName ?? "" }),
    contact: el("input", { id: "contact", type: "text", value: s.contact ?? "", placeholder: "π.χ. Γραμματεία: 2310 000000, mail@sch.gr" }),
    deadline: el("input", { id: "deadline", type: "datetime-local", value: toLocal(s.deadline) }),
    mandatory: ["Α", "Β", "Γ"].map((g) => el("input", { type: "checkbox", value: g, checked: (s.mandatoryGrades ?? []).includes(g), "aria-label": `Υποχρεωτική ένταξη ${g} τάξης` })),
    parentPassword: el("input", { id: "ppw", type: "text", autocomplete: "off", placeholder: s.parentPasswordSet ? "Έχει οριστεί — γράψτε νέο για αλλαγή" : "Τουλάχιστον 6 χαρακτήρες" }),
    teacherPassword: el("input", { id: "tpw", type: "text", autocomplete: "off", placeholder: s.teacherPasswordSet ? "Έχει οριστεί — γράψτε νέο για αλλαγή" : "Τουλάχιστον 6 χαρακτήρες" }),
  };
  const save = el("button.primary", { type: "button" }, "Αποθήκευση ρυθμίσεων");
  save.addEventListener("click", () => busy(save, out, async () => {
    const body = {
      schoolName: f.schoolName.value, contact: f.contact.value, deadline: f.deadline.value ? new Date(f.deadline.value).toISOString() : null,
      mandatoryGrades: f.mandatory.filter((c) => c.checked).map((c) => c.value),
    };
    if (f.parentPassword.value) body.parentPassword = f.parentPassword.value;
    if (f.teacherPassword.value) body.teacherPassword = f.teacherPassword.value;
    await api("PUT", "/api/admin/settings", body);
    await refresh();
    $("#app [role=tabpanel]").prepend(message("ok", "Αποθηκεύτηκε."));
  }));

  const submitted = Object.keys(state.submissions).length;
  const byGrade = ["Α", "Β", "Γ"].map((g) => {
    const all = state.students.filter((x) => x.grade === g);
    return `${g}: ${all.filter((x) => state.submissions[x.am]).length}/${all.length}`;
  }).join(" · ");
  return el("div", {},
    el("section.card", {},
      el("h2", { style: "margin-top:0" }, "Σύνοψη"),
      el("ul", {},
        el("li", {}, `Μαθητές: ${state.students.length}`),
        el("li", {}, `Όμιλοι: ${state.clubs.length} · Εκπαιδευτικοί: ${state.teachers.length}`),
        el("li", {}, `Δηλώσεις: ${submitted} από ${state.students.length} (${byGrade})`),
        state.results ? el("li", {}, `Κατανομή: ${formatDateTime(state.results.at)} (seed «${state.results.seed}»)`) : null)),
    checksCard(),
    addressesCard(),
    el("section.card", {},
      el("h2", { style: "margin-top:0" }, "Ρυθμίσεις"),
      el("div.grid-2", {},
        el("label", { for: "school" }, "Όνομα σχολείου", f.schoolName),
        el("label", { for: "contact" }, "Επικοινωνία για γονείς", el("span.hint", {}, "Εμφανίζεται όταν αποτυγχάνει η σύνδεση."), f.contact),
        el("label", { for: "deadline" }, "Προθεσμία δηλώσεων", f.deadline),
        el("label", { for: "ppw" }, "Κοινός κωδικός γονέων", el("span.hint", {}, "Ίδιος για όλους· ανακοινώνεται από το σχολείο."), f.parentPassword),
        el("label", { for: "tpw" }, "Κοινός κωδικός εκπαιδευτικών", el("span.hint", {}, "Ίδιος για όλους· μαζί με τον ΑΜ ή ΑΦΜ του ο καθένας βλέπει μόνο τους ομίλους του. Μπορεί να είναι ίδιος με των γονέων. Αν αλλάξει, όσοι είχαν συνδεθεί αποσυνδέονται."), f.teacherPassword)),
      el("fieldset", { style: "border:0;padding:0;margin:12px 0 0" },
        el("legend", { style: "font-weight:600" }, "Υποχρεωτική ένταξη σε όμιλο"),
        el("p.small.muted", { style: "margin:2px 0 6px" }, "Για αυτές τις τάξεις κάθε μαθητής πρέπει να πάρει όμιλο κάθε ημέρα: οι δηλώσεις δεν ανοίγουν αν οι θέσεις δεν φτάνουν, και μετά την κατανομή εμφανίζεται όποιος έμεινε εκτός."),
        el("div.actions", { style: "margin-top:0" }, f.mandatory.map((c) => el("label", { style: "font-weight:400;margin:0" }, c, ` ${c.value} τάξη`)))),
      out,
      el("div.actions", {}, save)),
    dangerZone());
}

// Seats and other checks for the current files and settings
function checksCard() {
  if (!state.students.length || !state.clubs.length) return null;
  const errors = state.readiness.filter((p) => p.level === "error");
  const warnings = state.readiness.filter((p) => p.level === "warning");
  const mandatory = state.settings.mandatoryGrades ?? [];
  return el("section.card", {},
    el("h2", { style: "margin-top:0" }, "Έλεγχοι θέσεων"),
    el("p.small.muted", {}, mandatory.length
      ? `Υποχρεωτική ένταξη: ${mandatory.map((g) => `${g}`).join(", ")} τάξη. Για κάθε ημέρα ελέγχεται κάθε τάξη μόνη της και κάθε συνδυασμός τους, ώστε οι όμιλοι που είναι κοινοί σε περισσότερες τάξεις να μη μετρώνται δύο φορές.`
      : "Καμία τάξη με υποχρεωτική ένταξη."),
    errors.length ? message("err", `Δεν φτάνουν οι θέσεις — οι δηλώσεις δεν μπορούν να ανοίξουν:`, errors.map((p) => p.message)) : message("ok", "Οι θέσεις φτάνουν για όλους τους μαθητές των τάξεων με υποχρεωτική ένταξη."),
    warnings.length ? message("warn", "Προσοχή:", warnings.map((p) => p.message)) : null);
}

function dangerZone() {
  const out = el("div");
  const word = el("input", { id: "resetword", type: "text", autocomplete: "off", placeholder: "ΔΙΑΓΡΑΦΗ" });
  const keep = el("input", { type: "checkbox", checked: true });
  const go = el("button.danger", { type: "button" }, "Διαγραφή όλων των δεδομένων");
  go.addEventListener("click", () => {
    if (!confirm("Θα διαγραφούν οριστικά μαθητές, όμιλοι, εκπαιδευτικοί, δηλώσεις και αποτελέσματα. Συνέχεια;")) return;
    busy(go, out, async () => {
      try {
        await api("POST", "/api/admin/reset", { confirm: word.value.trim(), keepSchoolInfo: keep.checked });
      } catch (err) {
        if (err.status === 422) throw err; // the confirmation word
        // Something may already be deleted: show what is there now, not the old page
        await refresh();
        $("#app [role=tabpanel]").prepend(message("err", `Η επαναφορά δεν ολοκληρώθηκε (${err.message}). Η σελίδα δείχνει πλέον την τρέχουσα κατάσταση· πατήστε ξανά «Διαγραφή όλων των δεδομένων» για να ολοκληρωθεί.`));
        return;
      }
      tab = "overview";
      await refresh();
      $("#app [role=tabpanel]").prepend(message("ok", "Η πλατφόρμα άδειασε. Είναι ξανά στη φάση «Προετοιμασία»."));
    });
  });
  return el("details.card", {},
    el("summary", {}, el("strong", { style: "color:var(--err)" }, "Επαναφορά πλατφόρμας (διαγραφή δεδομένων)")),
    el("p", {}, "Χρήσιμο μετά από δοκιμή, π.χ. με τα περσινά δεδομένα. Διαγράφονται ", el("strong", {}, "οριστικά"),
      ": μαθητές, όμιλοι, εκπαιδευτικοί και λίστες τους, δηλώσεις γονέων, αποτελέσματα κατανομής, ιστορικό ενεργειών και εξερχόμενα. Ο κωδικός γονέων και η προθεσμία σβήνονται· η φάση γίνεται «Προετοιμασία». Ο κωδικός διαχείρισης δεν αλλάζει."),
    el("p.small.muted", {}, "Αν θέλετε να κρατήσετε αντίγραφο, κατεβάστε πρώτα από την καρτέλα «Κατανομή» τα δεδομένα για R και τα αποτελέσματα."),
    el("label", { style: "font-weight:400" }, keep, " Να κρατηθούν το όνομα του σχολείου και τα στοιχεία επικοινωνίας"),
    el("label", { for: "resetword" }, "Για επιβεβαίωση γράψτε ΔΙΑΓΡΑΦΗ", word),
    out,
    el("div.actions", {}, go));
}

// ---------- Files ----------

async function loadXlsx() {
  try {
    return await import("/vendor/xlsx.mjs");
  } catch {
    return await import("https://cdn.sheetjs.com/xlsx-0.20.3/package/xlsx.mjs");
  }
}

function reportView(report) {
  const where = (p) => [p.sheet ? `«${p.sheet}»` : null, p.row ? `γραμμή ${p.row}` : null].filter(Boolean).join(", ");
  const line = (p) => (where(p) ? `${where(p)}: ${p.message}` : p.message);
  const errors = report.problems.filter((p) => p.level === "error");
  const warnings = report.problems.filter((p) => p.level === "warning");
  const infos = report.problems.filter((p) => p.level === "info");
  return el("div", {},
    errors.length ? message("err", `Το αρχείο ΔΕΝ έγινε δεκτό — ${errors.length} σφάλματα:`, errors.map(line)) : message("ok", "Το αρχείο έγινε δεκτό."),
    warnings.length ? message("warn", `${warnings.length} προειδοποιήσεις:`, warnings.map(line)) : null,
    infos.length ? message("info", infos.map((p) => p.message).join(" ")) : null);
}

function data() {
  const locked = ["parents", "closed", "allocated", "published"].includes(state.settings.phase);
  const upload = (title, help, handler, disabled) => {
    const out = el("div");
    const input = el("input", { type: "file", accept: ".xls,.xlsx", disabled, "aria-label": title });
    input.addEventListener("change", async () => {
      const file = input.files[0];
      if (!file) return;
      show(out, message("info", `Ανάγνωση «${file.name}»…`));
      try {
        const XLSX = await loadXlsx();
        const sheets = readWorkbook(XLSX, new Uint8Array(await file.arrayBuffer()));
        const report = await handler(sheets, file.name);
        await refresh();
        tab = "data";
        render();
        $(`#out-${title.length}`)?.replaceChildren(reportView(report));
      } catch (err) {
        show(out, err.data?.report ? reportView(err.data.report) : message("err", err.message));
      } finally {
        input.value = "";
      }
    });
    return el("section.card", {}, el("h2", { style: "margin-top:0" }, title), el("p.muted", {}, help), input, el("div", { id: `out-${title.length}` }, out));
  };
  const find = (sheets, name) => Object.entries(sheets).find(([n]) => headerKey(n) === headerKey(name))?.[1];
  return el("div", {},
    uploadsStatus(),
    upload("Κατάλογος μαθητών (myschool)", locked ? "Οι δηλώσεις είναι ανοιχτές: από νέο αρχείο προστίθενται μόνο οι νέοι μαθητές." : "Το «Κατάλογος Μαθητών» όπως εξάγεται από το myschool (.xls).", async (sheets, fileName) => {
      const rows = Object.values(sheets)[0] ?? [];
      return (await api("PUT", "/api/admin/students", { rows, fileName })).report;
    }, false),
    upload("Όμιλοι και εκπαιδευτικοί", locked ? "Κλειδωμένο: οι δηλώσεις έχουν ανοίξει." : "Το πρότυπο της πλατφόρμας, συμπληρωμένο.", async (sheets, fileName) => {
      const clubs = find(sheets, "Όμιλοι");
      const teachers = find(sheets, "Εκπαιδευτικοί");
      if (!clubs || !teachers) throw new Error("Το αρχείο δεν έχει τα φύλλα «Όμιλοι» και «Εκπαιδευτικοί». Χρησιμοποιήστε το πρότυπο.");
      return (await api("PUT", "/api/admin/clubs", { clubs, teachers, fileName })).report;
    }, locked),
    el("p", {}, el("a.button", { href: "/templates/omiloi_protypo.xlsx", download: "omiloi_protypo.xlsx" }, "Λήψη κενού προτύπου ομίλων")),
    legacyImport(),
    el("p.small.muted", {}, "Τα αρχεία διαβάζονται στον υπολογιστή σας· στον διακομιστή στέλνονται μόνο τα στοιχεία των πινάκων και ελέγχονται ξανά."));
}

// What is in the database now
function uploadsStatus() {
  const u = state.uploads ?? {};
  const when = (x) => (x ? `${x.fileName ? `«${x.fileName}» · ` : ""}${formatDateTime(x.at)}` : "");
  const subs = Object.values(state.submissions);
  const imported = subs.filter((s) => s.imported).length;
  const lists = Object.values(state.teacherLists).filter((l) => l.ams?.length).length;
  const row = (label, ok, detail, info) => el("tr", {},
    el("td", {}, ok ? el("span.badge.ok", {}, "✓") : el("span.badge", {}, "—")), el("td", {}, el("strong", {}, label)),
    el("td", {}, detail), el("td.small.muted", {}, info));
  const byGrade = (g) => ["Α", "Β", "Γ"].map((k) => `${k}: ${g?.[k] ?? 0}`).join(" · ");
  return el("section.card", {},
    el("h2", { style: "margin-top:0" }, "Τι υπάρχει ήδη στη βάση"),
    el("div.table-wrap", {}, el("table", {}, el("tbody", {},
      row("Κατάλογος μαθητών", state.students.length > 0, state.students.length ? `${state.students.length} μαθητές (${byGrade(Object.fromEntries(["Α", "Β", "Γ"].map((g) => [g, state.students.filter((s) => s.grade === g).length])))})` : "δεν έχει ανέβει", when(u.students)),
      row("Όμιλοι και εκπαιδευτικοί", state.clubs.length > 0, state.clubs.length ? `${state.clubs.length} όμιλοι (${state.clubs.filter((c) => c.days.length > 1).length} πολυήμεροι) · ${state.teachers.length} εκπαιδευτικοί` : "δεν έχει ανέβει", when(u.clubs)),
      row("Λίστες εκπαιδευτικών", lists > 0, `${lists} όμιλοι με προτιμώμενους μαθητές`, ""),
      row("Δηλώσεις γονέων", subs.length - imported > 0, `${subs.length - imported} δηλώσεις`, ""),
      // Trial import: all five days, so a forgotten day shows up. Counts come
      // from the submissions themselves (also for uploads made before file
      // names were recorded).
      ...(imported || u.legacy ? DAYS.map((d) => {
        const n = subs.filter((s) => s.imported && s.days?.includes(d)).length;
        return row(`Περσινές δηλώσεις — ${DAY_LABELS[d]}`, n > 0, n > 0 ? `${n} μαθητές με επιλογές` : el("strong", { style: "color:var(--warn)" }, "δεν έχει ανέβει"), when(u.legacy?.[d]));
      }) : []),
      row("Κατανομή", Boolean(state.results), state.results ? `seed «${state.results.seed}»` : "δεν έχει γίνει", state.results ? formatDateTime(state.results.at) : "")))));
}

// Trial: last year's per-day responses
function legacyImport() {
  const out = el("div");
  const day = el("select", { id: "legacyday" }, el("option", { value: "" }, "— επιλέξτε —"), DAYS.map((d) => el("option", { value: d }, DAY_LABELS[d])));
  const addMissing = el("input", { type: "checkbox" });
  const grade = el("select", { "aria-label": "Τάξη για τους δοκιμαστικούς μαθητές" }, ["Α", "Β", "Γ"].map((g) => el("option", { value: g }, g)));
  const input = el("input", { type: "file", accept: ".csv,.xls,.xlsx", "aria-label": "Αρχείο περσινών δηλώσεων" });
  input.addEventListener("change", async () => {
    const file = input.files[0];
    if (!file) return;
    if (!day.value) {
      show(out, message("err", "Επιλέξτε πρώτα την ημέρα του αρχείου."));
      input.value = "";
      return;
    }
    show(out, message("info", `Ανάγνωση «${file.name}»…`));
    try {
      const bytes = new Uint8Array(await file.arrayBuffer());
      const rows = /\.csv$/i.test(file.name) ? parseCsv(decodeCsv(bytes)) : Object.values(readWorkbook(await loadXlsx(), bytes))[0] ?? [];
      const { report } = await api("POST", "/api/admin/import-legacy", { day: day.value, rows, addMissingGrade: addMissing.checked ? grade.value : null, fileName: file.name });
      await refresh();
      tab = "data";
      render();
      $("#legacy-out")?.closest("details")?.setAttribute("open", "");
      $("#legacy-out")?.replaceChildren(message("ok", `${DAY_LABELS[day.value]}: εισήχθησαν ${report.summary.rows} δηλώσεις${report.summary.newStudents ? `, προστέθηκαν ${report.summary.newStudents} δοκιμαστικοί μαθητές` : ""}.`), reportView({ problems: report.problems.filter((p) => p.level !== "error") }));
    } catch (err) {
      show(out, err.data?.report ? reportView(err.data.report) : message("err", err.message));
    } finally {
      input.value = "";
    }
  });
  return el("details.card", {},
    el("summary", {}, el("strong", {}, "Δοκιμή: εισαγωγή περσινών δηλώσεων")),
    el("p", {}, "Για δοκιμή της κατανομής με τα περσινά δεδομένα. Ένα αρχείο ανά ημέρα, όπως το περσινό ", el("code", {}, "dailyresponses.csv"),
      ": στήλες ΑΜ (RegistryNr), επώνυμο, όνομα και μία στήλη ανά όμιλο με τη θέση προτίμησης. Οι στήλες ομίλων πρέπει να έχουν το ίδιο όνομα με το πρότυπο ομίλων (ή τον κωδικό του ομίλου)."),
    message("warn", "Μόνο για δοκιμή. Οι εισαγόμενες δηλώσεις φαίνονται ως «Εισαγωγή δοκιμής». Μετά τη δοκιμή κάντε «Επαναφορά πλατφόρμας» (καρτέλα «Πορεία & ρυθμίσεις»)."),
    el("label", { for: "legacyday" }, "Ημέρα του αρχείου", day),
    el("label", { style: "font-weight:400" }, addMissing, " Όσοι ΑΜ δεν υπάρχουν στον κατάλογο να προστεθούν ως δοκιμαστικοί μαθητές της τάξης ", grade),
    el("label", {}, "Αρχείο (.csv, .xls, .xlsx)", input),
    el("div", { id: "legacy-out" }, out));
}

// ---------- Students ----------

function students() {
  const search = el("input", { type: "search", placeholder: "Αναζήτηση (επώνυμο, όνομα, ΑΜ)", "aria-label": "Αναζήτηση μαθητή" });
  const filter = el("select", { "aria-label": "Φίλτρο" },
    el("option", { value: "" }, "Όλοι"), el("option", { value: "missing" }, "Χωρίς δήλωση"), el("option", { value: "done" }, "Με δήλωση"), el("option", { value: "exception" }, "Με εξαίρεση σύνδεσης"));
  const body = el("tbody");
  const draw = () => {
    const q = search.value.trim().toUpperCase();
    const rows = state.students.filter((s) => {
      const sub = state.submissions[s.am];
      if (filter.value === "missing" && sub) return false;
      if (filter.value === "done" && !sub) return false;
      if (filter.value === "exception" && !s.loginException) return false;
      return !q || `${s.surname} ${s.name} ${s.am}`.toUpperCase().includes(q);
    });
    body.replaceChildren(...rows.map((s) => {
      const sub = state.submissions[s.am];
      const cb = el("input", { type: "checkbox", checked: s.loginException, "aria-label": `Εξαίρεση σύνδεσης για ΑΜ ${s.am}` });
      cb.addEventListener("change", async () => {
        try {
          await api("PATCH", `/api/admin/students/${s.am}`, { loginException: cb.checked });
          s.loginException = cb.checked;
        } catch (err) { alert(err.message); cb.checked = !cb.checked; }
      });
      return el("tr", {},
        el("td.num", {}, s.am), el("td", {}, `${s.surname} ${s.name}`), el("td", {}, s.grade),
        el("td", {}, sub ? el("span", {}, el("span.badge.ok", {}, "✓"), " ", el("span.small", {}, `${formatDateTime(sub.submittedAt)} · ${sub.parentEmail ?? ""}`)) : el("span.badge", {}, "—")),
        el("td", {}, el("label", { style: "margin:0;font-weight:400" }, cb, " μόνο ΑΜ + επώνυμο")));
    }));
  };
  search.addEventListener("input", draw);
  filter.addEventListener("change", draw);
  draw();
  return el("section.card", {},
    el("p.small.muted", {}, "Η «εξαίρεση σύνδεσης» επιτρέπει στον γονέα να συνδεθεί μόνο με ΑΜ και επώνυμο (π.χ. όταν τα ονόματα των γονέων δεν είναι γραμμένα όπως τα ξέρει)."),
    el("div.grid-2", {}, search, filter),
    el("div.table-wrap", {}, el("table", {}, el("thead", {}, el("tr", {}, el("th.num", {}, "ΑΜ"), el("th", {}, "Μαθητής"), el("th", {}, "Τάξη"), el("th", {}, "Δήλωση"), el("th", {}, "Σύνδεση"))), body)));
}

// ---------- Clubs ----------

function clubs() {
  return el("div", {}, clubsTable(), teacherListFiles(), teacherLinks());
}

/** Teachers' lists stay open while parents declare (same deadline); clubs lock earlier. */
const listsEditable = () => ["setup", "teachers"].includes(state.settings.phase)
  || (state.settings.phase === "parents" && !(state.settings.deadline && Date.now() > Date.parse(state.settings.deadline)));

// Teachers' lists by Excel: one file per club out, all files back in
function teacherListFiles() {
  const editable = listsEditable();
  const out = el("div");
  const preview = el("div");
  const download = el("button", { type: "button", disabled: !state.clubs.length }, "Αρχεία για τους εκπαιδευτικούς (.zip)");
  download.addEventListener("click", () => busy(download, out, async () => {
    const XLSX = await loadXlsx();
    const files = {};
    for (const c of state.clubs) {
      const wb = XLSX.utils.book_new();
      const ws = XLSX.utils.aoa_to_sheet(teacherListRows(c, state.students, state.teacherLists[c.code]?.ams ?? []));
      ws["!cols"] = [{ wch: 10 }, { wch: 10 }, { wch: 24 }, { wch: 20 }, { wch: 6 }];
      XLSX.utils.book_append_sheet(wb, ws, LIST_SHEET);
      files[teacherListFileName(c)] = new Uint8Array(XLSX.write(wb, { type: "array", bookType: "xlsx" }));
    }
    const url = URL.createObjectURL(new Blob([makeZip(files)], { type: "application/zip" }));
    el("a", { href: url, download: "listes_ekpaideutikon.zip" }).click();
    setTimeout(() => URL.revokeObjectURL(url), 10000);
  }));

  const input = el("input", { type: "file", multiple: true, accept: ".xlsx,.xls,.csv", disabled: !editable, "aria-label": "Αρχεία λιστών εκπαιδευτικών" });
  let files = [];
  const send = (apply) => api("POST", "/api/admin/teacher-lists/import", { files, apply });
  const showReport = ({ report, ok }) => {
    const save = el("button.primary", { type: "button", disabled: !ok }, `Αποθήκευση ${report.filter((r) => r.code).length} λιστών`);
    save.addEventListener("click", () => busy(save, preview, async () => {
      await send(true);
      await refresh();
      tab = "clubs";
      render();
    }));
    show(preview,
      el("div.table-wrap", {}, el("table", {},
        el("thead", {}, el("tr", {}, el("th", {}, "Αρχείο"), el("th", {}, "Όμιλος"), el("th.num", {}, "Μαθητές"), el("th", {}, "Έλεγχος"))),
        el("tbody", {}, report.map((r) => el("tr", {},
          el("td.small", {}, r.fileName), el("td", {}, r.code ? `${r.code} ${r.clubName}` : "—"),
          el("td.num", {}, r.code ? `${r.ams.length} / ${r.capacity ?? "?"}` : ""),
          el("td.small", {},
            r.problems.map((m) => el("div", { style: "color:var(--err)" }, `✗ ${m}`)),
            r.warnings.map((m) => el("div", { style: "color:var(--warn)" }, `! ${m}`)),
            !r.problems.length && !r.warnings.length ? el("span", { style: "color:var(--ok)" }, "✓") : null)))))),
      ok ? message("info", "Ελέγξτε τον πίνακα και πατήστε «Αποθήκευση». Οι λίστες αντικαθιστούν όσες υπάρχουν για αυτούς τους ομίλους.")
        : message("err", "Κάποια αρχεία έχουν σφάλματα (✗). Διορθώστε τα και επιλέξτε ξανά όλα τα αρχεία· δεν αποθηκεύτηκε τίποτα."),
      el("div.actions", {}, save));
  };
  input.addEventListener("change", () => busy(input, preview, async () => {
    const XLSX = [...input.files].some((f) => !/\.csv$/i.test(f.name)) ? await loadXlsx() : null;
    files = [];
    for (const f of input.files) {
      const bytes = new Uint8Array(await f.arrayBuffer());
      let rows;
      if (/\.csv$/i.test(f.name)) rows = parseCsv(decodeCsv(bytes));
      else {
        const sheets = readWorkbook(XLSX, bytes);
        rows = Object.entries(sheets).find(([n]) => headerKey(n) === headerKey(LIST_SHEET))?.[1] ?? Object.values(sheets)[0] ?? [];
      }
      files.push({ fileName: f.name, rows });
    }
    showReport(await send(false));
  }));

  return el("section.card", {},
    el("h2", { style: "margin-top:0" }, "Λίστες εκπαιδευτικών από Excel"),
    el("p.small", {}, "Εναλλακτικά στη σελίδα των εκπαιδευτικών: κατεβάστε ένα Excel για κάθε όμιλο, με όλους τους μαθητές των τάξεών του. Ο εκπαιδευτικός βάζει Χ στη στήλη «Επιλογή» δίπλα στους μαθητές που προτιμά (έως τις θέσεις) — ή σβήνει τις γραμμές όσων δεν θέλει — και σας το επιστρέφει. Ανεβάστε όλα τα αρχεία μαζί: θα δείτε έλεγχο πριν την αποθήκευση."),
    el("p.small.muted", {}, "Γίνεται δεκτός και ένας πίνακας για πολλούς ομίλους, με στήλες «Κωδικός ομίλου» και «ΑΜ» (μία γραμμή ανά μαθητή). Η χωρητικότητα δεν αλλάζει από τα αρχεία."),
    out,
    el("div.actions", {}, download),
    editable ? el("label", {}, "Επιστρεφόμενα αρχεία (.xlsx, .xls, .csv — πολλά μαζί)", input) : message("info", "Οι λίστες κλείδωσαν με το κλείσιμο των δηλώσεων."),
    preview);
}

// The three addresses: each role gets only its own (no links between them).
function addressesCard() {
  const row = (who, url, note) => el("tr", {}, el("td", {}, who), el("td", {}, el("code", {}, url)), el("td.small.muted", {}, note));
  return el("section.card", {},
    el("h2", { style: "margin-top:0" }, "Διευθύνσεις"),
    el("p.small.muted", {}, "Δώστε σε κάθε ομάδα μόνο τη δική της διεύθυνση. Οι σελίδες δεν έχουν συνδέσμους μεταξύ τους, και η καθεμία ζητά τον δικό της κωδικό."),
    el("div.table-wrap", {}, el("table", {}, el("tbody", {},
      row("Γονείς", `${location.origin}/`, "κοινός κωδικός γονέων + στοιχεία μαθητή"),
      state.teacherPath ? row("Εκπαιδευτικοί", `${location.origin}${state.teacherPath}`, "κοινός κωδικός εκπαιδευτικών + ΑΜ ή ΑΦΜ") : null,
      row("Διαχείριση", `${location.origin}${location.pathname}`, "μόνο για εσάς")))));
}

// Login links for teachers, passed on by the admin (no e-mail service).
function teacherLinks() {
  const out = el("div");
  const rows = state.teachers.map((t) => {
    const cell = el("td");
    const make = el("button.small", { type: "button" }, "Σύνδεσμος εισόδου");
    make.addEventListener("click", () => busy(make, out, async () => {
      const { link, expiresAt } = await api("POST", "/api/admin/teacher-link", { email: t.email });
      const clubNames = state.clubs.filter((c) => t.clubs.includes(c.code)).map((c) => `«${c.name}»`).join(", ");
      const text = `Καλημέρα ${t.name} ${t.surname},\n\nΓια την πλατφόρμα ομίλων (${clubNames}) ο προσωπικός σας σύνδεσμος εισόδου είναι:\n${link}\n\nΙσχύει έως ${formatDateTime(expiresAt)}. Μην τον προωθήσετε σε άλλους.`;
      const area = el("textarea", { readonly: true, rows: 6, "aria-label": `Μήνυμα για ${t.email}` }, text);
      const copy = el("button.small.primary", { type: "button" }, "Αντιγραφή μηνύματος");
      copy.addEventListener("click", async () => {
        try {
          await navigator.clipboard.writeText(text);
          copy.textContent = "Αντιγράφηκε ✓";
        } catch {
          area.select();
          document.execCommand?.("copy");
          copy.textContent = "Επιλέχθηκε — Ctrl+C";
        }
      });
      cell.replaceChildren(area, el("div.actions", {}, copy, el("a.button.small", { href: `mailto:${t.email}?subject=${encodeURIComponent("Πλατφόρμα ομίλων: σύνδεσμος εισόδου")}&body=${encodeURIComponent(text)}` }, "Άνοιγμα στο email")));
    }));
    cell.append(make);
    return el("tr", {}, el("td", {}, `${t.name} ${t.surname}`, el("br"), el("span.small.muted", {}, t.email)),
      el("td.small", {}, state.clubs.filter((c) => t.clubs.includes(c.code)).map((c) => c.name).join(", ")),
      el("td.small", {}, t.hasPersonalId ? "✓" : el("span.muted", { title: "Χωρίς ΑΜ/ΑΦΜ στο αρχείο ομίλων: δεν μπορεί να συνδεθεί με τον κοινό κωδικό" }, "—")), cell);
  });
  return el("section.card", {},
    el("h2", { style: "margin-top:0" }, "Σύνδεσμοι εισόδου εκπαιδευτικών"),
    el("p.small.muted", {}, state.settings.teacherPasswordSet
      ? "Οι εκπαιδευτικοί μπαίνουν με τον κοινό κωδικό εκπαιδευτικών και τον ΑΜ ή ΑΦΜ τους (στήλη «ΑΜ ή ΑΦΜ» του αρχείου ομίλων) και βλέπουν μόνο τους δικούς τους ομίλους. Όσοι δεν έχουν ΑΜ/ΑΦΜ (—) χρειάζονται προσωπικό σύνδεσμο ή συμπλήρωση του αρχείου."
      : "Χωρίς υπηρεσία email, στείλτε εσείς σε κάθε εκπαιδευτικό τον προσωπικό του σύνδεσμο (π.χ. από το email του σχολείου), ή ορίστε κοινό κωδικό εκπαιδευτικών στην «Πορεία & ρυθμίσεις». Ο σύνδεσμος ισχύει μία εβδομάδα· αν λήξει, φτιάξτε νέο."),
    out,
    rows.length ? el("div.table-wrap", {}, el("table", {},
      el("thead", {}, el("tr", {}, el("th", {}, "Εκπαιδευτικός"), el("th", {}, "Όμιλοι"), el("th", {}, "ΑΜ/ΑΦΜ"), el("th", {}, ""))),
      el("tbody", {}, rows))) : el("p.muted", {}, "Δεν υπάρχουν εκπαιδευτικοί ακόμα (ανεβάστε το αρχείο ομίλων)."));
}

function clubsTable() {
  const editable = ["setup", "teachers"].includes(state.settings.phase); // clubs and «Παρεμφερείς»
  const listEditable = listsEditable();
  const byAm = new Map(state.students.map((s) => [s.am, s]));
  return el("section.card", {},
    el("div.table-wrap", {}, el("table", {},
      el("thead", {}, el("tr", {}, el("th.num", {}, "Κωδ."), el("th", {}, "Όμιλος"), el("th", {}, "Ημέρες"), el("th", {}, "Τάξεις"), el("th.num", {}, "Θέσεις"), el("th", {}, "Παρεμφερείς"), el("th", {}, "Εκπαιδευτικοί"), el("th", {}, "Λίστα εκπαιδευτικού"))),
      el("tbody", {}, state.clubs.map((c) => {
        const list = state.teacherLists[c.code];
        const teachers = state.teachers.filter((t) => t.clubs.includes(c.code));
        const cell = el("td");
        const summary = () => cell.replaceChildren(
          list?.ams?.length ? el("span", {}, `${list.ams.length} μαθητές`, el("br"), el("span.small.muted", {}, list.ams.map((am) => byAm.get(am)?.surname ?? am).join(", "))) : el("span.muted", {}, "—"),
          listEditable ? el("div", {}, el("button.small", { type: "button", onclick: () => editList(c, list, cell) }, "Διόρθωση")) : null);
        summary();
        return el("tr", {},
          el("td.num", {}, c.code), el("td", {}, c.name), el("td", {}, c.days.map((d) => DAY_LABELS[d]).join(" + ")),
          el("td", {}, c.grades.join(", ")), el("td.num", {}, c.capacity),
          similarCell(c, editable),
          el("td.small", {}, teachers.map((t) => el("div", {}, `${t.name} ${t.surname}`, el("br"), el("span.muted", {}, t.email)))),
          cell);
      })))));
}

// «Παρεμφερείς»: clubs with the same word — a student gets at most one
function similarCell(club, editable) {
  const cell = el("td");
  const others = () => state.clubs.filter((o) => o.code !== club.code && o.similar && o.similar === club.similar).map((o) => o.code);
  const view = () => cell.replaceChildren(
    club.similar ? el("span", {}, el("strong", {}, club.similar), others().length ? el("span.small.muted", {}, el("br"), `με ${others().join(", ")}`) : el("span.small.muted", {}, el("br"), "μόνος του")) : el("span.muted", {}, "—"),
    editable ? el("div", {}, el("button.small", { type: "button", onclick: edit }, "Αλλαγή")) : null);
  function edit() {
    const out = el("div");
    const input = el("input", { type: "text", value: club.similar ?? "", "aria-label": `Παρεμφερείς για τον όμιλο ${club.code}`, placeholder: "π.χ. ΑΓΓΛΙΚΑ" });
    const save = el("button.small.primary", { type: "button" }, "Αποθήκευση");
    save.addEventListener("click", () => busy(save, out, async () => {
      await api("PUT", `/api/admin/clubs/${club.code}/similar`, { similar: input.value });
      await refresh();
    }));
    cell.replaceChildren(input, el("p.small.muted", {}, "Ίδια λέξη = παρεμφερείς· κενό = κανένας."), out, el("div.actions", {}, save, el("button.small", { type: "button", onclick: view }, "Άκυρο")));
    input.focus();
  }
  view();
  return cell;
}

function editList(club, list, cell) {
  const out = el("div");
  const cap = el("input", { type: "number", min: 1, value: list?.capacity ?? club.capacity, "aria-label": "Χωρητικότητα" });
  const ams = el("textarea", { "aria-label": "ΑΜ, ένας ανά γραμμή, με σειρά προτίμησης" }, (list?.ams ?? []).join("\n"));
  const save = el("button.small.primary", { type: "button" }, "Αποθήκευση");
  save.addEventListener("click", () => busy(save, out, async () => {
    await api("PUT", `/api/admin/teacher-lists/${club.code}`, { capacity: Number(cap.value), ams: ams.value.split(/\s+/).filter(Boolean) });
    await refresh();
  }));
  cell.replaceChildren(el("label", {}, "Χωρητικότητα", cap), el("label", {}, "ΑΜ (ένας ανά γραμμή, με σειρά)", ams), out, el("div.actions", {}, save, el("button.small", { type: "button", onclick: render }, "Άκυρο")));
}

// ---------- Allocation ----------

function allocation() {
  const phase = state.settings.phase;
  const out = el("div");
  const resultsBox = el("div");
  const seed = el("input", { id: "seed", type: "text", value: state.results?.seed ?? "", placeholder: "π.χ. αριθμοί που κληρώθηκαν δημόσια" });
  const randomSeed = el("button", { type: "button" }, "Τυχαίο seed");
  randomSeed.addEventListener("click", () => {
    const b = crypto.getRandomValues(new Uint8Array(6));
    seed.value = `${new Date().toISOString().slice(0, 10)}-${[...b].map((x) => x.toString(16).padStart(2, "0")).join("")}`;
  });
  const run = el("button.primary", { type: "button", disabled: !["closed", "allocated"].includes(phase) }, state.results ? "Νέα εκτέλεση κατανομής" : "Εκτέλεση κατανομής");
  run.addEventListener("click", () => {
    if (state.results && !confirm("Η νέα εκτέλεση αντικαθιστά την προηγούμενη κατανομή. Συνέχεια;")) return;
    busy(run, out, async () => {
      const { results } = await api("POST", "/api/admin/allocate", { seed: seed.value });
      await refresh();
      tab = "allocation";
      render();
      $("#results-box")?.replaceChildren(resultsView(results));
    });
  });
  const rZip = el("button", { type: "button" }, "Δεδομένα για R (.zip)");
  rZip.addEventListener("click", () => busy(rZip, out, () => api.download(`/api/admin/export/r-package.zip${state.results ? "" : `?seed=${encodeURIComponent(seed.value)}`}`, "omiloi_dedomena_R.zip")));
  const csv = el("button", { type: "button", disabled: !state.results }, "Αποτελέσματα (.csv)");
  csv.addEventListener("click", () => busy(csv, out, () => api.download("/api/admin/export/results.csv", "katanomi_omilon.csv")));
  const xlsx = el("button", { type: "button", disabled: !state.results }, "Αποτελέσματα (.xlsx)");
  xlsx.addEventListener("click", () => busy(xlsx, out, async () => resultsWorkbook((await api("GET", "/api/admin/results")).results)));
  const audit = el("button", { type: "button", disabled: !state.results }, "Audit log κατανομής (.csv)");
  audit.addEventListener("click", () => busy(audit, out, () => api.download("/api/admin/export/audit_log.csv", "audit_log_katanomis.csv")));
  const lotteryCsv = el("button", { type: "button" }, "Κλήρωση: ΑΜ και αριθμός (.csv)");
  lotteryCsv.addEventListener("click", () => busy(lotteryCsv, out, () => api.download(`/api/admin/export/lottery.csv${state.results ? "" : `?seed=${encodeURIComponent(seed.value)}`}`, "klirosi.csv")));
  const summary = el("button", { type: "button", disabled: !state.results }, "Σύνοψη ανά όμιλο (.csv)");
  summary.addEventListener("click", () => busy(summary, out, () => api.download("/api/admin/export/club_summary.csv", "synopsi_omilon.csv")));

  if (state.results) api("GET", "/api/admin/results").then(({ results }) => resultsBox.replaceChildren(resultsView(results))).catch(() => {});
  resultsBox.id = "results-box";

  return el("div", {},
    el("section.card", {},
      el("h2", { style: "margin-top:0" }, "Κλήρωση και κατανομή"),
      ["closed", "allocated"].includes(phase) ? null : message("info", "Η κατανομή γίνεται αφού κλείσουν οι δηλώσεις (φάση «Κλειστές δηλώσεις»)."),
      el("p.small", {}, "Το seed καθορίζει την κλήρωση: ένας σταθερός αριθμός για κάθε μαθητή, για όλη την εβδομάδα. Ανακοινώστε το seed· με αυτό οποιοσδήποτε ξαναβγάζει την ίδια κλήρωση και την ίδια κατανομή, και offline στο R (φάκελος R/ του αποθετηρίου)."),
      el("label", { for: "seed" }, "Seed κλήρωσης", seed),
      out,
      el("div.actions", {}, run, randomSeed),
      el("h3", {}, "Εξαγωγές"),
      el("div.actions", {}, xlsx, csv, audit, summary, lotteryCsv, rZip),
      el("p.small.muted", {}, "«Κλήρωση»: μόνο ΑΜ και αριθμός κλήρωσης, ταξινομημένα κατά ΑΜ, χωρίς ονόματα — για γρήγορη σύγκριση δύο μαθητών ή για ανακοίνωση. Μικρότερος αριθμός = προηγείται στην ίδια προτεραιότητα. Πριν την κατανομή βγαίνει για το seed που έχετε γράψει.")),
    resultsBox);
}

function resultsView(results) {
  const byAm = new Map(state.students.map((s) => [s.am, s]));
  const clubsByDay = DAYS.map((d) => [d, state.clubs.filter((c) => c.days.includes(d))]).filter(([, cs]) => cs.length);
  // Students who submitted but did not fit anywhere on a day (per day), and
  // students who did not submit at all (once each).
  const rejected = DAYS.flatMap((d) => results.unassigned[d].filter((u) => u.reason === "all_rejected").map((u) => ({ ...u, day: d })));
  const noSubmission = state.students.filter((s) => !state.submissions[s.am]);
  const placed = DAYS.reduce((n, d) => n + Object.values(results.enrolled[d]).reduce((a, b) => a + b, 0), 0);
  const who = (s) => (s ? `${s.surname} ${s.name}` : "");
  const panels = [
    ["fill", "Πληρότητα ανά ημέρα", () => el("div", {},
      el("div.table-wrap", {}, el("table", {},
        el("thead", {}, el("tr", {}, el("th", {}, "Ημέρα"), el("th", {}, "Όμιλος"), el("th.num", {}, "Τοποθετήθηκαν"), el("th.num", {}, "Θέσεις"))),
        el("tbody", {}, clubsByDay.flatMap(([d, cs]) => cs.map((c) =>
          el("tr", {}, el("td", {}, DAY_LABELS[d]), el("td", {}, c.name), el("td.num", {}, results.enrolled[d][String(c.code)] ?? 0), el("td.num", {}, c.capacity))))))))],
    ["gaps", "Κενά ανά ημέρα", () => el("div", {},
      el("div.table-wrap", {}, el("table", {},
        el("thead", {}, el("tr", {}, el("th", {}, "Ημέρα"), el("th.num", {}, "Τοποθετήθηκαν"), el("th.num", {}, "Δεν χώρεσαν"), el("th.num", {}, "Χωρίς προτιμήσεις"), el("th.num", {}, "Χωρίς όμιλο για την τάξη τους"))),
        el("tbody", {}, DAYS.map((d) => el("tr", {},
          el("td", {}, DAY_LABELS[d]),
          el("td.num", {}, Object.values(results.enrolled[d]).reduce((a, b) => a + b, 0)),
          el("td.num", {}, results.gapCounts[d].all_rejected),
          el("td.num", {}, results.gapCounts[d].no_preferences),
          el("td.num", {}, results.gapCounts[d].not_offered)))))),
      el("p.small.muted", {}, "«Χωρίς προτιμήσεις»: ο μαθητής δεν δήλωσε κανέναν όμιλο εκείνης της ημέρας (ή δεν υπέβαλε δήλωση). «Δεν χώρεσε»: όλοι οι όμιλοι που δήλωσε γέμισαν."),
      noPrefsByDay(results, byAm, who))],
    ["rejected", `Δεν χώρεσαν (${rejected.length})`, () => rejected.length
      ? el("div", {}, el("p.small.muted", {}, "Μαθητές που δήλωσαν, αλλά όλοι οι όμιλοι της λίστας τους γέμισαν εκείνη την ημέρα."),
        el("div.table-wrap", {}, el("table", {},
          el("thead", {}, el("tr", {}, el("th", {}, "Ημέρα"), el("th.num", {}, "ΑΜ"), el("th", {}, "Μαθητής"), el("th", {}, "Τάξη"))),
          el("tbody", {}, rejected.map((u) => {
            const s = byAm.get(u.am);
            return el("tr", {}, el("td", {}, DAY_LABELS[u.day]), el("td.num", {}, u.am), el("td", {}, who(s)), el("td", {}, s?.grade ?? ""));
          })))))
      : el("p.muted", {}, "Όσοι δήλωσαν πήραν όμιλο κάθε ημέρα.")],
    ["nosub", `Χωρίς δήλωση (${noSubmission.length})`, () => noSubmission.length
      ? el("div", {}, el("p.small.muted", {}, "Δεν υπέβαλαν δήλωση, άρα δεν τοποθετήθηκαν σε κανέναν όμιλο."),
        el("ul", {}, noSubmission.map((s) => el("li", {}, `${who(s)} · ${s.grade} · ΑΜ ${s.am}`))))
      : el("p.muted", {}, "Όλοι υπέβαλαν δήλωση.")],
    ["week", "Ανά μαθητή", () => weekView(results)],
    ["rosters", "Παρουσιολόγια", () => rostersView(results)],
    ["report", "Αναφορά μαθητή", reportSearch],
    ["stats", "Στατιστικά", () => statsView(results)],
  ];
  if (!panels.some(([id]) => id === resultsTab)) resultsTab = "fill";
  const nav = el("nav.tabs.subtabs", { role: "tablist", "aria-label": "Αποτελέσματα" });
  const panel = el("div", { role: "tabpanel" });
  const draw = () => {
    nav.replaceChildren(...panels.map(([id, label]) => el("button", {
      type: "button", role: "tab", "aria-selected": String(id === resultsTab), onclick: () => { resultsTab = id; draw(); },
    }, label)));
    panel.replaceChildren(panels.find(([id]) => id === resultsTab)[2]());
  };
  draw();
  return el("section.card", {},
    el("h2", { style: "margin-top:0" }, "Αποτελέσματα"),
    mandatoryGapsView(results, byAm, who),
    el("p", {}, `Seed «${results.seed}» · ${formatDateTime(results.at)} · δηλώσεις: ${results.submitted} · τοποθετήσεις (μαθητής × ημέρα): ${placed}`),
    nav, panel);
}

// Students of mandatory grades without a club, one row per student
function mandatoryGapsView(results, byAm, who) {
  if (!results.mandatoryGrades?.length) return null;
  const gaps = results.mandatoryGaps ?? [];
  if (!gaps.length) return message("ok", `Υποχρεωτική ένταξη (${results.mandatoryGrades.join(", ")} τάξη): όλοι οι μαθητές πήραν όμιλο κάθε ημέρα.`);
  const perStudent = new Map();
  for (const g of gaps) (perStudent.get(g.am) ?? perStudent.set(g.am, []).get(g.am)).push(g);
  const rows = [...perStudent].map(([am, list]) => {
    const rejected = list.filter((g) => g.reason === "all_rejected");
    const reason = !state.submissions[am]
      ? "δεν υπέβαλε δήλωση"
      : [rejected.length ? `δεν χώρεσε: ${rejected.map((g) => DAY_LABELS[g.day]).join(", ")}` : "",
        list.length > rejected.length ? `χωρίς προτιμήσεις: ${list.filter((g) => g.reason !== "all_rejected").map((g) => DAY_LABELS[g.day]).join(", ")}` : ""].filter(Boolean).join(" · ");
    return { am, s: byAm.get(am), days: list.map((g) => DAY_LABELS[g.day]).join(", "), reason, fixable: rejected.length > 0 };
  }).sort((a, b) => Number(b.fixable) - Number(a.fixable) || (a.s?.surname ?? "").localeCompare(b.s?.surname ?? "", "el"));
  const fixable = rows.filter((r) => r.fixable).length;
  return el("div.msg.err", { role: "alert" },
    el("strong", {}, `Υποχρεωτική ένταξη (${results.mandatoryGrades.join(", ")} τάξη): ${rows.length} μαθητές χωρίς όμιλο κάποια ημέρα`),
    el("p.small", { style: "margin:4px 0" }, `${fixable} δήλωσαν αλλά δεν χώρεσαν (αυξήστε τη χωρητικότητα κάποιου ομίλου της λίστας τους και ξανατρέξτε την κατανομή)· ${rows.length - fixable} δεν έχουν προτιμήσεις (χρειάζεται δήλωση).`),
    el("details", { open: rows.length <= 20 }, el("summary", {}, "Λίστα μαθητών"),
      el("div.table-wrap", {}, el("table", {},
        el("thead", {}, el("tr", {}, el("th.num", {}, "ΑΜ"), el("th", {}, "Μαθητής"), el("th", {}, "Τάξη"), el("th", {}, "Αιτία"))),
        el("tbody", {}, rows.map((r) => el("tr", {},
          el("td.num", {}, r.am), el("td", {}, who(r.s)), el("td", {}, r.s?.grade ?? ""), el("td", {}, r.reason))))))));
}

// Students with a submission but nothing to rank on a given day
function noPrefsByDay(results, byAm, who) {
  const blocks = DAYS.map((d) => {
    const ams = Object.entries(results.gaps).filter(([am, g]) => g[d] === "no_preferences" && state.submissions[am]).map(([am]) => am);
    if (!ams.length) return null;
    return el("details", {}, el("summary", {}, `${DAY_LABELS[d]}: ${ams.length} μαθητές με δήλωση αλλά χωρίς προτιμήσεις αυτή την ημέρα`),
      el("ul.small", {}, ams.map((am) => el("li", {}, `${who(byAm.get(am))} · ${byAm.get(am)?.grade ?? ""} · ΑΜ ${am}`))));
  }).filter(Boolean);
  return blocks.length ? el("div", {}, ...blocks) : null;
}

// Look up one student's report
function reportSearch() {
  const out = el("div");
  const input = el("input", { type: "search", placeholder: "ΑΜ ή επώνυμο", "aria-label": "Αναζήτηση μαθητή για αναφορά", list: "report-students" });
  const list = el("datalist", { id: "report-students" }, state.students.map((s) => el("option", { value: s.am }, `${s.surname} ${s.name} · ${s.grade}`)));
  const go = el("button", { type: "button" }, "Εμφάνιση αναφοράς");
  const run = () => busy(go, out, async () => {
    const q = input.value.trim().toUpperCase();
    const s = state.students.find((x) => x.am === q) ?? state.students.find((x) => x.surname.toUpperCase().startsWith(q));
    if (!s) throw new Error("Δεν βρέθηκε μαθητής.");
    const report = await api("GET", `/api/admin/report/${encodeURIComponent(s.am)}`);
    show(out, el("div.card", {}, el("strong", {}, `${s.surname} ${s.name} · ${s.grade} · ΑΜ ${s.am}`), studentReportView(report, { title: null })));
  });
  go.addEventListener("click", run);
  input.addEventListener("keydown", (e) => { if (e.key === "Enter") run(); });
  return el("div", {}, el("p.small.muted", {}, "Βήμα προς βήμα πώς προέκυψε η θέση του μαθητή κάθε ημέρα (αιτήσεις, απορρίψεις, αποδοχές). Την ίδια βλέπει ο γονέας μετά την ανακοίνωση."),
    el("div.actions", {}, input, list, go), out);
}

// ---------- Statistics ----------

const CHOICE_SERIES = [
  { key: "1", label: "1η επιλογή", color: "--viz-1" },
  { key: "2", label: "2η", color: "--viz-2" },
  { key: "3", label: "3η", color: "--viz-3" },
  { key: "4+", label: "4η ή χαμηλότερη", color: "--viz-4" },
  { key: "none", label: "Δεν χώρεσε", color: "--viz-none" },
];
const sumOf = (o) => Object.values(o).reduce((a, n) => a + n, 0);

function statsView(results) {
  const st = results.stats;
  if (!st) return el("p.muted", {}, "Ξανατρέξτε την κατανομή για να υπολογιστούν τα στατιστικά.");
  const asked = sumOf(st.overall);
  const dayRows = DAYS.filter((d) => sumOf(st.byDay[d])).map((d) => ({ label: DAY_LABELS[d], values: st.byDay[d] }));
  const gradeRows = Object.entries(st.byGrade).filter(([, v]) => sumOf(v)).map(([g, v]) => ({ label: `${g} τάξη`, values: v }));
  const clubLabel = (c) => `${c.name} (${DAY_LABELS[c.day]}${c.days.length > 1 ? "+" : ""})`;
  const byPressure = [...st.demand].sort((a, b) => (b.pressure ?? 0) - (a.pressure ?? 0));
  const byFill = [...st.demand].sort((a, b) => (b.fill ?? 0) - (a.fill ?? 0));
  const section = (title, intro, ...body) => el("section", {}, el("h3", {}, title), intro ? el("p.small.muted", { style: "margin-top:0" }, intro) : null, ...body);
  return el("div", {},
    el("p.small.muted", {}, "Μετρά κάθε ημέρα κάθε μαθητή που δήλωσε ομίλους εκείνη την ημέρα («ημέρα-μαθητή»). Οι ημέρες όμιλου πολλών ημερών μετρούν με τη θέση που είχε ο όμιλος την πρώτη του ημέρα. Οι ημέρες χωρίς δήλωση δεν μετρούν. «+» δίπλα στην ημέρα: όμιλος πολλών ημερών (μετρά στην πρώτη του). Περάστε τον δείκτη πάνω από μια μπάρα για τους αριθμούς· κάτω από κάθε διάγραμμα, «Πίνακας»."),
    el("div.viz-tiles", {},
      statTile("Πήραν την 1η επιλογή", st.firstChoiceShare === null ? "—" : pct(st.firstChoiceShare), `${fmt(st.overall["1"])} από ${fmt(asked)} ημέρες-μαθητή`),
      statTile("Μία από τις 3 πρώτες", st.topThreeShare === null ? "—" : pct(st.topThreeShare)),
      statTile("Δεν χώρεσαν", fmt(st.overall.none), "ημέρες-μαθητή χωρίς όμιλο, ενώ είχαν δηλώσει"),
      statTile("Δηλώσεις", fmt(results.submitted), `από ${fmt(state.students.length)} μαθητές`)),
    section("Ποια επιλογή πήραν, ανά ημέρα", null, stackedBars(dayRows, CHOICE_SERIES, { unit: "ημέρες-μαθητή" })),
    section("Ποια επιλογή πήραν, ανά τάξη", "Όλες οι ημέρες μαζί.", stackedBars(gradeRows, CHOICE_SERIES, { unit: "ημέρες-μαθητή" })),
    section("Ιστόγραμμα: σε ποια θέση της λίστας τους ήταν ο όμιλος που πήραν", null,
      columns(st.histogram.map((h) => ({ label: `${h.rank}η`, value: h.count })), { unit: "ημέρες-μαθητή",
        caption: st.overall.none ? `Επιπλέον ${fmt(st.overall.none)} ημέρες-μαθητή χωρίς όμιλο (δεν χώρεσαν).` : "" })),
    section("Ζήτηση: πόσοι τον έβαλαν 1η επιλογή, σε σχέση με τις θέσεις", "Πάνω οι όμιλοι με τη μεγαλύτερη πίεση. Όπου η μπάρα ξεπερνά τη γραμμή των θέσεων, τον ήθελαν πρώτο περισσότεροι απ' όσους χωρούν — υποψήφιοι για περισσότερες θέσεις ή δεύτερο τμήμα.",
      barsWithTarget(byPressure.map((c) => ({ label: clubLabel(c), value: c.first, target: c.capacity, note: `τον δήλωσαν ${c.applicants}, μπήκαν ${c.placed}${c.missed === null ? "" : `, δεν χώρεσαν ${c.missed}`}` })),
        { valueLabel: "1η επιλογή", targetLabel: "θέσεις" })),
    section("Πληρότητα: πόσοι μπήκαν, σε σχέση με τις θέσεις", null,
      barsWithTarget(byFill.map((c) => ({ label: clubLabel(c), value: c.placed, target: c.capacity, note: c.fill === null ? "" : `πληρότητα ${pct(c.fill)}` })),
        { valueLabel: "μπήκαν", targetLabel: "θέσεις" })),
    section("Ανά όμιλο", "«Δεν χώρεσαν»: έκαναν αίτηση στον όμιλο και δεν χώρεσαν (πήγαν σε επόμενη επιλογή τους ή έμειναν χωρίς όμιλο).",
      el("div.table-wrap", {}, el("table", {},
        el("thead", {}, el("tr", {}, ["Όμιλος", "Θέσεις", "1η επιλογή", "Τον δήλωσαν", "Μπήκαν", "Δεν χώρεσαν", "Πληρότητα"].map((h, i) => el(i ? "th.num" : "th", {}, h)))),
        el("tbody", {}, byPressure.map((c) => el("tr", {}, el("td", {}, clubLabel(c)),
          [c.capacity, c.first, c.applicants, c.placed, c.missed].map((v) => el("td.num", {}, v === null ? "—" : fmt(v))), el("td.num", {}, c.fill === null ? "" : pct(c.fill)))))))),
    section("Λίστες εκπαιδευτικών", "Τι έγιναν οι μαθητές που επέλεξαν οι εκπαιδευτικοί.",
      st.teacherLists.length
        ? el("div.table-wrap", {}, el("table", {},
          el("thead", {}, el("tr", {}, ["Όμιλος", "Στη λίστα", "Δήλωσαν τον όμιλο", "Μπήκαν", "Πήραν όμιλο που ήθελαν περισσότερο", "Δεν τον δήλωσαν"].map((h, i) => el(i ? "th.num" : "th", {}, h)))),
          el("tbody", {}, st.teacherLists.map((t) => el("tr", {}, el("td", {}, t.name), [t.listed, t.ranked, t.placed, t.elsewhere, t.notRanked].map((v) => el("td.num", {}, fmt(v))))))))
        : el("p.muted", {}, "Δεν υπάρχουν λίστες εκπαιδευτικών.")));
}

/** The statistics as rows for the results workbook. */
function statsSheet(results) {
  const st = results.stats;
  if (!st) return [["Ξανατρέξτε την κατανομή για τα στατιστικά."]];
  const head = ["", ...CHOICE_SERIES.map((s) => s.label), "Σύνολο"];
  const line = (label, v) => [label, ...CHOICE_SERIES.map((s) => v[s.key]), sumOf(v)];
  return [
    ["Ποια επιλογή πήραν (ημέρες-μαθητή)"], head,
    ...DAYS.filter((d) => sumOf(st.byDay[d])).map((d) => line(DAY_LABELS[d], st.byDay[d])),
    ...Object.entries(st.byGrade).map(([g, v]) => line(`${g} τάξη`, v)),
    line("Σύνολο", st.overall),
    [],
    ["Ιστόγραμμα: θέση του ομίλου που πήραν"], ["Θέση", "Ημέρες-μαθητή"], ...st.histogram.map((h) => [h.rank, h.count]),
    [],
    ["Ανά όμιλο (πρώτη ημέρα του)"], ["Κωδικός", "Όμιλος", "Ημέρα", "Θέσεις", "1η επιλογή", "Τον δήλωσαν", "Μπήκαν", "Δεν χώρεσαν", "Πληρότητα %"],
    ...st.demand.map((c) => [c.code, c.name, DAY_LABELS[c.day], c.capacity, c.first, c.applicants, c.placed, c.missed, c.fill === null ? "" : Math.round(c.fill * 100)]),
    [],
    ["Λίστες εκπαιδευτικών"], ["Κωδικός", "Όμιλος", "Στη λίστα", "Δήλωσαν τον όμιλο", "Μπήκαν", "Πήραν όμιλο που ήθελαν περισσότερο", "Δεν τον δήλωσαν"],
    ...st.teacherLists.map((t) => [t.code, t.name, t.listed, t.ranked, t.placed, t.elsewhere, t.notRanked]),
  ];
}

// ---------- The week per student, and the clubs' rosters ----------

/** Save rows as a one-sheet .xlsx in the browser. */
async function saveSheet(fileName, sheetName, rows, cols) {
  const XLSX = await loadXlsx();
  const wb = XLSX.utils.book_new();
  const ws = XLSX.utils.aoa_to_sheet(rows);
  if (cols) ws["!cols"] = cols.map((wch) => ({ wch }));
  XLSX.utils.book_append_sheet(wb, ws, sheetName);
  // Through a link (as for the .zip downloads)
  const url = URL.createObjectURL(new Blob([XLSX.write(wb, { type: "array", bookType: "xlsx" })], { type: "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" }));
  el("a", { href: url, download: fileName }).click();
  setTimeout(() => URL.revokeObjectURL(url), 10000);
}

const WEEK_COLS = [8, 5, 20, 16, 14, 14, 22, 22, 22, 22, 22];

function weekView(results) {
  const out = el("div");
  const all = weekRows(state.students, results, state.clubs);
  const grades = [...new Set(state.students.map((s) => s.grade))].sort();
  const search = el("input", { type: "search", placeholder: "Αναζήτηση (επώνυμο, όνομα, ΑΜ, όμιλος)", "aria-label": "Αναζήτηση στην κατανομή" });
  const grade = el("select", { "aria-label": "Τάξη" }, el("option", { value: "" }, "Όλες οι τάξεις"), grades.map((g) => el("option", { value: g }, `${g} τάξη`)));
  const count = el("p.small.muted");
  const body = el("tbody");
  const visible = () => {
    const q = search.value.trim().toUpperCase();
    return all.filter((r) => (!grade.value || r[1] === grade.value) && (!q || r.join(" ").toUpperCase().includes(q)));
  };
  const draw = () => {
    const rows = visible();
    count.textContent = `${rows.length} μαθητές`;
    body.replaceChildren(...rows.map((r) => el("tr", {}, r.map((v, i) => el(i === 0 ? "td.num" : "td", {}, String(v).startsWith("—") ? el("span.muted", {}, v) : v)))));
  };
  search.addEventListener("input", draw);
  grade.addEventListener("change", draw);
  draw();
  const save = el("button", { type: "button" }, "Λήψη (.xlsx)");
  save.addEventListener("click", () => busy(save, out, () => saveSheet(grade.value ? `katanomi_ana_mathiti_${grade.value}.xlsx` : "katanomi_ana_mathiti.xlsx", "Ανά μαθητή", [WEEK_HEADER, ...visible()], WEEK_COLS)));
  return el("div", {},
    el("p.small.muted", {}, "Ο όμιλος κάθε μαθητή για κάθε ημέρα. Ταξινόμηση: τάξη, επώνυμο, όνομα, πατρώνυμο, μητρώνυμο. Η λήψη περιέχει όσους εμφανίζονται με τα τρέχοντα φίλτρα."),
    el("div.grid-2", {}, search, grade),
    el("div.actions", {}, save, count), out,
    el("div.table-wrap", {}, el("table", {}, el("thead", {}, el("tr", {}, WEEK_HEADER.map((h, i) => el(i === 0 ? "th.num" : "th", {}, h)))), body)));
}

const ROSTER_COLS = [6, 8, 22, 18, 16, 6];

function rosterFor(club, results) {
  const teachers = state.teachers.filter((t) => t.clubs.includes(club.code)).map((t) => `${t.name} ${t.surname}`);
  return rosterRows(club, clubMembers(club, state.students, results), teachers);
}

function rostersView(results) {
  const out = el("div");
  const clubs = [...state.clubs].sort((a, b) => a.code - b.code);
  const picker = el("select", { "aria-label": "Όμιλος" }, clubs.map((c) => el("option", { value: String(c.code) },
    `${c.code} · ${c.name} (${c.days.map((d) => DAY_LABELS[d]).join(" + ")}) — ${clubMembers(c, state.students, results).length} μαθητές`)));
  const holder = el("div");
  const current = () => clubs.find((c) => String(c.code) === picker.value);
  const draw = () => {
    const club = current();
    if (!club) return holder.replaceChildren(el("p.muted", {}, "Δεν υπάρχουν όμιλοι."));
    const rows = rosterFor(club, results);
    const head = rows.findIndex((r) => r[0] === "Α/Α");
    holder.replaceChildren(
      el("p", {}, rows.slice(0, head - 1).map(([k, v]) => el("span", { style: "margin-right:16px" }, el("strong", {}, `${k}: `), v))),
      rows.length > head + 1
        ? el("div.table-wrap", {}, el("table", {},
          el("thead", {}, el("tr", {}, rows[head].map((h, i) => el(i < 2 ? "th.num" : "th", {}, h)))),
          el("tbody", {}, rows.slice(head + 1).map((r) => el("tr", {}, r.map((v, i) => el(i < 2 ? "td.num" : "td", {}, v)))))))
        : el("p.muted", {}, "Κανένας μαθητής σε αυτόν τον όμιλο."));
  };
  picker.addEventListener("change", draw);
  draw();
  const one = el("button", { type: "button", disabled: !clubs.length }, "Λήψη (.xlsx)");
  one.addEventListener("click", () => busy(one, out, async () => {
    const club = current();
    // A Latin name for a direct download (some browsers drop Greek ones); the .zip keeps the club names
    await saveSheet(`parousiologio_${club.code}.xlsx`, ROSTER_SHEET, rosterFor(club, results), ROSTER_COLS);
  }));
  const all = el("button", { type: "button", disabled: !clubs.length }, "Όλα τα παρουσιολόγια (.zip)");
  all.addEventListener("click", () => busy(all, out, async () => {
    const XLSX = await loadXlsx();
    const files = {};
    for (const c of clubs) {
      const wb = XLSX.utils.book_new();
      const ws = XLSX.utils.aoa_to_sheet(rosterFor(c, results));
      ws["!cols"] = ROSTER_COLS.map((wch) => ({ wch }));
      XLSX.utils.book_append_sheet(wb, ws, ROSTER_SHEET);
      files[rosterFileName(c)] = new Uint8Array(XLSX.write(wb, { type: "array", bookType: "xlsx" }));
    }
    const url = URL.createObjectURL(new Blob([makeZip(files)], { type: "application/zip" }));
    el("a", { href: url, download: "parousiologia.zip" }).click();
    setTimeout(() => URL.revokeObjectURL(url), 10000);
  }));
  return el("div", {},
    el("p.small.muted", {}, "Τα μέλη κάθε ομίλου, αλφαβητικά, για να τα δώσετε στον εκπαιδευτικό. Όμιλος πολλών ημερών: ένα παρουσιολόγιο, αφού τα μέλη του είναι ίδια όλες τις ημέρες."),
    el("label", {}, "Όμιλος", picker),
    el("div.actions", {}, one, all), out,
    holder);
}

// Results workbook, like R's week_results.xlsx
async function resultsWorkbook(results) {
  const XLSX = await loadXlsx();
  const byAm = new Map(state.students.map((s) => [s.am, s]));
  const nameOf = new Map(state.clubs.map((c) => [String(c.code), c.name]));
  const REASON = { all_rejected: "δεν χώρεσε", no_preferences: "χωρίς προτιμήσεις", not_offered: "" };
  const cell = (am, d) => {
    const code = results.byStudent[am]?.[d];
    return code ? nameOf.get(code) ?? code : REASON[results.gaps[am]?.[d]] ? `— ${REASON[results.gaps[am][d]]}` : "";
  };
  const sorted = [...state.students].sort((a, b) => a.grade.localeCompare(b.grade) || a.surname.localeCompare(b.surname, "el") || a.name.localeCompare(b.name, "el"));
  const perStudent = [WEEK_HEADER, ...weekRows(state.students, results, state.clubs)];
  const perClub = [["Κωδικός", "Όμιλος", "Ημέρα", "ΑΜ", "Επώνυμο", "Όνομα", "Τάξη"]];
  for (const c of state.clubs) for (const d of c.days) {
    for (const s of sorted.filter((x) => results.byStudent[x.am]?.[d] === String(c.code))) perClub.push([c.code, c.name, DAY_LABELS[d], s.am, s.surname, s.name, s.grade]);
  }
  const fill = [["Κωδικός", "Όμιλος", "Ημέρα", "Χωρητικότητα", "Τοποθετήθηκαν"], ...state.clubs.flatMap((c) => c.days.map((d) => [c.code, c.name, DAY_LABELS[d], c.capacity, results.enrolled[d][String(c.code)] ?? 0]))];
  const gaps = [["ΑΜ", "Επώνυμο", "Όνομα", "Τάξη", "Ημέρα", "Αιτία"]];
  for (const d of DAYS) for (const s of sorted) {
    const g = results.gaps[s.am]?.[d];
    if (g && g !== "not_offered") gaps.push([s.am, s.surname, s.name, s.grade, DAY_LABELS[d], REASON[g]]);
  }
  const info = [["Στοιχείο", "Τιμή"], ["Seed κλήρωσης", results.seed], ["Εκτέλεση", formatDateTime(results.at)], ["Μαθητές", state.students.length], ["Δηλώσεις", results.submitted]];
  const wb = XLSX.utils.book_new();
  for (const [name, rows] of [["Ανά μαθητή", perStudent], ["Ανά όμιλο", perClub], ["Πληρότητα", fill], ["Χωρίς όμιλο", gaps], ["Στατιστικά", statsSheet(results)], ["Πληροφορίες", info]]) {
    XLSX.utils.book_append_sheet(wb, XLSX.utils.aoa_to_sheet(rows), name);
  }
  XLSX.writeFile(wb, "katanomi_omilon.xlsx");
}

// ---------- History (platform events) ----------

const EVENT_LABELS = {
  login: "Σύνδεση διαχείρισης", students_uploaded: "Ανέβασμα καταλόγου μαθητών", clubs_uploaded: "Ανέβασμα ομίλων",
  import_legacy: "Εισαγωγή περσινών δηλώσεων", settings: "Αλλαγή ρυθμίσεων", phase: "Αλλαγή φάσης", login_exception: "Εξαίρεση σύνδεσης",
  teacher_link: "Σύνδεσμος εκπαιδευτικού", teacher_login: "Σύνδεση εκπαιδευτικού", teacher_list: "Λίστα εκπαιδευτικού",
  parent_login: "Σύνδεση γονέα", submission: "Δήλωση γονέα", allocated: "Κατανομή", reset: "Επαναφορά πλατφόρμας", mail_failed: "Αποτυχία email",
};
const PHASE_NAMES = Object.fromEntries(PHASES);

function history() {
  const box = el("div", {}, el("p.muted", {}, "Φόρτωση…"));
  const filter = el("select", { "aria-label": "Φίλτρο ιστορικού" }, el("option", { value: "" }, "Όλα"), el("option", { value: "admin" }, "Διαχείριση"),
    el("option", { value: "teacher" }, "Εκπαιδευτικοί"), el("option", { value: "parent" }, "Γονείς"));
  let events = [];
  const detail = (e) => {
    const { at, who, what, ...rest } = e;
    if (what === "phase") return `${PHASE_NAMES[rest.from] ?? rest.from} → ${PHASE_NAMES[rest.to] ?? rest.to}`;
    return Object.entries(rest).map(([k, v]) => `${k}: ${Array.isArray(v) ? v.join(", ") : v}`).join(" · ");
  };
  const draw = () => {
    const f = filter.value;
    const shown = [...events].reverse().filter((e) => !f || (f === "admin" ? e.who === "admin" : f === "parent" ? String(e.who).startsWith("parent:") : e.who.includes("@") || e.what.startsWith("teacher")));
    box.replaceChildren(el("div.table-wrap", {}, el("table", {},
      el("thead", {}, el("tr", {}, el("th", {}, "Πότε"), el("th", {}, "Ποιος"), el("th", {}, "Ενέργεια"), el("th", {}, "Λεπτομέρειες"))),
      el("tbody", {}, shown.map((e) => el("tr", {},
        el("td.small", {}, formatDateTime(e.at)), el("td.small", {}, String(e.who).replace(/^parent:/, "γονέας ΑΜ ")),
        el("td", {}, EVENT_LABELS[e.what] ?? e.what), el("td.small.muted", {}, detail(e))))))));
  };
  filter.addEventListener("change", draw);
  api("GET", "/api/admin/events").then((r) => { events = r.events; draw(); }).catch((err) => box.replaceChildren(message("err", err.message)));
  return el("section.card", {},
    el("h2", { style: "margin-top:0" }, "Ιστορικό ενεργειών"),
    el("p.small.muted", {}, "Όλες οι ενέργειες στην πλατφόρμα (τελευταίες 1000): ανεβάσματα αρχείων, αλλαγές φάσης, δηλώσεις, κατανομές. Το βήμα-βήμα της κατανομής είναι στο «Audit log κατανομής» (καρτέλα «Κατανομή»)."),
    el("div.actions", {}, filter), box);
}

// ---------- Outbox (local development) ----------

function outbox() {
  const box = el("div", {}, el("p.muted", {}, "Φόρτωση…"));
  const out = el("div");
  const to = el("input", { id: "testto", type: "email", placeholder: "π.χ. το δικό σας email", autocomplete: "email" });
  const send = el("button", { type: "button" }, "Αποστολή δοκιμαστικού email");
  send.addEventListener("click", () => busy(send, out, async () => {
    const r = await api("POST", "/api/admin/test-email", { to: to.value });
    show(out, r.status === "sent"
      ? message("ok", `Στάλθηκε στο ${to.value}. Ελέγξτε ότι έφτασε — και ότι δεν μπήκε στα ανεπιθύμητα (spam).`)
      : message("warn", "Δεν έχει ρυθμιστεί υπηρεσία email: το μήνυμα καταγράφηκε μόνο εδώ."));
    load();
  }));
  const STATUS = {
    sent: ["ok", "στάλθηκε"],
    failed: ["warn", "απέτυχε"],
    not_sent: ["", "δεν στάλθηκε"],
  };
  const load = () => api("GET", "/api/admin/outbox").then(({ outbox: mails, mailConfigured }) => box.replaceChildren(
    mailConfigured
      ? message("info", "Τα email στέλνονται κανονικά. Εδώ φαίνεται τι στάλθηκε· οι σύνδεσμοι εισόδου των εκπαιδευτικών είναι κρυφοί.")
      : message("warn", "Δεν έχει ρυθμιστεί υπηρεσία email: τα μηνύματα ΔΕΝ στέλνονται, εμφανίζονται μόνο εδώ. Τους συνδέσμους εισόδου των εκπαιδευτικών τους προωθείτε εσείς."),
    mails.length === 0 ? el("p.muted", {}, "Δεν υπάρχουν μηνύματα ακόμα.") : null,
    ...[...mails].reverse().map((m) => {
      const [kind, label] = STATUS[m.status] ?? ["", ""];
      return el("section.card", {},
        el("p.small.muted", {}, `${formatDateTime(m.at)} → ${m.to} `, label ? el(`span.badge${kind ? `.${kind}` : ""}`, {}, label) : null),
        m.error ? el("p.small", { style: "color:var(--err)" }, m.error) : null,
        el("strong", {}, m.subject),
        el("pre", { style: "white-space:pre-wrap;font:inherit" }, ...linkify(m.text)));
    })));
  load();
  return el("div", {},
    el("section.card", {},
      el("h2", { style: "margin-top:0" }, "Δοκιμή αποστολής"),
      el("label", { for: "testto" }, "Email παραλήπτη", to),
      out,
      el("div.actions", {}, send)),
    box);
}

function linkify(text) {
  return text.split(/(https?:\/\/\S+)/).map((part) => (/^https?:\/\//.test(part) ? el("a", { href: part }, part) : part));
}

start();
