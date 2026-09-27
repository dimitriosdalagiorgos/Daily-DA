import { $, busy, createApi, el, formatDateTime, message, show } from "./ui.js";
import { readWorkbook } from "/lib/import/workbook.js";
import { headerKey } from "/lib/import/table.js";

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
  const tabs = [["overview", "Πορεία & ρυθμίσεις"], ["data", "Αρχεία"], ["students", "Μαθητές"], ["clubs", "Όμιλοι"], ["allocation", "Κατανομή"], ...(devOutbox ? [["outbox", "Εξερχόμενα email"]] : [])];
  const nav = el("nav.tabs", { role: "tablist" }, tabs.map(([id, label]) =>
    el("button", { type: "button", role: "tab", "aria-selected": String(tab === id), onclick: () => { tab = id; render(); } }, label)));
  const views = { overview, data, students, clubs, allocation, outbox };
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
    parents: "Άνοιγμα δηλώσεων: οι όμιλοι και οι λίστες των εκπαιδευτικών κλειδώνουν. Συνέχεια;",
    closed: "Κλείσιμο δηλώσεων: οι γονείς δεν θα μπορούν πια να αλλάξουν δηλώσεις. Συνέχεια;",
    published: "Ανακοίνωση: οι γονείς θα βλέπουν το αποτέλεσμα. Συνέχεια;",
  };
  return el("section.card", {},
    el("ol.steps", {}, PHASES.map(([p, label], i) => el(`li${i < idx ? ".done" : i === idx ? ".current" : ""}`, {}, label))),
    el("p", {}, PHASE_HELP[phase]),
    out,
    el("div.actions", {},
      next && next[0] !== "allocated" ? el("button.primary", { type: "button", onclick: go(next[0], confirmNext[next[0]]) }, `Επόμενη φάση: ${next[1]}`) : null,
      next && next[0] === "allocated" ? el("span.muted.small", {}, "Επόμενο βήμα: καρτέλα «Κατανομή».") : null,
      prev ? el("button", { type: "button", onclick: go(prev[0], `Επιστροφή στη φάση «${prev[1]}»;`) }, `Επιστροφή: ${prev[1]}`) : null));
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
    parentPassword: el("input", { id: "ppw", type: "text", autocomplete: "off", placeholder: s.parentPasswordSet ? "Έχει οριστεί — γράψτε νέο για αλλαγή" : "Τουλάχιστον 6 χαρακτήρες" }),
  };
  const save = el("button.primary", { type: "button" }, "Αποθήκευση ρυθμίσεων");
  save.addEventListener("click", () => busy(save, out, async () => {
    const body = { schoolName: f.schoolName.value, contact: f.contact.value, deadline: f.deadline.value ? new Date(f.deadline.value).toISOString() : null };
    if (f.parentPassword.value) body.parentPassword = f.parentPassword.value;
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
    state.readiness.length ? el("section.card", {}, el("h2", { style: "margin-top:0" }, "Έλεγχοι"), message("warn", "Προσοχή:", state.readiness.map((p) => p.message))) : null,
    el("section.card", {},
      el("h2", { style: "margin-top:0" }, "Ρυθμίσεις"),
      el("div.grid-2", {},
        el("label", { for: "school" }, "Όνομα σχολείου", f.schoolName),
        el("label", { for: "contact" }, "Επικοινωνία για γονείς", el("span.hint", {}, "Εμφανίζεται όταν αποτυγχάνει η σύνδεση."), f.contact),
        el("label", { for: "deadline" }, "Προθεσμία δηλώσεων", f.deadline),
        el("label", { for: "ppw" }, "Κοινός κωδικός γονέων", el("span.hint", {}, "Ίδιος για όλους· ανακοινώνεται από το σχολείο."), f.parentPassword)),
      out,
      el("div.actions", {}, save)),
    dangerZone());
}

function dangerZone() {
  const out = el("div");
  const word = el("input", { id: "resetword", type: "text", autocomplete: "off", placeholder: "ΔΙΑΓΡΑΦΗ" });
  const keep = el("input", { type: "checkbox", checked: true });
  const go = el("button.danger", { type: "button" }, "Διαγραφή όλων των δεδομένων");
  go.addEventListener("click", () => {
    if (!confirm("Θα διαγραφούν οριστικά μαθητές, όμιλοι, εκπαιδευτικοί, δηλώσεις και αποτελέσματα. Συνέχεια;")) return;
    busy(go, out, async () => {
      await api("POST", "/api/admin/reset", { confirm: word.value.trim(), keepSchoolInfo: keep.checked });
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
        const report = await handler(sheets);
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
    upload("Κατάλογος μαθητών (myschool)", locked ? "Οι δηλώσεις είναι ανοιχτές: από νέο αρχείο προστίθενται μόνο οι νέοι μαθητές." : "Το «Κατάλογος Μαθητών» όπως εξάγεται από το myschool (.xls).", async (sheets) => {
      const rows = Object.values(sheets)[0] ?? [];
      return (await api("PUT", "/api/admin/students", { rows })).report;
    }, false),
    upload("Όμιλοι και εκπαιδευτικοί", locked ? "Κλειδωμένο: οι δηλώσεις έχουν ανοίξει." : "Το πρότυπο της πλατφόρμας, συμπληρωμένο.", async (sheets) => {
      const clubs = find(sheets, "Όμιλοι");
      const teachers = find(sheets, "Εκπαιδευτικοί");
      if (!clubs || !teachers) throw new Error("Το αρχείο δεν έχει τα φύλλα «Όμιλοι» και «Εκπαιδευτικοί». Χρησιμοποιήστε το πρότυπο.");
      return (await api("PUT", "/api/admin/clubs", { clubs, teachers })).report;
    }, locked),
    el("p", {}, el("a.button", { href: "/templates/omiloi_protypo.xlsx", download: "omiloi_protypo.xlsx" }, "Λήψη κενού προτύπου ομίλων")),
    el("p.small.muted", {}, "Τα αρχεία διαβάζονται στον υπολογιστή σας· στον διακομιστή στέλνονται μόνο τα στοιχεία των πινάκων και ελέγχονται ξανά."));
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
  return el("div", {}, clubsTable(), teacherLinks());
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
      el("td.small", {}, state.clubs.filter((c) => t.clubs.includes(c.code)).map((c) => c.name).join(", ")), cell);
  });
  return el("section.card", {},
    el("h2", { style: "margin-top:0" }, "Σύνδεσμοι εισόδου εκπαιδευτικών"),
    el("p.small.muted", {}, "Χωρίς υπηρεσία email, στείλτε εσείς σε κάθε εκπαιδευτικό τον προσωπικό του σύνδεσμο (π.χ. από το email του σχολείου). Ισχύει μία εβδομάδα· αν λήξει, φτιάξτε νέο."),
    out,
    rows.length ? el("div.table-wrap", {}, el("table", {},
      el("thead", {}, el("tr", {}, el("th", {}, "Εκπαιδευτικός"), el("th", {}, "Όμιλοι"), el("th", {}, ""))),
      el("tbody", {}, rows))) : el("p.muted", {}, "Δεν υπάρχουν εκπαιδευτικοί ακόμα (ανεβάστε το αρχείο ομίλων)."));
}

function clubsTable() {
  const editable = ["setup", "teachers"].includes(state.settings.phase);
  const byAm = new Map(state.students.map((s) => [s.am, s]));
  return el("section.card", {},
    el("div.table-wrap", {}, el("table", {},
      el("thead", {}, el("tr", {}, el("th.num", {}, "Κωδ."), el("th", {}, "Όμιλος"), el("th", {}, "Ημέρες"), el("th", {}, "Τάξεις"), el("th.num", {}, "Θέσεις"), el("th", {}, "Εκπαιδευτικοί"), el("th", {}, "Λίστα εκπαιδευτικού"))),
      el("tbody", {}, state.clubs.map((c) => {
        const list = state.teacherLists[c.code];
        const teachers = state.teachers.filter((t) => t.clubs.includes(c.code));
        const cell = el("td");
        const summary = () => cell.replaceChildren(
          list?.ams?.length ? el("span", {}, `${list.ams.length} μαθητές`, el("br"), el("span.small.muted", {}, list.ams.map((am) => byAm.get(am)?.surname ?? am).join(", "))) : el("span.muted", {}, "—"),
          editable ? el("div", {}, el("button.small", { type: "button", onclick: () => editList(c, list, cell) }, "Διόρθωση")) : null);
        summary();
        return el("tr", {},
          el("td.num", {}, c.code), el("td", {}, c.name), el("td", {}, c.days.map((d) => DAY_LABELS[d]).join(" + ")),
          el("td", {}, c.grades.join(", ")), el("td.num", {}, c.capacity),
          el("td.small", {}, teachers.map((t) => el("div", {}, `${t.name} ${t.surname}`, el("br"), el("span.muted", {}, t.email)))),
          cell);
      })))));
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

  if (state.results) api("GET", "/api/admin/results").then(({ results }) => resultsBox.replaceChildren(resultsView(results))).catch(() => {});
  resultsBox.id = "results-box";

  return el("div", {},
    el("section.card", {},
      el("h2", { style: "margin-top:0" }, "Κλήρωση και κατανομή"),
      ["closed", "allocated"].includes(phase) ? null : message("info", "Η κατανομή γίνεται αφού κλείσουν οι δηλώσεις (φάση «Κλειστές δηλώσεις»)."),
      el("p.small", {}, "Το seed καθορίζει την κλήρωση: ένας σταθερός αριθμός για κάθε μαθητή, για όλη την εβδομάδα. Ανακοινώστε το seed· με αυτό οποιοσδήποτε ξαναβγάζει την ίδια κλήρωση και την ίδια κατανομή, και offline στο R (φάκελος R/ του αποθετηρίου)."),
      el("label", { for: "seed" }, "Seed κλήρωσης", seed),
      out,
      el("div.actions", {}, run, randomSeed, rZip, csv)),
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
  return el("section.card", {},
    el("h2", { style: "margin-top:0" }, "Αποτελέσματα"),
    el("p", {}, `Seed «${results.seed}» · ${formatDateTime(results.at)} · δηλώσεις: ${results.submitted} · τοποθετήσεις (μαθητής × ημέρα): ${placed}`),
    el("h3", {}, "Πληρότητα ανά ημέρα"),
    el("div.table-wrap", {}, el("table", {},
      el("thead", {}, el("tr", {}, el("th", {}, "Ημέρα"), el("th", {}, "Όμιλος"), el("th.num", {}, "Τοποθετήθηκαν"), el("th.num", {}, "Θέσεις"))),
      el("tbody", {}, clubsByDay.flatMap(([d, cs]) => cs.map((c) =>
        el("tr", {}, el("td", {}, DAY_LABELS[d]), el("td", {}, c.name), el("td.num", {}, results.enrolled[d][String(c.code)] ?? 0), el("td.num", {}, c.capacity))))))),
    el("h3", {}, `Δήλωσαν αλλά δεν χώρεσαν (${rejected.length})`),
    rejected.length
      ? el("div.table-wrap", {}, el("table", {},
        el("thead", {}, el("tr", {}, el("th", {}, "Ημέρα"), el("th.num", {}, "ΑΜ"), el("th", {}, "Μαθητής"), el("th", {}, "Τάξη"))),
        el("tbody", {}, rejected.map((u) => {
          const s = byAm.get(u.am);
          return el("tr", {}, el("td", {}, DAY_LABELS[u.day]), el("td.num", {}, u.am), el("td", {}, who(s)), el("td", {}, s?.grade ?? ""));
        }))))
      : el("p.muted", {}, "Όσοι δήλωσαν πήραν όμιλο κάθε ημέρα."),
    el("h3", {}, `Χωρίς δήλωση (${noSubmission.length})`),
    noSubmission.length
      ? el("details", {}, el("summary", {}, "Εμφάνιση λίστας"),
        el("p.small.muted", {}, "Δεν τοποθετήθηκαν σε κανέναν όμιλο."),
        el("ul", {}, noSubmission.map((s) => el("li", {}, `${who(s)} · ${s.grade} · ΑΜ ${s.am}`))))
      : el("p.muted", {}, "Όλοι υπέβαλαν δήλωση."));
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
