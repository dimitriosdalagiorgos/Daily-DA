import { $, busy, createApi, el, formatDateTime, message, show } from "./ui.js";
import { createRanker, layoutPinned } from "./ranker.js";
import { reportView } from "./report.js";

const api = createApi("parent");
let pub = {};
const app = $("#app");
const logout = $("#logout");

const DAY_KEYS = ["mon", "tue", "wed", "thu", "fri"];
const DAY_NAMES = { mon: "Δευτέρα", tue: "Τρίτη", wed: "Τετάρτη", thu: "Πέμπτη", fri: "Παρασκευή" };
const DAY_GEN = { mon: "της Δευτέρας", tue: "της Τρίτης", wed: "της Τετάρτης", thu: "της Πέμπτης", fri: "της Παρασκευής" };
const DAY_ACC = { mon: "τη Δευτέρα", tue: "την Τρίτη", wed: "την Τετάρτη", thu: "την Πέμπτη", fri: "την Παρασκευή" };

// First-time order of a day's clubs: shuffled per student (same ΑΜ → same
// order), so no club gains from its place in the clubs template.
function initialOrder(codes, key) {
  let h = 2166136261;
  for (const ch of key) h = Math.imul(h ^ ch.codePointAt(0), 16777619) >>> 0;
  const rnd = () => { h = (Math.imul(h, 1664525) + 1013904223) >>> 0; return h / 2 ** 32; };
  const a = [...codes];
  for (let i = a.length - 1; i > 0; i--) {
    const j = Math.floor(rnd() * (i + 1));
    [a[i], a[j]] = [a[j], a[i]];
  }
  return a;
}

api.onExpired = () => { api.setToken(null); loginView(message("warn", "Η σύνδεση έληξε. Συνδεθείτε ξανά.")); };
// The parents' password is kept in memory only, so a parent with more
// children does not type it again. Logging out forgets it (next parent).
let rememberedPassword = "";
function signOut() {
  api.setToken(null);
  rememberedPassword = "";
  loginView(message("ok", "Αποσυνδεθήκατε. Ο επόμενος γονέας μπορεί να συνδεθεί με τα δικά του στοιχεία."));
}
function nextChild() {
  api.setToken(null);
  loginView(message("info", "Συμπληρώστε τα στοιχεία του επόμενου παιδιού. Ο κωδικός γονέων έχει ήδη συμπληρωθεί."));
}
logout.addEventListener("click", signOut);

async function start() {
  pub = await fetch("/api/public").then((r) => r.json()).catch(() => ({}));
  if (api.hasToken()) {
    try {
      return mainView(await api("GET", "/api/parent/me"));
    } catch { api.setToken(null); }
  }
  loginView();
}

// ---------- Login ----------

async function loginView(notice) {
  logout.classList.add("hidden");
  const out = el("div");
  // Hints go under the box, so boxes side by side stay in line.
  const field = (name, label, hint, type = "text", extra = {}) =>
    el("label", { for: name }, label, el("input", { id: name, name, type, required: true, autocomplete: "off", ...extra }), hint ? el("span.hint.after", {}, hint) : null);
  const form = el("form.card", { novalidate: true },
    el("div.login-short", {},
      field("password", "Κωδικός γονέων", "Τον έχει ανακοινώσει το σχολείο.", "password", { autocomplete: "current-password", value: rememberedPassword, size: 12 }),
      field("am", "Αριθμός μητρώου μαθητή", null, "text", { inputmode: "numeric", pattern: "[0-9]*", size: 8 })),
    el("div.grid-2", {},
      field("surname", "Επώνυμο μαθητή"),
      field("name", "Όνομα μαθητή", "Αν είναι σύνθετο, αρκεί το ένα από τα δύο."),
      field("father", "Όνομα πατέρα"),
      field("mother", "Όνομα μητέρας")),
    el("p.small.muted", {}, "Γράψτε τα ονόματα όπως είναι καταχωρισμένα στο σχολείο. Δεν πειράζουν τόνοι, πεζά/κεφαλαία ή παύλες."),
    out,
    el("div.actions", {}, el("button.primary", { type: "submit" }, "Σύνδεση")));
  form.addEventListener("submit", (e) => {
    e.preventDefault();
    const data = Object.fromEntries(new FormData(form));
    busy(form.querySelector("button[type=submit]"), out, async () => {
      const { token } = await api("POST", "/api/parent/login", data);
      api.setToken(token);
      rememberedPassword = data.password;
      mainView(await api("GET", "/api/parent/me"));
    }).then(() => {
      if (out.querySelector(".msg.err") && pub.contact) out.append(el("p.small", {}, `Αν δεν μπορείτε να συνδεθείτε: ${pub.contact}`));
    });
  });

  const closedBefore = pub.phase && ["setup", "teachers"].includes(pub.phase);
  show(app,
    el("h1", {}, "Δήλωση ομίλων"),
    notice,
    closedBefore ? message("info", "Οι δηλώσεις δεν έχουν ανοίξει ακόμα.") : null,
    pub.phase === "parents" && pub.deadline ? message("info", `Οι δηλώσεις είναι ανοιχτές έως: ${formatDateTime(pub.deadline)}.`) : null,
    form);
}

// ---------- Main ----------

function mainView(me, { justSubmitted = false } = {}) {
  logout.classList.remove("hidden");
  const { student, days, submission, canEdit, result } = me;
  const out = el("div");
  const nodes = [
    el("h1", {}, `${student.surname} ${student.name}`),
    el("p.muted", {}, `Τάξη ${student.grade} · ΑΜ ${student.am}`),
  ];

  if (result) {
    nodes.push(el("section.card", {},
      el("h2", { style: "margin-top:0" }, "Αποτελέσματα κατανομής"),
      el("div.table-wrap", {}, el("table", {},
        el("thead", {}, el("tr", {}, el("th", {}, "Ημέρα"), el("th", {}, "Όμιλος"))),
        el("tbody", {}, ["mon", "tue", "wed", "thu", "fri"].filter((d) => days.some((x) => x.day === d)).map((d) => {
          const label = days.find((x) => x.day === d).label;
          return el("tr", {}, el("td", {}, label), el("td", {}, result[d]?.name ?? el("span.muted", {}, "—")));
        })))),
      me.report ? el("details", {}, el("summary", {}, "Πώς προέκυψε η κατανομή (βήμα προς βήμα)"), reportView(me.report, { title: null })) : null));
  }

  if (submission) {
    nodes.push(message("ok", `Υπάρχει δήλωση από ${formatDateTime(submission.submittedAt)}${canEdit ? ". Μπορείτε να την αλλάξετε μέχρι την προθεσμία." : "."}`));
    nodes.push(receiptView(me, justSubmitted), doneView(), historyView(me));
  }
  if (!canEdit && !result) {
    nodes.push(message("info", me.phase === "parents" ? "Η προθεσμία έληξε· η δήλωση δεν αλλάζει πια." : "Οι δηλώσεις δεν δέχονται αλλαγές αυτή τη στιγμή."));
  }
  if (canEdit && me.deadline) nodes.push(el("p", {}, "Προθεσμία: ", el("strong", {}, formatDateTime(me.deadline))));

  if (!canEdit && !submission) {
    show(app, nodes);
    return;
  }

  // How it works (pure Gale–Shapley: the ranking decides where the child
  // applies, priority is teacher list → mandatory grade → lottery)
  nodes.push(el("details.card", {},
    el("summary", {}, el("strong", {}, "Πώς γίνεται η κατανομή")),
    el("ul", {},
      el("li", {}, "Για κάθε ημέρα βάζετε σε σειρά ", el("strong", {}, "όλους"), " τους ομίλους που αφορούν την τάξη του παιδιού."),
      el("li", {}, "Το παιδί κάνει αίτηση πρώτα στην 1η επιλογή σας· αν δεν χωρέσει, στη 2η, και ούτω καθεξής."),
      el("li", {}, "Όταν ένας όμιλος έχει περισσότερες αιτήσεις από θέσεις, προηγούνται: (1) οι μαθητές που επέλεξε ο/η εκπαιδευτικός του ομίλου, (2) οι μαθητές των τάξεων όπου η συμμετοχή σε όμιλο είναι υποχρεωτική, (3) με κλήρωση οι υπόλοιποι — ", el("strong", {}, "όλοι με την ίδια προτεραιότητα"), ", είτε έβαλαν τον όμιλο 1ο είτε 5ο."),
      el("li", {}, el("strong", {}, "Βάλτε τη σειρά που πραγματικά θέλετε."), " Δεν κερδίζετε τίποτα βάζοντας πρώτο έναν όμιλο «για σιγουριά»: αν το παιδί δεν χωρέσει στην 1η επιλογή, διεκδικεί τη 2η με την ίδια πιθανότητα με όλους."),
      el("li", {}, "Στην αρχή οι όμιλοι εμφανίζονται σε τυχαία σειρά. Κάθε ημέρα είναι σε δική της καρτέλα."),
      el("li", {}, "Διπλοί και τριπλοί όμιλοι (δύο ή τρεις ημέρες) μπαίνουν στη σειρά την πρώτη τους ημέρα. Τις άλλες ημέρες τους εμφανίζονται κλειδωμένοι 🔒 στην αρχή της λίστας: αν το παιδί μπει σε αυτούς την πρώτη ημέρα, τις άλλες ημέρες πηγαίνει αυτόματα στον ίδιο όμιλο."))));

  const parentName = el("input", { id: "pname", type: "text", required: true, autocomplete: "name", value: submission?.parent?.name ?? "", disabled: !canEdit });
  const parentEmail = el("input", { id: "pemail", type: "email", required: true, autocomplete: "email", value: submission?.parent?.email ?? "", disabled: !canEdit });
  nodes.push(el("section.card", {},
    el("h2", { style: "margin-top:0" }, "Στοιχεία γονέα / κηδεμόνα"),
    el("div.grid-2", {},
      el("label", { for: "pname" }, "Ονοματεπώνυμο", parentName),
      el("label", { for: "pemail" }, "Email", el("span.hint", {}, pub.mailEnabled ? "Εδώ θα έρθει η επιβεβαίωση της δήλωσης." : "Για επικοινωνία από το σχολείο. Επιβεβαίωση με email δεν στέλνεται: κρατήστε την απόδειξη που εμφανίζεται μετά την υποβολή."), parentEmail))));

  if (!days.length) {
    // No club for this grade on any day (the admin sees a warning for this).
    nodes.push(message("warn", `Δεν υπάρχουν όμιλοι για δήλωση για την ${student.grade} τάξη. Επικοινωνήστε με το σχολείο${me.contact ? `: ${me.contact}` : "."}`));
    show(app, nodes);
    return;
  }

  // One tab per day. Multi-day clubs are ranked on their first day; on
  // their later days they are locked in the same list, at the number they
  // have on their first day (the free clubs take the other numbers).
  const kind = (c) => (c.days.length === 3 ? "Τριπλός όμιλος" : "Διπλός όμιλος");
  const daysText = (c) => c.days.map((x) => DAY_NAMES[x]).join(" + ");
  const rankers = {};
  const visited = new Set(submission ? days.map((d) => d.day) : []);

  // Multi-day clubs of the student's grade, for clashes between them
  const multi = [...new Map(days.flatMap((d) => [...d.clubs, ...d.locked]).filter((c) => c.days.length > 1).map((c) => [c.code, c])).values()];
  const earlierClashes = (c) => multi.filter((o) => o.code !== c.code &&
    DAY_KEYS.indexOf(o.days[0]) < DAY_KEYS.indexOf(c.days[0]) && o.days.some((x) => c.days.includes(x)));
  const allClubs = [...new Map(days.flatMap((d) => [...d.clubs, ...d.locked]).map((c) => [c.code, c])).values()];
  const earlierSimilar = (c) => (c.similar ? allClubs.filter((o) => o.code !== c.code && o.similar === c.similar &&
    DAY_KEYS.indexOf(o.days[0]) < DAY_KEYS.indexOf(c.days[0])) : []);
  const shared = (a, b) => a.days.filter((x) => b.days.includes(x)).map((x) => DAY_ACC[x]).join(" και ");

  // Locked rows of a later day: position = rank on the first day; equal
  // positions ordered by first day.
  const pinsFor = (d) => {
    const rows = d.locked.map((c) => ({ c, rank: (rankers[c.firstDay]?.order() ?? []).indexOf(c.code) + 1 }))
      .sort((a, b) => a.rank - b.rank || DAY_KEYS.indexOf(a.c.firstDay) - DAY_KEYS.indexOf(b.c.firstDay));
    return rows.map(({ c, rank }) => {
      // Decided earlier on a clashing day: this one counts only without it
      const before = rows.filter((o) => DAY_KEYS.indexOf(o.c.firstDay) < DAY_KEYS.indexOf(c.firstDay)).map((o) => `«${o.c.name}»`);
      return {
        position: rank,
        node: (number, clamped) => el("li.locked", {},
          el("span.handle", { "aria-hidden": "true" }, "🔒"),
          el("span.lockpos", { "aria-label": `Θέση ${number}, κλειδωμένη` }, String(number)),
          el("span.name", {}, c.name, el("span.tag", {}, `${kind(c)}: ${daysText(c)}`),
            el("span.desc", {}, `Κλειδωμένος: ${rank}η επιλογή ${DAY_GEN[c.firstDay]}${clamped ? " (εδώ στο τέλος, γιατί η ημέρα έχει λιγότερους ομίλους)" : ", ίδια θέση και εδώ"}. Αν το παιδί μπει σε αυτόν ${DAY_ACC[c.firstDay]}, ${DAY_ACC[d.day]} πηγαίνει αυτόματα εδώ.`),
            before.length ? el("span.note", {}, `Μετράει μόνο αν δεν μπει ${before.length > 1 ? `σε κανέναν από τους ${before.join(", ")}` : `στον όμιλο ${before[0]}`}.`) : null)),
      };
    });
  };
  const refreshAll = () => { for (const r of Object.values(rankers)) r.refresh(); };

  const panels = {};
  for (const d of days) {
    const panel = el("div.day", { role: "tabpanel", "aria-label": d.label });
    if (d.clubs.length) {
      panel.append(el("p.day-intro", {},
        `Βάλτε σε σειρά `, el("strong", {}, `όλους τους ομίλους ${DAY_GEN[d.day]}`),
        `: 1ος αυτός που θέλετε περισσότερο.${submission?.preferences?.[d.day] ? "" : " Στην αρχή η σειρά είναι τυχαία."} Μετακινήστε τους σύροντας τη λαβή ⠿, γράφοντας τον αριθμό της θέσης ή με τα κουμπιά ↑ ↓.`,
        d.locked.length ? el("span.block", {}, `Οι όμιλοι με 🔒 είναι διπλοί/τριπλοί όμιλοι που βάλατε σε σειρά σε προηγούμενη ημέρα. Μπαίνουν αυτόματα στην ίδια θέση και δεν μετακινούνται από εδώ· αν αλλάξετε τη θέση τους εκείνη την ημέρα, αλλάζει και εδώ.`) : null));
    }
    if (d.clubs.length) {
      const saved = submission?.preferences?.[d.day];
      const order = saved ?? initialOrder(d.clubs.map((c) => c.code), `${student.am}:${d.day}`);
      const items = d.clubs.map((c) => {
        const notes = [];
        if (c.days.length > 1) {
          for (const o of earlierClashes(c)) notes.push(`Αν το παιδί μπει στον όμιλο «${o.name}» ${DAY_ACC[o.days[0]]}, αυτός παραλείπεται (έχουν και οι δύο ${shared(o, c)}).`);
        }
        // Similar clubs decided on an earlier day: at most one per week
        const similar = earlierSimilar(c);
        if (similar.length) {
          notes.push(`Παρεμφερής με ${similar.map((o) => `«${o.name}» (${DAY_NAMES[o.days[0]]})`).join(", ")}: αν το παιδί μπει ${similar.length > 1 ? "σε κάποιον από αυτούς" : "εκεί"}, αυτός παραλείπεται.`);
        }
        if (c.days.length === 1 && !notes.length) return c;
        return { ...c, ...(c.days.length > 1 ? { tag: `${kind(c)}: ${daysText(c)}` } : {}), note: notes.join(" ") || null };
      });
      rankers[d.day] = createRanker({
        items, order, disabled: !canEdit, label: `Σειρά ομίλων ${d.label}`,
        pinned: d.locked.length ? () => pinsFor(d) : undefined,
        onChange: refreshAll,
      });
      panel.append(rankers[d.day].node);
    } else {
      // Only locked clubs today: the same rows, numbered from 1
      const locked = el("ol.ranker", { "aria-label": `Κλειδωμένοι όμιλοι ${d.label}` });
      rankers[d.day] = { order: () => [], refresh: () => {
        const pins = pinsFor(d);
        const { pinnedNumbers } = layoutPinned(0, pins.map((p) => p.position));
        locked.replaceChildren(...pins.map((p, k) => p.node(pinnedNumbers[k], pinnedNumbers[k] !== p.position)));
      } };
      rankers[d.day].refresh();
      panel.append(el("p.day-intro", {}, `${DAY_ACC[d.day][0].toUpperCase()}${DAY_ACC[d.day].slice(1)} δεν υπάρχει άλλος όμιλος για την τάξη του παιδιού.`), locked);
    }
    panels[d.day] = panel;
  }

  let current = days[0].day;
  const nav = el("nav.tabs.day-tabs", { role: "tablist", "aria-label": "Ημέρες" });
  const panelBox = el("div");
  const dayActions = el("div.actions.phase-actions");
  const idx = () => days.findIndex((d) => d.day === current);
  const go = (day) => { current = day; drawTabs(); nav.scrollIntoView({ block: "start", behavior: "smooth" }); };
  const submit = canEdit ? el("button", { type: "button" }, submission ? "Αποθήκευση αλλαγών" : "Υποβολή δήλωσης") : null;
  function drawTabs() {
    visited.add(current);
    nav.replaceChildren(...days.map((d) => el("button", {
      type: "button", role: "tab", "aria-selected": String(d.day === current), onclick: () => go(d.day),
    }, d.label, visited.has(d.day) && d.day !== current ? " ✓" : "")));
    panelBox.replaceChildren(panels[current]);
    const i = idx();
    const prev = days[i - 1];
    const next = days[i + 1];
    if (submit) submit.className = next ? "" : "primary";
    dayActions.replaceChildren(...[
      prev ? el("button", { type: "button", onclick: () => go(prev.day) }, `← ${prev.label}`) : null,
      el("span.spacer"),
      next ? el("button.primary", { type: "button", onclick: () => go(next.day) }, `${next.label} →`) : null,
      submit].filter(Boolean));
  }
  drawTabs();
  nodes.push(el("section.card", {},
    el("h2", { style: "margin-top:0" }, "Σειρά προτίμησης ανά ημέρα"),
    nav, panelBox, out, dayActions));

  if (canEdit) {
    submit.addEventListener("click", () => busy(submit, out, async () => {
      const unseen = days.filter((d) => !visited.has(d.day) && d.clubs.length).map((d) => d.label);
      if (unseen.length && !confirm(`Δεν έχετε δει τη σειρά για: ${unseen.join(", ")}. Εκεί θα μείνει η τυχαία σειρά. Υποβολή παρ' όλα αυτά;`)) return;
      const preferences = Object.fromEntries(days.filter((d) => d.clubs.length).map((d) => [d.day, rankers[d.day].order()]));
      await api("PUT", "/api/parent/submission", { parent: { name: parentName.value, email: parentEmail.value }, preferences });
      const me2 = await api("GET", "/api/parent/me");
      mainView(me2, { justSubmitted: true });
      $("#app").prepend(message("ok", pub.mailEnabled
        ? `Η δήλωση καταχωρίστηκε. Στάλθηκε επιβεβαίωση στο ${parentEmail.value}.`
        : `Η δήλωση καταχωρίστηκε. Εκτυπώστε ή αποθηκεύστε την απόδειξη (κωδικός ${me2.submission.receipt}).`));
      window.scrollTo({ top: 0, behavior: "smooth" });
    }));
  }
  show(app, nodes);
}

// After a submission: another child, or leave the device to the next parent
function doneView() {
  return el("section.card.no-print", {},
    el("h2", { style: "margin-top:0" }, "Τελειώσατε;"),
    el("p", {}, "Αν έχετε κι άλλο παιδί στο σχολείο, κάντε δήλωση και για εκείνο. Αν τη συσκευή θα τη χρησιμοποιήσει άλλος γονέας, πατήστε «Αποσύνδεση γονέα»."),
    el("div.actions", {},
      el("button", { type: "button", onclick: nextChild }, "Δήλωση για άλλο παιδί"),
      el("button.primary", { type: "button", onclick: signOut }, "Αποσύνδεση γονέα")));
}

// ---------- Receipt & history ----------

function receiptView(me, open) {
  const { student, days, submission } = me;
  const nameOf = new Map(days.flatMap((d) => d.clubs.map((c) => [c.code, c.name])));
  const print = el("button", { type: "button", onclick: () => window.print() }, "Εκτύπωση / αποθήκευση ως PDF");
  // Closed unless just submitted: the parent opens it when needed.
  return el("details.card.receipt", { open },
    el("summary", {}, el("span.summary-title", {}, `Απόδειξη δήλωσης (κωδικός ${submission.receipt})`)),
    el("h2", {}, "Απόδειξη δήλωσης ομίλων"),
    pub.schoolName ? el("p", {}, pub.schoolName) : null,
    el("p", {}, el("strong", {}, `${student.surname} ${student.name}`), ` · Τάξη ${student.grade} · ΑΜ ${student.am}`),
    el("p", {}, "Υποβλήθηκε: ", el("strong", {}, formatDateTime(submission.submittedAt)), el("br"),
      "Κωδικός απόδειξης: ", el("strong", { style: "font-size:1.2em;letter-spacing:0.05em" }, submission.receipt)),
    days.filter((d) => submission.preferences[d.day]?.length || d.locked.length).map((d) => el("div.summary-day", {},
      el("strong", {}, d.label),
      el("ul.receipt-list", {}, receiptRows(d, submission.preferences, nameOf).map(([n, name, locked]) =>
        el("li", {}, `${n}. ${name}`, locked ? ` 🔒 (${locked})` : ""))))),
    el("p.small.muted", {}, "Ο κωδικός αλλάζει σε κάθε αλλαγή της δήλωσης. Αν συνδεθείτε ξανά και δείτε άλλον κωδικό από αυτόν της απόδειξής σας, η δήλωση έχει αλλάξει."),
    el("div.actions.no-print", {}, print));
}

// A day's list as on the form: locked multi-day clubs at their first-day
// number, the ranked clubs in the other numbers.
function receiptRows(d, preferences, nameOf) {
  const free = preferences[d.day] ?? [];
  const pins = d.locked.map((c) => ({ c, rank: (preferences[c.firstDay] ?? []).indexOf(c.code) + 1 }))
    .filter((p) => p.rank > 0)
    .sort((a, b) => a.rank - b.rank || DAY_KEYS.indexOf(a.c.firstDay) - DAY_KEYS.indexOf(b.c.firstDay));
  const { pinnedNumbers, freeNumbers } = layoutPinned(free.length, pins.map((p) => p.rank));
  return [
    ...pins.map((p, k) => [pinnedNumbers[k], p.c.name, `από ${DAY_ACC[p.c.firstDay]}`]),
    ...free.map((code, i) => [freeNumbers[i], nameOf.get(code) ?? code, null]),
  ].sort((a, b) => a[0] - b[0]);
}

function historyView(me) {
  const history = me.submission.history ?? [];
  if (history.length < 2) return null;
  return el("details.card", {},
    el("summary", {}, el("strong", {}, `Η δήλωση έχει αποθηκευτεί ${history.length} φορές`)),
    el("ul", {}, [...history].reverse().map((h) => el("li", {}, `${formatDateTime(h.at)} — ${h.email}`))),
    el("p.small", {}, `Αν κάποια από αυτές τις αλλαγές δεν την κάνατε εσείς, επικοινωνήστε αμέσως με το σχολείο${pub.contact ? `: ${pub.contact}` : "."}`));
}

start();
