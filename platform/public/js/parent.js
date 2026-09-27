import { $, busy, createApi, el, formatDateTime, message, show } from "./ui.js";
import { createRanker } from "./ranker.js";

const api = createApi("parent");
let pub = {};
const app = $("#app");
const logout = $("#logout");

api.onExpired = () => { api.setToken(null); loginView(message("warn", "Η σύνδεση έληξε. Συνδεθείτε ξανά.")); };
logout.addEventListener("click", () => { api.setToken(null); loginView(); });

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
  const field = (name, label, hint, type = "text", extra = {}) =>
    el("label", { for: name }, label, hint ? el("span.hint", {}, hint) : null, el("input", { id: name, name, type, required: true, autocomplete: "off", ...extra }));
  const form = el("form.card", { novalidate: true },
    field("password", "Κωδικός γονέων", "Τον έχει ανακοινώσει το σχολείο.", "password", { autocomplete: "current-password" }),
    field("am", "Αριθμός μητρώου μαθητή", null, "text", { inputmode: "numeric", pattern: "[0-9]*" }),
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

function mainView(me) {
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
        }))))));
  }

  if (submission) {
    nodes.push(message("ok", `Υπάρχει δήλωση από ${formatDateTime(submission.submittedAt)}${canEdit ? ". Μπορείτε να την αλλάξετε μέχρι την προθεσμία." : "."}`));
    nodes.push(receiptView(me), historyView(me));
  }
  if (!canEdit && !result) {
    nodes.push(message("info", me.phase === "parents" ? "Η προθεσμία έληξε· η δήλωση δεν αλλάζει πια." : "Οι δηλώσεις δεν δέχονται αλλαγές αυτή τη στιγμή."));
  }
  if (canEdit && me.deadline) nodes.push(el("p", {}, "Προθεσμία: ", el("strong", {}, formatDateTime(me.deadline))));

  if (!canEdit && !submission) {
    show(app, nodes);
    return;
  }

  // How it works (SPEC: do not claim strategy-proofness; the 1st choice gives an advantage)
  nodes.push(el("details.card", {},
    el("summary", {}, el("strong", {}, "Πώς γίνεται η κατανομή")),
    el("ul", {},
      el("li", {}, "Για κάθε ημέρα βάζετε σε σειρά ", el("strong", {}, "όλους"), " τους ομίλους που αφορούν την τάξη του παιδιού."),
      el("li", {}, "Όταν ένας όμιλος έχει περισσότερες αιτήσεις από θέσεις, προηγούνται: (1) οι μαθητές που επέλεξε ο/η εκπαιδευτικός του ομίλου, (2) όσοι τον έβαλαν ψηλότερα στη λίστα τους, (3) κλήρωση."),
      el("li", {}, "Άρα η θέση που δίνετε σε έναν όμιλο μετράει: ο όμιλος που βάζετε 1ο σας δίνει προβάδισμα έναντι όσων τον έβαλαν χαμηλότερα."),
      el("li", {}, "Αν το παιδί δεν χωρέσει στην 1η επιλογή, δοκιμάζει τη 2η, και ούτω καθεξής."),
      el("li", {}, "Όμιλοι που γίνονται δύο ή τρεις ημέρες μπαίνουν στη σειρά μόνο την πρώτη τους ημέρα· τις άλλες ημέρες εμφανίζονται κλειδωμένοι."))));

  const parentName = el("input", { id: "pname", type: "text", required: true, autocomplete: "name", value: submission?.parent?.name ?? "", disabled: !canEdit });
  const parentEmail = el("input", { id: "pemail", type: "email", required: true, autocomplete: "email", value: submission?.parent?.email ?? "", disabled: !canEdit });
  nodes.push(el("section.card", {},
    el("h2", { style: "margin-top:0" }, "Στοιχεία γονέα / κηδεμόνα"),
    el("div.grid-2", {},
      el("label", { for: "pname" }, "Ονοματεπώνυμο", parentName),
      el("label", { for: "pemail" }, "Email", el("span.hint", {}, pub.mailEnabled ? "Εδώ θα έρθει η επιβεβαίωση της δήλωσης." : "Για επικοινωνία από το σχολείο. Επιβεβαίωση με email δεν στέλνεται: κρατήστε την απόδειξη που εμφανίζεται μετά την υποβολή."), parentEmail))));

  const rankers = {};
  const daysCard = el("section.card", {}, el("h2", { style: "margin-top:0" }, "Σειρά προτίμησης ανά ημέρα"));
  for (const d of days) {
    const section = el("div.day", {}, el("h3", {}, d.label, d.clubs.length ? el("span.badge", {}, `${d.clubs.length} όμιλοι`) : null));
    if (d.clubs.length) {
      const r = createRanker({ items: d.clubs, order: submission?.preferences?.[d.day], disabled: !canEdit, label: `Σειρά ομίλων ${d.label}` });
      rankers[d.day] = r;
      section.append(r.node);
    }
    if (d.locked.length) {
      section.append(el("ol.ranker", { "aria-label": `Κλειδωμένοι όμιλοι ${d.label}` }, d.locked.map((c) =>
        el("li.locked", {}, el("span.handle", { "aria-hidden": "true" }, "🔒"),
          el("span", {}, el("span.name", {}, c.name), el("span.desc", {}, `Γίνεται και ${d.label}. Ισχύει η θέση που του δώσατε τη ${c.firstDayLabel}: αν μπει εκεί, έχει θέση και ${d.label}.`))))));
    }
    daysCard.append(section);
  }
  nodes.push(daysCard);

  if (canEdit) {
    const submit = el("button.primary", { type: "button" }, submission ? "Αποθήκευση αλλαγών" : "Υποβολή δήλωσης");
    submit.addEventListener("click", () => busy(submit, out, async () => {
      const preferences = Object.fromEntries(Object.entries(rankers).map(([day, r]) => [day, r.order()]));
      await api("PUT", "/api/parent/submission", { parent: { name: parentName.value, email: parentEmail.value }, preferences });
      const me2 = await api("GET", "/api/parent/me");
      mainView(me2);
      $("#app").prepend(message("ok", pub.mailEnabled
        ? `Η δήλωση καταχωρίστηκε. Στάλθηκε επιβεβαίωση στο ${parentEmail.value}.`
        : `Η δήλωση καταχωρίστηκε. Εκτυπώστε ή αποθηκεύστε την απόδειξη (κωδικός ${me2.submission.receipt}).`));
      window.scrollTo({ top: 0, behavior: "smooth" });
    }));
    nodes.push(out, el("div.actions", {}, submit));
  }
  show(app, nodes);
}

// ---------- Receipt & history ----------

function receiptView(me) {
  const { student, days, submission } = me;
  const nameOf = new Map(days.flatMap((d) => d.clubs.map((c) => [c.code, c.name])));
  const print = el("button", { type: "button", onclick: () => window.print() }, "Εκτύπωση / αποθήκευση ως PDF");
  return el("section.card.receipt", {},
    el("h2", { style: "margin-top:0" }, "Απόδειξη δήλωσης ομίλων"),
    pub.schoolName ? el("p", {}, pub.schoolName) : null,
    el("p", {}, el("strong", {}, `${student.surname} ${student.name}`), ` · Τάξη ${student.grade} · ΑΜ ${student.am}`),
    el("p", {}, "Υποβλήθηκε: ", el("strong", {}, formatDateTime(submission.submittedAt)), el("br"),
      "Κωδικός απόδειξης: ", el("strong", { style: "font-size:1.2em;letter-spacing:0.05em" }, submission.receipt)),
    days.filter((d) => submission.preferences[d.day]?.length).map((d) => el("div.summary-day", {},
      el("strong", {}, d.label),
      el("ol", {}, submission.preferences[d.day].map((code) => el("li", {}, nameOf.get(code) ?? code))))),
    el("p.small.muted", {}, "Ο κωδικός αλλάζει σε κάθε αλλαγή της δήλωσης. Αν συνδεθείτε ξανά και δείτε άλλον κωδικό από αυτόν της απόδειξής σας, η δήλωση έχει αλλάξει."),
    el("div.actions.no-print", {}, print));
}

function historyView(me) {
  const history = me.submission.history ?? [];
  if (history.length < 2) return null;
  return el("details.card", { open: true },
    el("summary", {}, el("strong", {}, `Η δήλωση έχει αποθηκευτεί ${history.length} φορές`)),
    el("ul", {}, [...history].reverse().map((h) => el("li", {}, `${formatDateTime(h.at)} — ${h.email}`))),
    el("p.small", {}, `Αν κάποια από αυτές τις αλλαγές δεν την κάνατε εσείς, επικοινωνήστε αμέσως με το σχολείο${pub.contact ? `: ${pub.contact}` : "."}`));
}

start();
