import { $, busy, codeField, createApi, el, formatDateTime, message, show } from "./ui.js";
import { listEditor } from "./listEditor.js";
import { helpView } from "./help.js";

const api = createApi("teacher");
const app = $("#app");
const logout = $("#logout");
const DAY_LABELS = { mon: "Δευτέρα", tue: "Τρίτη", wed: "Τετάρτη", thu: "Πέμπτη", fri: "Παρασκευή" };

api.onExpired = () => { api.setToken(null); loginView(message("warn", "Η σύνδεση έληξε. Συνδεθείτε ξανά.")); };
// Clubs with ticks not saved yet: leaving or logging out asks first
const unsaved = {};
const hasUnsaved = () => Object.values(unsaved).some(Boolean);
logout.addEventListener("click", () => {
  if (hasUnsaved() && !confirm("Υπάρχουν αλλαγές που δεν αποθηκεύσατε. Αποσύνδεση χωρίς αποθήκευση;")) return;
  for (const k of Object.keys(unsaved)) delete unsaved[k];
  api.setToken(null);
  loginView(message("ok", "Αποσυνδεθήκατε."));
});

// Help inside the page (the public help page is for parents only)
const helpButton = $("#help");
const helpPanel = $("#help-panel");
helpButton.addEventListener("click", () => {
  const open = helpPanel.classList.toggle("hidden") === false;
  helpButton.setAttribute("aria-expanded", String(open));
  helpButton.textContent = open ? "Κλείσιμο βοήθειας" : "Βοήθεια";
  if (open && !helpPanel.firstChild) {
    const close = el("button.small", { type: "button", onclick: () => helpButton.click() }, "Κλείσιμο βοήθειας");
    helpPanel.append(el("h1", {}, "Βοήθεια για εκπαιδευτικούς"), helpView("teacher"), el("div.actions", {}, close));
  }
  if (open) helpPanel.scrollIntoView();
});

async function start() {
  // Personal login link: <teachers' page>#token=…
  const magic = new URLSearchParams(location.hash.slice(1)).get("token");
  if (magic) {
    history.replaceState(null, "", location.pathname);
    try {
      const { token } = await api("POST", "/api/teacher/session", { token: magic });
      api.setToken(token);
    } catch (err) {
      return loginView(message("err", err.message));
    }
  }
  if (api.hasToken()) {
    try {
      return mainView(await api("GET", "/api/teacher/me"));
    } catch { api.setToken(null); }
  }
  loginView();
}

async function loginView(notice) {
  logout.classList.add("hidden");
  const pub = await fetch("/api/public").then((r) => r.json()).catch(() => ({}));
  if (pub.teacherCode) return codeLoginView(notice, pub);
  if (pub.mailEnabled === false) {
    show(app, el("h1", {}, "Σύνδεση εκπαιδευτικού"), notice,
      message("info", "Η είσοδος εκπαιδευτικών δεν έχει ενεργοποιηθεί ακόμα. Η διαχείριση της πλατφόρμας θα σας δώσει τον κωδικό εκπαιδευτικών (ή έναν προσωπικό σύνδεσμο εισόδου)."),
      pub.contact ? el("p.small.muted", {}, `Επικοινωνία: ${pub.contact}`) : null);
    return;
  }
  const out = el("div");
  const email = el("input", { id: "email", type: "email", required: true, autocomplete: "email", placeholder: "onoma@sch.gr" });
  const form = el("form.card", {},
    el("label", { for: "email" }, "Email", el("span.hint", {}, "Η διεύθυνση @sch.gr που έχει δηλώσει το σχολείο για τον όμιλό σας."), email),
    out,
    el("div.actions", {}, el("button.primary", { type: "submit" }, "Αποστολή συνδέσμου")));
  form.addEventListener("submit", (e) => {
    e.preventDefault();
    busy(form.querySelector("button"), out, async () => {
      const { message: text } = await api("POST", "/api/teacher/login", { email: email.value });
      show(out, message("ok", text));
    });
  });
  show(app, el("h1", {}, "Σύνδεση εκπαιδευτικού"), notice, form);
}

// Login with the teachers' common code (set by the admin) and the teacher's own ΑΜ or ΑΦΜ
function codeLoginView(notice, pub) {
  const out = el("div");
  // No autofill: a browser would put the parents' code (same site) or an old one behind the dots
  const password = el("input", { id: "tcode", type: "password", required: true, autocomplete: "off", size: 14 });
  const personalId = el("input", { id: "tid", type: "text", required: true, inputmode: "numeric", autocomplete: "off", size: 12 });
  const form = el("form.card", {},
    el("div.login-short", {},
      el("label", { for: "tcode" }, "Κωδικός εκπαιδευτικών", codeField(password), el("span.hint.after", {}, "Τον έχει δώσει η διαχείριση της πλατφόρμας.")),
      el("label", { for: "tid" }, "ΑΜ ή ΑΦΜ σας", personalId, el("span.hint.after", {}, "Μόνιμοι: αριθμός μητρώου. Αναπληρωτές: ΑΦΜ."))),
    out,
    el("div.actions", {}, el("button.primary", { type: "submit" }, "Σύνδεση")));
  form.addEventListener("submit", (e) => {
    e.preventDefault();
    busy(form.querySelector("button"), out, async () => {
      const { token } = await api("POST", "/api/teacher/code-login", { password: password.value, personalId: personalId.value });
      api.setToken(token);
      mainView(await api("GET", "/api/teacher/me"));
    }).then(() => {
      if (out.querySelector(".msg.err") && pub.contact) out.append(el("p.small", {}, `Επικοινωνία: ${pub.contact}`));
    });
  });
  show(app, el("h1", {}, "Σύνδεση εκπαιδευτικού"), notice, form);
  password.focus();
}

function mainView(me) {
  logout.classList.remove("hidden");
  const nodes = [
    el("h1", {}, `${me.teacher.name} ${me.teacher.surname}`),
    me.canEdit
      ? message("info", "Αν θέλετε, ορίστε για κάθε μαθητή: ⭐ προτίμηση (έως τις θέσεις του ομίλου: έχει προτεραιότητα αν δηλώσει τον όμιλο, δεν τοποθετείται υποχρεωτικά), ○ ουδέτερος (κρίνεται με την κλήρωση) ή ⛔ αποκλεισμός (δεν μπαίνει στον όμιλο και δεν τον βλέπει στη φόρμα των γονέων). Αν δεν μείνει κανένας ○, η λίστα γίνεται «κλειστή»: ο όμιλος δέχεται μόνο τους ⭐. Τις θέσεις τις ορίζει η διαχείριση."
        + (me.phase === "parents" && me.deadline ? ` Οι λίστες μένουν ανοιχτές μέχρι το κλείσιμο των δηλώσεων των γονέων: ${formatDateTime(me.deadline)}.` : ""))
      : me.phase === "published"
        ? message("info", "Η κατανομή ανακοινώθηκε. Για κάθε όμιλό σας βλέπετε ποιοι μαθητές μπήκαν και πώς.")
        : message("warn", me.phase === "setup" ? "Η φάση των εκπαιδευτικών δεν έχει ανοίξει ακόμα." : "Οι λίστες κλείδωσαν με το κλείσιμο των δηλώσεων. Για διορθώσεις επικοινωνήστε με τη διαχείριση."),
  ];
  let drawTabs = null;
  const cards = me.clubs.map((club) => clubCard(club, me, () => drawTabs?.()));
  if (me.clubs.length === 0) nodes.push(message("warn", "Δεν υπάρχει όμιλος με το email σας."));
  // Several clubs: one tab each, so only one long list is on the page at a time.
  // The other cards stay in the page (hidden), keeping their unsaved ticks.
  if (cards.length > 1) {
    const codes = me.clubs.map((c) => String(c.code));
    let current = rememberedTab();
    if (!codes.includes(current)) current = codes[0];
    const nav = el("nav.tabs.club-tabs", { role: "tablist", "aria-label": "Οι όμιλοί σας" });
    // The buttons are updated in place, so a keyboard user keeps focus on the tab
    const buttons = me.clubs.map((club, i) => el("button", {
      type: "button", role: "tab",
      onclick: () => { current = codes[i]; rememberTab(current); drawTabs(); nav.scrollIntoView({ block: "nearest" }); },
    }));
    nav.append(...buttons);
    drawTabs = () => {
      me.clubs.forEach((club, i) => {
        buttons[i].setAttribute("aria-selected", String(codes[i] === current));
        buttons[i].title = unsaved[club.code] ? "Μη αποθηκευμένες αλλαγές" : "";
        buttons[i].textContent = club.name + (unsaved[club.code] ? " ●" : "");
      });
      cards.forEach((card, i) => { card.hidden = codes[i] !== current; });
    };
    drawTabs();
    nodes.push(nav);
  }
  nodes.push(...cards);
  show(app, nodes);
}

// The chosen tab survives a reload (best effort: storage may be blocked)
const TAB_KEY = "teacher-club-tab";
function rememberedTab() { try { return sessionStorage.getItem(TAB_KEY); } catch { return null; } }
function rememberTab(code) { try { sessionStorage.setItem(TAB_KEY, code); } catch { /* ignore */ } }

function clubCard(club, me, onTabChange) {
  const changeText = () => (club.updatedAt
    ? `Τελευταία αλλαγή: ${{ admin: "διαχείριση", "admin-file": "διαχείριση (από αρχείο)" }[club.updatedBy] ?? club.updatedBy}, ${formatDateTime(club.updatedAt)}`
    : "");
  const lastChange = el("p.small.muted", {}, changeText());
  // The ⭐ were emptied by a clubs file with fewer seats: until the list is saved again
  const resetNotice = club.reset ? message("warn", `${club.reset} Ορίστε ξανά τους προτιμώμενους μαθητές (⭐), έως ${club.capacity}, και πατήστε αποθήκευση.`) : null;
  const editor = listEditor({
    club,
    eligible: club.eligible,
    list: { ams: club.list, blocked: club.blocked, closed: club.closed },
    canEdit: me.canEdit,
    parentsPhase: me.phase === "parents",
    onDirty: (dirty) => {
      const changed = Boolean(unsaved[club.code]) !== dirty;
      unsaved[club.code] = dirty;
      if (changed) onTabChange();
    },
    onSave: async (payload) => {
      const { list } = await api("PUT", `/api/teacher/clubs/${club.code}`, payload);
      Object.assign(club, { list: list.ams, blocked: list.blocked ?? [], closed: Boolean(list.closed), updatedBy: list.updatedBy, updatedAt: list.updatedAt });
      lastChange.textContent = changeText();
      resetNotice?.remove();
      return list;
    },
  });
  // After the announcement the result comes first; the locked list folds away
  const listPart = club.allocation
    ? el("details.locked-list", {}, el("summary", {}, "Η λίστα σας (κλειδωμένη)"), lastChange, editor)
    : null;
  return el("section.card", {},
    el("h2", { style: "margin-top:0" }, club.name),
    el("p.muted.small", {}, `${club.days.map((d) => DAY_LABELS[d]).join(" + ")} · Τάξεις ${club.grades.join(", ")} · `, el("strong", {}, `${club.capacity} θέσεις`),
      club.coTeachers.length ? ` · Μαζί με: ${club.coTeachers.join(", ")}` : ""),
    club.coTeachers.length ? message("info", "Ο όμιλος έχει κοινή λίστα για όλους τους εκπαιδευτικούς του. Συνεννοηθείτε ώστε να τη συμπληρώσει ένας.") : null,
    club.allocation ? allocationView(club) : null,
    resetNotice,
    listPart ?? lastChange,
    listPart ? null : editor);
}

// ---------- After the announcement: who got in and why ----------

const WHY = {
  wish: "⭐ από τη λίστα σας",
  mandatory: "Τάξη με υποχρεωτική ένταξη",
  lottery: "Κλήρωση",
  seat: "Υπήρχε θέση",
};

function allocationView(club) {
  const { applicants, notPlaced, full, members } = club.allocation;
  const days = club.days.map((d) => DAY_LABELS[d]).join(" + ");
  const summary = [
    `Τον ζήτησαν ${applicants} ${applicants === 1 ? "μαθητής" : "μαθητές"} για ${club.capacity} θέσεις.`,
    `Μπήκαν ${members.length}.`,
    notPlaced ? ` ${notPlaced === 1 ? "1 δεν χώρεσε και τοποθετήθηκε" : `${notPlaced} δεν χώρεσαν και τοποθετήθηκαν`} σε επόμενη επιλογή του ή έμειν${notPlaced === 1 ? "ε" : "αν"} χωρίς όμιλο.` : "",
  ].join(" ");
  return el("div.allocation", {},
    el("h3", {}, `Η κατανομή του ομίλου${club.days.length > 1 ? ` (${days})` : ""}`),
    el("p", {}, summary),
    el("p.small.muted", {}, full
      ? "Ο όμιλος γέμισε. Σειρά προτεραιότητας: (1) οι ⭐ της λίστας σας, (2) οι τάξεις με υποχρεωτική ένταξη, (3) η κλήρωση: μικρότερος αριθμός προηγείται."
      : "Ο όμιλος δεν γέμισε: μπήκαν όλοι όσοι τον ζήτησαν.",
      " Οι δηλώσεις των γονέων (η σειρά των ομίλων τους) δεν εμφανίζονται."),
    members.length
      ? el("div.table-wrap", {}, el("table", {},
        el("thead", {}, el("tr", {}, el("th", {}, "#"), el("th", {}, "Μαθητής"), el("th", {}, "Τάξη"), el("th", {}, "Πώς μπήκε"))),
        el("tbody", {}, members.map((m, i) => el("tr", {},
          el("td", {}, String(i + 1)),
          el("td", {}, `${m.surname} ${m.name}`, el("span.muted.small", {}, ` · ΑΜ ${m.am}`)),
          el("td", {}, `${m.grade}${m.section ? ` · ${m.section}` : ""}`),
          el("td", {}, m.lottery ? `${WHY[m.why]} · αρ. κλήρωσης ${m.lottery}` : WHY[m.why]))))))
      : message("info", "Δεν μπήκε κανένας μαθητής."));
}

window.addEventListener("beforeunload", (e) => { if (hasUnsaved()) e.preventDefault(); });
window.addEventListener("hashchange", () => { if (location.hash.includes("token=")) start(); });
start();
