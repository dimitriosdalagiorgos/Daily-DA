import { $, busy, createApi, el, formatDateTime, message, show } from "./ui.js";
import { normalizeName } from "/lib/import/names.js";
import { helpView } from "./help.js";

const api = createApi("teacher");
const app = $("#app");
const logout = $("#logout");
const DAY_LABELS = { mon: "Δευτέρα", tue: "Τρίτη", wed: "Τετάρτη", thu: "Πέμπτη", fri: "Παρασκευή" };

api.onExpired = () => { api.setToken(null); loginView(message("warn", "Η σύνδεση έληξε. Συνδεθείτε ξανά.")); };
logout.addEventListener("click", () => { api.setToken(null); loginView(message("ok", "Αποσυνδεθήκατε.")); });

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
      message("info", "Τον προσωπικό σας σύνδεσμο εισόδου θα σας τον στείλει η διαχείριση της πλατφόρμας. Ανοίξτε τον από τη συσκευή σας· ισχύει μία εβδομάδα."),
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

// Login with the teachers' common code (set by the admin)
function codeLoginView(notice, pub) {
  const out = el("div");
  const password = el("input", { id: "tcode", type: "password", required: true, autocomplete: "current-password", size: 14 });
  const form = el("form.card", {},
    el("div.login-short", {}, el("label", { for: "tcode" }, "Κωδικός εκπαιδευτικών", password, el("span.hint.after", {}, "Τον έχει δώσει η διαχείριση της πλατφόρμας."))),
    out,
    el("div.actions", {}, el("button.primary", { type: "submit" }, "Σύνδεση")));
  form.addEventListener("submit", (e) => {
    e.preventDefault();
    busy(form.querySelector("button"), out, async () => {
      const { token } = await api("POST", "/api/teacher/code-login", { password: password.value });
      api.setToken(token);
      mainView(await api("GET", "/api/teacher/me"));
    }).then(() => {
      if (out.querySelector(".msg.err") && pub.contact) out.append(el("p.small", {}, `Επικοινωνία: ${pub.contact}`));
    });
  });
  show(app, el("h1", {}, "Σύνδεση εκπαιδευτικού"), notice, form);
  password.focus();
}

const CLUB_KEY = "omiloi.teacherClub";
const remember = (code) => { try { localStorage.setItem(CLUB_KEY, String(code)); } catch { /* private window */ } };
const remembered = () => { try { return localStorage.getItem(CLUB_KEY); } catch { return null; } };

function mainView(me) {
  logout.classList.remove("hidden");
  if (me.shared) return sharedView(me);
  const nodes = [
    el("h1", {}, `${me.teacher.name} ${me.teacher.surname}`),
    me.canEdit
      ? message("info", "Αν θέλετε, τσεκάρετε τους μαθητές που προτιμάτε για κάθε όμιλό σας, έως τις θέσεις του. Οι μαθητές της λίστας σας έχουν προτεραιότητα αν δηλώσουν τον όμιλο — δεν τοποθετούνται υποχρεωτικά. Τις θέσεις τις ορίζει η διαχείριση.")
      : message("warn", me.phase === "setup" ? "Η φάση των εκπαιδευτικών δεν έχει ανοίξει ακόμα." : "Οι λίστες κλείδωσαν. Για διορθώσεις επικοινωνήστε με τη διαχείριση."),
  ];
  for (const club of me.clubs) nodes.push(clubCard(club, me));
  if (me.clubs.length === 0) nodes.push(message("warn", "Δεν υπάρχει όμιλος με το email σας."));
  show(app, nodes);
}

// With the common code: choose the club, then the same card as above.
function sharedView(me) {
  const holder = el("div");
  const sorted = [...me.clubs].sort((a, b) => a.code - b.code);
  const picker = el("select", { id: "club", "aria-label": "Ο όμιλός σας" },
    el("option", { value: "" }, "— Επιλέξτε τον όμιλό σας —"),
    sorted.map((c) => el("option", { value: String(c.code) }, `${c.code} · ${c.name} (${c.days.map((d) => DAY_LABELS[d]).join(" + ")})`)));
  const draw = () => {
    const club = sorted.find((c) => String(c.code) === picker.value);
    holder.replaceChildren(club ? clubCard(club, me) : el("p.muted", {}, "Επιλέξτε τον όμιλό σας για να δείτε τους μαθητές."));
    if (club) remember(club.code);
  };
  picker.addEventListener("change", draw);
  if (sorted.some((c) => String(c.code) === remembered())) picker.value = remembered();
  show(app,
    el("h1", {}, "Λίστες ομίλων"),
    me.canEdit
      ? message("info", "Επιλέξτε τον όμιλό σας και, αν θέλετε, τσεκάρετε τους μαθητές που προτιμάτε, έως τις θέσεις του. Αλλάζετε μόνο τη λίστα του δικού σας ομίλου. Οι μαθητές της λίστας έχουν προτεραιότητα αν δηλώσουν τον όμιλο — δεν τοποθετούνται υποχρεωτικά. Τις θέσεις τις ορίζει η διαχείριση.")
      : message("warn", me.phase === "setup" ? "Η φάση των εκπαιδευτικών δεν έχει ανοίξει ακόμα." : "Οι λίστες κλείδωσαν. Για διορθώσεις επικοινωνήστε με τη διαχείριση."),
    sorted.length ? el("label", { for: "club" }, "Ο όμιλός σας", picker) : message("warn", "Δεν υπάρχουν όμιλοι ακόμα."),
    holder);
  draw();
}

function clubCard(club, me) {
  const chosen = new Set(club.list);
  const out = el("div");
  const full = () => chosen.size >= club.capacity;
  const grades = [...new Set(club.eligible.map((s) => s.grade))].sort();
  const gradeFilter = el("select", { "aria-label": "Τάξη", disabled: grades.length < 2 },
    el("option", { value: "" }, "Όλες οι τάξεις"), grades.map((g) => el("option", { value: g }, `${g} τάξη`)));
  const search = el("input", { type: "search", placeholder: "Αναζήτηση με όνομα ή ΑΜ", "aria-label": "Αναζήτηση μαθητή" });
  const onlyChosen = el("input", { type: "checkbox" });
  const count = el("span.badge");
  const chosenBox = el("div.small");
  const fullNote = el("div");
  const listBox = el("div.check-list", { role: "group", "aria-label": `Μαθητές για «${club.name}»` });
  club.eligible.sort((a, b) => a.grade.localeCompare(b.grade, "el") || a.surname.localeCompare(b.surname, "el") || a.name.localeCompare(b.name, "el"));
  const byAm = new Map(club.eligible.map((s) => [s.am, s]));
  const label = (s) => `${s.surname} ${s.name}`;

  const renderCount = () => {
    count.textContent = `${chosen.size} / ${club.capacity}`;
    count.className = `badge ${chosen.size > club.capacity ? "warn" : "primary"}`;
    fullNote.replaceChildren(me.canEdit && full() ? message("info", `Η λίστα έφτασε τις ${club.capacity} θέσεις του ομίλου. Για να προσθέσετε άλλον, αφαιρέστε πρώτα κάποιον.`) : "");
    chosenBox.replaceChildren(chosen.size
      ? el("span", {}, "Επιλεγμένοι: ", [...chosen].map((am, i) => el("span.chip", {},
        i ? " " : "", byAm.get(am) ? label(byAm.get(am)) : `ΑΜ ${am}`,
        me.canEdit ? el("button.small.link", { type: "button", "aria-label": `Αφαίρεση ${byAm.get(am)?.surname ?? am}`, onclick: () => { chosen.delete(am); renderCount(); syncBoxes(); } }, "✕") : null)))
      : el("span.muted", {}, "Κανένας — χωρίς λίστα, όσοι δηλώσουν τον όμιλο κρίνονται με την κλήρωση."));
  };
  const renderList = () => {
    const q = normalizeName(search.value);
    const rows = club.eligible.filter((s) => (!gradeFilter.value || s.grade === gradeFilter.value)
      && (!onlyChosen.checked || chosen.has(s.am))
      && (!q || normalizeName(label(s)).includes(q) || s.am.startsWith(search.value.trim())));
    listBox.replaceChildren(...rows.map((s) => {
      const box = el("input", { type: "checkbox", checked: chosen.has(s.am), disabled: !me.canEdit || (!chosen.has(s.am) && full()), dataset: { am: s.am } });
      box.addEventListener("change", () => { if (box.checked) chosen.add(s.am); else chosen.delete(s.am); renderCount(); syncBoxes(); });
      return el("label.check-row", {}, box, el("span", {}, label(s)), el("span.muted.small", {}, `${s.grade} · ΑΜ ${s.am}`));
    }));
    if (!rows.length) listBox.replaceChildren(el("p.muted.small", {}, "Κανένας μαθητής με αυτά τα κριτήρια."));
  };
  // Ticks update the boxes in place (the list keeps its scroll position)
  const syncBoxes = () => {
    for (const box of listBox.querySelectorAll("input[type=checkbox]")) {
      box.checked = chosen.has(box.dataset.am);
      box.disabled = !me.canEdit || (!box.checked && full());
    }
  };
  const renderAll = () => { renderCount(); renderList(); };
  for (const input of [search, gradeFilter, onlyChosen]) input.addEventListener("input", renderList);
  renderAll();

  const save = el("button.primary", { type: "button", disabled: !me.canEdit }, "Αποθήκευση");
  save.addEventListener("click", () => busy(save, out, async () => {
    await api("PUT", `/api/teacher/clubs/${club.code}`, { ams: [...chosen] });
    show(out, message("ok", "Αποθηκεύτηκε."));
  }));

  const by = { admin: "διαχείριση", "admin-file": "διαχείριση (από αρχείο)", "teacher-code": "εκπαιδευτικός (κοινός κωδικός)" }[club.updatedBy] ?? club.updatedBy;
  return el("section.card", {},
    el("h2", { style: "margin-top:0" }, club.name),
    el("p.muted.small", {}, `${club.days.map((d) => DAY_LABELS[d]).join(" + ")} · Τάξεις ${club.grades.join(", ")} · `, el("strong", {}, `${club.capacity} θέσεις`),
      club.coTeachers.length ? ` · ${me.shared ? "Εκπαιδευτικοί" : "Μαζί με"}: ${club.coTeachers.join(", ")}` : ""),
    club.coTeachers.length > (me.shared ? 1 : 0) ? message("info", "Ο όμιλος έχει κοινή λίστα για όλους τους εκπαιδευτικούς του. Συνεννοηθείτε ώστε να τη συμπληρώσει ένας.") : null,
    club.updatedAt ? el("p.small.muted", {}, `Τελευταία αλλαγή: ${by}, ${formatDateTime(club.updatedAt)}`) : null,
    el("h3", {}, "Προτιμώμενοι μαθητές ", count),
    chosenBox,
    fullNote,
    el("div.filters", {}, search, gradeFilter, el("label.inline", {}, onlyChosen, " μόνο οι επιλεγμένοι")),
    listBox,
    out,
    el("div.actions", {}, save));
}

window.addEventListener("hashchange", () => { if (location.hash.includes("token=")) start(); });
start();
