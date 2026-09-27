import { $, busy, createApi, el, formatDateTime, message, show } from "./ui.js";
import { normalizeName } from "/lib/import/names.js";

const api = createApi("teacher");
const app = $("#app");
const logout = $("#logout");
const DAY_LABELS = { mon: "Δευτέρα", tue: "Τρίτη", wed: "Τετάρτη", thu: "Πέμπτη", fri: "Παρασκευή" };

api.onExpired = () => { api.setToken(null); loginView(message("warn", "Η σύνδεση έληξε. Ζητήστε νέο σύνδεσμο.")); };
logout.addEventListener("click", () => { api.setToken(null); loginView(); });

async function start() {
  // Magic link: /teacher.html#token=…
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

function mainView(me) {
  logout.classList.remove("hidden");
  const nodes = [
    el("h1", {}, `${me.teacher.name} ${me.teacher.surname}`),
    me.canEdit
      ? message("info", "Ορίστε τη χωρητικότητα κάθε ομίλου σας και, αν θέλετε, τους μαθητές που προτιμάτε (έως τη χωρητικότητα). Οι μαθητές της λίστας σας έχουν προτεραιότητα αν δηλώσουν τον όμιλο — δεν τοποθετούνται υποχρεωτικά.")
      : message("warn", me.phase === "setup" ? "Η φάση των εκπαιδευτικών δεν έχει ανοίξει ακόμα." : "Οι λίστες κλείδωσαν. Για διορθώσεις επικοινωνήστε με τη διαχείριση."),
  ];
  for (const club of me.clubs) nodes.push(clubCard(club, me));
  if (me.clubs.length === 0) nodes.push(message("warn", "Δεν υπάρχει όμιλος με το email σας."));
  show(app, nodes);
}

function clubCard(club, me) {
  let chosen = [...club.list];
  const byAm = new Map(club.eligible.map((s) => [s.am, s]));
  const out = el("div");
  const capacity = el("input", { id: `cap-${club.code}`, type: "number", min: 1, max: 500, value: club.capacity, disabled: !me.canEdit, style: "max-width:140px" });
  const chosenList = el("ol.ranker", { "aria-label": `Προτιμώμενοι μαθητές για «${club.name}»` });
  const count = el("span.badge");
  const search = el("input", { type: "search", placeholder: "Αναζήτηση με όνομα ή ΑΜ", "aria-label": "Αναζήτηση μαθητή", disabled: !me.canEdit });
  const results = el("div.pick-list");

  const label = (s) => `${s.surname} ${s.name} · ${s.grade} · ΑΜ ${s.am}`;
  const renderChosen = () => {
    count.textContent = `${chosen.length} / ${capacity.value}`;
    count.className = `badge ${chosen.length > Number(capacity.value) ? "warn" : "primary"}`;
    chosenList.replaceChildren(...chosen.map((am, i) => el("li", {},
      el("span.handle", { "aria-hidden": "true" }, i + 1),
      el("span.name", { style: "grid-column: span 2" }, byAm.get(am) ? label(byAm.get(am)) : `ΑΜ ${am}`),
      el("span.moves", {},
        el("button.small", { type: "button", disabled: !me.canEdit || i === 0, "aria-label": "Πάνω", onclick: () => { [chosen[i - 1], chosen[i]] = [chosen[i], chosen[i - 1]]; renderChosen(); } }, "↑"),
        el("button.small", { type: "button", disabled: !me.canEdit || i === chosen.length - 1, "aria-label": "Κάτω", onclick: () => { [chosen[i + 1], chosen[i]] = [chosen[i], chosen[i + 1]]; renderChosen(); } }, "↓"),
        el("button.small.danger", { type: "button", disabled: !me.canEdit, "aria-label": `Αφαίρεση ${byAm.get(am)?.surname ?? am}`, onclick: () => { chosen.splice(i, 1); renderChosen(); renderResults(); } }, "✕")))));
    if (chosen.length === 0) chosenList.replaceChildren(el("li.locked", {}, el("span"), el("span", {}, "Καμία προτίμηση — η σειρά θα καθοριστεί από τις δηλώσεις και την κλήρωση.")));
  };
  const renderResults = () => {
    const q = normalizeName(search.value);
    const matches = club.eligible.filter((s) => !chosen.includes(s.am) && (!q || normalizeName(`${s.surname} ${s.name}`).includes(q) || s.am.startsWith(search.value.trim()))).slice(0, 50);
    results.replaceChildren(...matches.map((s) => el("button", { type: "button", disabled: !me.canEdit, onclick: () => { chosen.push(s.am); renderChosen(); renderResults(); } }, `＋ ${label(s)}`)));
    results.classList.toggle("hidden", !me.canEdit);
  };
  capacity.addEventListener("input", renderChosen);
  search.addEventListener("input", renderResults);
  renderChosen();
  renderResults();

  const save = el("button.primary", { type: "button", disabled: !me.canEdit }, "Αποθήκευση");
  save.addEventListener("click", () => busy(save, out, async () => {
    await api("PUT", `/api/teacher/clubs/${club.code}`, { capacity: Number(capacity.value), ams: chosen });
    show(out, message("ok", "Αποθηκεύτηκε."));
  }));

  return el("section.card", {},
    el("h2", { style: "margin-top:0" }, club.name),
    el("p.muted.small", {}, `${club.days.map((d) => DAY_LABELS[d]).join(" + ")} · Τάξεις ${club.grades.join(", ")}`,
      club.coTeachers.length ? ` · Μαζί με: ${club.coTeachers.join(", ")}` : ""),
    club.coTeachers.length ? message("info", "Ο όμιλος έχει κοινή λίστα για όλους τους εκπαιδευτικούς του. Συνεννοηθείτε ώστε να τη συμπληρώσει ένας· οι άλλοι ειδοποιούνται με email σε κάθε αλλαγή.") : null,
    club.updatedAt ? el("p.small.muted", {}, `Τελευταία αλλαγή: ${club.updatedBy === "admin" ? "διαχείριση" : club.updatedBy}, ${formatDateTime(club.updatedAt)}`) : null,
    el("label", { for: `cap-${club.code}` }, "Χωρητικότητα", capacity),
    el("h3", {}, "Προτιμώμενοι μαθητές ", count),
    chosenList,
    me.canEdit ? el("div", {}, el("label", {}, "Προσθήκη μαθητή", el("span.hint", {}, `Μόνο μαθητές των τάξεων ${club.grades.join(", ")}.`), search), results) : null,
    out,
    el("div.actions", {}, save));
}

window.addEventListener("hashchange", () => { if (location.hash.includes("token=")) start(); });
start();
