// A club's list, as the teacher (and the admin's «Διόρθωση») edits it: every
// eligible student in one of three states, ⭐ preferred (priority, up to the
// seats), ○ neutral (the lottery decides) or ⛔ excluded (never placed in
// the club). A closed list excludes everyone without ⭐. Excluded students
// do not see the club in their parents' form. While parents declare,
// exclusions may only be lifted (the server checks it too).

import { el, message, show } from "./ui.js";
import { normalizeName } from "/lib/import/names.js";

const WISH = "wish";
const NONE = "none";
const OUT = "out";
const STATES = [
  [WISH, "⭐", "Προτίμηση"],
  [NONE, "○", "Ουδέτερος (κλήρωση)"],
  [OUT, "⛔", "Αποκλεισμός"],
];

/**
 * @param {{
 *   club: {code: number, name: string, capacity: number},
 *   eligible: {am: string, surname: string, name: string, grade: string, section?: string}[],
 *   list: {ams?: string[], blocked?: string[], closed?: boolean},
 *   canEdit: boolean,
 *   parentsPhase: boolean,   // exclusions may only be lifted
 *   capacityInput?: HTMLInputElement, // the admin's: the seats may change before saving
 *   onSave: (payload: {ams: string[], blocked: string[], closed: boolean}) => Promise<object>,
 *   onDirty?: (dirty: boolean) => void,
 * }} options
 * @returns {HTMLElement}
 */
export function listEditor({ club, eligible, list, canEdit, parentsPhase, capacityInput, onSave, onDirty = () => {} }) {
  const students = [...eligible].sort((a, b) => a.grade.localeCompare(b.grade, "el") || a.surname.localeCompare(b.surname, "el") || a.name.localeCompare(b.name, "el"));
  const byAm = new Map(students.map((s) => [s.am, s]));
  const label = (s) => `${s.surname} ${s.name}`;
  const capacity = () => (capacityInput ? Number(capacityInput.value) || 0 : club.capacity);

  let saved = { ams: [...(list.ams ?? [])], blocked: [...(list.blocked ?? [])], closed: Boolean(list.closed) };
  const wish = new Set(saved.ams);
  const blocked = new Set(saved.blocked);
  let closed = saved.closed;
  // Excluded when the page opened: while parents declare, only these may stay ⛔
  const excludedAtStart = () => new Set(students.filter((s) => (saved.closed ? !saved.ams.includes(s.am) : saved.blocked.includes(s.am))).map((s) => s.am));
  let mayStayOut = excludedAtStart();

  const stateOf = (am) => (wish.has(am) ? WISH : closed || blocked.has(am) ? OUT : NONE);
  const counts = () => {
    const c = { [WISH]: 0, [NONE]: 0, [OUT]: 0 };
    for (const s of students) c[stateOf(s.am)]++;
    return c;
  };
  const full = () => wish.size >= capacity();
  const canBeOut = (am) => !parentsPhase || mayStayOut.has(am);
  const allowed = (am, state) => {
    if (!canEdit) return false;
    if (state === WISH) return wish.has(am) || !full();
    if (state === NONE) return !closed;
    return canBeOut(am); // in a closed list: takes the ⭐ away
  };
  const setState = (am, state) => {
    wish.delete(am);
    blocked.delete(am);
    if (state === WISH) wish.add(am);
    if (state === OUT) blocked.add(am);
  };

  // ---------- filters ----------
  const out = el("div");
  const grades = [...new Set(students.map((s) => s.grade))].sort();
  const sections = [...new Set(students.map((s) => s.section).filter(Boolean))].sort((a, b) => a.localeCompare(b, "el", { numeric: true }));
  const gradeFilter = el("select", { "aria-label": "Τάξη", disabled: grades.length < 2 },
    el("option", { value: "" }, "Όλες οι τάξεις"), grades.map((g) => el("option", { value: g }, `${g} τάξη`)));
  const sectionFilter = sections.length
    ? el("select", { "aria-label": "Τμήμα" }, el("option", { value: "" }, "Όλα τα τμήματα"), sections.map((x) => el("option", { value: x }, `Τμήμα ${x}`)))
    : null;
  const stateFilter = el("select", { "aria-label": "Κατάσταση" },
    el("option", { value: "" }, "Όλοι"), STATES.map(([v, icon, text]) => el("option", { value: v }, `${icon} ${text}`)));
  const search = el("input", { type: "search", placeholder: "Αναζήτηση με όνομα ή ΑΜ", "aria-label": "Αναζήτηση μαθητή" });
  const visible = () => {
    const q = normalizeName(search.value);
    return students.filter((s) => (!gradeFilter.value || s.grade === gradeFilter.value)
      && (!sectionFilter?.value || s.section === sectionFilter.value)
      && (!stateFilter.value || stateOf(s.am) === stateFilter.value)
      && (!q || normalizeName(label(s)).includes(q) || s.am.startsWith(search.value.trim())));
  };

  // ---------- list type ----------
  const name = `mode-${club.code}-${Math.random().toString(36).slice(2, 7)}`;
  const openRadio = el("input", { type: "radio", name, value: "open" });
  const closedRadio = el("input", { type: "radio", name, value: "closed" });
  const modeBox = el("fieldset.mode", {},
    el("legend.small", {}, "Τύπος λίστας"),
    el("label.inline", {}, openRadio, el("span", {}, el("strong", {}, "Ανοιχτή"), " — όσοι δεν έχουν ⭐ ή ⛔ κρίνονται με την κλήρωση.")),
    el("label.inline", {}, closedRadio, el("span", {}, el("strong", {}, "Κλειστή"), " — ο όμιλος δέχεται ", el("em", {}, "μόνο"), " τους ⭐· όλοι οι άλλοι είναι ⛔. Αν δεν τους έρθουν όλοι, μένουν κενές θέσεις.")));
  const question = el("div");
  openRadio.addEventListener("change", () => {
    if (!openRadio.checked || !closed) return;
    // Closed → open: the others become neutral, or stay excluded (a block list)
    const others = students.filter((s) => !wish.has(s.am)).map((s) => s.am);
    closed = false;
    for (const am of others) blocked.add(am);
    show(question, el("div.msg.info", {},
      el("p", { style: "margin:0 0 6px" }, `Η λίστα έγινε ανοιχτή. Οι ${others.length} μαθητές χωρίς ⭐ τι να γίνουν;`),
      el("div.actions", { style: "margin:0" },
        el("button.small", { type: "button", onclick: () => { show(question, null); update(); } }, "Να μείνουν ⛔ (αποκλεισμένοι)"),
        el("button.small", {
          type: "button",
          disabled: parentsPhase,
          onclick: () => { for (const am of others) blocked.delete(am); show(question, null); update(); },
        }, "Να γίνουν ○ (κλήρωση)"))));
    update();
  });
  closedRadio.addEventListener("change", () => {
    if (!closedRadio.checked || closed) return;
    toClosed();
  });
  function toClosed() {
    const add = students.filter((s) => !wish.has(s.am) && !canBeOut(s.am));
    if (add.length) {
      closedRadio.checked = false; openRadio.checked = true;
      show(out, message("warn", `Αφού άνοιξαν οι δηλώσεις, δεν μπορούν να προστεθούν αποκλεισμοί: η κλειστή λίστα θα απέκλειε ${add.length} μαθητές που τώρα δέχεται ο όμιλος.`));
      return;
    }
    closed = true;
    blocked.clear();
    show(question, null);
    update();
  }

  // ---------- quick actions ----------
  const bulk = (state) => {
    const rows = visible().filter((s) => stateOf(s.am) !== state);
    if (state === WISH) {
      const free = capacity() - wish.size;
      if (rows.length > free) {
        show(out, message("warn", `Δεν χωρούν: θα έπαιρναν ⭐ ${rows.length} μαθητές, ενώ οι ελεύθερες θέσεις είναι ${free}. Περιορίστε τη λίστα με τα φίλτρα ή επιλέξτε έναν έναν.`));
        return;
      }
    }
    if (state === OUT) {
      const refused = rows.filter((s) => !canBeOut(s.am));
      if (refused.length) {
        show(out, message("warn", `Αφού άνοιξαν οι δηλώσεις, δεν μπορούν να προστεθούν αποκλεισμοί (${refused.length} μαθητές).`));
        return;
      }
    }
    for (const s of rows) setState(s.am, state);
    show(out, null);
    update();
  };
  const bulkButtons = el("div.actions", { style: "margin:4px 0 8px" },
    el("span.small", {}, "Σε όσους εμφανίζονται:"),
    el("button.small", { type: "button", onclick: () => bulk(WISH) }, "⭐ Προτίμηση"),
    el("button.small", { type: "button", onclick: () => bulk(NONE) }, "○ Ουδέτεροι"),
    el("button.small", { type: "button", onclick: () => bulk(OUT) }, "⛔ Αποκλεισμός"));
  // The two shortcuts of the request: wish list → closed list, and the reverse
  const shortcuts = el("div.actions", { style: "margin:4px 0" });
  const renderShortcuts = () => {
    const c = counts();
    const items = [];
    if (canEdit && !closed && wish.size && c[NONE] + c[OUT] > 0) {
      items.push(el("button.small", { type: "button", onclick: () => { closedRadio.checked = true; toClosed(); } },
        `Μόνο οι ⭐: όλοι οι υπόλοιποι (${c[NONE] + c[OUT]}) → ⛔`));
    }
    if (canEdit && !closed && c[OUT] && c[NONE] && c[NONE] <= capacity() - wish.size) {
      items.push(el("button.small", {
        type: "button",
        onclick: () => { for (const s of students) if (stateOf(s.am) === NONE) setState(s.am, WISH); update(); },
      }, `Όλοι οι μη αποκλεισμένοι (${c[NONE]}) → ⭐`));
    }
    shortcuts.replaceChildren(...items);
  };

  // ---------- rows ----------
  const listBox = el("div.check-list", { role: "group", "aria-label": `Μαθητές για «${club.name}»` });
  const rowFor = (s) => {
    const state = stateOf(s.am);
    const buttons = STATES.map(([v, icon, text]) => el(`button.s-${v}`, {
      type: "button",
      "aria-pressed": String(state === v),
      "aria-label": `${text}: ${label(s)}`,
      title: text,
      disabled: state !== v && !allowed(s.am, v),
      onclick: () => { setState(s.am, v); show(out, null); update(); },
    }, icon));
    return el(`div.tri-row.${state}`, {},
      el("span.tri", { role: "group", "aria-label": label(s) }, buttons),
      el("span.tri-name", {}, label(s)),
      el("span.muted.small", {}, `${s.grade}${s.section ? ` · ${s.section}` : ""} · ΑΜ ${s.am}`));
  };
  const renderList = () => {
    const rows = visible();
    // Keep the scroll position when the rows are redrawn
    const top = listBox.scrollTop;
    listBox.replaceChildren(...(rows.length ? rows.map(rowFor) : [el("p.muted.small", { style: "padding:0 12px" }, "Κανένας μαθητής με αυτά τα κριτήρια.")]));
    listBox.scrollTop = top;
  };

  // ---------- summary ----------
  const summary = el("div");
  const isSaved = () => closed === saved.closed && wish.size === saved.ams.length && saved.ams.every((am) => wish.has(am))
    && (closed || (blocked.size === saved.blocked.length && saved.blocked.every((am) => blocked.has(am))));
  const unsavedNote = el("span.small.muted");
  const renderSummary = () => {
    const c = counts();
    const cap = capacity();
    const accepted = c[WISH] + c[NONE];
    const chips = wish.size
      ? el("div.small", {}, "⭐ ", [...wish].map((am) => el("span.chip", {}, byAm.get(am) ? label(byAm.get(am)) : `ΑΜ ${am}`,
        canEdit && (!closed || canBeOut(am)) ? el("button.small.link", { type: "button", "aria-label": `Αφαίρεση ${byAm.get(am)?.surname ?? am}`, onclick: () => { setState(am, closed ? OUT : NONE); update(); } }, "✕") : null)))
      : null;
    let meaning;
    if (closed) {
      meaning = c[WISH] === 0
        ? message("warn", "Κλειστή λίστα χωρίς μαθητές: ο όμιλος δεν θα δεχτεί κανέναν.")
        : message(c[WISH] < cap ? "warn" : "info", `Κλειστή λίστα: ο όμιλος δέχεται μόνο αυτούς τους ${c[WISH]}, αν τον δηλώσουν.${c[WISH] < cap ? ` Θα μείνουν τουλάχιστον ${cap - c[WISH]} κενές θέσεις.` : ""}`);
    } else if (c[OUT]) {
      meaning = message(accepted < cap ? "warn" : "info", `Ο όμιλος δεν δέχεται ${c[OUT]} μαθητές· τον βλέπουν μόνο οι υπόλοιποι ${accepted}.${accepted < cap ? ` Θα μείνουν τουλάχιστον ${cap - accepted} κενές θέσεις.` : ""}`);
    } else {
      meaning = el("p.small.muted", {}, c[WISH] ? "Οι ⭐ έχουν προτεραιότητα αν δηλώσουν τον όμιλο· οι υπόλοιποι κρίνονται με την κλήρωση." : "Χωρίς λίστα: όσοι δηλώσουν τον όμιλο κρίνονται με την κλήρωση.");
    }
    show(summary,
      el("div.list-summary", {},
        el(`span.badge.${wish.size > cap ? "warn" : "primary"}`, {}, `⭐ ${c[WISH]} / ${cap} θέσεις`),
        el("span.badge", {}, `○ ${c[NONE]}`),
        el(`span.badge${c[OUT] ? ".err" : ""}`, {}, `⛔ ${c[OUT]}`)),
      canEdit && full() ? el("p.small.muted", {}, `Οι ⭐ έφτασαν τις ${cap} θέσεις του ομίλου· για να δώσετε ⭐ σε άλλον, αφαιρέστε πρώτα κάποιον.`) : null,
      meaning, chips);
    unsavedNote.textContent = isSaved() ? "" : "Μη αποθηκευμένες αλλαγές";
    onDirty(!isSaved());
  };

  function update() {
    openRadio.checked = !closed;
    closedRadio.checked = closed;
    openRadio.disabled = closedRadio.disabled = !canEdit;
    renderSummary();
    renderShortcuts();
    renderList();
  }
  for (const input of [search, gradeFilter, sectionFilter, stateFilter].filter(Boolean)) input.addEventListener("input", renderList);
  capacityInput?.addEventListener("input", update);

  const save = el("button.primary", { type: "button", disabled: !canEdit }, "Αποθήκευση");
  save.addEventListener("click", async () => {
    save.disabled = true;
    try {
      const entry = await onSave({ ams: [...wish], blocked: closed ? [] : [...blocked], closed });
      saved = { ams: [...(entry.ams ?? [])], blocked: [...(entry.blocked ?? [])], closed: Boolean(entry.closed) };
      mayStayOut = excludedAtStart();
      update();
      show(out, message("ok", "Αποθηκεύτηκε."),
        entry.liftedAfterSubmission ? message("warn", `${entry.liftedAfterSubmission} από τους μαθητές που δεν είναι πια αποκλεισμένοι έχουν ήδη δήλωση χωρίς αυτόν τον όμιλο. Ο όμιλος θα ληφθεί υπόψη μόνο αν ο γονέας ξαναμπεί και αποθηκεύσει τη δήλωση (θα του εμφανιστεί στο τέλος της σειράς). Ενημερώστε τη διαχείριση.`) : null);
    } catch (err) {
      show(out, message("err", err.message));
    } finally {
      save.disabled = !canEdit;
    }
  });

  update();
  return el("div", {},
    modeBox,
    question,
    summary,
    parentsPhase && canEdit ? message("info", "Οι δηλώσεις των γονέων είναι ανοιχτές: μπορείτε να αφαιρέσετε αποκλεισμούς (⛔ → ○ ή ⭐), όχι να προσθέσετε.") : null,
    el("div.filters", {}, search, gradeFilter, sectionFilter, stateFilter),
    canEdit ? el("div", {}, bulkButtons, shortcuts) : null,
    listBox,
    out,
    el("div.actions", {}, save, unsavedNote));
}
