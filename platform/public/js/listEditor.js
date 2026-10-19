// A club's list, as the teacher (and the admin's «Διόρθωση») edits it: every
// eligible student in one of three states, ⭐ preferred (priority, up to the
// seats), ○ neutral (the lottery decides) or ⛔ excluded (never placed in
// the club). Excluded students do not see the club in their parents' form.
// While parents declare, an exclusion takes the club out of a declaration
// already made (the server says how many, after saving).
//
// Open or closed is not a switch but what the states add up to (decided
// 15/10/2026): no ○ left and some ⛔ → a closed list, saved as `closed`, so
// students added to the catalogue later are excluded too. One ○ anywhere
// makes the list open again.
//
// Layout: a bar that stays on screen (list type, the counters, which also
// filter the rows, «Αποθήκευση» and «Ακύρωση αλλαγών»), then search and filters, the
// shortcuts, a box for changing all the rows shown (only once something
// narrows them), and the rows.
//
// Filters and changes look different (decided 19/10/2026, after teachers
// closed their list by mistake): the filters are grey with a funnel and
// round chips; the changes are in an amber box with a pencil and square
// buttons. Closing the list, or saving one that newly excludes students,
// asks first.

import { ask, el, message, show } from "./ui.js";
import { normalizeName } from "/lib/import/names.js";

const WISH = "wish";
const NONE = "none";
const OUT = "out";
const STATES = [
  [WISH, "⭐", "Προτίμηση"],
  [NONE, "○", "Ουδέτερος (κλήρωση)"],
  [OUT, "⛔", "Αποκλεισμός"],
];
const ICON = Object.fromEntries(STATES.map(([v, icon]) => [v, icon]));
const studentsAcc = (n) => `${n} ${n === 1 ? "μαθητή" : "μαθητές"}`;
const studentsNom = (n) => `${n} ${n === 1 ? "μαθητής" : "μαθητές"}`;

// A funnel, as on the filter buttons of spreadsheets
const funnel = () => {
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.setAttribute("viewBox", "0 0 16 16");
  svg.setAttribute("aria-hidden", "true");
  const path = document.createElementNS("http://www.w3.org/2000/svg", "path");
  path.setAttribute("d", "M1 2h14l-5.5 6.5V14l-3-1.5V8.5z");
  svg.append(path);
  return svg;
};
const viewLabel = (text) => el("span.view-label", {}, funnel(), text);

/**
 * @param {{
 *   club: {code: number, name: string, capacity: number},
 *   eligible: {am: string, surname: string, name: string, grade: string, section?: string}[],
 *   list: {ams?: string[], blocked?: string[], closed?: boolean},
 *   canEdit: boolean,
 *   parentsPhase: boolean,   // parents are declaring: explain what an exclusion does to them
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

  // Each student's state; a closed list from the server = ⛔ for everyone without ⭐
  const state = new Map();
  const load = ({ ams = [], blocked = [], closed = false }) => {
    const wished = new Set(ams.map(String));
    const out = new Set(blocked.map(String));
    for (const s of students) state.set(s.am, wished.has(s.am) ? WISH : closed || out.has(s.am) ? OUT : NONE);
  };
  load(list);
  let savedKey = "";
  const ofState = (v) => students.filter((s) => state.get(s.am) === v).map((s) => s.am);
  const counts = () => ({ [WISH]: ofState(WISH).length, [NONE]: ofState(NONE).length, [OUT]: ofState(OUT).length });
  const isClosed = () => { const c = counts(); return c[NONE] === 0 && c[OUT] > 0; };
  const payload = () => {
    const closed = isClosed();
    return { ams: ofState(WISH), blocked: closed ? [] : ofState(OUT), closed };
  };
  // What would be saved, normalized: compared with what is saved, so a list
  // whose stored `closed` differs from what its states now mean counts as unsaved
  const eligibleAms = new Set(students.map((s) => s.am));
  const normal = ({ ams = [], blocked = [], closed = false }) => JSON.stringify({
    ams: ams.map(String).filter((am) => eligibleAms.has(am)).sort(),
    blocked: closed ? [] : blocked.map(String).filter((am) => eligibleAms.has(am)).sort(),
    closed: Boolean(closed),
  });
  const key = () => normal(payload());
  savedKey = normal(list);
  const full = () => counts()[WISH] >= capacity();
  const allowed = (am, v) => canEdit && (v !== WISH || state.get(am) === WISH || !full());

  const out = el("div");
  const set = (am, v) => { state.set(am, v); };

  // ---------- filters ----------
  const grades = [...new Set(students.map((s) => s.grade))].sort();
  const sections = [...new Set(students.map((s) => s.section).filter(Boolean))].sort((a, b) => a.localeCompare(b, "el", { numeric: true }));
  const gradeFilter = grades.length > 1
    ? el("select", { "aria-label": "Τάξη" }, el("option", { value: "" }, "Όλες οι τάξεις"), grades.map((g) => el("option", { value: g }, `${g} τάξη`)))
    : null;
  const sectionFilter = sections.length
    ? el("select", { "aria-label": "Τμήμα" }, el("option", { value: "" }, "Όλα τα τμήματα"), sections.map((x) => el("option", { value: x }, `Τμήμα ${x}`)))
    : null;
  const search = el("input", { type: "search", placeholder: "Αναζήτηση με όνομα ή ΑΜ", "aria-label": "Αναζήτηση μαθητή" });
  let shown = ""; // the counters filter by state: "" = everyone
  const narrowed = () => Boolean(shown || search.value.trim() || gradeFilter?.value || sectionFilter?.value);
  const visible = () => {
    const q = normalizeName(search.value);
    return students.filter((s) => (!gradeFilter?.value || s.grade === gradeFilter.value)
      && (!sectionFilter?.value || s.section === sectionFilter.value)
      && (!shown || state.get(s.am) === shown)
      && (!q || normalizeName(label(s)).includes(q) || s.am.startsWith(search.value.trim())));
  };

  // ---------- the bar on top: list type, counters (= filters), save ----------
  const typeLine = el("div.list-type");
  const counters = el("div.counters", { role: "group", "aria-label": "Προβολή" });
  const unsavedNote = el("span.small.unsaved");
  // Back to what is saved: the states, and the admin's seats too
  const undo = el("button.small", { type: "button", onclick: () => {
    load(saved);
    if (capacityInput) capacityInput.value = savedCapacity;
    show(out, null);
    update();
  } }, "Ακύρωση αλλαγών");
  const save = el("button.primary", { type: "button", disabled: !canEdit }, "Αποθήκευση");
  let saved = { ...list };
  let savedCapacity = capacityInput?.value;
  const bar = el("div.list-bar", {}, typeLine, counters, canEdit ? el("div.actions.bar-actions", {}, save, unsavedNote, undo) : null);

  const renderBar = () => {
    const c = counts();
    const cap = capacity();
    const accepted = c[WISH] + c[NONE];
    let type;
    if (c[OUT] === 0) {
      type = el("span", {}, el("strong", {}, "Ανοιχτή λίστα"), c[WISH] ? " — οι ⭐ προηγούνται, οι υπόλοιποι κρίνονται με την κλήρωση." : " — όλοι κρίνονται με την κλήρωση.");
    } else if (isClosed()) {
      type = c[WISH]
        ? el("span", {}, el("strong", {}, "🔒 Κλειστή λίστα"), ` — ο όμιλος δέχεται μόνο ${c[WISH] === 1 ? "τον 1 ⭐" : `τους ${c[WISH]} ⭐`}.`)
        : el("span", {}, el("strong", {}, "🔒 Κλειστή λίστα"), " — όλοι είναι ⛔: ο όμιλος δεν θα δεχτεί κανέναν.");
    } else {
      type = el("span", {}, el("strong", {}, "Ανοιχτή λίστα με αποκλεισμούς"), ` — ο όμιλος δεν δέχεται ${studentsAcc(c[OUT])}.`);
    }
    const empty = c[OUT] ? Math.max(0, cap - accepted) : 0;
    typeLine.className = `list-type ${isClosed() || empty ? "warn" : ""}`;
    show(typeLine, type,
      empty ? el("span.block.small", {}, `Θα μείνουν τουλάχιστον ${empty} κενές θέσεις (ο όμιλος δέχεται ${accepted} για ${cap} θέσεις).`) : null,
      canEdit && c[WISH] >= cap ? el("span.block.small", {}, `Οι ⭐ έφτασαν τις ${cap} θέσεις· για να δώσετε ⭐ σε άλλον, αφαιρέστε πρώτα κάποιον.`) : null);

    const tab = (v, text, n) => el(`button.counter${v ? `.c-${v}` : ""}`, {
      type: "button",
      "aria-pressed": String(shown === v),
      title: v ? `Εμφάνιση μόνο: ${text}` : "Εμφάνιση όλων",
      onclick: () => { shown = shown === v ? "" : v; update(); },
    }, text, " ", el("strong", {}, String(n)));
    show(counters,
      viewLabel("Προβολή:"),
      tab("", "Όλοι", students.length),
      tab(WISH, "⭐", `${c[WISH]} / ${cap}`),
      tab(NONE, "○", c[NONE]),
      tab(OUT, "⛔", c[OUT]));

    const dirty = key() !== savedKey || (Boolean(capacityInput) && capacityInput.value !== savedCapacity);
    unsavedNote.textContent = dirty ? "Μη αποθηκευμένες αλλαγές" : "";
    undo.hidden = !dirty;
    onDirty(dirty);
  };

  // ---------- shortcuts ----------
  const shortcuts = el("div.actions.shortcuts");
  // Whole-list shortcuts only while nothing narrows the rows; with a filter
  // on, the box offers changes for the rows shown. One exception: the ⭐
  // chip alone keeps «Κλειστή λίστα», the natural step from the ⭐ list
  // (decided 19/10/2026); a search or another filter hides it.
  const starsOnly = () => shown === WISH && !search.value.trim() && !gradeFilter?.value && !sectionFilter?.value;
  const renderShortcuts = () => {
    if (!canEdit || (narrowed() && !starsOnly())) { shortcuts.replaceChildren(); return renderChanges(); }
    const c = counts();
    const items = [];
    if (c[WISH] && c[NONE]) {
      items.push(el("button.small", {
        type: "button",
        onclick: async () => {
          const n = ofState(NONE).length;
          if (!await ask({
            title: "Κλειστή λίστα;",
            tone: "warn",
            body: [
              el("p", {}, "Ο όμιλος θα δέχεται ", el("strong", {}, `μόνο ${c[WISH] === 1 ? "τον 1 ⭐" : `τους ${c[WISH]} ⭐`}`), "."),
              el("p", {}, `Οι άλλοι ${studentsNom(n)} γίνονται ⛔: δεν θα βλέπουν τον όμιλο στη φόρμα των γονέων τους.`),
              el("p.small.muted", {}, "Η αλλαγή ισχύει όταν πατήσετε «Αποθήκευση»."),
            ],
            ok: "Ναι, κλειστή λίστα",
          })) return;
          for (const am of ofState(NONE)) set(am, OUT);
          show(out, null);
          update();
        },
      }, `🔒 Κλειστή λίστα: μόνο οι ⭐ (${c[NONE] === 1 ? "ο άλλος" : `οι άλλοι ${c[NONE]}`} → ⛔)`));
    }
    if (!narrowed() && c[OUT] && c[NONE] && c[NONE] <= capacity() - c[WISH]) {
      items.push(el("button.small", {
        type: "button",
        onclick: () => { for (const am of ofState(NONE)) set(am, WISH); show(out, null); update(); },
      }, `⭐ σε όλους τους μη αποκλεισμένους (${c[NONE]})`));
    }
    if (!narrowed() && (c[WISH] || c[OUT])) {
      items.push(el("button.small", {
        type: "button",
        onclick: () => {
          if (!confirm(`Καθαρισμός της λίστας: και οι ${students.length} μαθητές θα γίνουν ○ (κλήρωση). Συνέχεια;`)) return;
          for (const s of students) set(s.am, NONE);
          show(out, null); update();
        },
      }, "○ Καθαρισμός: όλοι ουδέτεροι"));
    }
    // Under the change for the ⭐ shown: said to be for the whole list
    if (items.length && narrowed()) items.unshift(el("span.small", {}, "Για όλη τη λίστα:"));
    shortcuts.replaceChildren(...items);
    renderChanges();
  };

  // ---------- change everyone shown (only when the rows are narrowed) ----------
  const bulkBox = el("div.bulk");
  const bulk = (v) => {
    const rows = visible().filter((s) => state.get(s.am) !== v);
    if (!rows.length) return;
    if (v === WISH && rows.length > capacity() - counts()[WISH]) {
      show(out, message("warn", `Δεν χωρούν: θα έπαιρναν ⭐ ${studentsAcc(rows.length)}, ενώ οι ελεύθερες θέσεις είναι ${Math.max(0, capacity() - counts()[WISH])}. Περιορίστε τη λίστα με τα φίλτρα ή επιλέξτε έναν έναν.`));
      return;
    }
    if (!confirm(`${rows.length === 1 ? "1 μαθητής θα γίνει" : `${rows.length} μαθητές θα γίνουν`} ${ICON[v]}. Συνέχεια;`)) return;
    for (const s of rows) set(s.am, v);
    show(out, null);
    update();
  };
  const renderBulk = () => {
    const n = visible().length;
    if (!canEdit || !narrowed() || !n) { bulkBox.replaceChildren(); return renderChanges(); }
    show(bulkBox,
      [[WISH, "⭐ Προτίμηση"], [NONE, "○ Ουδέτερ" + (n === 1 ? "ος" : "οι")], [OUT, "⛔ Αποκλεισμός"]].map(([v, text]) => el("button.small", { type: "button", onclick: () => bulk(v) }, text)));
  };

  // The amber box: the whole-list shortcuts, or (with a filter on) the
  // change for the rows shown; hidden when there is nothing to offer
  const changes = el("div.changes");
  const changesTitle = el("span.changes-title");
  const renderChanges = () => {
    if (!canEdit || (!shortcuts.childElementCount && !bulkBox.childElementCount)) return changes.replaceChildren();
    const n = visible().length;
    changesTitle.textContent = bulkBox.childElementCount
      ? `✎ Αλλαγή μόνο για ${n === 1 ? "τον μαθητή" : `τους ${n} μαθητές`} που βλέπετε τώρα`
      : "✎ Αλλαγή σε όλη τη λίστα";
    if (!changes.childElementCount) {
      changes.append(
        el("div", {}, changesTitle, " ", el("span.changes-hint", {}, "— αλλάζουν τη λίστα· ισχύουν με «Αποθήκευση»")),
        bulkBox, shortcuts);
    }
  };

  // ---------- rows ----------
  const listBox = el("div.tri-list", { role: "group", "aria-label": `Μαθητές για «${club.name}»` });
  const rowFor = (s) => {
    const current = state.get(s.am);
    const buttons = STATES.map(([v, icon, text]) => el(`button.s-${v}`, {
      type: "button",
      "aria-pressed": String(current === v),
      "aria-label": `${text}: ${label(s)}`,
      title: v === WISH && !allowed(s.am, v) ? `Οι ⭐ έφτασαν τις ${capacity()} θέσεις· αφαιρέστε πρώτα κάποιον.` : text,
      disabled: current !== v && !allowed(s.am, v),
      onclick: () => { set(s.am, v); show(out, null); update(); },
    }, icon));
    return el(`div.tri-row.${current}`, {},
      el("span.tri-who", {}, el("span.tri-name", {}, label(s)), el("span.muted.small", {}, ` ${s.grade}${s.section ? ` · ${s.section}` : ""} · ΑΜ ${s.am}`)),
      el("span.tri", { role: "group", "aria-label": label(s) }, buttons));
  };
  const renderList = () => {
    const rows = visible();
    listBox.replaceChildren(...(rows.length ? rows.map(rowFor) : [el("p.muted.small", { style: "padding:0 12px" }, "Κανένας μαθητής με αυτά τα κριτήρια.")]));
  };

  function update() {
    renderBar();
    renderShortcuts();
    renderBulk();
    renderChanges();
    renderList();
  }
  for (const input of [search, gradeFilter, sectionFilter].filter(Boolean)) input.addEventListener("input", () => { renderShortcuts(); renderBulk(); renderChanges(); renderList(); });
  capacityInput?.addEventListener("input", update);

  // No edits while saving: the reply replaces the states with what was saved
  const editing = el("div.list-editing");
  // Closing the list, or excluding students who were not excluded: ask first
  const confirmSave = async () => {
    const before = new Set(saved.closed ? students.filter((s) => !(saved.ams ?? []).map(String).includes(s.am)).map((s) => s.am) : (saved.blocked ?? []).map(String));
    const fresh = ofState(OUT).filter((am) => !before.has(am));
    if (!fresh.length) return true;
    const c = counts();
    const closing = isClosed() && !saved.closed;
    const names = fresh.slice(0, 5).map((am) => label(byAm.get(am)));
    return ask({
      title: closing ? "Αποθήκευση κλειστής λίστας;" : `Νέοι αποκλεισμοί: ${fresh.length}`,
      tone: "warn",
      body: [
        closing
          ? el("p", {}, "Η λίστα είναι ", el("strong", {}, "κλειστή"), `: ο όμιλος θα δέχεται μόνο ${c[WISH] === 1 ? "τον 1 ⭐" : `τους ${c[WISH]} ⭐`}. `, `${studentsNom(c[OUT])} δεν θα βλέπουν τον όμιλο στη φόρμα των γονέων τους.`)
          : el("p", {}, `${fresh.length === 1 ? "1 μαθητής γίνεται" : `${fresh.length} μαθητές γίνονται`} ⛔: δεν θα μπουν στον όμιλο και δεν θα τον βλέπουν στη φόρμα των γονέων τους.`),
        closing ? null : el("ul", {}, names.map((n) => el("li", {}, n)), fresh.length > names.length ? el("li.muted", {}, `και ${fresh.length - names.length} ακόμα`) : null),
        parentsPhase ? el("p", {}, "Οι δηλώσεις των γονέων είναι ανοιχτές: όσοι από αυτούς έχουν ήδη δηλώσει τον όμιλο, θα τον χάσουν από τη δήλωσή τους.") : null,
      ],
      ok: closing ? "Ναι, αποθήκευση κλειστής λίστας" : "Ναι, αποθήκευση",
    });
  };
  save.addEventListener("click", async () => {
    if (!await confirmSave()) return;
    save.disabled = true;
    undo.disabled = true;
    editing.inert = true;
    editing.classList.add("saving");
    try {
      const entry = await onSave(payload());
      saved = { ams: entry.ams ?? [], blocked: entry.blocked ?? [], closed: Boolean(entry.closed) };
      load(saved);
      savedKey = normal(saved);
      if (capacityInput) {
        if (entry.capacity !== undefined) capacityInput.value = entry.capacity;
        savedCapacity = capacityInput.value;
      }
      update();
      show(out, message("ok", "Αποθηκεύτηκε."),
        entry.excludedAfterSubmission ? message("info", `${entry.excludedAfterSubmission === 1 ? "1 από τους μαθητές που αποκλείσατε τώρα είχε" : `${entry.excludedAfterSubmission} από τους μαθητές που αποκλείσατε τώρα είχαν`} ήδη δηλώσει τον όμιλο. Στην κατανομή ο όμιλος θα παραλειφθεί από τη δήλωση (η υπόλοιπη σειρά μένει ίδια), και ο γονέας θα το δει αν ξαναμπεί.`) : null,
        entry.liftedAfterSubmission ? message("warn", `${entry.liftedAfterSubmission === 1 ? "1 μαθητής που δεν είναι πια αποκλεισμένος έχει" : `${entry.liftedAfterSubmission} μαθητές που δεν είναι πια αποκλεισμένοι έχουν`} ήδη δήλωση χωρίς αυτόν τον όμιλο. Ο όμιλος θα ληφθεί υπόψη μόνο αν ο γονέας ξαναμπεί και αποθηκεύσει τη δήλωση (θα του εμφανιστεί στο τέλος της σειράς). Ενημερώστε τη διαχείριση.`) : null);
    } catch (err) {
      show(out, message("err", err.message));
    } finally {
      save.disabled = !canEdit;
      undo.disabled = false;
      editing.inert = false;
      editing.classList.remove("saving");
    }
  });

  update();
  show(editing, el("div.filters.view", {}, viewLabel("Φίλτρα:"), search, gradeFilter, sectionFilter,
    el("span.view-hint", {}, "αλλάζουν μόνο ποιους βλέπετε")), changes, listBox);
  return el("div.list-editor", {},
    bar,
    out,
    parentsPhase && canEdit ? message("info", "Οι δηλώσεις των γονέων είναι ανοιχτές. Αν αποκλείσετε (⛔) μαθητή που έχει ήδη δηλώσει τον όμιλο, ο όμιλος θα παραλειφθεί από τη δήλωσή του. Αν άρετε έναν αποκλεισμό, ο γονέας πρέπει να ξαναμπεί για να βάλει τον όμιλο στη σειρά του.") : null,
    editing);
}
