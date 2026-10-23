// Browser walk-through of the whole year on the demo school.
//   npm run dev -- --demo      (in another terminal)
//   node e2e/walkthrough.mjs [--shots <dir>]
// Needs Playwright (npx playwright / global install) and Chromium.

import { tmpdir } from "node:os";
import { createRequire } from "node:module";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";

const require = createRequire(import.meta.url);
let chromium;
try {
  ({ chromium } = require("playwright"));
} catch {
  ({ chromium } = require("/opt/node22/lib/node_modules/playwright"));
}

const BASE = process.env.BASE_URL ?? "http://localhost:8888";
// Hidden addresses of the teachers' and admin's pages (dev server defaults)
const TEACHER_URL = `${BASE}/${process.env.TEACHER_PATH ?? "ekpaideutikoi-dev"}/`;
const ADMIN_URL = `${BASE}/${process.env.ADMIN_PATH ?? "diaxeirisi-dev"}/`;
const shotsArg = process.argv.indexOf("--shots");
const shots = shotsArg > 0 ? process.argv[shotsArg + 1] : null;
if (shots) mkdirSync(shots, { recursive: true });

const browser = await chromium.launch();
const errors = [];
const newPage = async (viewport = { width: 1100, height: 900 }) => {
  const page = await browser.newPage({ viewport, locale: "el-GR" });
  page.on("pageerror", (e) => errors.push(`pageerror: ${e.message}`));
  page.on("console", (m) => { if (m.type() === "error" && !/404|Failed to load resource/.test(m.text())) errors.push(`console: ${m.text()}`); });
  return page;
};
const shot = async (page, name) => { if (shots) await page.screenshot({ path: join(shots, `${name}.png`), fullPage: true }); };
const step = (text) => console.log(`• ${text}`);
const api = async (path, { token, method = "GET", body } = {}) => {
  const res = await fetch(`${BASE}${path}`, { method, headers: { "content-type": "application/json", ...(token ? { authorization: `Bearer ${token}` } : {}) }, body: body && JSON.stringify(body) });
  return res.json();
};

try {
  // ---------- Admin: login, move to teachers' phase ----------
  const admin = await newPage();
  await admin.goto(ADMIN_URL);
  await admin.fill("#pw", "admin");
  await admin.click("button[type=submit]");
  await admin.waitForSelector("ol.steps");
  step("admin: logged in");
  await shot(admin, "01-admin-overview");
  admin.on("dialog", (d) => d.accept());
  await admin.click("text=Επόμενη φάση: Εκπαιδευτικοί");
  await admin.waitForSelector("ol.steps li.current:has-text('Εκπαιδευτικοί')");
  step("admin: phase → teachers");
  await admin.click("role=tab[name='Όμιλοι']");
  await admin.waitForSelector("table td:has-text('Ρομποτική')");
  await shot(admin, "02-admin-clubs");

  // ---------- Teacher: login link (by e-mail, or from the admin) ----------
  const { mailEnabled } = await api("/api/public");
  const teacher = await newPage();
  await teacher.goto(TEACHER_URL);
  let link;
  if (mailEnabled) {
    await teacher.fill("#email", "EThEatr@sch.gr");
    await teacher.click("button[type=submit]");
    await teacher.waitForSelector(".msg.ok");
    const { token: adminToken } = await api("/api/admin/login", { method: "POST", body: { password: "admin" } });
    const { outbox } = await api("/api/admin/outbox", { token: adminToken });
    link = outbox.at(-1).text.match(/http\S+/)[0];
    step("teacher: link requested by e-mail");
  } else {
    await teacher.waitForSelector(".msg.info:has-text('διαχείριση')");
    const row = admin.locator("tr", { hasText: "etheatr@sch.gr" });
    await row.locator("button:has-text('Σύνδεσμος εισόδου')").click();
    link = (await row.locator("textarea").inputValue()).match(/http\S+/)[0];
    await shot(admin, "02b-admin-teacher-link");
    step("teacher: link created by the admin (no e-mail service)");
  }
  await teacher.goto(link);
  await teacher.waitForSelector("h2:has-text('Αντιγόνη')");
  step("teacher: logged in with the magic link");
  if (await teacher.locator("input[type=number]").count()) throw new Error("teachers must not edit the capacity");
  await teacher.fill("input[type=search]", "γεωργ");
  await teacher.locator(".tri-list .tri button.s-wish >> nth=0").click();
  await teacher.fill("input[type=search]", "");
  await teacher.locator(".tri-list .tri-row.none button.s-wish >> nth=0").click();
  const badge = await teacher.locator(".counter.c-wish").textContent();
  if (badge.trim() !== "⭐ 2 / 20") throw new Error(`teacher count: ${badge}`);
  await teacher.click("text=Αποθήκευση");
  await teacher.waitForSelector(".msg.ok:has-text('Αποθηκεύτηκε')");
  step("teacher: two students ticked and saved (capacity shown, not editable)");
  await shot(teacher, "03-teacher");

  // Teachers' common code: set by the admin; any club, chosen from a list
  await admin.click("role=tab[name='Πορεία & ρυθμίσεις']");
  await admin.waitForSelector(`td code:has-text('${TEACHER_URL}')`);
  await admin.fill("#tpw", "kodikos-ek");
  await admin.click("text=Αποθήκευση ρυθμίσεων");
  await admin.waitForSelector(".msg.ok:has-text('Αποθηκεύτηκε')");
  const colleague = await newPage();
  colleague.on("dialog", (d) => d.accept());
  await colleague.goto(TEACHER_URL);
  await colleague.fill("#tcode", "λάθος-κωδικός");
  await colleague.fill("#tid", "700107");
  await colleague.click("button[type=submit]");
  await colleague.waitForSelector(".msg.err:has-text('ΑΜ/ΑΦΜ')");
  await colleague.fill("#tcode", "kodikos-ek");
  await colleague.click("button[type=submit]");
  // Only this teacher's two clubs
  await colleague.waitForSelector("h2:has-text('Μαθηματικοί γρίφοι')");
  if ((await colleague.locator("section.card h2").count()) !== 2) throw new Error("the teacher must see only their own two clubs");
  // One tab per club: only the chosen club's list is shown
  if ((await colleague.locator("nav.club-tabs >> role=tab").count()) !== 2) throw new Error("one tab per club expected");
  if ((await colleague.locator("section.card:visible").count()) !== 1) throw new Error("only one club's list at a time");
  const puzzles = colleague.locator("section.card:has(h2:has-text('Μαθηματικοί γρίφοι'))");
  const algebra = colleague.locator("section.card:has(h2:has-text('Άλγεβρα'))");
  // The bulk buttons take whoever the filters show
  // Changing everyone shown appears only once the rows are narrowed (and asks first)
  const wishBadge = (card) => card.locator(".counter.c-wish").textContent().then((t) => t.trim());
  const outBadge = (card) => card.locator(".counter.c-out").textContent().then((t) => t.trim());
  if (await puzzles.locator(".bulk button").count()) throw new Error("bulk change shown without a filter");
  await puzzles.locator("input[type=search]").fill("γεωργ");
  // With a search, the amber box offers changes for the rows shown only
  await puzzles.locator(".changes-title:has-text('μόνο για')").waitFor();
  if (await puzzles.locator(".changes button:has-text('Κλειστή λίστα')").count()) throw new Error("«Κλειστή λίστα» offered while a search narrows the rows");
  await puzzles.locator(".bulk button:has-text('⭐')").click();
  const picked = Number((await wishBadge(puzzles)).match(/⭐ (\d+)/)[1]);
  if (picked < 1) throw new Error("bulk ⭐ picked nobody");
  await puzzles.locator(".bulk button:has-text('○')").click();
  if (await wishBadge(puzzles) !== "⭐ 0 / 15") throw new Error("bulk ○ left some ⭐");
  await puzzles.locator(".bulk button:has-text('⭐')").click();
  await puzzles.locator("input[type=search]").fill("");
  await puzzles.locator("text=Μη αποθηκευμένες αλλαγές").waitFor();
  await puzzles.locator("button:has-text('Αποθήκευση')").click();
  await puzzles.locator(".msg.ok:has-text('Αποθηκεύτηκε')").waitFor();
  if (await puzzles.locator("text=Μη αποθηκευμένες αλλαγές").count()) throw new Error("still marked unsaved after saving");
  // Wish list → closed list in one click; the others become ⛔
  await colleague.click("role=tab[name=/Άλγεβρα/]");
  if (!(await colleague.locator("role=tab[name=/Άλγεβρα/]").evaluate((b) => b === document.activeElement))) throw new Error("the chosen tab lost the focus");
  if (await puzzles.isVisible()) throw new Error("the other club's list is still shown");
  await algebra.locator(".tri-list .tri button.s-wish >> nth=0").click();
  // From the ⭐ filter alone (not with a search), and only after a question
  await algebra.locator("button.counter.c-wish").click();
  await algebra.locator("input[type=search]").fill("α");
  if (await algebra.locator(".changes button:has-text('Κλειστή λίστα')").count()) throw new Error("«Κλειστή λίστα» offered with ⭐ + search");
  await algebra.locator("input[type=search]").fill("");
  await algebra.locator(".changes button:has-text('Κλειστή λίστα')").click();
  await colleague.locator("dialog.ask:has-text('Κλειστή λίστα;')").waitFor();
  await colleague.click("dialog.ask button[value=cancel]");
  if (await algebra.locator(".list-type:has-text('Κλειστή λίστα')").count()) throw new Error("«Ακύρωση» still closed the list");
  await algebra.locator(".changes button:has-text('Κλειστή λίστα')").click();
  await colleague.click("dialog.ask button[value=ok]");
  await algebra.locator(".list-type:has-text('Κλειστή λίστα'):has-text('μόνο τον 1 ⭐')").waitFor();
  await algebra.locator("button.counter.c-wish").click();
  // One ○ makes it open again, no switch; «Καθαρισμός» clears everything
  await algebra.locator(".tri-list .tri-row.out button.s-none >> nth=0").click();
  await algebra.locator(".list-type:has-text('Ανοιχτή λίστα με αποκλεισμούς')").waitFor();
  await algebra.locator("button:has-text('Καθαρισμός')").click();
  if (await outBadge(algebra) !== "⛔ 0" || await wishBadge(algebra) !== "⭐ 0 / 15") throw new Error("«Καθαρισμός» left some ⭐/⛔");
  await algebra.locator(".tri-list .tri button.s-wish >> nth=0").click();
  // One ⛔: the student will not see the club
  await algebra.locator(".tri-list .tri-row.none button.s-out >> nth=0").click();
  await algebra.locator("button:has-text('Αποθήκευση')").click();
  // A new ⛔ asks before saving
  await colleague.locator("dialog.ask:has-text('Νέοι αποκλεισμοί: 1')").waitFor();
  await colleague.click("dialog.ask button[value=ok]");
  await algebra.locator(".msg.ok:has-text('Αποθηκεύτηκε')").waitFor();
  // After a reload both lists are still there
  await colleague.reload();
  // The chosen tab is kept after a reload
  await colleague.locator("role=tab[name=/Άλγεβρα/][selected=true]").waitFor();
  await algebra.waitFor();
  if (await wishBadge(puzzles) !== `⭐ ${picked} / 15`) throw new Error("first club's list not kept");
  if (await wishBadge(algebra) !== "⭐ 1 / 15") throw new Error("second club's list not kept");
  if (await outBadge(algebra) !== "⛔ 1") throw new Error("the ⛔ was not kept");
  await colleague.click("header button:has-text('Βοήθεια')");
  await colleague.waitForSelector("#help-panel h2:has-text('Σύνδεση')");
  await shot(colleague, "03b-teacher-code");
  await algebra.locator(".tri-list .tri-row.none button.s-wish >> nth=0").click(); // unsaved: logging out asks first
  await colleague.locator("role=tab[name=/Άλγεβρα.*●/]").waitFor(); // the tab marks unsaved changes
  await colleague.click("text=Αποσύνδεση εκπαιδευτικού");
  await colleague.waitForSelector("#tcode");
  step("teacher: common code + own ΑΜ/ΑΦΜ → only their clubs; select/clear all; lists kept after a reload; in-page help");

  // Admin: lists for other clubs from a file (CSV: club code + ΑΜ per row)
  await admin.click("role=tab[name='Όμιλοι']");
  const csvPath = join(tmpdir(), `listes-${process.pid}.csv`);
  writeFileSync(csvPath, "Κωδικός ομίλου;ΑΜ\n105;9001\n105;9002\n108;9041\n");
  await admin.setInputFiles("input[aria-label='Αρχεία λιστών εκπαιδευτικών']", csvPath);
  await admin.waitForSelector("button:has-text('Αποθήκευση 2 λιστών'):not([disabled])");
  await admin.click("button:has-text('Αποθήκευση 2 λιστών')");
  await admin.waitForSelector("tr:has-text('Σκάκι') td:has-text('⭐ 2')");
  step("admin: two teachers' lists imported from a file, after the preview");
  // The admin's «Διόρθωση»: the teachers' editor, plus the seats
  const chess = admin.locator("tr:has-text('Σκάκι')");
  await chess.locator("button:has-text('Διόρθωση')").click();
  const editor = admin.locator("tr.list-edit");
  // «Ακύρωση αλλαγών» brings back the saved list and seats
  const seats = await editor.locator("input[aria-label='Χωρητικότητα']").inputValue();
  await editor.locator("input[aria-label='Χωρητικότητα']").fill("99");
  await editor.locator(".tri-list .tri-row.none button.s-wish >> nth=0").click();
  await editor.locator(".unsaved:has-text('Μη αποθηκευμένες αλλαγές')").waitFor();
  await editor.locator("button:has-text('Ακύρωση αλλαγών')").click();
  if (await editor.locator("input[aria-label='Χωρητικότητα']").inputValue() !== seats) throw new Error("«Ακύρωση αλλαγών» did not restore the seats");
  if (await editor.locator(".tri-list .tri-row.wish").count() !== 2 || await editor.locator(".unsaved").textContent()) throw new Error("«Ακύρωση αλλαγών» did not restore the list");
  await editor.locator(".tri-list .tri-row.none button.s-out >> nth=0").click();
  await editor.locator("button:has-text('Αποθήκευση')").click();
  await admin.click("dialog.ask button[value=ok]");
  await editor.locator(".msg.ok:has-text('Αποθηκεύτηκε')").waitFor();
  await shot(admin, "02c-admin-list-editor");
  await editor.locator("button:has-text('Κλείσιμο')").click();
  await admin.waitForSelector("tr:has-text('Σκάκι') td:has-text('⭐ 2 · ⛔ 1')");
  step("admin: «Ακύρωση αλλαγών» in «Διόρθωση», then one ⛔ added");
  // Two «Διορθώσεις» open: closing the clean one keeps the other's edits
  const fixRows = admin.locator("tr:has(button:has-text('Διόρθωση'))");
  await fixRows.nth(0).locator("button:has-text('Διόρθωση')").click();
  await fixRows.nth(1).locator("button:has-text('Διόρθωση')").click();
  const [firstEdit, secondEdit] = [admin.locator("tr.list-edit").nth(0), admin.locator("tr.list-edit").nth(1)];
  await firstEdit.locator(".tri-list .tri-row.none button.s-out >> nth=0").click();
  await secondEdit.locator("button:has-text('Κλείσιμο')").click();
  if (await admin.locator("tr.list-edit").count() !== 1 || !(await firstEdit.locator(".unsaved").textContent())) throw new Error("closing one «Διόρθωση» lost the other's edits");
  await firstEdit.locator("button:has-text('Ακύρωση αλλαγών')").click();
  await firstEdit.locator("button:has-text('Κλείσιμο')").click();
  if (await admin.locator("tr.list-edit").count()) throw new Error("«Διόρθωση» not closed");
  step("admin: two «Διορθώσεις» open — closing one keeps the other");

  // ---------- Admin: settings, open declarations ----------
  await admin.click("role=tab[name='Πορεία & ρυθμίσεις']");
  // Unsaved settings: another tab asks first (no → stay), «Ακύρωση αλλαγών» brings back what is saved
  const school = await admin.inputValue("#school");
  await admin.fill("#school", "Λάθος όνομα");
  await admin.fill("#tpw", "xyzxyz");
  let leaveAsked = "";
  admin.removeAllListeners("dialog");
  admin.once("dialog", (d) => { leaveAsked = d.message(); d.dismiss(); });
  await admin.click("role=tab[name='Αρχεία']");
  admin.on("dialog", (d) => d.accept());
  if (!leaveAsked.includes("δεν αποθηκεύσατε") || await admin.inputValue("#school") !== "Λάθος όνομα") throw new Error(`changing tab with unsaved settings: ${leaveAsked}`);
  await admin.click("button:has-text('Ακύρωση αλλαγών')");
  if (await admin.inputValue("#school") !== school || await admin.inputValue("#tpw")) throw new Error("«Ακύρωση αλλαγών» did not restore the settings");
  await admin.fill("#ppw", "omiloi2026");
  const inTwoDays = new Date(Date.now() + 2 * 86400000);
  await admin.fill("#deadline", new Date(inTwoDays.getTime() - inTwoDays.getTimezoneOffset() * 60000).toISOString().slice(0, 16));
  await admin.click("text=Αποθήκευση ρυθμίσεων");
  await admin.waitForSelector(".msg.ok:has-text('Αποθηκεύτηκε')");
  await admin.click("text=Επόμενη φάση: Δηλώσεις γονέων");
  // The check before opening: clubs some students will not see
  const opening = admin.locator("dialog.ask");
  await opening.locator("h3:has-text('Αποκλεισμοί')").waitFor();
  if (!(await opening.textContent()).includes("«Σκάκι»")) throw new Error(`opening check: ${await opening.textContent()}`);
  await shot(admin, "04a-admin-opening-check");
  await admin.click("dialog.ask button[value=ok]");
  await admin.waitForSelector("ol.steps li.current:has-text('Δηλώσεις γονέων')");
  step("admin: settings — «Ακύρωση αλλαγών»; parents' password + deadline, phase → parents");

  // ---------- Parent (phone size) ----------
  const parent = await newPage({ width: 390, height: 844 });
  await parent.goto(`${BASE}/`);
  for (const href of await parent.locator("a").evaluateAll((as) => as.map((a) => a.getAttribute("href")))) {
    if (!["/", "/help.html"].includes(href)) throw new Error(`unexpected link on the parents' page: ${href}`);
  }
  await parent.fill("#password", "omiloi2026");
  await parent.fill("#am", "9022");
  await parent.fill("#surname", "Γεωργίου");
  await parent.fill("#name", "Ελένη");
  await parent.fill("#father", "Γιάννης");
  await parent.fill("#mother", "Δέσποινα");
  await parent.click("button[type=submit]");
  await parent.waitForSelector(".msg.err");
  step("parent: wrong father's name → generic error");
  await shot(parent, "04-parent-login-error");
  await parent.fill("#father", "ιωάννης");
  await parent.click("button[type=submit]");
  await parent.waitForSelector("h1:has-text('ΓΕΩΡΓΙΟΥ ΕΛΕΝΗ')");
  step("parent: logged in (lower case, accents)");

  // Monday: move the last club to position 1 by typing, then drag another
  const mon = parent.locator("ol.ranker[aria-label='Σειρά ομίλων Δευτέρα']");
  const names = async () => mon.locator("li .name").evaluateAll((ns) => ns.map((n) => n.firstChild.textContent));
  const before = await names();
  await mon.locator("li >> nth=2").locator("input.pos").fill("1");
  await mon.locator("li >> nth=2").locator("input.pos").press("Enter");
  const afterType = await names();
  if (afterType[0] !== before[2]) throw new Error(`typing a position failed: ${afterType}`);
  // Drag the first club below the middle of the second one.
  await mon.locator("li >> nth=1").scrollIntoViewIfNeeded();
  const h0 = await mon.locator("li >> nth=0").locator(".handle").boundingBox();
  const li1 = await mon.locator("li >> nth=1").boundingBox();
  await parent.mouse.move(h0.x + h0.width / 2, h0.y + h0.height / 2);
  await parent.mouse.down();
  await parent.mouse.move(h0.x + h0.width / 2, li1.y + li1.height * 0.8, { steps: 8 });
  await parent.mouse.up();
  const afterDrag = await names();
  if (afterDrag[1] !== afterType[0] || afterDrag[0] !== afterType[1]) throw new Error(`dragging failed: ${afterDrag}`);
  step(`parent: reorder by typing and by dragging (${afterDrag.join(" › ")})`);
  await parent.fill("#pname", "Γιάννης Γεωργίου");
  await parent.fill("#pemail", "parent@example.com");

  // Submitting before opening the other days asks first (dismissed here)
  let asked = "";
  parent.once("dialog", (d) => { asked = d.message(); d.dismiss(); });
  await parent.click("button:has-text('Υποβολή δήλωσης')");
  await parent.waitForTimeout(300);
  if (!asked.includes("Δεν έχετε δει τη σειρά για: Τρίτη")) throw new Error(`no reminder for unseen days: ${asked}`);
  if (await parent.locator(".receipt").count()) throw new Error("submitted although the reminder was dismissed");
  step("parent: reminder for days not opened yet");

  // Tuesday: «Ποδόσφαιρο» (Tuesday + Thursday) to position 1; it clashes
  // with «Αντιγόνη» (Monday + Thursday), which is decided first.
  const antigoneRank = afterDrag.findIndex((n) => n.includes("Αντιγόνη")) + 1;
  await parent.click("nav.day-tabs button:has-text('Τρίτη')");
  const tue = parent.locator("ol.ranker[aria-label='Σειρά ομίλων Τρίτη']");
  await tue.locator("li:has-text('Ποδόσφαιρο') input.pos").fill("1");
  await tue.locator("li:has-text('Ποδόσφαιρο') input.pos").press("Enter");
  const football = await tue.locator("li:has-text('Ποδόσφαιρο')").textContent();
  if (!football.includes("Αν το παιδί μπει στον όμιλο «Θεατρική παράσταση «Αντιγόνη»» τη Δευτέρα, αυτός παραλείπεται")) throw new Error(`clash note: ${football}`);
  step("parent: clash note on Tuesday's double club");

  // Thursday's «Επιτραπέζια παιχνίδια» is similar to Tuesday's «Σκάκι»
  await parent.click("nav.day-tabs button:has-text('Πέμπτη')");
  const games = await parent.locator("ol.ranker[aria-label='Σειρά ομίλων Πέμπτη'] li:has-text('Επιτραπέζια')").textContent();
  if (!games.includes("Παρεμφερής με «Σκάκι» (Τρίτη)")) throw new Error(`similar note: ${games}`);
  step("parent: similar-club note on Thursday");

  // Thursday: one list; the locked clubs keep their first-day numbers
  // («Ποδόσφαιρο» 1, «Αντιγόνη» 3) and the free clubs take 2 and 4.
  await parent.click("nav.day-tabs button:has-text('Πέμπτη')");
  const thu = parent.locator("ol.ranker[aria-label='Σειρά ομίλων Πέμπτη']");
  await thu.waitFor();
  const rows = await thu.locator("li").evaluateAll((lis) => lis.map((li) =>
    `${li.querySelector(".lockpos")?.textContent ?? li.querySelector("input.pos").value}${li.classList.contains("locked") ? "🔒" : ""} ${li.querySelector(".name").firstChild.textContent}`));
  const expected = antigoneRank === 3 ? ["1🔒 Ποδόσφαιρο", "2 ", "3🔒 Θεατρική παράσταση «Αντιγόνη»", "4 "] : null;
  if (!expected || rows.length !== 4 || rows.some((r, i) => !r.startsWith(expected[i]))) throw new Error(`Thursday list: ${rows.join(" | ")}`);
  const lockedText = await thu.locator("li.locked:has-text('Ποδόσφαιρο')").textContent();
  if (!lockedText.includes("1η επιλογή της Τρίτης") || !lockedText.includes("Μετράει μόνο αν δεν μπει στον όμιλο «Θεατρική παράσταση «Αντιγόνη»»")) throw new Error(`locked text: ${lockedText}`);
  step(`parent: Thursday is one list — ${rows.join(" · ")}`);
  await shot(parent, "05-parent-ranking");

  // Visit every day with «next», then submit on the last one
  await parent.click("nav.day-tabs button:has-text('Δευτέρα')");
  for (const day of ["Τρίτη", "Τετάρτη", "Πέμπτη", "Παρασκευή"]) await parent.click(`button:has-text('${day} →')`);
  await parent.click("button:has-text('Υποβολή δήλωσης')");
  await parent.waitForSelector(".msg.ok:has-text('καταχωρίστηκε')");
  const code = await parent.locator(".receipt strong").nth(2).textContent();
  if (!/^[0-9A-F]{4}-[0-9A-F]{4}$/.test(code)) throw new Error(`no receipt code: ${code}`);
  step(`parent: submission saved, receipt ${code}`);
  await shot(parent, "06-parent-submitted");

  // «Δήλωση για άλλο παιδί» keeps the parents' password, the rest is empty
  await parent.click("button:has-text('Δήλωση για άλλο παιδί')");
  await parent.waitForSelector("#am");
  if (!(await parent.inputValue("#password")) || await parent.inputValue("#am")) throw new Error("next child: form not prepared");
  await parent.fill("#am", "9022");
  await parent.fill("#surname", "Γεωργίου");
  await parent.fill("#name", "Ελένη");
  await parent.fill("#father", "ιωάννης");
  await parent.fill("#mother", "Δέσποινα");
  await parent.click("button[type=submit]");
  await parent.waitForSelector("h1:has-text('ΓΕΩΡΓΙΟΥ ΕΛΕΝΗ')");
  step("parent: «another child» keeps only the parents' password");

  // «Επαναφορά της Δευτέρας» brings back the first-visit order; «Διαγραφή
  // δήλωσης» deletes the saved declaration. Then the same one is made again.
  if ((await names()).join() !== afterDrag.join()) throw new Error(`saved Monday order not shown: ${await names()}`);
  // A change not saved: logging out asks (no → stay); «Ακύρωση αλλαγών» brings back the saved order
  await mon.locator("li >> nth=0").locator("button[aria-label$='μία θέση κάτω']").click();
  await parent.locator(".unsaved:has-text('Μη αποθηκευμένες αλλαγές')").waitFor();
  let logoutAsked = "";
  parent.once("dialog", (d) => { logoutAsked = d.message(); d.dismiss(); });
  await parent.click("#logout");
  if (!logoutAsked.includes("δεν αποθηκεύσατε") || !(await parent.locator("ol.ranker").count())) throw new Error(`logout with unsaved changes: ${logoutAsked}`);
  await parent.click("button:has-text('Ακύρωση αλλαγών')");
  if ((await names()).join() !== afterDrag.join()) throw new Error(`«Ακύρωση αλλαγών» did not restore Monday: ${await names()}`);
  if (await parent.locator("button:has-text('Ακύρωση αλλαγών')").isVisible()) throw new Error("«Ακύρωση αλλαγών» still shown");
  step("parent: unsaved change — logout asks, «Ακύρωση αλλαγών» restores the saved order");
  parent.once("dialog", (d) => d.accept());
  await parent.click("button:has-text('Επαναφορά της Δευτέρας')");
  if ((await names()).join() !== before.join()) throw new Error(`Monday reset failed: ${await names()}`);
  parent.once("dialog", (d) => d.accept());
  await parent.click("button:has-text('Διαγραφή δήλωσης')");
  await parent.waitForSelector(".msg.ok:has-text('Η δήλωση διαγράφηκε')");
  if (await parent.locator(".receipt").count() || await parent.inputValue("#pname")) throw new Error("declaration still shown after deleting");
  if ((await names()).join() !== before.join()) throw new Error(`after deleting, Monday is not in the first-visit order: ${await names()}`);
  step("parent: reset Monday, delete the declaration");
  await shot(parent, "06b-parent-deleted");
  for (const [i, club] of afterDrag.entries()) {
    const j = (await names()).indexOf(club);
    await mon.locator(`li >> nth=${j}`).locator("input.pos").fill(String(i + 1));
    await mon.locator(`li >> nth=${j}`).locator("input.pos").press("Enter");
  }
  if ((await names()).join() !== afterDrag.join()) throw new Error(`Monday order not restored: ${await names()}`);
  await parent.click("nav.day-tabs button:has-text('Τρίτη')");
  await tue.locator("li:has-text('Ποδόσφαιρο') input.pos").fill("1");
  await tue.locator("li:has-text('Ποδόσφαιρο') input.pos").press("Enter");
  await parent.fill("#pname", "Γιάννης Γεωργίου");
  await parent.fill("#pemail", "parent@example.com");
  await parent.click("nav.day-tabs button:has-text('Δευτέρα')");
  for (const day of ["Τρίτη", "Τετάρτη", "Πέμπτη", "Παρασκευή"]) await parent.click(`button:has-text('${day} →')`);
  await parent.click("button:has-text('Υποβολή δήλωσης')");
  await parent.waitForSelector(".msg.ok:has-text('καταχωρίστηκε')");
  step("parent: the same declaration submitted again");

  // A teacher lifts a ⛔ while the form is open (the club was hidden when it
  // loaded): the save is refused, the form is redrawn with the club last and
  // the parent's order kept, and the next save goes through.
  const { token: adminToken } = await api("/api/admin/login", { method: "POST", body: { password: "admin" } });
  const parentToken = await parent.evaluate(() => sessionStorage.getItem("omiloi.parent.token"));
  // The last club of the saved Monday: it comes back last, so the declaration ends up the same
  const meNow = await api("/api/parent/me", { token: parentToken });
  const monClub = meNow.days.find((d) => d.day === "mon").clubs.find((c) => c.code === meNow.submission.preferences.mon.at(-1));
  const monList = (await api("/api/admin/state", { token: adminToken })).teacherLists[monClub.code] ?? {};
  const putList = (blocked) => api(`/api/admin/teacher-lists/${monClub.code}`, { token: adminToken, method: "PUT", body: { ams: monList.ams ?? [], blocked, closed: false } });
  await putList([...(monList.blocked ?? []), "9022"]);
  await parent.reload();
  await mon.waitFor();
  if ((await names()).includes(monClub.name)) throw new Error("the ⛔ club is still on the form");
  const monShown = await names();
  await putList(monList.blocked ?? []);
  await parent.click("button:has-text('Αποθήκευση αλλαγών')");
  await parent.waitForSelector(".msg.warn:has-text('οι όμιλοι άλλαξαν όσο ήταν ανοιχτή η σελίδα')");
  await parent.locator(`.msg.warn:has-text('προστέθηκε ο όμιλος «${monClub.name}»')`).waitFor();
  if ((await names()).join() !== [...monShown, monClub.name].join()) throw new Error(`redrawn Monday: ${await names()}`);
  await parent.click("button:has-text('Αποθήκευση αλλαγών')");
  await parent.waitForSelector(".msg.ok:has-text('καταχωρίστηκε')");
  step("parent: a club unhidden while the form was open — save refused, form redrawn with it last, saved again");

  // Two more parents via the API, so the allocation has competition
  for (const [am, surname, name, father, mother] of [["9021", "ΠΑΠΑΔΟΠΟΥΛΟΣ", "ΝΙΚΟΛΑΟΣ", "ΓΕΩΡΓΙΟΣ", "ΕΛΕΝΗ ΜΑΡΙΑ"], ["9041", "ΠΑΠΑΔΟΠΟΥΛΟΣ", "ΝΙΚΟΛΑΟΣ", "ΓΕΩΡΓΙΟΣ", "ΕΛΕΝΗ"]]) {
    const { token } = await api("/api/parent/login", { method: "POST", body: { password: "omiloi2026", am, surname, name, father, mother } });
    if (!token) throw new Error(`api login failed for ${am}`);
    const me = await api("/api/parent/me", { token });
    const preferences = Object.fromEntries(me.days.filter((d) => d.clubs.length).map((d) => [d.day, d.clubs.map((c) => c.code)]));
    await api("/api/parent/submission", { token, method: "PUT", body: { parent: { name: "Γονέας", email: `${am}@example.com` }, preferences } });
  }
  step("two more submissions through the API");

  // ---------- Admin: close, allocate, publish ----------
  await admin.click("text=Επόμενη φάση: Κλειστές δηλώσεις");
  await admin.waitForSelector("ol.steps li.current:has-text('Κλειστές')");
  await admin.click("role=tab[name='Κατανομή']");
  await admin.fill("#seed", "Κλήρωση δοκιμής");
  await admin.click("text=Εκτέλεση κατανομής");
  await admin.waitForSelector("h2:has-text('Αποτελέσματα')");
  step("admin: allocation run");
  await admin.click("role=tab[name='Κενά ανά ημέρα']");
  await admin.waitForSelector("th:has-text('Χωρίς προτιμήσεις')");
  await admin.click("role=tab[name='Αναφορά μαθητή']");
  await admin.fill("input[aria-label='Αναζήτηση μαθητή για αναφορά']", "9022");
  await admin.click("button:has-text('Εμφάνιση αναφοράς')");
  await admin.waitForSelector(".report:has-text('Αριθμός κλήρωσης')");
  step("admin: gaps per day and a student's report");
  await admin.click("role=tab[name='Στατιστικά']");
  await admin.waitForSelector(".viz-tile:has-text('Πήραν την 1η επιλογή')");
  if ((await admin.locator(".viz-seg").count()) === 0) throw new Error("no stacked bars in the statistics");
  await admin.locator(".viz-seg >> nth=0").hover();
  await admin.waitForSelector(".viz-tip:not([hidden])");
  await shot(admin, "07c-admin-stats");
  await admin.setViewportSize({ width: 390, height: 844 });
  await shot(admin, "07d-admin-stats-phone");
  await admin.setViewportSize({ width: 1100, height: 900 });
  step("admin: statistics with charts (tooltip on hover)");
  await admin.click("role=tab[name='Ανά μαθητή']");
  await admin.waitForSelector("th:has-text('Πατρώνυμο')");
  if ((await admin.locator("div[role=tabpanel] tbody tr").count()) < 60) throw new Error("the week table should list every student");
  await admin.click("role=tab[name='Παρουσιολόγια']");
  await admin.waitForSelector("th:has-text('Α/Α')");
  await shot(admin, "07e-admin-rosters");
  step("admin: the week per student and the clubs' rosters on screen");
  await shot(admin, "07-admin-results");
  for (const [label, file] of [["Audit log κατανομής (.csv)", "audit"], ["Επαλήθευση κατανομής (.xlsx)", "verification"], ["Σύνοψη ανά όμιλο (.csv)", "summary"], ["Κλήρωση: ΑΜ και αριθμός (.csv)", "lottery"]]) {
    const [d] = await Promise.all([admin.waitForEvent("download"), admin.click(`button:has-text('${label}')`)]);
    step(`admin: ${file} downloaded (${d.suggestedFilename()})`);
  }
  const [rounds] = await Promise.all([admin.waitForEvent("popup"), admin.click("button:has-text('Γύροι κατανομής')")]);
  await rounds.waitForSelector(".club tr.in");
  if (await rounds.isVisible("#warning")) throw new Error(`round view: ${await rounds.textContent("#warning")}`);
  if (!(await rounds.textContent("#source")).includes("Κατανομή της πλατφόρμας")) throw new Error("round view: not loaded from the platform");
  await rounds.click("#clubs .pill[data-code] >> nth=0");
  if ((await rounds.locator(".club").count()) !== 1) throw new Error("round view: the club filter should leave one club");
  await shot(rounds, "07f-admin-rounds");
  await rounds.close();
  step("admin: the allocation round by round, in a new tab");
  const [download] = await Promise.all([admin.waitForEvent("download"), admin.click("text=Δεδομένα για R (.zip)")]);
  step(`admin: R package downloaded (${download.suggestedFilename()})`);
  await admin.click("text=Επόμενη φάση: Ανακοίνωση");
  await admin.waitForSelector("ol.steps li.current:has-text('Ανακοίνωση')");
  step("admin: published");
  // The teacher sees who got into each club and why
  await colleague.fill("#tcode", "kodikos-ek");
  await colleague.fill("#tid", "700107");
  await colleague.click("button[type=submit]");
  await colleague.locator(".allocation:visible h3:has-text('Η κατανομή του ομίλου')").waitFor();
  if (!(await colleague.locator(".allocation:visible").textContent()).includes("Τον ζήτησαν")) throw new Error("teacher: no allocation summary");
  await shot(colleague, "08b-teacher-allocation");
  step("teacher: after the announcement, who got in and why");

  await parent.reload();
  await parent.waitForSelector("h2:has-text('Αποτελέσματα κατανομής')");
  await parent.click("summary:has-text('Πώς προέκυψε')");
  await parent.waitForSelector(".report li:has-text('Αίτηση στον όμιλο')");
  step("parent: sees the result and how it came about");
  await shot(parent, "08-parent-result");

  await admin.click("role=tab[name='Ιστορικό']");
  await admin.waitForSelector("td:has-text('Κατανομή')");
  await admin.click("role=tab[name='Αρχεία']");
  await admin.waitForSelector("h2:has-text('Τι υπάρχει ήδη στη βάση')");
  step("admin: history and database status");
  await shot(admin, "08b-admin-status");

  // Sections from the two myschool reports (fictional, for the demo's ΑΜ 9001–9060)
  const defsPath = join(tmpdir(), `tmimata-${process.pid}.csv`);
  const sectPath = join(tmpdir(), `mathites-${process.pid}.csv`);
  writeFileSync(defsPath, "Γενικά στοιχεία τμημάτων;;;;\r\nΑ/Α;Τμήμα;Τάξη;Τύπος τμήματος;Μάθημα\r\n" +
    ["Α", "Β", "Γ"].flatMap((g, i) => [`${2 * i + 1};${g}1;${g};Γενικής Παιδείας Λυκείου;Ιστορία`, `${2 * i + 2};${g}-ΑΓΓΛΙΚΑ 1;${g};Ξένων Γλωσσών Λυκείου;Αγγλικά`]).join("\r\n"));
  writeFileSync(sectPath, "Τμήματα μαθητών;;;;;\r\n" + ["Α", "Β", "Γ"].map((g, i) =>
    `Τάξη Εγγραφής: ;;;${g};;\r\nΑ/Α;Αριθμός μητρώου;Επώνυμο μαθητή;Όνομα μαθητή;Όνομα πατέρα;Τμήματα\r\n` +
    Array.from({ length: 20 }, (_, k) => `${k + 1};${9001 + 20 * i + k};Χ;Χ;Χ;${g}-ΑΓΓΛΙΚΑ 1, ${g}1`).join("\r\n")).join("\r\n"));
  await admin.click("summary:has-text('Τμήματα μαθητών')");
  await admin.setInputFiles("input[aria-label='Αρχεία τμημάτων']", [defsPath, sectPath]);
  await admin.waitForSelector("#sections-out .msg.ok:has-text('Τμήμα για 60 από τους 60 μαθητές')");
  await admin.waitForSelector("td:has-text('60 από 60 μαθητές με τμήμα')");
  await admin.click("role=tab[name='Κατανομή']");
  await admin.click("role=tab[name='Ανά μαθητή']");
  await admin.waitForSelector("th:has-text('Τμήμα')");
  await admin.waitForSelector("tbody td:text-is('Β1')");
  step("admin: students' sections from the two myschool reports, shown in the week table");

  // Students tab: a header sorts by its column, a second click reverses
  await admin.click("role=tab[name='Μαθητές']");
  await admin.locator("th[aria-sort=ascending]:has-text('Μαθητής')").waitFor(); // alphabetical by default
  const amColumn = () => admin.locator("tbody tr td:first-child").allInnerTexts().then((t) => t.map(Number));
  const isSorted = (v, dir) => v.every((x, i) => i === 0 || dir * (x - v[i - 1]) >= 0);
  await admin.click("th button:has-text('ΑΜ')");
  await admin.locator("th[aria-sort=ascending]:has-text('ΑΜ')").waitFor();
  if (!isSorted(await amColumn(), 1)) throw new Error("students not sorted by AM (ascending)");
  await admin.click("th button:has-text('ΑΜ')");
  await admin.locator("th[aria-sort=descending]:has-text('ΑΜ')").waitFor();
  if (!isSorted(await amColumn(), -1)) throw new Error("students not sorted by AM (descending)");
  for (const dir of ["ascending", "descending"]) {
    await admin.click("th button:has-text('Δήλωση')");
    await admin.locator(`th[aria-sort=${dir}]:has-text('Δήλωση')`).waitFor();
    const done = await admin.locator("tbody tr").evaluateAll((rows) => rows.map((r) => !!r.querySelector(".badge.ok")));
    if (done.indexOf(false) !== -1 && done.lastIndexOf(true) > done.indexOf(false)) throw new Error(`students without a declaration must stay last (${dir})`);
  }
  step("admin: students table sorted by clicking the headers");

  await admin.click("role=tab[name='Βοήθεια']");
  await admin.waitForSelector("h2:has-text('1. Προετοιμασία')");
  await admin.click("role=tab[name='Εκπαιδευτικοί']");
  await admin.waitForSelector("h2:has-text('Τι σημαίνουν ⭐ και ⛔')");
  await parent.click("header a:has-text('Βοήθεια')");
  await parent.waitForSelector("h2:has-text('Η δήλωση')");
  if (await parent.locator("role=tab").count()) throw new Error("the parents' help must not show the teachers' help");
  step("help: admin tab and the public help page");
  await shot(parent, "08c-help");

  // ---------- Admin: reset after the trial ----------
  await admin.click("role=tab[name='Πορεία & ρυθμίσεις']");
  await admin.click("summary:has-text('Επαναφορά πλατφόρμας')");
  await admin.fill("#resetword", "ΔΙΑΓΡΑΦΗ");
  await admin.click("button:has-text('Διαγραφή όλων των δεδομένων')");
  await admin.waitForSelector(".msg.ok:has-text('άδειασε')");
  await admin.waitForSelector("li:has-text('Μαθητές: 0')");
  step("admin: platform reset (0 students, back to setup)");
  await shot(admin, "09-admin-reset");

  if (errors.length) throw new Error(`browser errors:\n${errors.join("\n")}`);
  console.log("\n✓ Walk-through completed without browser errors.");
} catch (err) {
  console.error(`\n✗ ${err.message}`);
  if (errors.length) console.error(errors.join("\n"));
  process.exitCode = 1;
} finally {
  await browser.close();
}
