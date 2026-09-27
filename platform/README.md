# Πλατφόρμα δήλωσης & κατανομής ομίλων

Προδιαγραφές: [`SPEC.md`](SPEC.md). Πρότυπο ομίλων/εκπαιδευτικών: [`templates/omiloi_protypo.xlsx`](templates/omiloi_protypo.xlsx).

## Τοπική εκτέλεση

Χρειάζεται μόνο Node.js ≥ 20 (χωρίς εγκατάσταση πακέτων):

```sh
cd platform
npm run dev -- --demo     # δοκιμαστικό σχολείο: 60 μαθητές, 12 όμιλοι
# ή
npm run dev               # κρατά τα δεδομένα στο .data/dev-store.json
npm run dev -- --demo --no-mail   # όπως τώρα στο Netlify: χωρίς email
```

Ανοίξτε http://localhost:8888 — διαχείριση: `/admin.html`, κωδικός `admin` (αλλάζει με `ADMIN_PASSWORD=…`).
Τα email δεν στέλνονται τοπικά: εμφανίζονται στην κονσόλα και στην καρτέλα «Εξερχόμενα» της διαχείρισης (εκεί είναι και οι σύνδεσμοι εισόδου των εκπαιδευτικών).
Για ανέβασμα αρχείων Excel ο browser φορτώνει το SheetJS από το cdn.sheetjs.com (χρειάζεται internet).

Δοκιμαστικοί λογαριασμοί στο `--demo`: εκπαιδευτικός `etheatr@sch.gr`· γονέας π.χ. ΑΜ 9022, ΓΕΩΡΓΙΟΥ ΕΛΕΝΗ, πατέρας ΙΩΑΝΝΗΣ, μητέρα ΔΕΣΠΟΙΝΑ (ο κωδικός γονέων ορίζεται από τη διαχείριση).

| Φάκελος | Περιεχόμενο |
|---|---|
| `public/` | Σελίδες: `index.html`, `parent.html`, `teacher.html`, `admin.html` (απλή HTML/JS, χωρίς build) |
| `src/server/` | API (`app.js`), αποθήκευση (`store.js`), sessions/κωδικοί (`auth.js`) |
| `dev/` | Τοπικός server και δοκιμαστικά δεδομένα |
| `netlify/functions/api.mjs` | Το ίδιο API στο Netlify, με αποθήκευση Supabase (`src/server/store-supabase.js`, πίνακες: `supabase/schema.sql`) |

## Αλγόριθμος (`src/algorithm/`)

Καθαρή JavaScript, χωρίς εξαρτήσεις — τρέχει ίδια σε Netlify Functions και στον browser.

| Αρχείο | Τι κάνει |
|---|---|
| `lottery.js` | Κλήρωση: ένας σταθερός αριθμός ανά μαθητή από δημοσιευμένο seed (`drawLottery`). |
| `allocate.js` | Deferred Acceptance ανά ημέρα, Δευτέρα → Παρασκευή, με πολυήμερους ομίλους (`allocateWeek`). |
| `validate.js` | Έλεγχοι δήλωσης γονέα, λίστας εκπαιδευτικού, ομίλων. |

Προτεραιότητα ομίλου: επιλογή εκπαιδευτικού → θέση του ομίλου στη λίστα του μαθητή (μετά την επαναρίθμηση) → κλήρωση.

## Ανάγνωση αρχείων (`src/import/`)

| Αρχείο | Τι κάνει |
|---|---|
| `students.js` | «Κατάλογος Μαθητών» του myschool → μαθητές (ΑΜ, τάξη, 4 ονόματα) + προβλήματα ανά γραμμή. |
| `clubs.js` | Πρότυπο ομίλων → όμιλοι + εκπαιδευτικοί + προβλήματα ανά φύλλο/γραμμή. |
| `readiness.js` | Έλεγχοι με βάση και τα δύο αρχεία (ημέρα χωρίς όμιλο για κάποια τάξη, λίγες θέσεις). |
| `names.js` | Κανονικοποίηση ονομάτων και κανόνες ταύτισης για την είσοδο γονέα. |
| `legacy.js` | **Μόνο για δοκιμή:** περσινές δηλώσεις ανά ημέρα (μορφή `dailyresponses.csv`) → δηλώσεις «Εισαγωγή δοκιμής». |
| `csv.js` | CSV σε UTF-8 ή Windows-1253 (ελληνικό Excel), με `,` ή `;`. |
| `workbook.js` | .xls/.xlsx → γραμμές κελιών μέσω SheetJS (το δίνει ο καλών· στον browser από το cdn.sheetjs.com). |

Κάθε πρόβλημα είναι `error` (το αρχείο δεν γίνεται δεκτό) ή `warning` (γίνεται δεκτό, να το δει ο διαχειριστής).

## Tests

```sh
cd platform
npm test
```

Τα tests με πραγματικά αρχεία Excel (`test/workbook.test.js`, εικονικά δεδομένα στο `test/fixtures/`) χρειάζονται το SheetJS: τοπικά παραλείπονται αν λείπει, στο CI εγκαθίσταται η επίσημη έκδοση και είναι υποχρεωτικά.

Διαδρομή σε browser (Playwright + Chromium) όλης της χρονιάς στο δοκιμαστικό σχολείο:

```sh
npm run dev -- --demo &
node e2e/walkthrough.mjs --shots /tmp/shots
```

Τρέχουν αυτόματα σε κάθε αλλαγή στο `platform/` (GitHub Actions, `.github/workflows/platform-tests.yml`).

## Σύγκριση με το R

Η κατανομή υπάρχει και σε R, για εκτέλεση offline: φάκελος [`R/`](../R/README.md). Το `src/export/rPackage.js` φτιάχνει τα αρχεία που διαβάζει το `R/run_week.R`.

`reference/compare.mjs`: ίδια δεδομένα σε πλατφόρμα και R (μέσω του πακέτου εξαγωγής· το R ξαναϋπολογίζει και ελέγχει την κλήρωση από το seed), σύγκριση τοποθετήσεων.

```sh
cd platform
node reference/compare.mjs --random 8   # χρειάζεται Rscript + dplyr, readr, tidyr, purrr, stringr, writexl
```

Τα πραγματικά αρχεία μαθητών **δεν** μπαίνουν στο αποθετήριο.
