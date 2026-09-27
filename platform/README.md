# Πλατφόρμα δήλωσης & κατανομής ομίλων

Προδιαγραφές: [`SPEC.md`](SPEC.md). Πρότυπο ομίλων/εκπαιδευτικών: [`templates/omiloi_protypo.xlsx`](templates/omiloi_protypo.xlsx).

## Αλγόριθμος (`src/algorithm/`)

Καθαρή JavaScript, χωρίς εξαρτήσεις — τρέχει ίδια σε Netlify Functions και στον browser.

| Αρχείο | Τι κάνει |
|---|---|
| `lottery.js` | Κλήρωση: ένας σταθερός αριθμός ανά μαθητή από δημοσιευμένο seed (`drawLottery`). |
| `allocate.js` | Deferred Acceptance ανά ημέρα, Δευτέρα → Παρασκευή, με πολυήμερους ομίλους (`allocateWeek`). |
| `validate.js` | Έλεγχοι δήλωσης γονέα, λίστας εκπαιδευτικού, ομίλων. |

Προτεραιότητα ομίλου: επιλογή εκπαιδευτικού → θέση του ομίλου στη λίστα του μαθητή (μετά την επαναρίθμηση) → κλήρωση.

## Tests

```sh
cd platform
npm test
```

Τρέχουν αυτόματα σε κάθε αλλαγή στο `platform/` (GitHub Actions, `.github/workflows/platform-tests.yml`).

Τα πραγματικά αρχεία μαθητών **δεν** μπαίνουν στο αποθετήριο.
