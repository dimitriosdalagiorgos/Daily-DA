# Daily-DA Overview

## Gale-Shapley / Deferred Acceptance in Plain Language
The Gale-Shapley algorithm—also known as Deferred Acceptance—matches two sets of participants based on ranked preferences while respecting capacity limits. In our context, students list clubs in order of preference and clubs have limited seats. The algorithm works iteratively:

1. Every unassigned student proposes to their highest-ranked club they have not yet tried.
2. Each club tentatively accepts up to its capacity of the best-ranked proposals it has received so far and rejects the rest.
3. Rejected students move to their next preferred club in the following round.
4. The process repeats until either all students are matched or no student has any new clubs to propose.

This approach guarantees that no student-club pair would prefer each other over their assigned outcome—a property called stability. It also means students can list clubs honestly without risking a worse outcome, because proposals are always initiated by students and clubs only make choices among the offers they receive.

## How `daily_da_audit.R` Implements the Algorithm
The `daily_da_audit.R` script is the production-ready workflow that prepares input files, runs the Deferred Acceptance loop, and generates detailed logs and reports.

1. **Loading and normalizing data** – The script ingests `dailyclubs.csv` and `dailyresponses.csv`, standardizes identifiers (like `club_id` and `student_id`), and validates that required columns exist before proceeding.
2. **Converting preferences to long format** – Student preference columns are reshaped into a long table where each row represents a student-club ranking. This makes it easy to filter out `NA`s and sort proposals by rank.
3. **Tracking algorithm state** – Empty tibbles keep track of tentative matches, attempted proposals, and a detailed audit log so every decision can be reconstructed later.
4. **Main Deferred Acceptance loop** – For each round, the script gathers unassigned students, lets them propose to their next club, and merges those proposals with existing tentative matches. Clubs then keep the highest-ranked students up to capacity and reject the rest, with every action recorded in the audit log.
5. **Finalization and reporting** – After the loop ends, the script explains why any students remain unmatched, recreates final assignments with original preference ranks, and produces CSV/Excel reports for students, parents, and clubs.

By pairing a clear implementation of Gale-Shapley with rich audit data, the repository makes it straightforward to trace each placement decision and to communicate results to stakeholders.

## Πώς εξηγείται ο αλγόριθμος Gale-Shapley στα ελληνικά
Ο αλγόριθμος Gale-Shapley—γνωστός και ως Deferred Acceptance—ταιριάζει δύο ομάδες συμμετεχόντων με βάση τις ιεραρχημένες προτιμήσεις τους, λαμβάνοντας υπόψη περιορισμούς χωρητικότητας. Στο δικό μας πλαίσιο, οι μαθητές κατατάσσουν τα clubs κατά σειρά προτίμησης και κάθε club διαθέτει συγκεκριμένο αριθμό θέσεων. Ο αλγόριθμος λειτουργεί επαναληπτικά:

1. Κάθε μαθητής που δεν έχει τοποθετηθεί ακόμα προτείνει στο club με την υψηλότερη θέση που δεν έχει δοκιμάσει.
2. Κάθε club δέχεται προσωρινά έως τη χωρητικότητά του τις καλύτερες προτάσεις που έχει λάβει μέχρι εκείνη τη στιγμή και απορρίπτει τις υπόλοιπες.
3. Οι μαθητές που απορρίπτονται προχωρούν στον επόμενο προτιμώμενο club στον επόμενο γύρο.
4. Η διαδικασία επαναλαμβάνεται μέχρι να τοποθετηθούν όλοι οι μαθητές ή μέχρι να μην έχουν νέα clubs στα οποία μπορούν να προτείνουν.

Η μέθοδος εγγυάται ότι δεν υπάρχει ζευγάρι μαθητή-club που να προτιμά ο ένας τον άλλο περισσότερο από την τελική ανάθεση—ιδιότητα που ονομάζεται «σταθερότητα». Επιπλέον, οι μαθητές μπορούν να δηλώνουν ειλικρινά τις προτιμήσεις τους χωρίς να φοβούνται χειρότερο αποτέλεσμα, επειδή οι προτάσεις ξεκινούν από αυτούς και τα clubs απλώς επιλέγουν ανάμεσα στις προσφορές που λαμβάνουν.

## Πώς το `daily_da_audit.R` υλοποιεί τον αλγόριθμο
Το script `daily_da_audit.R` είναι η πλήρης ροή εργασίας που προετοιμάζει τα αρχεία εισόδου, τρέχει τον αλγόριθμο Deferred Acceptance και δημιουργεί λεπτομερή logs και αναφορές.

1. **Φόρτωση και κανονικοποίηση δεδομένων** – Το script διαβάζει τα `dailyclubs.csv` και `dailyresponses.csv`, ενοποιεί τα αναγνωριστικά (όπως `club_id` και `student_id`) και ελέγχει ότι υπάρχουν όλες οι απαραίτητες στήλες πριν συνεχίσει.
2. **Μετατροπή προτιμήσεων σε long format** – Οι στήλες προτιμήσεων των μαθητών μετασχηματίζονται σε έναν πίνακα όπου κάθε γραμμή εκπροσωπεί ένα ζεύγος μαθητή-club με τη θέση προτίμησης. Αυτό διευκολύνει τον αποκλεισμό των `NA` και την ταξινόμηση των προτάσεων ανά rank.
3. **Παρακολούθηση κατάστασης αλγορίθμου** – Κενά tibbles αποθηκεύουν τις προσωρινές τοποθετήσεις, τις προτάσεις που έχουν δοκιμαστεί και ένα αναλυτικό audit log ώστε κάθε απόφαση να μπορεί να ανασυγκροτηθεί.
4. **Κύριος βρόχος Deferred Acceptance** – Σε κάθε γύρο, το script εντοπίζει τους μη τοποθετημένους μαθητές, τους αφήνει να προτείνουν στο επόμενο club τους και συγχωνεύει αυτές τις προτάσεις με τις υπάρχουσες προσωρινές αντιστοιχίσεις. Τα clubs διατηρούν τους μαθητές με την καλύτερη κατάταξη μέχρι τη χωρητικότητά τους και απορρίπτουν τους υπόλοιπους, με κάθε ενέργεια να καταγράφεται στο audit log.
5. **Ολοκλήρωση και αναφορές** – Μετά το τέλος του βρόχου, το script εξηγεί γιατί κάποιοι μαθητές μπορεί να παραμένουν χωρίς τοποθέτηση, ανασυνθέτει τις τελικές αναθέσεις με τις αρχικές βαθμίδες προτίμησης και παράγει αναφορές CSV/Excel για μαθητές, γονείς και clubs.

Συνδυάζοντας μια καθαρή υλοποίηση του Gale-Shapley με λεπτομερή δεδομένα audit, το αποθετήριο διευκολύνει την ανίχνευση κάθε απόφασης τοποθέτησης και την αποτελεσματική επικοινωνία των αποτελεσμάτων σε όλους τους εμπλεκόμενους.
