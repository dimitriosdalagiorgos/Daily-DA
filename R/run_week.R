# ----------------------------------------------------------
# run_week.R
# ----------------------------------------------------------
# Offline allocation for the whole week, Monday → Friday, from the data
# package exported by the platform. Gives the same allocation as the
# platform (checked automatically in CI), using daily_da.R for each day.
#
# Multi-day clubs (as in SPEC.md and last year's manual workflow with
# remove_club_interactive.R / prepare_dailyresponses_enhanced2.R):
#   - a multi-day club takes part only on its first day;
#   - students placed in it keep the seat on its later days and sit out
#     those days' allocations;
#   - for everyone else the club leaves their later-day lists and the
#     remaining ranks are renumbered 1..k;
#   - a multi-day club that would clash with a day the student already
#     holds is removed too (ranks renumbered).
# Students are matched by RegistryNr (ΑΜ) throughout.
#
# Input folder (exported by the platform):
#   students.csv       RegistryNr, Surname, Name, grade
#   clubs.csv          code, name, days ("mon;thu"), grades ("Α;Β"), capacity
#   preferences.csv    RegistryNr, day, rank, club_code
#   teacher_lists.csv  club_code, position, RegistryNr
#   seed.txt           published seed        } at least one of the two;
#   lottery.csv        RegistryNr, lottery_number } if both, they must agree
#
# Output folder:
#   <day>/                 all reports of daily_da.R for that day
#   week_assignments.csv   RegistryNr, day, club_code, club_name, via
#   week_results.xlsx      per student, per club, per grade, without club, lottery
#   lottery.csv            the lottery used
#
# Usage:
#   Rscript R/run_week.R <input_folder> [output_folder]
# (Greek file names need a UTF-8 locale; on Windows use R ≥ 4.2.)
# ----------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(writexl)
})

args <- commandArgs(trailingOnly = TRUE)
if (!length(args) %in% c(1, 2)) stop("Usage: Rscript R/run_week.R <input_folder> [output_folder]")
input_dir <- normalizePath(args[1])
output_dir <- if (length(args) == 2) args[2] else file.path(input_dir, "results")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
output_dir <- normalizePath(output_dir)

script_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))))
da_script <- file.path(script_dir, "daily_da.R")
source(file.path(script_dir, "lottery.R"))

DAY_KEYS <- c("mon", "tue", "wed", "thu", "fri")
day_labels <- c(mon = "Δευτέρα", tue = "Τρίτη", wed = "Τετάρτη", thu = "Πέμπτη", fri = "Παρασκευή")

read_input <- function(name, required = TRUE) {
  path <- file.path(input_dir, name)
  if (!file.exists(path)) {
    if (required) stop(sprintf("Λείπει το αρχείο %s", path))
    return(NULL)
  }
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"), locale = locale(encoding = "UTF-8"))
}

students <- read_input("students.csv") %>%
  # daily_da.R needs a surname and name for its reports
  mutate(Surname = coalesce(na_if(trimws(Surname), ""), RegistryNr), Name = coalesce(na_if(trimws(Name), ""), "-"))
clubs <- read_input("clubs.csv") %>% mutate(capacity = as.integer(capacity))
prefs <- read_input("preferences.csv") %>% mutate(rank = as.integer(rank))
teacher_lists <- read_input("teacher_lists.csv", required = FALSE)
if (is.null(teacher_lists)) teacher_lists <- tibble(club_code = character(0), position = character(0), RegistryNr = character(0))

cat(sprintf("Μαθητές: %d, όμιλοι: %d, προτιμήσεις: %d\n", nrow(students), nrow(clubs), nrow(prefs)))

# ---------- Lottery ----------
seed_file <- file.path(input_dir, "seed.txt")
lottery_given <- read_input("lottery.csv", required = FALSE)
if (file.exists(seed_file)) {
  seed <- sub("\n$", "", paste(readLines(seed_file, encoding = "UTF-8", warn = FALSE), collapse = "\n"))
  lot <- draw_lottery(students$RegistryNr, seed)
  lottery <- tibble(RegistryNr = names(lot), lottery_number = unname(lot))
  cat(sprintf("Κλήρωση υπολογισμένη στο R από το seed «%s».\n", seed))
  if (!is.null(lottery_given)) {
    check <- lottery_given %>% mutate(lottery_number = as.integer(lottery_number)) %>%
      full_join(lottery, by = "RegistryNr", suffix = c("_given", "_r"))
    bad <- check %>% filter(is.na(lottery_number_given) | is.na(lottery_number_r) | lottery_number_given != lottery_number_r)
    if (nrow(bad) > 0) stop(sprintf("Η κλήρωση της πλατφόρμας ΔΕΝ συμφωνεί με το seed (%d μαθητές). Έλεγχος: %s", nrow(bad), paste(head(bad$RegistryNr, 10), collapse = ", ")))
    cat("✓ Η κλήρωση της πλατφόρμας συμφωνεί με το seed.\n")
  }
} else if (!is.null(lottery_given)) {
  seed <- NA_character_
  lottery <- lottery_given %>% mutate(lottery_number = as.integer(lottery_number))
  cat("Κλήρωση από το lottery.csv (χωρίς seed για έλεγχο).\n")
} else {
  stop("Χρειάζεται seed.txt ή lottery.csv")
}
write_csv(lottery, file.path(output_dir, "lottery.csv"))
lottery_file <- file.path(output_dir, "lottery.csv")

# ---------- Clubs ----------
# daily_da.R identifies clubs by name and finds teacher lists by file name,
# so each club gets a unique, file-safe label "<code> <name>".
clubs <- clubs %>%
  mutate(label = paste(code, gsub("[/\\\\:*?\"<>|,]", "-", trimws(name))))
label_of <- setNames(clubs$label, clubs$code)
club_days <- setNames(strsplit(clubs$days, ";", fixed = TRUE), clubs$code)
first_day <- sapply(club_days, `[`, 1)

prefs_dir <- file.path(output_dir, "teacherpreferences")
dir.create(prefs_dir, showWarnings = FALSE)
for (code in unique(teacher_lists$club_code)) {
  tl <- teacher_lists %>% filter(club_code == code) %>% arrange(as.integer(position))
  write_csv(tibble(RegistryNr = tl$RegistryNr, teacher_preference_rank = seq_len(nrow(tl))),
            file.path(prefs_dir, paste0(label_of[[code]], ".csv")))
}

# ---------- Week loop ----------
committed <- tibble(RegistryNr = character(0), day = character(0), club_code = character(0))
week <- tibble(RegistryNr = character(0), day = character(0), club_code = character(0), via = character(0))

for (day in DAY_KEYS) {
  carried <- committed %>% filter(day == !!day)
  week <- bind_rows(week, carried %>% mutate(via = "carried"))

  today_codes <- names(first_day)[first_day == day]
  if (length(today_codes) == 0) next

  # Today's lists: clubs whose allocation is today; drop multi-day clubs
  # from earlier days and clashes with days already held; renumber.
  held <- committed %>% select(RegistryNr, held_day = day)
  day_prefs <- prefs %>%
    filter(day == !!day, !RegistryNr %in% carried$RegistryNr, club_code %in% today_codes) %>%
    rowwise() %>%
    filter(!any(club_days[[club_code]] %in% held$held_day[held$RegistryNr == RegistryNr])) %>%
    ungroup() %>%
    group_by(RegistryNr) %>%
    arrange(rank, .by_group = TRUE) %>%
    mutate(rank = row_number()) %>%
    ungroup()
  if (nrow(day_prefs) == 0) next

  responses <- day_prefs %>%
    mutate(label = label_of[club_code]) %>%
    select(RegistryNr, label, rank) %>%
    pivot_wider(names_from = label, values_from = rank) %>%
    left_join(students %>% select(RegistryNr, Surname, Name), by = "RegistryNr") %>%
    select(RegistryNr, Surname, Name, everything())

  work <- file.path(output_dir, day)
  dir.create(work, showWarnings = FALSE)
  write_csv(clubs %>% filter(code %in% today_codes) %>% transmute(club_name = label, club_capacity = capacity),
            file.path(work, "dailyclubs.csv"))
  write_csv(responses, file.path(work, "dailyresponses.csv"), na = "")

  # Run in the day folder, so daily_da.R finds no stray
  # dailyresponses_original.csv and writes its reports there.
  old_wd <- setwd(work)
  status <- system2("Rscript",
                    c(shQuote(da_script), day_labels[[day]], "dailyclubs.csv", "dailyresponses.csv",
                      shQuote(prefs_dir), shQuote(lottery_file), "reports"),
                    stdout = "run.log", stderr = "run.log")
  setwd(old_wd)
  if (status != 0) stop(sprintf("Το daily_da.R απέτυχε για %s — δείτε %s", day_labels[[day]], file.path(work, "run.log")))

  assigned <- read_csv(file.path(work, "reports", sprintf("%s_assignments.csv", tolower(day_labels[[day]]))),
                       show_col_types = FALSE, col_types = cols(.default = "c"))
  assigned <- assigned %>% mutate(club_code = sub(" .*$", "", club_name))
  week <- bind_rows(week, assigned %>% transmute(RegistryNr, day = day, club_code, via = "da"))
  cat(sprintf("%s: %d τοποθετήσεις (+%d από πολυήμερους ομίλους)\n", day_labels[[day]], nrow(assigned), nrow(carried)))

  for (k in seq_len(nrow(assigned))) {
    later <- club_days[[assigned$club_code[k]]][-1]
    if (length(later) > 0) {
      committed <- bind_rows(committed, tibble(RegistryNr = assigned$RegistryNr[k], day = later,
                                               club_code = assigned$club_code[k]))
    }
  }
}

# ---------- Outputs ----------
week <- week %>%
  left_join(clubs %>% select(club_code = code, club_name = name), by = "club_code") %>%
  arrange(match(day, DAY_KEYS), as.numeric(RegistryNr))
write_csv(week %>% select(RegistryNr, day, club_code, club_name, via), file.path(output_dir, "week_assignments.csv"))

by_student <- students %>%
  transmute(`ΑΜ` = RegistryNr, `Επώνυμο` = Surname, `Όνομα` = Name, `Τάξη` = grade)
for (d in DAY_KEYS) {
  on_day <- week %>% filter(day == d)
  by_student[[day_labels[[d]]]] <- on_day$club_name[match(by_student$`ΑΜ`, on_day$RegistryNr)]
}
by_student <- by_student %>% arrange(`Τάξη`, `Επώνυμο`, `Όνομα`)

by_club <- week %>%
  left_join(students, by = "RegistryNr") %>%
  transmute(`Κωδικός` = club_code, `Όμιλος` = club_name, `Ημέρα` = day_labels[day], `ΑΜ` = RegistryNr,
            `Επώνυμο` = Surname, `Όνομα` = Name, `Τάξη` = grade, day_order = match(day, DAY_KEYS)) %>%
  arrange(as.numeric(`Κωδικός`), day_order, `Επώνυμο`, `Όνομα`) %>%
  select(-day_order)

# A student is "without club" on a day where clubs for their grade run
# but they got none.
offered <- clubs %>%
  separate_rows(days, sep = ";") %>% separate_rows(grades, sep = ";") %>%
  distinct(day = days, grade = grades)
without <- students %>%
  inner_join(offered, by = "grade", relationship = "many-to-many") %>%
  anti_join(week, by = c("RegistryNr", "day")) %>%
  left_join(prefs %>% distinct(RegistryNr, day) %>% mutate(submitted = TRUE), by = c("RegistryNr", "day")) %>%
  transmute(`ΑΜ` = RegistryNr, `Επώνυμο` = Surname, `Όνομα` = Name, `Τάξη` = grade, `Ημέρα` = day_labels[day],
            `Αιτία` = ifelse(is.na(submitted), "Δεν δηλώθηκαν προτιμήσεις", "Δεν χώρεσε σε κανέναν όμιλο της λίστας"),
            day_order = match(day, DAY_KEYS)) %>%
  arrange(day_order, `Τάξη`, `Επώνυμο`) %>% select(-day_order)

summary_sheet <- clubs %>%
  separate_rows(days, sep = ";") %>%
  left_join(week %>% count(club_code, day, name = "enrolled"), by = c("code" = "club_code", "days" = "day")) %>%
  transmute(`Κωδικός` = code, `Όμιλος` = name, `Ημέρα` = day_labels[days], `Χωρητικότητα` = capacity,
            `Τοποθετήθηκαν` = coalesce(enrolled, 0L), day_order = match(days, DAY_KEYS)) %>%
  arrange(as.numeric(`Κωδικός`), day_order) %>% select(-day_order)

lottery_sheet <- lottery %>%
  left_join(students, by = "RegistryNr") %>%
  transmute(`Αριθμός κλήρωσης` = lottery_number, `ΑΜ` = RegistryNr, `Επώνυμο` = Surname, `Όνομα` = Name) %>%
  arrange(`Αριθμός κλήρωσης`)
info <- tibble(`Στοιχείο` = c("Seed κλήρωσης", "Μαθητές", "Όμιλοι", "Εκτέλεση"),
               `Τιμή` = c(ifelse(is.na(seed), "(από lottery.csv)", seed), nrow(students), nrow(clubs),
                          format(Sys.time(), "%Y-%m-%d %H:%M")))

write_xlsx(list(
  `Ανά μαθητή` = by_student,
  `Ανά όμιλο` = by_club,
  `Πληρότητα` = summary_sheet,
  `Χωρίς όμιλο` = without,
  `Κλήρωση` = lottery_sheet,
  `Πληροφορίες` = info
), file.path(output_dir, "week_results.xlsx"))

cat(sprintf("\n✓ Αποτελέσματα: %s\n  week_results.xlsx, week_assignments.csv, και αναφορές ανά ημέρα\n", output_dir))
cat(sprintf("  Χωρίς όμιλο: %d περιπτώσεις (μαθητής × ημέρα)\n", nrow(without)))
