# ----------------------------------------------------------
# compare_runs.R
# ----------------------------------------------------------
# What changes between two runs of run_week.R: one row per student and day
# whose club differs, with the student's name, the day in Greek, and the
# club's place in the student's declaration before and after.
#
# For what-if runs (docs/manual-R.md §7): unzip the platform's data package
# into two folders, change the second (a corrected declaration, teachers'
# lists, capacities), run run_week.R on both, then compare them.
#
# A multi-day club is in the declaration on its first day only, so its
# place comes from there on its later days too. The place "before" comes
# from the first folder's declarations, "after" from the second's.
#
# Usage:
#   Rscript R/compare_runs.R <folder_before> <folder_after> [output.xlsx]
# (default output: allages.xlsx in the current folder)
# ----------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(writexl)
})

args <- commandArgs(trailingOnly = TRUE)
if (!length(args) %in% c(2, 3)) stop("Χρήση: Rscript R/compare_runs.R <φάκελος_πριν> <φάκελος_μετά> [αρχείο.xlsx]")
before_dir <- args[1]
after_dir <- args[2]
output <- if (length(args) == 3) args[3] else "allages.xlsx"

day_labels <- c(mon = "Δευτέρα", tue = "Τρίτη", wed = "Τετάρτη", thu = "Πέμπτη", fri = "Παρασκευή")

read_input <- function(dir, name) {
  path <- file.path(dir, name)
  if (!file.exists(path)) {
    hint <- if (grepl("^results", name)) sprintf(" Τρέξτε πρώτα: Rscript R/run_week.R %s", dir) else ""
    stop(sprintf("Λείπει το αρχείο %s.%s", path, hint))
  }
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"), locale = locale(encoding = "UTF-8"))
}

# Each student's club per day, with its place in the student's declaration
clubs_of <- function(dir) {
  place <- read_input(dir, "preferences.csv") %>%
    distinct(RegistryNr, club_code, .keep_all = TRUE) %>%
    select(RegistryNr, club_code, rank)
  read_input(dir, "results/week_assignments.csv") %>%
    left_join(place, by = c("RegistryNr", "club_code")) %>%
    select(RegistryNr, day, club_code, club_name, rank)
}

students <- bind_rows(read_input(after_dir, "students.csv"), read_input(before_dir, "students.csv")) %>%
  distinct(RegistryNr, .keep_all = TRUE) %>%
  transmute(RegistryNr, name = trimws(paste(coalesce(Surname, ""), coalesce(Name, ""))), grade)

changes <- full_join(clubs_of(before_dir), clubs_of(after_dir), by = c("RegistryNr", "day"), suffix = c("_before", "_after")) %>%
  filter(coalesce(club_code_before, "") != coalesce(club_code_after, "")) %>%
  left_join(students, by = "RegistryNr") %>%
  arrange(match(day, names(day_labels)), name, RegistryNr) %>%
  transmute(
    `ΑΜ` = RegistryNr,
    `Ονοματεπώνυμο` = name,
    `Τάξη` = grade,
    `Ημέρα` = unname(day_labels[day]),
    `Πριν: όμιλος` = coalesce(club_name_before, "— χωρίς όμιλο"),
    `Πριν: σειρά` = coalesce(rank_before, ""),
    `Μετά: όμιλος` = coalesce(club_name_after, "— χωρίς όμιλο"),
    `Μετά: σειρά` = coalesce(rank_after, "")
  )

cat(sprintf("Σύγκριση: «%s» (πριν) → «%s» (μετά)\n\n", before_dir, after_dir))
if (nrow(changes) == 0) {
  cat("Καμία αλλαγή: όλοι οι μαθητές έχουν τους ίδιους ομίλους.\n")
} else {
  print(as.data.frame(changes), row.names = FALSE)
}
write_xlsx(list(`Αλλαγές` = changes), output)
cat(sprintf("\n%d αλλαγές (μαθητής × ημέρα), %d μαθητές. Αρχείο: %s\n",
            nrow(changes), n_distinct(changes$`ΑΜ`), normalizePath(output)))
