# ----------------------------------------------------------
# run_week_reference.R
# ----------------------------------------------------------
# Runs daily_da_reference.R for Monday → Friday the way last year's
# workflow did by hand (remove_club_interactive.R /
# prepare_dailyresponses_enhanced2.R), so the platform can be checked
# against it on a whole week, multi-day clubs included:
#   - a multi-day club takes part only on its first day;
#   - students placed in it keep the seat on its later days and are
#     removed from those days' allocations;
#   - for everyone else the club is removed from the later days and the
#     remaining ranks are renumbered 1..k;
#   - a multi-day club that would clash with a day the student already
#     holds is removed too (and the ranks renumbered).
# Unlike the old helper scripts, students are matched by RegistryNr.
#
# Input directory:
#   clubs.csv            club_name, club_capacity, days ("mon;thu")
#   responses_<day>.csv  RegistryNr, Surname, Name, <club columns with ranks>
#   teacherpreferences/  <club_name>.csv (optional)
#   lottery.csv          RegistryNr, lottery_number
# Output:
#   week_assignments.csv RegistryNr, day, club_name, via
#
# Usage: Rscript run_week_reference.R <input_dir>
# ----------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) stop("Usage: Rscript run_week_reference.R <input_dir>")
input_dir <- normalizePath(args[1])

script_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))))
da_script <- file.path(script_dir, "daily_da_reference.R")

days <- c("mon", "tue", "wed", "thu", "fri")

clubs <- read_csv(file.path(input_dir, "clubs.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  mutate(club_capacity = as.integer(club_capacity))
club_days <- setNames(strsplit(clubs$days, ";", fixed = TRUE), clubs$club_name)
first_day <- sapply(club_days, `[`, 1)

committed <- tibble(RegistryNr = character(0), day = character(0), club_name = character(0))
week <- tibble(RegistryNr = character(0), day = character(0), club_name = character(0), via = character(0))

for (day in days) {
  responses_file <- file.path(input_dir, sprintf("responses_%s.csv", day))
  if (!file.exists(responses_file)) next
  resp <- read_csv(responses_file, show_col_types = FALSE, col_types = cols(.default = "c"))

  # Seats carried from the first day of multi-day clubs
  carried <- committed %>% filter(day == !!day)
  week <- bind_rows(week, carried %>% mutate(via = "carried"))
  resp <- resp %>% filter(!RegistryNr %in% carried$RegistryNr)

  # Clubs in today's allocation: those whose first day is today
  today_clubs <- names(first_day)[first_day == day]
  club_cols <- intersect(setdiff(colnames(resp), c("RegistryNr", "Surname", "Name")), names(first_day))
  dropped_cols <- setdiff(club_cols, today_clubs)
  resp <- resp %>% select(-all_of(dropped_cols))
  keep_cols <- intersect(club_cols, today_clubs)

  if (length(today_clubs) == 0 || nrow(resp) == 0) next

  # Ranks as integers; blank clubs that clash with days already held
  for (col in keep_cols) resp[[col]] <- suppressWarnings(as.integer(resp[[col]]))
  for (i in seq_len(nrow(resp))) {
    held_days <- committed$day[committed$RegistryNr == resp$RegistryNr[i]]
    for (col in keep_cols) {
      if (!is.na(resp[[col]][i]) && any(club_days[[col]] %in% held_days)) resp[[col]][i] <- NA_integer_
    }
    # Renumber the remaining ranks 1..k (same as adjust_ranks in
    # prepare_dailyresponses_enhanced2.R)
    ranks <- unlist(resp[i, keep_cols], use.names = FALSE)
    if (any(!is.na(ranks))) {
      new_ranks <- rep(NA_integer_, length(ranks))
      ord <- order(ranks, na.last = NA)
      new_ranks[ord] <- seq_along(ord)
      resp[i, keep_cols] <- as.list(new_ranks)
    }
  }

  # Students with nothing left to rank are left out, as last year
  has_pref <- if (length(keep_cols) > 0) rowSums(!is.na(resp[keep_cols])) > 0 else rep(FALSE, nrow(resp))
  resp <- resp[has_pref, c("RegistryNr", "Surname", "Name", keep_cols)]
  if (nrow(resp) == 0) next

  work <- tempfile(sprintf("day_%s_", day))
  dir.create(work)
  write_csv(clubs %>% filter(club_name %in% today_clubs) %>% select(club_name, club_capacity),
            file.path(work, "dailyclubs.csv"))
  write_csv(resp, file.path(work, "dailyresponses.csv"), na = "")

  # Run in the work directory, so the script finds no stray
  # dailyresponses_original.csv and writes its reports there.
  old_wd <- setwd(work)
  status <- system2("Rscript",
                    c(shQuote(da_script), day, "dailyclubs.csv", "dailyresponses.csv",
                      shQuote(file.path(input_dir, "teacherpreferences")),
                      shQuote(file.path(input_dir, "lottery.csv")), "reports"),
                    stdout = "run.log", stderr = "run.log")
  setwd(old_wd)
  if (status != 0) stop(sprintf("daily_da_reference.R failed on %s, see %s", day, file.path(work, "run.log")))

  assigned <- read_csv(file.path(work, "reports", sprintf("%s_assignments.csv", day)),
                       show_col_types = FALSE, col_types = cols(.default = "c"))
  week <- bind_rows(week, assigned %>% transmute(RegistryNr, day = day, club_name, via = "da"))

  for (k in seq_len(nrow(assigned))) {
    later <- club_days[[assigned$club_name[k]]][-1]
    if (length(later) > 0) {
      committed <- bind_rows(committed, tibble(RegistryNr = assigned$RegistryNr[k], day = later,
                                               club_name = assigned$club_name[k]))
    }
  }
}

write_csv(week %>% arrange(day, RegistryNr), file.path(input_dir, "week_assignments.csv"))
cat(sprintf("Wrote %d assignments to %s\n", nrow(week), file.path(input_dir, "week_assignments.csv")))
