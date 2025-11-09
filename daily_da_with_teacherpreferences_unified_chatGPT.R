# ============================================================
# daily_da_with_teacherpreferences_unified.R
# Unified single-day Deferred Acceptance with teacher preferences
# ============================================================
# Key properties:
# - Student-proposing Gale–Shapley (many-to-one).
# - Uses RegistryNr ONLY as a join/identity key. Never for ordering.
# - When no teacher preference file exists for a club, falls back to
#   audit script's lottery: sort by student preference rank, then RANDOM
#   per-round tie-breaker via sample(n()) inside arrange().
# - When teacher preferences exist for a club, sort by:
#     teacher_priority (1..k, unranked = Inf),
#     student preference rank,
#     per-round random tiebreaker sample(n()).
# - set.seed(42) for reproducible lotteries.
# - Generates ALL reports listed in the requirements.
#
# Expected usage:
#   day_name <- "Monday"
#   source("daily_da_with_teacherpreferences_unified.R")
#
# Inputs (in working directory):
#   1) dailyclubs.csv
#        - columns: club_name, club_capacity
#   2) dailyresponses.csv
#        - columns: RegistryNr, Surname, Name, [one column per club with ranks]
#   Optional:
#   3) dailyresponses_original.csv  OR "dailyresponses - original.csv"
#        - same shape as dailyresponses.csv; used to add original_preference_rank
#   4) teacherpreferences/[club_name].csv (exact file name == club_name, .csv)
#        - columns: RegistryNr, Surname, Name, teacher_preference_rank (optional)
#
# Outputs:
#   <day>_reports/ containing:
#     1. <day>_assignments.csv
#     2. <day>_audit_log.csv
#     3. <day>_student_reports.csv
#     4. <day>_club_reports.csv
#     5. <day>_club_summary.csv
#     6. <day>_unassigned.csv  (only if unassigned exist)
#     7. <day>_club_members.xlsx  (one sheet per club)
#     8. <day>_report_[RegistryNr]_[Surname]_[Name].txt (per student)
#     9. <day>_club_report_[ClubName].txt              (per club)
#
# Dependencies:
#   install.packages(c("dplyr","tidyr","readr","purrr","stringr","writexl"))
#
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(stringr)
  library(writexl)
})

# ---------------- Configuration ----------------
if (!exists("day_name") || is.null(day_name) || length(day_name) != 1) {
  day_name <- format(Sys.Date(), "%A")
}
clubs_file         <- "dailyclubs.csv"
responses_file     <- "dailyresponses.csv"
orig1_file         <- "dailyresponses_original.csv"
orig2_file         <- "dailyresponses - original.csv"
teacher_pref_dir   <- "teacherpreferences"
seed_value         <- 42

set.seed(seed_value)

cat(sprintf("All reports will be saved under: %s_reports/\n\n", tolower(day_name)))
cat(sprintf("=== SINGLE DAY CLUB ASSIGNMENT WITH TEACHER PREFERENCES: %s ===\n\n", day_name))

# ---------------- Helpers ----------------

sanitize_club_id <- function(x) {
  x %>%
    trimws() %>%
    tolower() %>%
    gsub("[^a-z0-9]+", "_", ., perl = TRUE) %>%
    gsub("^_|_$", "", ., perl = TRUE)
}

sanitize_sheet_name <- function(x) {
  # Excel sheet constraints
  s <- x
  s <- gsub("[:/\\\\?\\*\\[\\]]", " ", s)   # illegal chars
  s <- substr(s, 1, 31)
  s <- ifelse(nchar(s) == 0, "Sheet", s)
  s
}

sanitize_filename <- function(x) {
  s <- x
  s <- gsub("[^A-Za-z0-9 _.-]", "_", s)
  s <- gsub("\\s+", "_", s)
  s
}

num_fmt_1dp <- function(x) {
  ifelse(is.na(x), NA_character_, sprintf("%.1f", x))
}

safe_coalesce_chr <- function(...) {
  coalesce(!!!map(list(...), ~ifelse(is.na(.x) | .x == "", NA, .x)))
}

# ---------------- Load core data ----------------
cat("Loading data...\n")

if (!file.exists(clubs_file)) stop("Missing dailyclubs.csv")
if (!file.exists(responses_file)) stop("Missing dailyresponses.csv")

clubs <- read_csv(clubs_file, show_col_types = FALSE) %>%
  mutate(
    club_name     = trimws(as.character(club_name)),
    club_capacity = as.integer(club_capacity),
    club_id       = sanitize_club_id(club_name)
  )

if (any(is.na(clubs$club_name) | clubs$club_name == "")) {
  stop("dailyclubs.csv has empty club_name values")
}
if (any(is.na(clubs$club_capacity) | clubs$club_capacity < 0)) {
  stop("dailyclubs.csv has invalid club_capacity values")
}
if (any(duplicated(clubs$club_id))) {
  dups <- clubs$club_name[duplicated(clubs$club_id)]
  stop(sprintf("Duplicate club_id after sanitization for clubs: %s", paste(dups, collapse = ", ")))
}

responses_raw <- read_csv(responses_file, show_col_types = FALSE) %>%
  rename_with(~trimws(.x))

required_cols <- c("RegistryNr","Surname","Name")
if (!all(required_cols %in% names(responses_raw))) {
  stop(sprintf("dailyresponses.csv must include columns: %s", paste(required_cols, collapse=", ")))
}

# Students
responses <- responses_raw %>%
  mutate(
    RegistryNr = as.character(RegistryNr),
    Surname    = trimws(as.character(Surname)),
    Name       = trimws(as.character(Name))
  )

if (any(is.na(responses$RegistryNr) | responses$RegistryNr == "")) {
  stop("dailyresponses.csv has empty RegistryNr")
}
if (any(duplicated(responses$RegistryNr))) {
  dups <- unique(responses$RegistryNr[duplicated(responses$RegistryNr)])
  stop(sprintf("Duplicate RegistryNr in dailyresponses.csv: %s", paste(dups, collapse=", ")))
}

# Resolve club columns that actually exist in dailyresponses
club_cols_present <- intersect(names(responses), clubs$club_name)
if (length(club_cols_present) == 0) {
  stop("No club columns from dailyclubs.csv were found in dailyresponses.csv")
}

student_lookup <- responses %>%
  transmute(
    student_id = RegistryNr,  # internal alias; do NOT sort by this anywhere
    RegistryNr,
    Surname, Name
  )

# Long preferences (student -> club, with preference rank)
student_prefs <- responses %>%
  select(RegistryNr, Surname, Name, all_of(club_cols_present)) %>%
  pivot_longer(cols = all_of(club_cols_present),
               names_to = "club_name",
               values_to = "preference_rank") %>%
  mutate(
    club_name       = as.character(club_name),
    preference_rank = suppressWarnings(as.integer(preference_rank))
  ) %>%
  filter(!is.na(preference_rank)) %>%
  left_join(clubs %>% select(club_name, club_id, club_capacity), by = "club_name") %>%
  transmute(
    student_id = RegistryNr,
    RegistryNr,
    Surname, Name,
    club_id, club_name, club_capacity,
    preference_rank
  )

cat(sprintf("Students: %d\n", n_distinct(student_lookup$student_id)))
cat(sprintf("Clubs: %d\n\n", nrow(clubs)))

# ---------------- Optional: original preferences ----------------
orig_prefs <- NULL
if (file.exists(orig1_file) || file.exists(orig2_file)) {
  orig_file <- if (file.exists(orig1_file)) orig1_file else orig2_file
  orig_raw <- read_csv(orig_file, show_col_types = FALSE) %>%
    rename_with(~trimws(.x))
  if (all(required_cols %in% names(orig_raw))) {
    orig_long <- orig_raw %>%
      mutate(
        RegistryNr = as.character(RegistryNr),
        Surname    = trimws(as.character(Surname)),
        Name       = trimws(as.character(Name))
      ) %>%
      select(RegistryNr, Surname, Name, all_of(club_cols_present)) %>%
      pivot_longer(cols = all_of(club_cols_present),
                   names_to = "club_name",
                   values_to = "original_preference_rank") %>%
      mutate(original_preference_rank = suppressWarnings(as.integer(original_preference_rank))) %>%
      filter(!is.na(original_preference_rank)) %>%
      left_join(clubs %>% select(club_name, club_id), by = "club_name") %>%
      transmute(
        RegistryNr, club_id, club_name, original_preference_rank
      )
    orig_prefs <- orig_long
  }
}

# ---------------- Load teacher preferences ----------------
cat("Loading teacher preferences...\n")

# For reports: count how many students each teacher ranked (file rows matched to responses)
teacher_rank_counts <- tibble(club_id=character(), ranked_by=integer())

# Map: per club_id -> tibble(RegistryNr, teacher_priority)
teacher_pref_map <- new.env(parent = emptyenv())

for (i in seq_len(nrow(clubs))) {
  cname <- clubs$club_name[i]
  cid   <- clubs$club_id[i]
  fpath <- file.path(teacher_pref_dir, paste0(cname, ".csv"))
  if (!file.exists(fpath)) {
    assign(cid, tibble(RegistryNr = character(0), teacher_priority = integer(0)), envir = teacher_pref_map)
    next
  }
  tp <- read_csv(fpath, show_col_types = FALSE) %>%
    rename_with(~trimws(.x)) %>%
    mutate(
      RegistryNr = as.character(RegistryNr),
      Surname    = trimws(as.character(Surname)),
      Name       = trimws(as.character(Name))
    )
  
  if (!all(c("RegistryNr","Surname","Name") %in% names(tp))) {
    stop(sprintf("Teacher preference file '%s' must contain RegistryNr, Surname, Name, optionally teacher_preference_rank", fpath))
  }
  
  if ("teacher_preference_rank" %in% names(tp)) {
    tp <- tp %>%
      mutate(teacher_preference_rank = suppressWarnings(as.integer(teacher_preference_rank))) %>%
      arrange(teacher_preference_rank, Surname, Name) %>%
      filter(!is.na(RegistryNr) & RegistryNr != "")
  } else {
    # Use row order if no explicit rank column
    tp <- tp %>%
      mutate(.row_order = row_number()) %>%
      arrange(.row_order) %>%
      select(-.row_order) %>%
      filter(!is.na(RegistryNr) & RegistryNr != "")
  }
  
  # Keep only students who exist in dailyresponses
  tp <- tp %>%
    filter(RegistryNr %in% student_lookup$RegistryNr) %>%
    distinct(RegistryNr, .keep_all = TRUE) %>%
    mutate(teacher_priority = row_number()) %>%
    select(RegistryNr, teacher_priority)
  
  assign(cid, tp, envir = teacher_pref_map)
  
  teacher_rank_counts <- teacher_rank_counts %>%
    bind_rows(tibble(club_id = cid, ranked_by = nrow(tp)))
  
  cat(sprintf("  \u2713 Loaded teacher preferences for %s: %d students ranked\n", cname, nrow(tp)))
}

# Ensure every club has a rank count row
teacher_rank_counts <- clubs %>%
  select(club_id) %>%
  left_join(teacher_rank_counts, by = "club_id") %>%
  mutate(ranked_by = coalesce(ranked_by, 0L))

cat("\nStarting Deferred Acceptance with teacher preferences...\n\n")

# ---------------- DA data structures ----------------
attempted <- tibble(student_id = character(), club_id = character())
tentative <- tibble(student_id = character(), club_id = character())
audit_log <- tibble(
  round = integer(), event = character(),
  student_id = character(), RegistryNr = character(),
  Surname = character(), Name = character(),
  club_name = character(), preference_rank = integer(),
  detail = character()
)

# Precompute quick lookups
pref_lookup <- student_prefs %>% select(student_id, club_id, preference_rank)
club_lookup <- clubs %>% select(club_id, club_name, club_capacity)

# ---------------- DA loop ----------------
round <- 1L
repeat {
  # Who is unassigned and can still propose?
  assigned_ids <- unique(tentative$student_id)
  unassigned <- setdiff(student_lookup$student_id, assigned_ids)
  
  # Next target per unassigned: lowest preference rank not yet proposed
  # Build candidate set of unproposed preferences
  next_options <- student_prefs %>%
    filter(student_id %in% unassigned) %>%
    anti_join(attempted, by = c("student_id","club_id"))
  
  cat(sprintf("Round %d: %d students unassigned\n", round, length(unassigned)))
  
  audit_log <- audit_log %>%
    bind_rows(tibble(
      round = round,
      event = "ROUND_START",
      student_id = NA_character_,
      RegistryNr = NA_character_,
      Surname = NA_character_,
      Name = NA_character_,
      club_name = NA_character_,
      preference_rank = NA_integer_,
      detail = sprintf("%d students unassigned at start of round", length(unassigned))
    ))
  
  # No more proposals possible?
  new_props <- next_options %>%
    group_by(student_id) %>%
    slice_min(preference_rank, with_ties = FALSE) %>%
    ungroup()
  
  if (nrow(new_props) == 0) {
    # Log and stop
    audit_log <- audit_log %>%
      bind_rows(tibble(
        round = round,
        event = "NO_MORE_PROPOSALS",
        student_id = NA_character_,
        RegistryNr = NA_character_,
        Surname = NA_character_,
        Name = NA_character_,
        club_name = NA_character_,
        preference_rank = NA_integer_,
        detail = "No students have remaining clubs to propose"
      )) %>%
      bind_rows(tibble(
        round = round,
        event = "ALGORITHM_COMPLETE",
        student_id = NA_character_,
        RegistryNr = NA_character_,
        Surname = NA_character_,
        Name = NA_character_,
        club_name = NA_character_,
        preference_rank = NA_integer_,
        detail = sprintf("Completed after %d rounds", max(1L, round - 1L))
      ))
    break
  }
  
  # Add names and club names for proposal logging
  new_props_named <- new_props %>%
    left_join(student_lookup, by = "student_id") %>%
    left_join(club_lookup, by = "club_id")
  
  audit_log <- audit_log %>%
    bind_rows(
      new_props_named %>%
        transmute(
          round = round, event = "PROPOSAL",
          student_id, RegistryNr, Surname, Name,
          club_name, preference_rank,
          detail = sprintf("Student proposes (preference #%d)", preference_rank)
        )
    )
  
  # Mark these proposals as attempted
  attempted <- bind_rows(attempted, new_props %>% select(student_id, club_id)) %>% distinct()
  
  # Combine with current tentative holders to build per-club candidate pools
  pool <- bind_rows(
    tentative %>%
      left_join(pref_lookup, by = c("student_id","club_id")),
    new_props %>%
      left_join(pref_lookup, by = c("student_id","club_id"))
  ) %>%
    left_join(student_lookup, by = "student_id") %>%
    left_join(club_lookup, by = "club_id")
  
  # Per-club evaluations
  decide_for_club <- function(df) {
    cid <- unique(df$club_id)
    cname <- unique(df$club_name)
    cap <- unique(df$club_capacity)
    cap <- ifelse(length(cap) == 0 || is.na(cap), 0L, cap)
    
    # Attach teacher priorities; unranked -> Inf
    tp <- get(cid, envir = teacher_pref_map, inherits = FALSE)
    df2 <- df %>%
      left_join(tp, by = c("RegistryNr" = "RegistryNr")) %>%
      mutate(teacher_priority = ifelse(is.na(teacher_priority), Inf, teacher_priority))
    
    # IMPORTANT: per-round random tiebreaker INSIDE arrange()
    df2 <- df2 %>%
      group_by(club_id) %>%
      mutate(random_tie = sample(dplyr::n())) %>%
      ungroup() %>%
      arrange(   # Sorting logic matches spec
        club_id,
        teacher_priority,
        preference_rank,
        random_tie
      )
    
    # Accept top cap, others rejected
    accepted <- df2 %>% slice_head(n = cap)
    rejected <- df2 %>% slice_tail(n = max(0, nrow(df2) - cap))
    
    # Log club evaluation summary
    audit_log <<- bind_rows(
      audit_log,
      tibble(
        round = round,
        event = "CLUB_EVALUATION",
        student_id = NA_character_,
        RegistryNr = NA_character_,
        Surname = NA_character_,
        Name = NA_character_,
        club_name = cname,
        preference_rank = NA_integer_,
        detail = sprintf("Evaluated %d candidates for %s (capacity %d). Accepted %d, rejected %d.",
                         nrow(df2), cname, cap, nrow(accepted), nrow(rejected))
      )
    )
    
    prev_held <- tentative %>% filter(club_id == cid)
    
    # Retained = previously held and still in accepted
    retained_ids <- intersect(prev_held$student_id, accepted$student_id)
    retained <- accepted %>% filter(student_id %in% retained_ids)
    
    # Newly accepted = in accepted but not previously held
    newly_acc <- accepted %>% filter(!(student_id %in% prev_held$student_id))
    
    # Previously held but dropped this round
    dropped <- prev_held %>% filter(!(student_id %in% accepted$student_id)) %>%
      left_join(pref_lookup, by = c("student_id","club_id")) %>%
      left_join(student_lookup, by = "student_id") %>%
      left_join(club_lookup, by = "club_id")
    
    # Log retained
    if (nrow(retained) > 0) {
      audit_log <<- bind_rows(
        audit_log,
        retained %>%
          transmute(
            round = round,
            event = "RETAINED",
            student_id, RegistryNr, Surname, Name,
            club_name = cname,
            preference_rank,
            detail = "Retained by club"
          )
      )
    }
    
    # Log newly accepted
    if (nrow(newly_acc) > 0) {
      audit_log <<- bind_rows(
        audit_log,
        newly_acc %>%
          transmute(
            round = round,
            event = "ACCEPTED",
            student_id, RegistryNr, Surname, Name,
            club_name = cname,
            preference_rank,
            detail = "Accepted by club"
          )
      )
    }
    
    # Log rejected: both dropped and new proposers who did not make the cut
    if (nrow(rejected) > 0) {
      rej <- rejected %>%
        transmute(
          round = round,
          event = "REJECTED",
          student_id, RegistryNr, Surname, Name,
          club_name = cname,
          preference_rank,
          detail = "Rejected; club at capacity or lower priority"
        )
      audit_log <<- bind_rows(audit_log, rej)
    }
    if (nrow(dropped) > 0) {
      dropped_log <- dropped %>%
        transmute(
          round = round,
          event = "REJECTED",
          student_id, RegistryNr, Surname, Name,
          club_name,
          preference_rank,
          detail = "Rejected; displaced by higher-priority student(s)"
        )
      audit_log <<- bind_rows(audit_log, dropped_log)
    }
    
    accepted %>% select(student_id, club_id)
  }
  
  # Evaluate for each club independently
  accepted_all <- pool %>%
    group_by(club_id) %>%
    group_modify(~decide_for_club(.x)) %>%
    ungroup()
  
  tentative <- accepted_all
  round <- round + 1L
}

# ---------------- Finalize assignment table ----------------
final_assignments <- tentative %>%
  left_join(pref_lookup, by = c("student_id","club_id")) %>%
  left_join(student_lookup, by = "student_id") %>%
  left_join(club_lookup, by = "club_id") %>%
  select(RegistryNr, Surname, Name, club_id, club_name, preference_rank, club_capacity)

# Attach original_preference_rank if available
if (!is.null(orig_prefs)) {
  final_assignments <- final_assignments %>%
    left_join(orig_prefs %>% select(RegistryNr, club_id, original_preference_rank),
              by = c("RegistryNr","club_id"))
}

# Unassigned students
unassigned_students <- setdiff(student_lookup$RegistryNr, final_assignments$RegistryNr)

# ---------------- Reports and audit enrichments ----------------
out_dir <- paste0(tolower(day_name), "_reports")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Student summaries
# Build student-friendly outcome + story
make_student_story <- function(student_id) {
  slog <- audit_log %>% filter(student_id == !!student_id)
  proposals <- sum(slog$event == "PROPOSAL")
  rejections <- sum(slog$event == "REJECTED")
  accepted_rows <- slog %>% filter(event == "ACCEPTED")
  final_row <- final_assignments %>% filter(RegistryNr == !!student_id)
  
  if (nrow(final_row) == 1) {
    club <- final_row$club_name[1]
    pr   <- final_row$preference_rank[1]
    # find first acceptance round
    first_acc_round <- if (nrow(accepted_rows) > 0) min(accepted_rows$round) else NA_integer_
    outcome <- sprintf("Assigned to %s (preference #%d)", club, pr)
    if (is.na(first_acc_round)) {
      story <- sprintf("Made %d proposals. Retained after initial acceptance.", proposals)
    } else {
      story <- sprintf("Made %d proposals. Rejected %d times. Finally accepted by %s in round %d.",
                       proposals, rejections, club, first_acc_round)
    }
  } else {
    if (proposals == 0) {
      outcome <- "Not assigned"
      story <- "Did not submit any valid preferences"
    } else {
      outcome <- "Not assigned"
      story <- sprintf("Made %d proposals. Rejected %d times.", proposals, rejections)
    }
  }
  list(outcome = outcome, story = story)
}

# Build student reports CSV
student_reports <- student_lookup %>%
  rowwise() %>%
  mutate(tmp = list(make_student_story(RegistryNr))) %>%
  mutate(outcome = tmp$outcome, story = tmp$story) %>%
  ungroup() %>%
  select(RegistryNr, Surname, Name, outcome, story)

# Club aggregates
club_enroll <- final_assignments %>%
  group_by(club_id, club_name, club_capacity) %>%
  summarise(enrolled = n(), avg_rank = mean(preference_rank, na.rm = TRUE), .groups = "drop")

club_enroll <- clubs %>%
  select(club_id, club_name, club_capacity) %>%
  left_join(club_enroll, by = c("club_id","club_name","club_capacity")) %>%
  mutate(
    enrolled = coalesce(enrolled, 0L),
    avg_rank = ifelse(is.finite(avg_rank), avg_rank, NA_real_)
  ) %>%
  left_join(teacher_rank_counts, by = "club_id") %>%
  mutate(
    fill_rate = ifelse(club_capacity > 0, 100 * enrolled / club_capacity, 0),
    fill_rate = as.numeric(sprintf("%.1f", fill_rate))
  )

# Totals from audit log
club_proposals <- audit_log %>%
  filter(!is.na(club_name)) %>%
  group_by(club_name) %>%
  summarise(
    total_proposals = sum(event == "PROPOSAL"),
    total_accepted  = sum(event == "ACCEPTED"),
    total_rejected  = sum(event == "REJECTED"),
    .groups = "drop"
  )

club_reports_csv <- club_enroll %>%
  left_join(club_proposals, by = "club_name") %>%
  mutate(
    total_proposals = coalesce(total_proposals, 0L),
    total_accepted  = coalesce(total_accepted,  0L),
    total_rejected  = coalesce(total_rejected,  0L)
  ) %>%
  select(club_name, club_capacity, enrolled,
         fill_rate, avg_rank, ranked_by,
         total_proposals, total_accepted, total_rejected)

club_summary_csv <- club_enroll %>%
  select(club_name, club_capacity, ranked_by) %>%
  left_join(club_proposals %>% select(club_name, proposals = total_proposals, accepted = total_accepted),
            by = "club_name")

# Unassigned CSV
unassigned_df <- tibble(
  RegistryNr = unassigned_students
) %>%
  left_join(student_lookup, by = "RegistryNr") %>%
  mutate(
    reason = case_when(
      RegistryNr %in% (student_prefs %>% distinct(RegistryNr) %>% pull()) ~
        "Applied to clubs but all proposals rejected",
      TRUE ~ "No valid preferences"
    )
  ) %>%
  select(RegistryNr, Surname, Name, reason)

# ---------------- Save CSVs ----------------
assignments_file <- file.path(out_dir, sprintf("%s_assignments.csv", tolower(day_name)))
audit_file       <- file.path(out_dir, sprintf("%s_audit_log.csv", tolower(day_name)))
students_file    <- file.path(out_dir, sprintf("%s_student_reports.csv", tolower(day_name)))
clubs_file_csv   <- file.path(out_dir, sprintf("%s_club_reports.csv", tolower(day_name)))
summary_file     <- file.path(out_dir, sprintf("%s_club_summary.csv", tolower(day_name)))
unassigned_file  <- file.path(out_dir, sprintf("%s_unassigned.csv", tolower(day_name)))

write_csv(final_assignments, assignments_file)
write_csv(
  audit_log %>%
    # enrich audit log with consistent columns and names
    mutate(club_name = ifelse(is.na(club_name), NA_character_, club_name)) %>%
    select(round, event, student_id, RegistryNr, Surname, Name, club_name, preference_rank, detail),
  audit_file
)
write_csv(student_reports, students_file)
write_csv(club_reports_csv, clubs_file_csv)
write_csv(club_summary_csv, summary_file)
if (length(unassigned_students) > 0) write_csv(unassigned_df, unassigned_file)

cat(sprintf("\n\u2713 Saved assignments: %s\n", assignments_file))
cat(sprintf("\u2713 Saved audit log: %s\n", audit_file))
cat(sprintf("\u2713 Saved student reports: %s\n", students_file))
cat(sprintf("\u2713 Saved club reports: %s\n", clubs_file_csv))
cat(sprintf("\u2713 Saved club summary: %s\n", summary_file))
if (length(unassigned_students) > 0) {
  cat(sprintf("\u2713 Saved unassigned: %s\n", unassigned_file))
}

# ---------------- Excel workbook: one sheet per club ----------------
club_sheets <- list()
for (i in seq_len(nrow(clubs))) {
  cid <- clubs$club_id[i]
  cname <- clubs$club_name[i]
  sheet_name <- sanitize_sheet_name(cname)
  members <- final_assignments %>%
    filter(club_id == cid) %>%
    arrange(preference_rank, Surname, Name) %>%
    select(RegistryNr, Surname, Name, preference_rank,
           original_preference_rank = if ("original_preference_rank" %in% names(final_assignments)) original_preference_rank else NULL)
  club_sheets[[sheet_name]] <- members
}

excel_file <- file.path(out_dir, sprintf("%s_club_members.xlsx", tolower(day_name)))
write_xlsx(club_sheets, excel_file)
cat(sprintf("\u2713 Saved Excel workbook: %s\n", excel_file))

# ---------------- Per-student TXT reports ----------------
for (sid in student_lookup$RegistryNr) {
  slog <- audit_log %>%
    filter(student_id == sid) %>%
    arrange(round, factor(event, levels = c("ROUND_START","PROPOSAL","CLUB_EVALUATION","ACCEPTED","REJECTED","RETAINED","NO_MORE_PROPOSALS","ALGORITHM_COMPLETE")))
  srow <- student_lookup %>% filter(RegistryNr == sid)
  fname <- file.path(out_dir, sprintf("%s_report_%s_%s_%s.txt",
                                      tolower(day_name),
                                      sanitize_filename(sid),
                                      sanitize_filename(srow$Surname),
                                      sanitize_filename(srow$Name)))
  con <- file(fname, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  
  cat(sprintf("CLUB ASSIGNMENT REPORT: %s\n", day_name), file = con)
  cat(sprintf("Student: %s %s (RegistryNr: %s)\n\n", srow$Surname, srow$Name, sid), file = con)
  cat("============================================================\n\n", file = con)
  
  fin <- final_assignments %>% filter(RegistryNr == sid)
  if (nrow(fin) == 1) {
    cat(sprintf("FINAL ASSIGNMENT: %s\n", fin$club_name[1]), file = con)
    cat(sprintf("This was your preference #%d\n\n", fin$preference_rank[1]), file = con)
  } else {
    cat("FINAL ASSIGNMENT: None\n\n", file = con)
  }
  
  cat("ASSIGNMENT PROCESS:\n\n", file = con)
  
  # Write a readable per-round narrative
  if (nrow(slog) == 0) {
    cat("No proposals were recorded for this student.\n", file = con)
  } else {
    curr_round <- NA_integer_
    for (i in seq_len(nrow(slog))) {
      e <- slog[i,]
      if (!is.na(e$round) && (is.na(curr_round) || e$round != curr_round)) {
        curr_round <- e$round
        cat(sprintf("\nRound %d\n", curr_round), file = con)
      }
      if (e$event == "PROPOSAL") {
        cat(sprintf("  PROPOSAL | Club: %s | preference #%d\n", e$club_name, e$preference_rank), file = con)
      } else if (e$event %in% c("ACCEPTED","REJECTED","RETAINED","CLUB_EVALUATION")) {
        detail <- ifelse(is.na(e$detail), "", e$detail)
        ename <- e$event
        if (e$event == "CLUB_EVALUATION") {
          cat(sprintf("  CLUB_EVALUATION | %s\n", detail), file = con)
        } else {
          cat(sprintf("  %-8s | Club: %s | %s\n", ename, e$club_name, detail), file = con)
        }
      } else if (e$event %in% c("NO_MORE_PROPOSALS","ALGORITHM_COMPLETE")) {
        cat(sprintf("  %s | %s\n", e$event, e$detail %||% ""), file = con)
      }
    }
  }
  
  cat("\n============================================================\n", file = con)
  cat("This assignment was made using the Deferred Acceptance algorithm,\n", file = con)
  cat("which ensures fair, preference-based matching.\n", file = con)
}

# ---------------- Per-club TXT reports ----------------
for (i in seq_len(nrow(clubs))) {
  cname <- clubs$club_name[i]
  cid   <- clubs$club_id[i]
  cap   <- clubs$club_capacity[i]
  log_c <- audit_log %>% filter(club_name == cname) %>% arrange(round)
  
  fname <- file.path(out_dir, sprintf("%s_club_report_%s.txt", tolower(day_name), sanitize_filename(cname)))
  con <- file(fname, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  
  cat(sprintf("CLUB REPORT: %s\n", cname), file = con)
  cat(sprintf("Capacity: %d\n\n", cap), file = con)
  
  if (nrow(log_c) == 0) {
    cat("No proposals received.\n", file = con)
  } else {
    curr_round <- NA_integer_
    round_buf <- list()
    for (irow in seq_len(nrow(log_c))) {
      e <- log_c[irow,]
      if (!is.na(e$round) && (is.na(curr_round) || e$round != curr_round)) {
        # flush previous round summary
        if (!is.na(curr_round)) {
          # summarize retained/accepted/rejected of previous round
          acc <- bind_rows(round_buf) %>% filter(event == "ACCEPTED")
          rej <- bind_rows(round_buf) %>% filter(event == "REJECTED")
          ret <- bind_rows(round_buf) %>% filter(event == "RETAINED")
          cat(sprintf("\nRound %d summary: Accepted %d, Rejected %d, Retained %d\n",
                      curr_round, nrow(acc), nrow(rej), nrow(ret)), file = con)
          round_buf <- list()
        }
        curr_round <- e$round
        cat(sprintf("\nRound %d\n", curr_round), file = con)
      }
      
      if (e$event == "PROPOSAL") {
        who <- safe_coalesce_chr(paste0(e$Surname, " ", e$Name), e$RegistryNr)
        cat(sprintf("  PROPOSAL | %s | preference #%d\n", who, e$preference_rank), file = con)
        round_buf <- append(round_buf, list(e))
      } else if (e$event == "CLUB_EVALUATION") {
        cat(sprintf("  CLUB_EVALUATION | %s\n", e$detail), file = con)
        round_buf <- append(round_buf, list(e))
      } else if (e$event %in% c("ACCEPTED","REJECTED","RETAINED")) {
        who <- safe_coalesce_chr(paste0(e$Surname, " ", e$Name), e$RegistryNr)
        cat(sprintf("  %-8s | %s | %s\n", e$event, who, e$detail %||% ""), file = con)
        round_buf <- append(round_buf, list(e))
      }
    }
    # flush last round
    if (!is.na(curr_round)) {
      acc <- bind_rows(round_buf) %>% filter(event == "ACCEPTED")
      rej <- bind_rows(round_buf) %>% filter(event == "REJECTED")
      ret <- bind_rows(round_buf) %>% filter(event == "RETAINED")
      cat(sprintf("\nRound %d summary: Accepted %d, Rejected %d, Retained %d\n",
                  curr_round, nrow(acc), nrow(rej), nrow(ret)), file = con)
    }
  }
  
  # final roster
  final_roster <- final_assignments %>%
    filter(club_id == cid) %>%
    arrange(preference_rank, Surname, Name) %>%
    select(RegistryNr, Surname, Name, preference_rank,
           original_preference_rank = if ("original_preference_rank" %in% names(final_assignments)) original_preference_rank else NULL)
  
  cat("\nFINAL ENROLLMENT:\n", file = con)
  if (nrow(final_roster) == 0) {
    cat("  (none)\n", file = con)
  } else {
    apply(final_roster, 1, function(r) {
      cat(sprintf("  %s %s (%s) - preference #%s%s\n",
                  r[["Surname"]], r[["Name"]], r[["RegistryNr"]],
                  r[["preference_rank"]],
                  if (!is.null(r[["original_preference_rank"]]) && !is.na(r[["original_preference_rank"]]))
                    sprintf(" [original #%s]", r[["original_preference_rank"]]) else ""), file = con)
    })
  }
}

# ---------------- Console summary ----------------
total_students <- nrow(student_lookup)
assigned_count <- nrow(final_assignments)
unassigned_count <- length(unassigned_students)
clubs_with_assignments <- n_distinct(final_assignments$club_id)

cat(sprintf("\n=== ASSIGNMENT COMPLETE ===\n\n"))
cat(sprintf("Total students processed: %d\n", total_students))
cat(sprintf("Students assigned: %d\n", assigned_count))
cat(sprintf("Students unassigned: %d\n", unassigned_count))
cat(sprintf("Clubs with assignments: %d / %d\n", clubs_with_assignments, nrow(clubs)))
cat(sprintf("\n=== END ===\n"))

# End of script
