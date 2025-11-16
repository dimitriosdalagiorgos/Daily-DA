# ----------------------------------------------------------
# daily_da_with_teacherpreferences_optimized.R
# ----------------------------------------------------------
# OPTIMIZED VERSION: Student-proposing Gale-Shapley with teacher preferences
#
# Key optimizations from original:
# 1. Uses RegistryNr directly (eliminated redundant student_id)
# 2. Pre-aggregates statistics (single pass instead of O(n²))
# 3. Batch processes reports (filters data once, not per student)
# 4. Removed string parsing (uses lookup table consistently)
# 5. Simplified filename generation with helper function
#
# Inputs:
#  - dailyclubs.csv: [club_name, club_capacity]
#  - dailyresponses.csv: [RegistryNr, Surname, Name, <club columns with ranks>]
#  - dailyresponses_original.csv (optional): original preferences before modifications
#  - teacherpreferences/*.csv: one file per club (optional)
#      Format: [RegistryNr, Surname, Name, teacher_preference_rank]
#
# Teacher preference logic:
#  - If teacher ranks students, those get priority (sorted by teacher rank)
#  - Remaining slots filled by student preference + lottery
#  - If no teacher CSV exists, pure lottery among students with same preference rank
# ----------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(writexl)
})

# ==================== SETTINGS ====================
day_name       <- "Monday"
clubs_file     <- "dailyclubs.csv"
responses_file <- "dailyresponses.csv"
prefs_dir      <- "teacherpreferences"
set.seed(42)

# ==================== HELPER FUNCTIONS ====================

# Make club names safe for Excel sheet names
# Excel restricts: : / \ * ? [ ] and max 31 characters
make_excel_safe_name <- function(name, max_len = 31) {
  name %>%
    substr(1, max_len) %>%
    str_replace_all("[:/\\\\*\\?\\[\\]]", "_")
}

# Read teacher preference file for a club
# Returns tibble with RegistryNr column, or NULL if no preferences exist
read_teacher_preferences <- function(club_name_original, club_id_normalized) {
  # Try multiple possible filenames
  possible_files <- c(
    file.path(prefs_dir, paste0(club_name_original, ".csv")),
    file.path(prefs_dir, paste0(club_id_normalized, ".csv"))
  )
  
  teacher_file <- NULL
  for (f in possible_files) {
    if (file.exists(f)) {
      teacher_file <- f
      break
    }
  }
  
  if (is.null(teacher_file)) {
    return(NULL)  # No teacher preferences for this club
  }
  
  # Read file with all columns as character initially
  tp <- read_csv(teacher_file, show_col_types = FALSE, col_types = cols(.default = "c"))
  colnames(tp) <- trimws(colnames(tp))
  
  if (!"RegistryNr" %in% colnames(tp)) {
    warning(sprintf("Teacher preference file %s missing RegistryNr column, skipping", teacher_file))
    return(NULL)
  }
  
  tp <- tp %>%
    mutate(RegistryNr = as.character(RegistryNr))
  
  # If teacher_preference_rank exists, use it; otherwise use row order
  if ("teacher_preference_rank" %in% colnames(tp)) {
    tp <- tp %>%
      mutate(teacher_preference_rank = suppressWarnings(as.integer(teacher_preference_rank))) %>%
      filter(!is.na(teacher_preference_rank)) %>%
      arrange(teacher_preference_rank)
  }
  
  cat(sprintf("  ✓ Loaded teacher preferences for %s: %d students ranked\n", 
              club_name_original, nrow(tp)))
  
  return(tp %>% select(RegistryNr))
}

# ==================== OUTPUT DIRECTORY ====================
output_dir <- sprintf("%s_reports", tolower(day_name))
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
cat(sprintf("All reports will be saved under: %s/\n\n", output_dir))
cat(sprintf("=== OPTIMIZED CLUB ASSIGNMENT WITH TEACHER PREFERENCES: %s ===\n\n", day_name))

# ==================== LOAD DATA ====================
cat("Loading data...\n")

# Load clubs
clubs <- read_csv(clubs_file, show_col_types = FALSE)
colnames(clubs) <- tolower(trimws(colnames(clubs)))

if (!"club_name" %in% colnames(clubs)) {
  stop("Error: dailyclubs.csv must have 'club_name' column")
}
if (!"club_capacity" %in% colnames(clubs)) {
  stop("Error: dailyclubs.csv must have 'club_capacity' column")
}

clubs <- clubs %>%
  mutate(
    club_name = trimws(as.character(club_name)),
    club_id = tolower(trimws(club_name)),
    club_capacity = as.integer(club_capacity)
  )

# Load student responses
responses_raw <- read_csv(responses_file, show_col_types = FALSE)
colnames(responses_raw) <- trimws(colnames(responses_raw))

# Find RegistryNr column (case-insensitive)
registry_col <- colnames(responses_raw)[tolower(colnames(responses_raw)) == "registrynr"]
if (length(registry_col) != 1) {
  stop("Error: dailyresponses.csv must have a 'RegistryNr' column")
}

# OPTIMIZATION: Use RegistryNr directly as the key (no redundant student_id)
responses <- responses_raw %>%
  rename(RegistryNr = all_of(registry_col)) %>%
  mutate(
    RegistryNr = as.character(RegistryNr),
    Surname = trimws(as.character(Surname)),
    Name = trimws(as.character(Name))
  )

if (!all(c("RegistryNr", "Surname", "Name") %in% colnames(responses))) {
  stop("Error: dailyresponses.csv must have RegistryNr, Surname, and Name columns")
}

# Check for duplicates
if (anyDuplicated(responses$RegistryNr) > 0) {
  duplicated_ids <- responses$RegistryNr[duplicated(responses$RegistryNr)]
  stop(sprintf("Error: Duplicate RegistryNr values: %s",
               paste(unique(duplicated_ids), collapse = ", ")))
}

# OPTIMIZATION: Student lookup table for consistent name resolution
# (eliminates need for string parsing throughout code)
student_lookup <- responses %>% select(RegistryNr, Surname, Name)

# Extract club preferences from responses
club_cols_in_responses <- setdiff(colnames(responses), c("RegistryNr", "Surname", "Name"))
if (length(club_cols_in_responses) == 0) {
  stop("Error: No club columns found in dailyresponses.csv")
}

# Convert to long format
student_preferences <- responses %>%
  select(RegistryNr, all_of(club_cols_in_responses)) %>%
  pivot_longer(
    cols = -RegistryNr,
    names_to = "club_id",
    values_to = "preference_rank"
  ) %>%
  mutate(
    club_id = tolower(trimws(club_id)),
    preference_rank = suppressWarnings(as.integer(preference_rank))
  ) %>%
  filter(!is.na(preference_rank)) %>%
  arrange(RegistryNr, preference_rank)

cat(sprintf("Students: %d\n", nrow(responses)))
cat(sprintf("Clubs: %d\n", nrow(clubs)))
cat("\n")

# ==================== LOAD TEACHER PREFERENCES ====================
cat("Loading teacher preferences...\n")

# Load all teacher preferences into list column
teacher_preferences <- clubs %>%
  rowwise() %>%
  mutate(
    teacher_list = list(read_teacher_preferences(club_name, club_id))
  ) %>%
  ungroup()

cat("\n")

# ==================== INITIALIZE TRACKING + AUDIT LOG ====================
# OPTIMIZATION: Variables renamed for clarity (using RegistryNr directly)
tentative_assignments <- tibble(
  RegistryNr = character(0),
  club_id = character(0),
  preference_rank = integer(0)
)

attempted_proposals <- tibble(
  RegistryNr = character(0),
  club_id = character(0)
)

assigned_registry_nrs <- character(0)

audit_log <- tibble(
  round = integer(0),
  event = character(0),
  RegistryNr = character(0),
  club_id = character(0),
  preference_rank = integer(0),
  detail = character(0)
)

round <- 1L
max_rounds <- nrow(clubs) + 20

cat("Starting Deferred Acceptance with teacher preferences...\n\n")

# ==================== MAIN DA LOOP ====================
while (round <= max_rounds) {
  
  # Find students who still need assignment
  unassigned_registry_nrs <- setdiff(responses$RegistryNr, assigned_registry_nrs)
  
  if (length(unassigned_registry_nrs) == 0) {
    audit_log <- audit_log %>%
      bind_rows(tibble(
        round = round,
        event = "ALGORITHM_COMPLETE",
        RegistryNr = NA_character_,
        club_id = NA_character_,
        preference_rank = NA_integer_,
        detail = "All students assigned"
      ))
    break
  }
  
  cat(sprintf("Round %d: %d students unassigned\n", round, length(unassigned_registry_nrs)))
  
  # Log round start
  audit_log <- audit_log %>%
    bind_rows(tibble(
      round = round,
      event = "ROUND_START",
      RegistryNr = NA_character_,
      club_id = NA_character_,
      preference_rank = NA_integer_,
      detail = sprintf("%d students seeking assignment", length(unassigned_registry_nrs))
    ))
  
  # Generate proposals: each unassigned student proposes to their next club
  new_proposals <- student_preferences %>%
    filter(RegistryNr %in% unassigned_registry_nrs) %>%
    anti_join(attempted_proposals, by = c("RegistryNr", "club_id")) %>%
    group_by(RegistryNr) %>%
    slice_min(preference_rank, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  if (nrow(new_proposals) == 0) {
    audit_log <- audit_log %>%
      bind_rows(tibble(
        round = round,
        event = "NO_MORE_PROPOSALS",
        RegistryNr = NA_character_,
        club_id = NA_character_,
        preference_rank = NA_integer_,
        detail = sprintf("%d students have no more clubs to propose to", length(unassigned_registry_nrs))
      ))
    break
  }
  
  # Log each proposal
  for (i in 1:nrow(new_proposals)) {
    prop <- new_proposals[i,]
    audit_log <- audit_log %>%
      bind_rows(tibble(
        round = round,
        event = "PROPOSAL",
        RegistryNr = prop$RegistryNr,
        club_id = prop$club_id,
        preference_rank = prop$preference_rank,
        detail = sprintf("Student proposes to club (preference #%d)", prop$preference_rank)
      ))
  }
  
  attempted_proposals <- attempted_proposals %>%
    bind_rows(new_proposals %>% select(RegistryNr, club_id))
  
  # ==================== CLUB DECISION PHASE ====================
  # Combine tentative assignments with new proposals
  all_proposals <- tentative_assignments %>%
    bind_rows(new_proposals)
  
  # Log club evaluations
  clubs_receiving_proposals <- unique(new_proposals$club_id)
  
  for (club in clubs_receiving_proposals) {
    club_proposals <- all_proposals %>% filter(club_id == club)
    club_cap <- clubs %>% filter(club_id == club) %>% pull(club_capacity)
    
    audit_log <- audit_log %>%
      bind_rows(tibble(
        round = round,
        event = "CLUB_EVALUATION",
        RegistryNr = NA_character_,
        club_id = club,
        preference_rank = NA_integer_,
        detail = sprintf("Club evaluates %d proposals (capacity: %d)", 
                         nrow(club_proposals), club_cap)
      ))
  }
  
  # Make decisions for each club
  new_tentative <- all_proposals %>%
    left_join(clubs %>% select(club_id, club_capacity), by = "club_id") %>%
    left_join(
      teacher_preferences %>% select(club_id, teacher_list),
      by = "club_id"
    ) %>%
    group_split(club_id) %>%
    purrr::map_dfr(function(club_data) {
      capacity <- club_data$club_capacity[1]
      club <- club_data$club_id[1]
      teacher_list <- club_data$teacher_list[[1]]
      
      # Assign priorities based on teacher preferences
      if (!is.null(teacher_list) && nrow(teacher_list) > 0) {
        # Students in teacher list get their position as priority (1, 2, 3, ...)
        teacher_ranked <- teacher_list %>%
          mutate(
            teacher_priority = row_number(),
            RegistryNr = RegistryNr
          ) %>%
          select(RegistryNr, teacher_priority)
        
        club_data <- club_data %>%
          left_join(teacher_ranked, by = "RegistryNr") %>%
          mutate(
            # Students not ranked by teacher get Inf (lowest priority)
            teacher_priority = ifelse(is.na(teacher_priority), Inf, teacher_priority)
          )
      } else {
        # No teacher preferences - all students get equal priority (Inf)
        club_data <- club_data %>%
          mutate(teacher_priority = Inf)
      }
      
      # Sort by: teacher priority, student preference rank, then random tiebreaker
      club_data <- club_data %>%
        mutate(random_tie = sample(n())) %>%
        arrange(teacher_priority, preference_rank, random_tie)
      
      # Accept top students up to capacity
      accepted <- club_data %>% 
        slice_head(n = capacity) %>% 
        select(RegistryNr, club_id, preference_rank)
      
      # Reject remaining students
      rejected <- club_data %>% 
        slice_tail(n = max(0, nrow(club_data) - capacity)) %>% 
        select(RegistryNr, club_id, preference_rank)
      
      # Log acceptances
      for (j in 1:nrow(accepted)) {
        acc <- accepted[j,]
        is_new <- !(acc$RegistryNr %in% tentative_assignments$RegistryNr & 
                      acc$club_id %in% tentative_assignments$club_id)
        
        audit_log <<- audit_log %>%
          bind_rows(tibble(
            round = round,
            event = if(is_new) "ACCEPTED" else "RETAINED",
            RegistryNr = acc$RegistryNr,
            club_id = acc$club_id,
            preference_rank = acc$preference_rank,
            detail = if(is_new) 
              sprintf("Student accepted (ranked club #%d)", acc$preference_rank) else
                "Student retained from previous round"
          ))
      }
      
      # Log rejections
      if (nrow(rejected) > 0) {
        for (j in 1:nrow(rejected)) {
          rej <- rejected[j,]
          audit_log <<- audit_log %>%
            bind_rows(tibble(
              round = round,
              event = "REJECTED",
              RegistryNr = rej$RegistryNr,
              club_id = rej$club_id,
              preference_rank = rej$preference_rank,
              detail = sprintf("Student rejected (ranked club #%d, club at capacity)", 
                               rej$preference_rank)
            ))
        }
      }
      
      accepted
    })
  
  tentative_assignments <- new_tentative
  assigned_registry_nrs <- unique(tentative_assignments$RegistryNr)
  
  round <- round + 1L
  cat("\n")
}

cat(sprintf("Algorithm completed after %d rounds\n\n", round - 1))

# ==================== CHECK FOR UNASSIGNED ====================
unassigned_final <- setdiff(responses$RegistryNr, tentative_assignments$RegistryNr)

if (length(unassigned_final) > 0) {
  cat(sprintf("Students UNASSIGNED: %d\n", length(unassigned_final)))
  
  unassigned_analysis <- tibble(RegistryNr = unassigned_final) %>%
    left_join(student_lookup, by = "RegistryNr") %>%
    left_join(
      student_preferences %>%
        group_by(RegistryNr) %>%
        summarise(
          num_preferences = n(),
          clubs_tried = n_distinct(club_id),
          .groups = 'drop'
        ),
      by = "RegistryNr"
    ) %>%
    replace_na(list(num_preferences = 0, clubs_tried = 0)) %>%
    mutate(
      reason = case_when(
        num_preferences == 0 ~ "No preferences submitted",
        clubs_tried > 0 ~ sprintf("Applied to %d clubs but all rejected", clubs_tried),
        TRUE ~ "Unknown"
      )
    )
  
  cat("  Details:\n")
  for (i in 1:nrow(unassigned_analysis)) {
    cat(sprintf("    %s %s - %s\n",
                unassigned_analysis$Surname[i],
                unassigned_analysis$Name[i],
                unassigned_analysis$reason[i]))
  }
  cat("\n")
  
  unassigned_file <- file.path(output_dir, sprintf("%s_unassigned.csv", tolower(day_name)))
  write_csv(unassigned_analysis %>%
              transmute(RegistryNr,
                        Surname = coalesce(Surname, ""),
                        Name = coalesce(Name, ""),
                        reason),
            unassigned_file)
  cat(sprintf("✓ Saved unassigned students with reasons: %s\n\n", unassigned_file))
}

# ==================== FINALIZE ASSIGNMENTS ====================
final_assignments <- tentative_assignments %>%
  left_join(clubs %>% select(club_id, club_name, club_capacity), by = "club_id") %>%
  left_join(student_lookup, by = "RegistryNr") %>%
  select(
    RegistryNr,
    Surname, Name,
    club_id, club_name,
    preference_rank, club_capacity
  ) %>%
  arrange(Surname, Name, RegistryNr)

# Add student names to audit log
audit_log <- audit_log %>%
  left_join(student_lookup, by = "RegistryNr") %>%
  left_join(clubs %>% select(club_id, club_name), by = "club_id") %>%
  select(round, event, RegistryNr, Surname, Name, club_name, preference_rank, detail)

# ==================== LOAD ORIGINAL PREFERENCES (OPTIONAL) ====================
find_original_file <- function() {
  cands <- c("dailyresponses_original.csv",
             "dailyresponses - original.csv",
             "dailyresponses-original.csv")
  hit <- cands[file.exists(cands)]
  if (length(hit) == 0) return(NULL)
  hit[1]
}

orig_file <- find_original_file()

if (!is.null(orig_file)) {
  cat("Loading original preferences...\n")
  
  orig <- read_csv(orig_file, show_col_types = FALSE)
  colnames(orig) <- trimws(colnames(orig))
  
  registry_col_orig <- colnames(orig)[tolower(colnames(orig)) == "registrynr"]
  if (length(registry_col_orig) == 1) {
    orig <- orig %>%
      rename(RegistryNr = all_of(registry_col_orig)) %>%
      mutate(RegistryNr = as.character(RegistryNr))
    
    club_cols_orig <- setdiff(colnames(orig), c("RegistryNr", "Surname", "Name"))
    
    if (length(club_cols_orig) > 0) {
      orig_prefs <- orig %>%
        select(RegistryNr, all_of(club_cols_orig))
      
      colnames(orig_prefs) <- c("RegistryNr", tolower(trimws(club_cols_orig)))
      
      original_rank_map <- orig_prefs %>%
        pivot_longer(
          cols = -RegistryNr,
          names_to = "club_id",
          values_to = "original_preference_rank"
        ) %>%
        mutate(
          club_id = tolower(trimws(club_id)),
          original_preference_rank = suppressWarnings(as.integer(original_preference_rank))
        ) %>%
        filter(!is.na(original_preference_rank))
      
      final_assignments <- final_assignments %>%
        left_join(original_rank_map, by = c("RegistryNr", "club_id")) %>%
        mutate(original_preference_rank = if_else(is.na(original_preference_rank), 
                                                  NA_integer_, 
                                                  original_preference_rank))
    }
  }
}

# ==================== SAVE OUTPUTS ====================
cat("\nSaving outputs...\n")

# Assignments CSV
output_file <- file.path(output_dir, sprintf("%s_assignments.csv", tolower(day_name)))
final_assignments_output <- final_assignments %>%
  select(RegistryNr, Surname, Name, club_id, club_name,
         preference_rank, any_of("original_preference_rank"), club_capacity)
write_csv(final_assignments_output, output_file)
cat(sprintf("✓ Saved assignments: %s\n", output_file))

# Audit log CSV
audit_file <- file.path(output_dir, sprintf("%s_audit_log.csv", tolower(day_name)))
write_csv(audit_log, audit_file)
cat(sprintf("✓ Saved audit log: %s\n", audit_file))

# ==================== GENERATE STUDENT REPORTS ====================
cat("\nGenerating individual student reports...\n")

student_reports <- tibble()

for (registry_nr in unique(responses$RegistryNr)) {
  student_info <- student_lookup %>% filter(RegistryNr == registry_nr)
  surname <- if (nrow(student_info) > 0) student_info$Surname[1] else ""
  name <- if (nrow(student_info) > 0) student_info$Name[1] else ""
  
  student_events <- audit_log %>%
    filter(RegistryNr == registry_nr) %>%
    arrange(round)
  
  final_club <- final_assignments %>%
    filter(RegistryNr == registry_nr)
  
  if (nrow(final_club) > 0) {
    summary <- sprintf("Assigned to %s (preference #%d)",
                       final_club$club_name[1],
                       final_club$preference_rank[1])
    
    num_proposals <- sum(student_events$event == "PROPOSAL")
    num_rejections <- sum(student_events$event == "REJECTED")
    
    accepted_rounds <- student_events$round[student_events$event == "ACCEPTED"]
    final_round <- if (length(accepted_rounds) > 0) max(accepted_rounds) else NA_integer_
    
    story <- sprintf("Made %d proposals. Rejected %d times. Finally accepted by %s%s.",
                     num_proposals,
                     num_rejections,
                     final_club$club_name[1],
                     if (!is.na(final_round)) sprintf(" in round %d", final_round) else "")
  } else {
    summary <- "Not assigned to any club"
    
    num_proposals <- sum(student_events$event == "PROPOSAL")
    if (num_proposals == 0) {
      story <- "Did not submit any preferences"
    } else {
      story <- sprintf("Made %d proposals but all were rejected", num_proposals)
    }
  }
  
  student_reports <- student_reports %>%
    bind_rows(tibble(
      RegistryNr = registry_nr,
      Surname = surname,
      Name = name,
      outcome = summary,
      story = story
    ))
}

student_reports <- student_reports %>% arrange(Surname, Name, RegistryNr)

report_file <- file.path(output_dir, sprintf("%s_student_reports.csv", tolower(day_name)))
write_csv(student_reports, report_file)
cat(sprintf("✓ Saved student reports: %s\n", report_file))

# ==================== GENERATE PARENT-FRIENDLY REPORTS ====================
cat("\nGenerating detailed parent reports...\n")

# OPTIMIZATION: Use group_nest() to split audit log once instead of filtering 500 times
nested_student_logs <- audit_log %>%
  filter(!is.na(RegistryNr)) %>%
  select(round, event, RegistryNr, Surname, Name, club_name, preference_rank, detail) %>%
  group_by(RegistryNr, Surname, Name) %>%
  nest() %>%
  rename(student_log = data)

for (i in 1:nrow(nested_student_logs)) {
  registry_nr <- nested_student_logs$RegistryNr[i]
  surname <- nested_student_logs$Surname[i]
  name <- nested_student_logs$Name[i]
  student_log <- nested_student_logs$student_log[[i]]
  
  if (nrow(student_log) > 0) {
    # Build filename using RegistryNr and names
    file_stub <- if (!is.na(surname) && surname != "" && !is.na(name) && name != "") {
      sprintf("%s_%s_%s", registry_nr, gsub(" ", "_", surname), gsub(" ", "_", name))
    } else if (!is.na(surname) && surname != "") {
      sprintf("%s_%s", registry_nr, gsub(" ", "_", surname))
    } else {
      registry_nr
    }
    
    student_file <- file.path(output_dir, sprintf("%s_report_%s.txt",
                                                  tolower(day_name),
                                                  file_stub))
    
    sink(student_file)
    cat(sprintf("CLUB ASSIGNMENT REPORT: %s\n", day_name))
    if (!is.na(name) && name != "") {
      cat(sprintf("Student: %s %s (RegistryNr: %s)\n\n", surname, name, registry_nr))
    } else {
      cat(sprintf("Student RegistryNr: %s\n\n", registry_nr))
    }
    cat(paste(rep("=", 60), collapse = ""), "\n\n")
    
    final_club <- final_assignments %>%
      filter(RegistryNr == registry_nr)
    
    if (nrow(final_club) > 0) {
      cat(sprintf("FINAL ASSIGNMENT: %s\n", final_club$club_name[1]))
      cat(sprintf("This was your preference #%d", final_club$preference_rank[1]))
      if ("original_preference_rank" %in% colnames(final_club) && 
          !is.na(final_club$original_preference_rank[1])) {
        cat(sprintf(" (original preference #%d)", final_club$original_preference_rank[1]))
      }
      cat("\n\n")
    } else {
      cat("FINAL ASSIGNMENT: None\n\n")
    }
    
    cat("ASSIGNMENT PROCESS:\n\n")
    
    for (j in 1:nrow(student_log)) {
      log_entry <- student_log[j,]
      cat(sprintf("Round %d - %s\n", log_entry$round, log_entry$event))
      if (!is.na(log_entry$club_name)) {
        cat(sprintf("  Club: %s\n", log_entry$club_name))
      }
      if (!is.na(log_entry$detail)) {
        cat(sprintf("  %s\n", log_entry$detail))
      }
      cat("\n")
    }
    
    cat(paste(rep("=", 60), collapse = ""), "\n")
    cat("\nThis assignment was made using the Deferred Acceptance algorithm,\n")
    cat("which ensures fair, preference-based matching.\n")
    
    sink()
  }
}

cat(sprintf("✓ Generated %d individual parent reports\n",
            nrow(nested_student_logs)))

# ==================== GENERATE CLUB REPORTS ====================
cat("\nGenerating club perspective reports...\n")

for (club in clubs$club_id) {
  club_name_original <- clubs %>% filter(club_id == club) %>% pull(club_name)
  club_cap <- clubs %>% filter(club_id == club) %>% pull(club_capacity)
  
  club_log <- audit_log %>%
    filter(club_name == club_name_original | 
             (event == "CLUB_EVALUATION" & !is.na(club_name) & club_name == club_name_original)) %>%
    arrange(round)
  
  if (nrow(club_log) > 0) {
    club_file <- file.path(output_dir, sprintf("%s_club_report_%s.txt",
                                               tolower(day_name),
                                               club_name_original))
    
    sink(club_file)
    cat(sprintf("CLUB ASSIGNMENT REPORT: %s\n", day_name))
    cat(sprintf("Club: %s\n", club_name_original))
    cat(sprintf("Capacity: %d students\n\n", club_cap))
    cat(paste(rep("=", 60), collapse = ""), "\n\n")
    
    final_enrolled <- final_assignments %>%
      filter(club_name == club_name_original)
    
    cat(sprintf("FINAL ENROLLMENT: %d / %d students\n\n", 
                nrow(final_enrolled), club_cap))
    
    if (nrow(final_enrolled) > 0) {
      cat("ENROLLED STUDENTS:\n")
      for (k in 1:nrow(final_enrolled)) {
        student <- final_enrolled[k,]
        cat(sprintf("  %d. %s %s (RegistryNr: %s, ranked this club #%d)\n",
                    k, student$Surname, student$Name, student$RegistryNr, student$preference_rank))
      }
      cat("\n")
    }
    
    cat(paste(rep("=", 60), collapse = ""), "\n\n")
    cat("SELECTION PROCESS:\n\n")
    
    current_round <- 0
    proposals_this_round <- tibble()
    
    for (j in 1:nrow(club_log)) {
      log_entry <- club_log[j,]
      
      if (log_entry$round != current_round) {
        if (current_round > 0 && nrow(proposals_this_round) > 0) {
          accepted <- proposals_this_round %>% filter(event == "ACCEPTED")
          rejected <- proposals_this_round %>% filter(event == "REJECTED")
          retained <- proposals_this_round %>% filter(event == "RETAINED")
          
          cat(sprintf("  Decision: Accepted %d, Rejected %d, Retained %d\n",
                      nrow(accepted), nrow(rejected), nrow(retained)))
          cat("\n")
        }
        
        current_round <- log_entry$round
        proposals_this_round <- tibble()
        cat(sprintf("Round %d:\n", current_round))
      }
      
      if (log_entry$event == "CLUB_EVALUATION") {
        cat(sprintf("  %s\n", log_entry$detail))
      } else if (log_entry$event == "PROPOSAL") {
        # OPTIMIZATION: Use names from audit log (already joined)
        student_label <- if (!is.na(log_entry$Surname) && log_entry$Surname != "") {
          str_trim(paste(log_entry$Surname, coalesce(log_entry$Name, "")))
        } else if (!is.na(log_entry$RegistryNr)) {
          log_entry$RegistryNr
        } else {
          "Unknown student"
        }
        cat(sprintf("  → Received proposal from %s (their preference #%d)\n",
                    student_label, log_entry$preference_rank))
        proposals_this_round <- proposals_this_round %>%
          bind_rows(log_entry)
      } else if (log_entry$event == "ACCEPTED") {
        student_label <- if (!is.na(log_entry$Surname) && log_entry$Surname != "") {
          str_trim(paste(log_entry$Surname, coalesce(log_entry$Name, "")))
        } else if (!is.na(log_entry$RegistryNr)) {
          log_entry$RegistryNr
        } else {
          "Unknown student"
        }
        cat(sprintf("  ✓ ACCEPTED: %s (preference #%d)\n",
                    student_label, log_entry$preference_rank))
        proposals_this_round <- proposals_this_round %>%
          bind_rows(log_entry)
      } else if (log_entry$event == "REJECTED") {
        student_label <- if (!is.na(log_entry$Surname) && log_entry$Surname != "") {
          str_trim(paste(log_entry$Surname, coalesce(log_entry$Name, "")))
        } else if (!is.na(log_entry$RegistryNr)) {
          log_entry$RegistryNr
        } else {
          "Unknown student"
        }
        cat(sprintf("  ✗ REJECTED: %s (preference #%d) - at capacity\n",
                    student_label, log_entry$preference_rank))
        proposals_this_round <- proposals_this_round %>%
          bind_rows(log_entry)
      } else if (log_entry$event == "RETAINED") {
        student_label <- if (!is.na(log_entry$Surname) && log_entry$Surname != "") {
          str_trim(paste(log_entry$Surname, coalesce(log_entry$Name, "")))
        } else if (!is.na(log_entry$RegistryNr)) {
          log_entry$RegistryNr
        } else {
          "Unknown student"
        }
        cat(sprintf("  ↻ RETAINED: %s (from previous round)\n",
                    student_label))
        proposals_this_round <- proposals_this_round %>%
          bind_rows(log_entry)
      }
    }
    
    if (nrow(proposals_this_round) > 0) {
      accepted <- proposals_this_round %>% filter(event == "ACCEPTED")
      rejected <- proposals_this_round %>% filter(event == "REJECTED")
      retained <- proposals_this_round %>% filter(event == "RETAINED")
      
      cat(sprintf("  Decision: Accepted %d, Rejected %d, Retained %d\n",
                  nrow(accepted), nrow(rejected), nrow(retained)))
    }
    
    cat("\n")
    cat(paste(rep("=", 60), collapse = ""), "\n\n")
    
    all_proposals <- club_log %>% filter(event == "PROPOSAL")
    all_accepted <- club_log %>% filter(event == "ACCEPTED")
    all_rejected <- club_log %>% filter(event == "REJECTED")
    
    cat("SUMMARY STATISTICS:\n")
    cat(sprintf("  Total proposals received: %d\n", nrow(all_proposals)))
    cat(sprintf("  Students accepted: %d\n", nrow(all_accepted)))
    cat(sprintf("  Students rejected: %d\n", nrow(all_rejected)))
    
    if (nrow(final_enrolled) > 0) {
      cat(sprintf("  Average preference rank: %.1f\n", 
                  mean(final_enrolled$preference_rank)))
      cat(sprintf("  Best rank: #%d\n", min(final_enrolled$preference_rank)))
      cat(sprintf("  Worst rank: #%d\n", max(final_enrolled$preference_rank)))
    }
    
    cat("\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
    cat("\nThis club's enrollment was determined using the Deferred Acceptance\n")
    cat("algorithm, which ensures students are matched based on their preferences\n")
    cat("while respecting capacity constraints.\n")
    
    sink()
  }
}

cat(sprintf("✓ Generated %d club reports\n", nrow(clubs)))

# ==================== OPTIMIZED CLUB SUMMARY GENERATION ====================
cat("\nGenerating club summaries...\n")

# OPTIMIZATION: Pre-aggregate statistics (single pass through data)
# Instead of filtering audit_log multiple times per club, calculate once

# Count how many students ranked each club
pref_counts <- student_preferences %>%
  count(club_id, name = "ranked_by")

# Count events per club from audit log
audit_counts <- audit_log %>%
  filter(event %in% c("PROPOSAL", "ACCEPTED", "REJECTED")) %>%
  count(club_name, event) %>%
  pivot_wider(
    names_from = event, 
    values_from = n, 
    values_fill = 0,
    names_prefix = "total_"
  )

# Calculate enrollment statistics
enrolled_counts <- final_assignments %>%
  group_by(club_name) %>%
  summarise(
    enrolled = n(),
    avg_rank = mean(preference_rank, na.rm = TRUE),
    .groups = 'drop'
  )

# Combine all statistics with single join operation
club_summary <- clubs %>%
  left_join(pref_counts, by = "club_id") %>%
  left_join(audit_counts, by = "club_name") %>%
  left_join(enrolled_counts, by = "club_name") %>%
  mutate(
    # Fill missing values with 0
    across(c(ranked_by, starts_with("total_"), enrolled), ~replace_na(.x, 0)),
    # Calculate fill rate percentage
    fill_rate = if_else(club_capacity > 0, 
                       round(100 * enrolled / club_capacity, 1), 
                       0)
  ) %>%
  select(
    club_name, 
    club_capacity, 
    enrolled, 
    fill_rate, 
    avg_rank,
    ranked_by, 
    total_PROPOSAL, 
    total_ACCEPTED, 
    total_REJECTED
  ) %>%
  # Rename for clarity
  rename(
    proposals = total_PROPOSAL,
    accepted = total_ACCEPTED,
    rejected = total_REJECTED
  )

# Save basic club summary
club_summary_file <- file.path(output_dir, sprintf("%s_club_summary.csv", tolower(day_name)))
write_csv(club_summary, club_summary_file)
cat(sprintf("✓ Saved club summary file: %s\n", club_summary_file))

# Save detailed club reports (same data, all columns)
daily_club_reports_file <- file.path(output_dir, sprintf("%s_club_reports.csv", tolower(day_name)))
write_csv(club_summary, daily_club_reports_file)
cat(sprintf("✓ Saved daily club reports summary: %s\n", daily_club_reports_file))

# ==================== EXCEL WORKBOOK WITH CLUB SHEETS ====================
cat("\nGenerating Excel workbook with per-club sheets...\n")

excel_file <- file.path(output_dir, sprintf("%s_club_members.xlsx", tolower(day_name)))

club_sheets <- list()

for (club in unique(final_assignments$club_name)) {
  df <- final_assignments %>%
    filter(club_name == club) %>%
    select(RegistryNr, Surname, Name, preference_rank, any_of("original_preference_rank")) %>%
    arrange(Surname, Name, RegistryNr)
  
  # OPTIMIZATION: Use helper function for Excel-safe names
  sheet_name <- make_excel_safe_name(club)
  club_sheets[[sheet_name]] <- df
}

writexl::write_xlsx(club_sheets, excel_file)

cat(sprintf("✓ Saved Excel file with per-club sheets: %s\n", excel_file))

# ==================== FINAL SUMMARY ====================
cat("\n=== ASSIGNMENT COMPLETE ===\n")
cat("\nFiles created:\n")
cat(sprintf("  1. %s - Final assignments\n", basename(output_file)))
cat(sprintf("  2. %s - Complete audit log\n", basename(audit_file)))
cat(sprintf("  3. %s - Student summaries\n", basename(report_file)))
cat(sprintf("  4. %s - Club summary statistics\n", basename(club_summary_file)))
cat(sprintf("  5. %s - Daily club reports\n", basename(daily_club_reports_file)))
cat(sprintf("  6. %s - Excel workbook with club rosters\n", basename(excel_file)))
if (length(unassigned_final) > 0) {
  cat(sprintf("  7. %s - Unassigned students\n", basename(unassigned_file)))
}
cat(sprintf("  8. Individual student reports: %s_report_[RegistryNr]_*.txt\n", tolower(day_name)))
cat(sprintf("  9. Individual club reports: %s_club_report_[ClubName].txt\n", tolower(day_name)))

cat("\n")
cat(sprintf("Total students processed: %d\n", nrow(responses)))
cat(sprintf("Students assigned: %d\n", length(assigned_registry_nrs)))
cat(sprintf("Students unassigned: %d\n", length(unassigned_final)))
cat(sprintf("Clubs with assignments: %d / %d\n", 
            sum(club_summary$enrolled > 0), 
            nrow(clubs)))

cat("\n=== OPTIMIZATION SUMMARY ===\n")
cat("This optimized version includes:\n")
cat("  • Direct use of RegistryNr (eliminated redundant student_id)\n")
cat("  • Pre-aggregated statistics (O(n) instead of O(n²))\n")
cat("  • Batch report generation (single pass through audit log)\n")
cat("  • Consistent use of lookup table (no string parsing)\n")
cat("  • Helper function for Excel name generation\n")

cat("\n=== END ===\n")