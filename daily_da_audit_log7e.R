# daily_da_audit_log.R
# Detailed audit log version of DA assignment
# Documents every round, every proposal, every acceptance/rejection

library(dplyr)
library(readr)
library(tidyr)
library(purrr)

# --- Settings ---
day_name <- "Monday"
clubs_file <- "dailyclubs.csv"
responses_file <- "dailyresponses.csv"
set.seed(42)

# --- Create output subfolder ---
output_dir <- sprintf("%s_reports", tolower(day_name))
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
cat(sprintf("All reports will be saved under: %s/\n\n", output_dir))

cat(sprintf("=== SINGLE DAY CLUB ASSIGNMENT WITH AUDIT LOG: %s ===\n\n", day_name))

# --- 1. Load Data (same as before) ---
clubs <- read_csv(clubs_file, show_col_types = FALSE)
responses <- read_csv(responses_file, show_col_types = FALSE)

colnames(clubs) <- tolower(trimws(colnames(clubs)))
colnames(responses) <- trimws(colnames(responses))

if (!"club_name" %in% colnames(clubs)) {
  stop("Error: dailyclubs.csv must have 'club_name' column")
}
if (!"club_capacity" %in% colnames(clubs)) {
  stop("Error: dailyclubs.csv must have 'club_capacity' column")
}

clubs <- clubs %>%
  mutate(
    club_id = tolower(trimws(club_name)),
    club_capacity = as.integer(club_capacity)
  )

if (!"Surname" %in% colnames(responses) || !"Name" %in% colnames(responses)) {
  stop("Error: dailyresponses.csv must have 'Surname' and 'Name' columns")
}

responses <- responses %>%
  mutate(student_id = paste(Surname, Name, sep = "_")) %>%
  select(student_id, Surname, Name, everything())

student_col <- "student_id"

club_names_in_responses <- colnames(responses)[-(1:3)]
responses_data <- responses %>% select(student_id, Surname, Name)
responses_prefs <- responses %>% select(-Surname, -Name)
colnames(responses_prefs) <- c("student_id", tolower(trimws(club_names_in_responses)))
responses <- responses_prefs

cat(sprintf("Students: %d\n", nrow(responses)))
cat(sprintf("Clubs: %d\n", nrow(clubs)))
cat("\n")

# --- 2. Convert to Long Format ---
student_preferences <- responses %>%
  pivot_longer(cols = -all_of(student_col),
               names_to = "club_id",
               values_to = "preference_rank") %>%
  mutate(
    club_id = trimws(tolower(club_id)),
    preference_rank = as.integer(preference_rank)
  ) %>%
  filter(!is.na(preference_rank)) %>%
  arrange(across(all_of(student_col)), preference_rank)

# --- 3. Initialize Tracking + Audit Log ---
tentative_assignments <- tibble(
  !!student_col := character(0),
  club_id = character(0),
  preference_rank = integer(0)
)

attempted_proposals <- tibble(
  !!student_col := character(0),
  club_id = character(0)
)

assigned_students <- character(0)

# AUDIT LOG STRUCTURE
audit_log <- tibble(
  round = integer(0),
  event = character(0),
  student_id = character(0),
  club_id = character(0),
  preference_rank = integer(0),
  detail = character(0)
)

round <- 1L
max_rounds <- nrow(clubs) + 10

cat("Starting Deferred Acceptance with detailed logging...\n\n")

# --- 4. Main DA Loop with Logging ---
while (round <= max_rounds) {
  
  unassigned_students <- setdiff(responses[[student_col]], assigned_students)
  
  if (length(unassigned_students) == 0) {
    audit_log <- audit_log %>%
      bind_rows(tibble(
        round = round,
        event = "ALGORITHM_COMPLETE",
        student_id = NA_character_,
        club_id = NA_character_,
        preference_rank = NA_integer_,
        detail = "All students assigned"
      ))
    break
  }
  
  cat(sprintf("Round %d: %d students unassigned\n", round, length(unassigned_students)))
  
  # Log round start
  audit_log <- audit_log %>%
    bind_rows(tibble(
      round = round,
      event = "ROUND_START",
      student_id = NA_character_,
      club_id = NA_character_,
      preference_rank = NA_integer_,
      detail = sprintf("%d students seeking assignment", length(unassigned_students))
    ))
  
  # Generate proposals
  new_proposals <- student_preferences %>%
    filter(get(student_col) %in% unassigned_students) %>%
    anti_join(attempted_proposals, by = c(student_col, "club_id")) %>%
    group_by(across(all_of(student_col))) %>%
    slice_min(preference_rank, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  if (nrow(new_proposals) == 0) {
    audit_log <- audit_log %>%
      bind_rows(tibble(
        round = round,
        event = "NO_MORE_PROPOSALS",
        student_id = NA_character_,
        club_id = NA_character_,
        preference_rank = NA_integer_,
        detail = sprintf("%d students have no more clubs to propose to", length(unassigned_students))
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
        student_id = prop[[student_col]],
        club_id = prop$club_id,
        preference_rank = prop$preference_rank,
        detail = sprintf("Student proposes to club (preference #%d)", prop$preference_rank)
      ))
  }
  
  attempted_proposals <- attempted_proposals %>%
    bind_rows(new_proposals %>% select(all_of(student_col), club_id))
  
  # --- Club Decision Phase ---
  all_proposals <- tentative_assignments %>%
    bind_rows(new_proposals)
  
  # Log club evaluation
  clubs_receiving_proposals <- unique(new_proposals$club_id)
  
  for (club in clubs_receiving_proposals) {
    club_proposals <- all_proposals %>% filter(club_id == club)
    club_cap <- clubs %>% filter(club_id == club) %>% pull(club_capacity)
    
    audit_log <- audit_log %>%
      bind_rows(tibble(
        round = round,
        event = "CLUB_EVALUATION",
        student_id = NA_character_,
        club_id = club,
        preference_rank = NA_integer_,
        detail = sprintf("Club evaluates %d proposals (capacity: %d)", 
                         nrow(club_proposals), club_cap)
      ))
  }
  
  # Make decisions
  new_tentative <- all_proposals %>%
    left_join(clubs %>% select(club_id, club_capacity), by = "club_id") %>%
    arrange(club_id, preference_rank, sample(n())) %>%
    group_split(club_id) %>%
    purrr::map_dfr(function(club_data) {
      capacity <- club_data$club_capacity[1]
      accepted <- club_data %>% slice_head(n = capacity) %>% select(-club_capacity)
      rejected <- club_data %>% slice_tail(n = max(0, nrow(club_data) - capacity)) %>% select(-club_capacity)
      
      # Log acceptances
      for (j in 1:nrow(accepted)) {
        acc <- accepted[j,]
        is_new <- !(acc[[student_col]] %in% tentative_assignments[[student_col]] & 
                      acc$club_id %in% tentative_assignments$club_id)
        
        audit_log <<- audit_log %>%
          bind_rows(tibble(
            round = round,
            event = if(is_new) "ACCEPTED" else "RETAINED",
            student_id = acc[[student_col]],
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
              student_id = rej[[student_col]],
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
  assigned_students <- unique(tentative_assignments[[student_col]])
  
  round <- round + 1L
  cat("\n")
}

cat(sprintf("Algorithm completed after %d rounds\n\n", round - 1))

# --- 5. Check for Unassigned and Log Reasons ---
unassigned_final <- setdiff(responses[[student_col]], tentative_assignments[[student_col]])

if (length(unassigned_final) > 0) {
  cat(sprintf("Students UNASSIGNED: %d\n", length(unassigned_final)))
  
  # Analyze why they're unassigned
  unassigned_analysis <- tibble(student_id = unassigned_final) %>%
    mutate(
      Surname = sub("_.*", "", student_id),
      Name = sub(".*?_", "", student_id)
    ) %>%
    left_join(
      student_preferences %>%
        group_by(across(all_of(student_col))) %>%
        summarise(
          num_preferences = n(),
          clubs_tried = n_distinct(club_id),
          .groups = 'drop'
        ),
      by = c("student_id" = student_col)
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
  
  # Save unassigned with reasons
  unassigned_file <- file.path(output_dir, sprintf("%s_unassigned.csv", tolower(day_name)))
  write_csv(unassigned_analysis %>% select(Surname, Name, reason), unassigned_file)
  cat(sprintf("✓ Saved unassigned students with reasons: %s\n\n", unassigned_file))
}

# --- 6. Finalize and Add Student Info ---
final_assignments <- tentative_assignments %>%
  left_join(clubs %>% select(club_id, club_name, club_capacity), by = "club_id") %>%
  mutate(
    Surname = sub("_.*", "", get(student_col)),
    Name = sub(".*?_", "", get(student_col))
  ) %>%
  select(Surname, Name, club_name, preference_rank, club_capacity) %>%
  arrange(Surname, Name)

# Add student names to audit log
audit_log <- audit_log %>%
  mutate(
    Surname = if_else(!is.na(student_id), sub("_.*", "", student_id), NA_character_),
    Name = if_else(!is.na(student_id), sub(".*?_", "", student_id), NA_character_),
    club_name = if_else(!is.na(club_id), 
                        clubs$club_name[match(club_id, clubs$club_id)], 
                        NA_character_)
  ) %>%
  select(round, event, Surname, Name, club_name, preference_rank, detail)

# --- [NEW] Attach ORIGINAL preference rank to final outputs -------------------

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(stringr)
})

# Helper: find original file name robustly
find_original_file <- function() {
  cands <- c("dailyresponses_original.csv",
             "dailyresponses - original.csv",
             "dailyresponses-original.csv")
  hit <- cands[file.exists(cands)]
  if (length(hit) == 0) {
    stop("Δεν βρέθηκε το αρχείο με τις αρχικές προτιμήσεις: "
         ,"dailyresponses_original.csv ή 'dailyresponses - original.csv'")
  }
  hit[1]
}

orig_file <- find_original_file()

# Διαβάζουμε το αρχικό (πλήρες) dailyresponses
orig <- read_csv(orig_file, show_col_types = FALSE)

# Έλεγχοι βασικής δομής
if (!all(c("Surname","Name") %in% names(orig))) {
  stop("Το αρχικό αρχείο πρέπει να έχει στήλες Surname, Name")
}

# Φτιάχνουμε student_id ώστε να ταυτίζεται με αυτό του script
orig <- orig %>%
  mutate(
    Surname = trimws(Surname),
    Name    = trimws(Name),
    student_id = paste(Surname, Name, sep = "_")
  ) %>%
  select(student_id, Surname, Name, everything())

# Ονόματα στηλών-ομίλων και κανονικοποίηση όπως στο main script
club_cols_orig <- colnames(orig)[-(1:3)]
orig_prefs <- orig %>%
  select(-Surname, -Name)

# ομογενοποίηση ονομάτων ομίλων σε club_id (lower + trim)
norm_names <- tolower(trimws(club_cols_orig))
colnames(orig_prefs) <- c("student_id", norm_names)

# Long format: αρχική-αρχική κατάταξη
original_rank_map <- orig_prefs %>%
  pivot_longer(
    cols = -student_id,
    names_to = "club_id",
    values_to = "original_preference_rank"
  ) %>%
  mutate(
    club_id = trimws(tolower(club_id)),
    original_preference_rank = suppressWarnings(as.integer(original_preference_rank))
  ) %>%
  filter(!is.na(original_preference_rank))

# -------------------------------------------------------------------------
# ΣΗΜΑΝΤΙΚΟ: Βεβαιώνουμε ότι το final_assignments ΚΡΑΤΑ student_id & club_id
# Αν το υπάρχον code τα έχει ήδη πετάξει, αντικατάστησε την κατασκευή του
# final_assignments με το παρακάτω "ασφαλές" variant:
# -------------------------------------------------------------------------

# [REPLACE your existing final_assignments block with this one]
final_assignments <- tentative_assignments %>%
  # κρατάμε ids από τον αλγόριθμο
  left_join(clubs %>% select(club_id, club_name, club_capacity), by = "club_id") %>%
  mutate(
    Surname = sub("_.*", "", !!sym(student_col)),
    Name    = sub(".*?_", "", !!sym(student_col))
  ) %>%
  # κρατάμε ΟΠΩΣΔΗΠΟΤΕ τα ids για το join
  select(
    student_id = !!sym(student_col),
    Surname, Name,
    club_id, club_name,
    preference_rank, club_capacity
  ) %>%
  arrange(Surname, Name)

# --- Join με το αρχικό rank
final_assignments <- final_assignments %>%
  left_join(original_rank_map, by = c("student_id","club_id")) %>%
  # προαιρετικά: αν δεν υπάρχει στο αρχικό, άφησέ το NA ή βάλε labeling
  mutate(original_preference_rank = if_else(is.na(original_preference_rank), NA_integer_, original_preference_rank)) %>%
  arrange(Surname, Name)

# Από εδώ και κάτω, ΟΤΙ γράφεις (CSV ή Excel) θα περιέχει ΚΑΙ τη στήλη
# original_preference_rank στα assignments.

# --- [OPTIONAL] Εμπλουτισμός των student_reports με την αρχική-αρχική θέση ----
# Αν έχεις ήδη φτιάξει το data frame student_reports, κάνε enrich με join
if (exists("student_reports")) {
  # Πιάνουμε την τελική τοποθέτηση ανά μαθητή
  final_for_students <- final_assignments %>%
    select(Surname, Name, club_name, original_preference_rank) %>%
    rename(final_club = club_name)
  
  student_reports <- student_reports %>%
    left_join(final_for_students, by = c("Surname","Name")) %>%
    # Προσθέτουμε στο "outcome" την ένδειξη για το αρχικό rank, χωρίς να χαλάσουμε το υπάρχον κείμενο
    mutate(
      outcome = ifelse(
        !is.na(original_preference_rank),
        paste0(outcome, " | αρχική-αρχική θέση: ", original_preference_rank),
        outcome
      )
    )
}

# --- [OPTIONAL] Αν γράφεις parent text reports, μπορείς να εμφανίσεις την ένδειξη:
# Στο σημείο που γράφεις το FINAL ASSIGNMENT, πρόσθεσε π.χ.:
# cat(sprintf("This was your preference #%d (original #%s)\n\n",
#             final_club$preference_rank[1],
#             ifelse(is.na(final_club$original_preference_rank[1]), "n/a",
#                    final_club$original_preference_rank[1])))
# ------------------------------------------------------------------------------


# --- 7. Save Outputs ---
output_file <- file.path(output_dir,sprintf("%s_assignments.csv", tolower(day_name)))
write_csv(final_assignments, output_file)
cat(sprintf("✓ Saved assignments: %s\n", output_file))

audit_file <- file.path(output_dir, sprintf("%s_audit_log.csv", tolower(day_name)))
write_csv(audit_log, audit_file)
cat(sprintf("✓ Saved audit log: %s\n", audit_file))

# --- 8. Generate Per-Student Reports ---
cat("\nGenerating individual student reports...\n")

student_reports <- tibble()

for (student in unique(responses$student_id)) {
  surname <- sub("_.*", "", student)
  name <- sub(".*?_", "", student)
  
  # Get student's story from audit log
  student_events <- audit_log %>%
    filter(Surname == surname & Name == name) %>%
    arrange(round)
  
  # Final assignment
  final_club <- final_assignments %>%
    filter(Surname == surname & Name == name)
  
  if (nrow(final_club) > 0) {
    summary <- sprintf("Assigned to %s (preference #%d)", 
                       final_club$club_name[1], 
                       final_club$preference_rank[1])
    
    # Count proposals and rejections
    num_proposals <- sum(student_events$event == "PROPOSAL")
    num_rejections <- sum(student_events$event == "REJECTED")
    
    story <- sprintf("Made %d proposals. Rejected %d times. Finally accepted by %s in round %d.", 
                     num_proposals, 
                     num_rejections,
                     final_club$club_name[1],
                     max(student_events$round[student_events$event == "ACCEPTED"]))
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
      Surname = surname,
      Name = name,
      outcome = summary,
      story = story
    ))
}

student_reports <- student_reports %>% arrange(Surname, Name)

report_file <- file.path(output_dir, sprintf("%s_student_reports.csv", tolower(day_name)))
write_csv(student_reports, report_file)
cat(sprintf("✓ Saved student reports: %s\n", report_file))

# --- 9. Generate Parent-Friendly Individual Reports ---
cat("\nGenerating detailed parent reports...\n")

for (student in unique(responses$student_id)) {
  surname <- sub("_.*", "", student)
  name <- sub(".*?_", "", student)
  
  student_log <- audit_log %>%
    filter(Surname == surname & Name == name) %>%
    select(round, event, club_name, preference_rank, detail)
  
  if (nrow(student_log) > 0) {
    student_file <- file.path(output_dir, sprintf("%s_report_%s_%s.txt", 
                            tolower(day_name), 
                            gsub(" ", "_", surname), 
                            gsub(" ", "_", name)))
    
    sink(student_file)
    cat(sprintf("CLUB ASSIGNMENT REPORT: %s\n", day_name))
    cat(sprintf("Student: %s %s\n\n", surname, name))
    cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")
    
    final_club <- final_assignments %>%
      filter(Surname == surname & Name == name)
    
    if (nrow(final_club) > 0) {
      cat(sprintf("FINAL ASSIGNMENT: %s\n", final_club$club_name[1]))
      cat(sprintf("This was your preference #%d\n\n", final_club$preference_rank[1]))
    } else {
      cat("FINAL ASSIGNMENT: None\n\n")
    }
    
    cat("ASSIGNMENT PROCESS:\n\n")
    
    for (i in 1:nrow(student_log)) {
      log_entry <- student_log[i,]
      cat(sprintf("Round %d - %s\n", log_entry$round, log_entry$event))
      if (!is.na(log_entry$club_name)) {
        cat(sprintf("  Club: %s\n", log_entry$club_name))
      }
      if (!is.na(log_entry$detail)) {
        cat(sprintf("  %s\n", log_entry$detail))
      }
      cat("\n")
    }
    
    cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
    cat("\nThis assignment was made using the Deferred Acceptance algorithm,\n")
    cat("which ensures fair, preference-based matching.\n")
    
    sink()
  }
}

cat(sprintf("✓ Generated %d individual parent reports\n", 
            length(unique(responses$student_id))))

# --- 10. Generate Per-Club Reports ---
cat("\nGenerating club perspective reports...\n")

for (club in clubs$club_id) {
  club_name_original <- clubs %>% filter(club_id == club) %>% pull(club_name)
  club_cap <- clubs %>% filter(club_id == club) %>% pull(club_capacity)
  
  # Get all events for this club
  club_log <- audit_log %>%
    filter(club_name == club_name_original | 
             event == "CLUB_EVALUATION" & !is.na(club_name) & club_name == club_name_original) %>%
    arrange(round)
  
  if (nrow(club_log) > 0) {
    club_index <- which(clubs$club_id == club)
    club_file <- file.path(output_dir, sprintf("%s_club_report_%s.txt",
                         tolower(day_name),
                         club_name_original))
    
    
    sink(club_file)
    cat(sprintf("CLUB ASSIGNMENT REPORT: %s\n", day_name))
    cat(sprintf("Club: %s\n", club_name_original))
    cat(sprintf("Capacity: %d students\n\n", club_cap))
    cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")
    
    # Final enrollment
    final_enrolled <- final_assignments %>%
      filter(club_name == club_name_original)
    
    cat(sprintf("FINAL ENROLLMENT: %d / %d students\n\n", 
                nrow(final_enrolled), club_cap))
    
    if (nrow(final_enrolled) > 0) {
      cat("ENROLLED STUDENTS:\n")
      for (i in 1:nrow(final_enrolled)) {
        student <- final_enrolled[i,]
        cat(sprintf("  %d. %s %s (ranked this club #%d)\n", 
                    i, student$Surname, student$Name, student$preference_rank))
      }
      cat("\n")
    }
    
    cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")
    cat("SELECTION PROCESS:\n\n")
    
    # Show round-by-round decisions
    current_round <- 0
    proposals_this_round <- tibble()
    
    for (i in 1:nrow(club_log)) {
      log_entry <- club_log[i,]
      
      # New round
      if (log_entry$round != current_round) {
        # Summarize previous round if there were proposals
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
      
      # Log the event
      if (log_entry$event == "CLUB_EVALUATION") {
        cat(sprintf("  %s\n", log_entry$detail))
      } else if (log_entry$event == "PROPOSAL") {
        cat(sprintf("  → Received proposal from %s %s (their preference #%d)\n",
                    log_entry$Surname, log_entry$Name, log_entry$preference_rank))
        proposals_this_round <- proposals_this_round %>%
          bind_rows(log_entry)
      } else if (log_entry$event == "ACCEPTED") {
        cat(sprintf("  ✓ ACCEPTED: %s %s (preference #%d)\n",
                    log_entry$Surname, log_entry$Name, log_entry$preference_rank))
        proposals_this_round <- proposals_this_round %>%
          bind_rows(log_entry)
      } else if (log_entry$event == "REJECTED") {
        cat(sprintf("  ✗ REJECTED: %s %s (preference #%d) - at capacity\n",
                    log_entry$Surname, log_entry$Name, log_entry$preference_rank))
        proposals_this_round <- proposals_this_round %>%
          bind_rows(log_entry)
      } else if (log_entry$event == "RETAINED") {
        cat(sprintf("  ↻ RETAINED: %s %s (from previous round)\n",
                    log_entry$Surname, log_entry$Name))
        proposals_this_round <- proposals_this_round %>%
          bind_rows(log_entry)
      }
    }
    
    # Final round summary
    if (nrow(proposals_this_round) > 0) {
      accepted <- proposals_this_round %>% filter(event == "ACCEPTED")
      rejected <- proposals_this_round %>% filter(event == "REJECTED")
      retained <- proposals_this_round %>% filter(event == "RETAINED")
      
      cat(sprintf("  Decision: Accepted %d, Rejected %d, Retained %d\n",
                  nrow(accepted), nrow(rejected), nrow(retained)))
    }
    
    cat("\n")
    cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")
    
    # Summary statistics
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
    cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
    cat("\nThis club's enrollment was determined using the Deferred Acceptance\n")
    cat("algorithm, which ensures students are matched based on their preferences\n")
    cat("while respecting capacity constraints.\n")
    
    sink()
  }
}

# --- 11. Club Summary CSV (proposals vs accepted + ranked_by) ---
club_summary <- clubs %>%
  rowwise() %>%
  mutate(
    ranked_by = sum(student_preferences$club_id == club_id, na.rm = TRUE),
    proposals = sum(audit_log$club_name == club_name & audit_log$event == "PROPOSAL", na.rm = TRUE),
    accepted  = sum(audit_log$club_name == club_name & audit_log$event == "ACCEPTED", na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(club_name, club_capacity, ranked_by, proposals, accepted)

club_summary_file <- file.path(output_dir, sprintf("%s_club_summary.csv", tolower(day_name)))
write_csv(club_summary, club_summary_file)
cat(sprintf("✓ Saved club summary file: %s\n", club_summary_file))


# --- 12. Daily Club Summary CSV (aggregate per club) ---
daily_club_reports <- clubs %>%
  rowwise() %>%
  mutate(
    ranked_by       = sum(student_preferences$club_id == club_id, na.rm = TRUE),
    total_proposals = sum(audit_log$club_name == club_name & audit_log$event == "PROPOSAL", na.rm = TRUE),
    total_accepted  = sum(audit_log$club_name == club_name & audit_log$event == "ACCEPTED", na.rm = TRUE),
    total_rejected  = sum(audit_log$club_name == club_name & audit_log$event == "REJECTED", na.rm = TRUE),
    enrolled        = sum(final_assignments$club_name == club_name, na.rm = TRUE),
    avg_rank        = ifelse(enrolled > 0,
                             mean(final_assignments$preference_rank[final_assignments$club_name == club_name], na.rm = TRUE),
                             NA_real_),
    fill_rate       = ifelse(club_capacity > 0, round(100 * enrolled / club_capacity, 1), 0)
  ) %>%
  ungroup() %>%
  select(club_name, club_capacity, enrolled, fill_rate, avg_rank,
         ranked_by, total_proposals, total_accepted, total_rejected)

daily_club_reports_file <- file.path(output_dir, sprintf("%s_club_reports.csv", tolower(day_name)))
write_csv(daily_club_reports, daily_club_reports_file)
cat(sprintf("✓ Saved daily club reports summary: %s\n", daily_club_reports_file))

# --- 13. Create Excel workbook with one sheet per club ---
suppressPackageStartupMessages(library(writexl))

excel_file <- file.path(output_dir, sprintf("%s_club_members.xlsx", tolower(day_name)))

# Build a list of data frames, one per club
club_sheets <- list()

for (club in unique(final_assignments$club_name)) {
  df <- final_assignments %>%
    filter(club_name == club) %>%
    select(Surname, Name, preference_rank, original_preference_rank) %>%
    arrange(Surname, Name)
  
  # Clean sheet name: remove illegal Excel chars and shorten if needed
  sheet_name <- gsub("[\\/:*?\\[\\]]", "_", substr(club, 1, 31))
  club_sheets[[sheet_name]] <- df
}

# Write all sheets to one Excel file
writexl::write_xlsx(club_sheets, excel_file)

cat(sprintf("✓ Saved Excel file with per-club sheets: %s\n", excel_file))


cat("\n=== AUDIT LOG COMPLETE ===\n")
cat("\nFiles created:\n")
cat(sprintf("  1. %s - Final assignments\n", output_file))
cat(sprintf("  2. %s - Complete audit log\n", audit_file))
cat(sprintf("  3. %s - Student summaries\n", report_file))
cat(sprintf("  4. Individual student reports: %s_report_[Surname]_[Name].txt\n", tolower(day_name)))
cat(sprintf("  5. Individual club reports: %s_club_report_[ClubName].txt\n", tolower(day_name)))