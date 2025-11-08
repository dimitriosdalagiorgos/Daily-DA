# ----------------------------------------------------------
# remove_first_choice_interactive.R
# ----------------------------------------------------------
# Removes the first-choice club (rank = 1) for a specified student
# in dailyresponses.csv and shifts all other ranks up by one.
# ----------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

cat("=== REMOVE FIRST CHOICE FOR A STUDENT ===\n\n")

# --- Step 1: Load file ---
responses_file <- "dailyresponses.csv"
if (!file.exists(responses_file)) stop("File dailyresponses.csv not found.")
responses <- read_csv(responses_file, show_col_types = FALSE)

if (!all(c("Surname", "Name") %in% colnames(responses))) {
  stop("dailyresponses.csv must start with columns: Surname, Name")
}

club_cols <- colnames(responses)[-(1:2)]

# --- Step 2: Ask for student ---
surname_input <- readline(prompt = "Enter student's surname: ")
name_input    <- readline(prompt = "Enter student's name: ")

surname_input <- trimws(surname_input)
name_input    <- trimws(name_input)

# --- Step 3: Find the student ---
student_row <- which(responses$Surname == surname_input &
                       responses$Name == name_input)

if (length(student_row) == 0) {
  stop("No matching student found in the file.")
}
if (length(student_row) > 1) {
  cat("Warning: multiple students found; editing the first match.\n")
  student_row <- student_row[1]
}

# --- Step 4: Remove first-choice (rank = 1) and shift others ---
ranks <- as.integer(unlist(responses[student_row, club_cols], use.names = FALSE))

if (all(is.na(ranks))) {
  cat("No rankings found for this student.\n")
} else {
  club_removed <- names(responses[student_row, club_cols])[which(ranks == 1)]
  
  ranks <- ifelse(is.na(ranks), NA, ranks)
  ranks <- ifelse(ranks > 1, ranks - 1, ifelse(ranks == 1, NA, ranks))
  
  # ✅ Correct tibble assignment
  responses[student_row, club_cols] <- as.list(ranks)
  
  cat(sprintf("\nRemoved first choice (%s) and shifted rankings.\n",
              if (length(club_removed)) paste(club_removed, collapse = ", ") else "none found"))
  
  # --- Step 5: Save updated file ---
  readr::write_csv(responses, responses_file, na = "")
  cat(sprintf("Updated file saved: %s\n", responses_file))
  
   # --- Step 6: Optional log for audit trail ---
  log_file <- "first_choice_removals_log.csv"
  log_entry <- tibble(
    Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    Surname = surname_input,
    Name = name_input,
    Club_Removed = ifelse(length(club_removed) > 0, club_removed, "none")
  )
  
  if (file.exists(log_file)) {
    log_old <- read_csv(log_file, show_col_types = FALSE) %>%
      mutate(Timestamp = as.character(Timestamp))   # ✅ ensure same type
    log_entry <- log_entry %>% mutate(Timestamp = as.character(Timestamp))
    log_entry <- bind_rows(log_old, log_entry)
  }
  
  write_csv(log_entry, log_file)
  cat(sprintf("Logged change to: %s\n", log_file))
}