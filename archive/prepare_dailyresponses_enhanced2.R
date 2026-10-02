# ----------------------------------------------------------
# prepare_dailyresponses_enhanced.R
# ----------------------------------------------------------
# Cleans dailyresponses.csv for Deferred Acceptance matching.
# 1. Removes all clubs listed in clubs_to_remove.csv
# 2. Adjusts rankings after removals
# 3. Removes students listed in students_to_remove.csv
# 4. Creates a timestamped backup of the original file
# 5. Overwrites dailyresponses.csv with the cleaned data
# ----------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

cat("=== PREPARING DAILY RESPONSES (Enhanced) ===\n\n")

# --- Step 1: Input files ---
responses_file <- "dailyresponses.csv"
clubs_file     <- "clubs_to_remove.csv"
students_file  <- "students_to_remove.csv"

# --- Step 2: Check file existence ---
if (!file.exists(responses_file)) stop("Missing file: dailyresponses.csv")
if (!file.exists(clubs_file))     stop("Missing file: clubs_to_remove.csv")
if (!file.exists(students_file))  stop("Missing file: students_to_remove.csv")

# --- Step 3: Make a timestamped backup of the original file ---
timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
backup_file <- sprintf("dailyresponses_backup_%s.csv", timestamp)
file.copy(responses_file, backup_file, overwrite = TRUE)
cat(sprintf("→ Backup created: %s\n\n", backup_file))

# --- Step 4: Load input files ---
responses <- read_csv(responses_file, show_col_types = FALSE)
clubs_to_remove <- read_csv(clubs_file, show_col_types = FALSE)
students_to_remove <- read_csv(students_file, show_col_types = FALSE)

# --- Step 5: Validate structure ---
if (!all(c("Surname", "Name") %in% colnames(responses))) {
  stop("dailyresponses.csv must start with columns: Surname, Name")
}
if (!"club_name" %in% tolower(colnames(clubs_to_remove))) {
  stop("clubs_to_remove.csv must contain a 'club_name' column")
}
if (!all(c("Surname", "Name") %in% colnames(students_to_remove))) {
  stop("students_to_remove.csv must contain columns: Surname, Name")
}

# --- Step 6: Normalize club names ---
clubs_to_remove <- clubs_to_remove %>%
  mutate(across(everything(), \(x) trimws(tolower(x))))

response_cols <- colnames(responses)
club_cols <- response_cols[-(1:2)]

# --- Step 7: Remove selected clubs ---
clubs_to_delete <- trimws(tolower(clubs_to_remove$club_name))
existing_clubs <- tolower(club_cols)

found <- intersect(existing_clubs, clubs_to_delete)
missing <- setdiff(clubs_to_delete, existing_clubs)

if (length(found) == 0) {
  stop("No matching club names found in dailyresponses.csv.")
}

cat("→ Clubs to remove:\n")
cat(paste("  -", found, collapse = "\n"), "\n")

if (length(missing) > 0) {
  cat("\nWarning: These clubs were not found in the file and were skipped:\n")
  cat(paste("  -", missing, collapse = "\n"), "\n")
}

# Keep only columns NOT in the removal list
keep_cols <- c("Surname", "Name", club_cols[!tolower(club_cols) %in% found])
responses <- responses %>% select(all_of(keep_cols))

# --- Step 8: Adjust rankings ---
rank_cols <- colnames(responses)[-(1:2)]
responses[rank_cols] <- lapply(responses[rank_cols], function(x) suppressWarnings(as.integer(x)))

adjust_ranks <- function(row) {
  ranks <- unlist(row[rank_cols])
  if (all(is.na(ranks))) return(row)
  order_idx <- order(ranks, na.last = NA)
  new_ranks <- rep(NA_integer_, length(ranks))
  new_ranks[order_idx] <- seq_along(order_idx)
  row[rank_cols] <- new_ranks
  row
}

responses <- as.data.frame(t(apply(responses, 1, adjust_ranks))) %>%
  mutate(across(everything(), \(x) ifelse(is.na(x), "", x)))

colnames(responses) <- keep_cols

# --- Step 9: Remove listed students ---
responses <- responses %>%
  anti_join(students_to_remove, by = c("Surname", "Name"))

# --- Step 10: Save cleaned output (overwrite original) ---
write_csv(responses, responses_file, na = "")

# --- Step 11: Summary ---
cat("\n=== SUMMARY ===\n")
cat(sprintf("Backup file created: %s\n", backup_file))
cat(sprintf("Clubs removed: %d (%s)\n",
            length(found), paste(found, collapse = ", ")))
cat(sprintf("Students removed: %d\n", nrow(students_to_remove)))
cat(sprintf("Remaining students: %d\n", nrow(responses)))
cat(sprintf("Updated file saved as: %s (original name overwritten)\n", responses_file))
cat("============================\n")
