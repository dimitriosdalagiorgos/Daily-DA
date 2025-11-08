# prepare_dailyresponses.R
# ----------------------------------------------------------
# Cleans dailyresponses.csv for the Deferred Acceptance run:
#  1. Removes one specified club and re-adjusts rankings
#  2. Removes students already assigned to double clubs
#  3. Produces final dailyresponses_clean.csv
# ----------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

cat("=== PREPARING DAILY RESPONSES ===\n\n")

# --- Step 1: Load input files ---
responses_file <- "dailyresponses.csv"
double_file    <- "doubleclubselections.csv"

if (!file.exists(responses_file)) stop("Missing file: dailyresponses.csv")
if (!file.exists(double_file))    stop("Missing file: doubleclubselections.csv")

responses <- read_csv(responses_file, show_col_types = FALSE)
doubleclubs <- read_csv(double_file, show_col_types = FALSE)

# --- Step 2: Ask which club to remove ---
cat("Available clubs:\n")
print(colnames(responses)[-(1:2)])
club_to_remove <- readline(prompt = "Enter the exact name of the club to remove: ")
club_to_remove <- trimws(club_to_remove)

if (!club_to_remove %in% colnames(responses)) {
  stop(sprintf("Club '%s' not found in the file.", club_to_remove))
}

cat(sprintf("\n→ Removing club: %s\n", club_to_remove))

# --- Step 3: Remove the club column ---
responses <- responses %>% select(-all_of(club_to_remove))

# --- Step 4: Adjust rankings ---
# Convert numeric columns safely, keep blanks as blanks
rank_cols <- colnames(responses)[-(1:2)]

for (col in rank_cols) {
  responses[[col]] <- suppressWarnings(as.integer(responses[[col]]))
}

# Function to adjust a single row’s ranks
adjust_ranks <- function(row) {
  numeric_ranks <- unlist(row[rank_cols])
  if (all(is.na(numeric_ranks))) return(row)
  ordered <- order(numeric_ranks, na.last = NA)
  # Reassign ranks 1,2,3,... in order
  new_ranks <- rep(NA_integer_, length(numeric_ranks))
  new_ranks[ordered] <- seq_along(ordered)
  row[rank_cols] <- new_ranks
  row
}

responses <- as.data.frame(t(apply(responses, 1, adjust_ranks))) %>%
  mutate(across(everything(), \(x) ifelse(is.na(x), "", x)))

# Restore column types and names
colnames(responses) <- c("Surname", "Name", rank_cols)

# --- Step 5: Remove students already assigned to double clubs ---
removed_students <- nrow(doubleclubs)
responses <- responses %>%
  anti_join(doubleclubs, by = c("Surname", "Name"))

# --- Step 6: Save output ---
output_file <- "dailyresponses_clean.csv"
write_csv(responses, output_file, na = "")

# --- Step 7: Summary ---
cat("\n=== SUMMARY ===\n")
cat(sprintf("Removed club: %s\n", club_to_remove))
cat(sprintf("Removed students already in double clubs: %d\n", removed_students))
cat(sprintf("Remaining students: %d\n", nrow(responses)))
cat(sprintf("Final output written to: %s\n", output_file))
cat("============================\n")
