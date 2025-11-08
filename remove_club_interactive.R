# remove_club_interactive.R
library(dplyr)
library(readr)

# --- Ρυθμίσεις ---
input_file <- "dailyresponses.csv"
output_file <- "dailyresponses_updated.csv"

# --- Φόρτωση δεδομένων ---
df <- read_csv(input_file)

# --- Εντοπισμός στηλών που είναι Όμιλοι ---
club_cols <- colnames(df)[3:ncol(df)]

cat("Διαθέσιμοι Όμιλοι:\n")
print(club_cols)

# --- Εισαγωγή Ομίλου προς αφαίρεση ---
club_to_remove <- readline(prompt = "Πληκτρολόγησε τον Όμιλο που θέλεις να αφαιρέσεις: ")

if (!(club_to_remove %in% club_cols)) {
  stop(paste("⚠️ Ο Όμιλος", club_to_remove, "δεν βρέθηκε στο αρχείο."))
}

cat(paste0("➡️ Αφαιρείται ο Όμιλος: ", club_to_remove, "\n"))

# --- Αφαίρεση του Ομίλου ---
club_cols <- setdiff(club_cols, club_to_remove)
df_removed <- df %>% select(-all_of(club_to_remove))

# --- Αναπροσαρμογή προτιμήσεων ---
for (i in 1:nrow(df)) {
  removed_rank <- df[i, club_to_remove, drop = TRUE]
  if (!is.na(removed_rank)) {
    # Μείωσε κατά 1 όλες τις προτιμήσεις μεγαλύτερες από αυτήν
    for (col in club_cols) {
      if (!is.na(df_removed[i, col]) && df_removed[i, col] > removed_rank) {
        df_removed[i, col] <- df_removed[i, col] - 1
      }
    }
  }
}

# --- Αποθήκευση νέου αρχείου ---
write_csv(df_removed, output_file, na = "")

cat(paste0("✅ Ο Όμιλος '", club_to_remove, "' αφαιρέθηκε και οι προτιμήσεις αναπροσαρμόστηκαν.\n"))
cat(paste0("💾 Το νέο αρχείο αποθηκεύτηκε ως: ", output_file, "\n"))
