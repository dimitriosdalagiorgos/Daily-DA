# ----------------------------------------------------------
# histograms.R
# ----------------------------------------------------------
# Histograms of the allocation: how many students got their 1st choice,
# their 2nd, their 3rd … — every place separately, never grouped
# ("6th and below"). One chart for the whole week, one per day, and one
# per club.
#
# The place is the club's position in the parent's declaration
# (preferences.csv, as the parent submitted it), not the renumbered
# position after removals (exclusions, similar or multi-day clubs).
# A multi-day club is judged on its first day, and its place comes from
# that day; on its later days the student keeps the same place, so the
# day charts count it on every day it runs, while the club chart counts
# each student once.
# "Χωρίς όμιλο": the parent declared for that day, but the student got
# no club. Students with no declaration for the day are not in the chart
# (their number is in the subtitle).
#
# Input: the platform's data package (students.csv, clubs.csv,
# preferences.csv) and the week_assignments.csv written by run_week.R.
#
# Output folder (default: <data_folder>/results/istogrammata):
#   istogrammata.pdf       all charts, one A4 landscape page each
#   ebdomada.png           whole week
#   imeres.png             the five days side by side
#   imera_<day>.png        the clubs of each day
#   omiloi/<code>.png      one chart per club (e.g. for its teachers)
#   istogrammata.xlsx      the same numbers as tables
#
# Usage:
#   Rscript R/run_week.R <data_folder>            (first, once)
#   Rscript R/histograms.R <data_folder> [week_assignments.csv] [output_folder]
#
# Needs: install.packages(c("dplyr", "readr", "tidyr", "ggplot2", "writexl"))
# ----------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(writexl)
})

args <- commandArgs(trailingOnly = TRUE)
if (!length(args) %in% 1:3) {
  stop("Χρήση: Rscript R/histograms.R <φάκελος_δεδομένων> [week_assignments.csv] [φάκελος_αποτελεσμάτων]")
}
data_dir <- args[1]
assignments_file <- if (length(args) >= 2) args[2] else file.path(data_dir, "results", "week_assignments.csv")
output_dir <- if (length(args) == 3) args[3] else file.path(dirname(assignments_file), "istogrammata")

DAY_KEYS <- c("mon", "tue", "wed", "thu", "fri")
day_labels <- c(mon = "Δευτέρα", tue = "Τρίτη", wed = "Τετάρτη", thu = "Πέμπτη", fri = "Παρασκευή")
day_files <- c(mon = "deutera", tue = "triti", wed = "tetarti", thu = "pempti", fri = "paraskeui")

read_input <- function(path) {
  if (!file.exists(path)) {
    hint <- if (grepl("week_assignments", path)) sprintf(" Τρέξτε πρώτα: Rscript R/run_week.R %s", data_dir) else ""
    stop(sprintf("Λείπει το αρχείο %s.%s", path, hint))
  }
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"), locale = locale(encoding = "UTF-8"))
}

students <- read_input(file.path(data_dir, "students.csv"))
clubs <- read_input(file.path(data_dir, "clubs.csv")) %>%
  mutate(capacity = as.integer(capacity), first_day = sub(";.*$", "", days))
prefs <- read_input(file.path(data_dir, "preferences.csv")) %>% mutate(rank = as.integer(rank))
week <- read_input(assignments_file) %>% filter(RegistryNr %in% students$RegistryNr)
if (nrow(prefs) == 0) {
  stop("Δεν υπάρχουν δηλώσεις γονέων (το preferences.csv είναι άδειο): δεν υπάρχει τίποτα για ιστογράμματα.")
}

# ---------- Place of each assignment in the declaration ----------

# The club's place on its first day (where it is judged); any other day as
# a fallback, for packages where the club is listed on one day only.
place <- prefs %>%
  left_join(clubs %>% select(club_code = code, first_day), by = "club_code") %>%
  arrange(RegistryNr, club_code, day != first_day, match(day, DAY_KEYS)) %>%
  distinct(RegistryNr, club_code, .keep_all = TRUE) %>%
  select(RegistryNr, club_code, rank)

placed <- week %>% left_join(place, by = c("RegistryNr", "club_code"))
unknown <- placed %>% filter(is.na(rank))
if (nrow(unknown) > 0) {
  warning(sprintf("%d τοποθετήσεις σε όμιλο που δεν υπάρχει στη δήλωση του μαθητή (δεν μετρούν): ΑΜ %s",
                  nrow(unknown), paste(head(unique(unknown$RegistryNr), 10), collapse = ", ")))
  placed <- placed %>% filter(!is.na(rank))
}

declared <- prefs %>% distinct(RegistryNr, day)
unplaced <- declared %>% anti_join(week, by = c("RegistryNr", "day"))

# Students of a grade with clubs that day, but no declaration and no club
offered <- clubs %>%
  separate_rows(days, sep = ";") %>% separate_rows(grades, sep = ";") %>%
  distinct(day = days, grade = grades)
no_declaration <- students %>%
  inner_join(offered, by = "grade", relationship = "many-to-many") %>%
  anti_join(declared, by = c("RegistryNr", "day")) %>%
  anti_join(week, by = c("RegistryNr", "day")) %>%
  count(day, name = "n")

# Longest list of each day: the x axis shows every place up to it
max_rank_day <- prefs %>% group_by(day) %>% summarise(max_rank = max(rank), .groups = "drop")

# ---------- Counts ----------

UNPLACED <- -1L  # rank code of "Χωρίς όμιλο"

complete_ranks <- function(counts, max_rank) {
  full <- tibble(rank = c(seq_len(max_rank), UNPLACED))
  full %>% left_join(counts, by = "rank") %>% mutate(n = coalesce(n, 0L))
}

day_counts <- bind_rows(lapply(intersect(DAY_KEYS, max_rank_day$day), function(d) {
  counts <- bind_rows(
    placed %>% filter(day == d) %>% count(rank),
    tibble(rank = UNPLACED, n = sum(unplaced$day == d))
  )
  complete_ranks(counts, max_rank_day$max_rank[max_rank_day$day == d]) %>% mutate(day = d)
}))

week_counts <- complete_ranks(
  day_counts %>% group_by(rank) %>% summarise(n = sum(n), .groups = "drop"),
  max(max_rank_day$max_rank)
)

# Per club: each student once, on the club's first day
club_counts <- bind_rows(lapply(seq_len(nrow(clubs)), function(i) {
  cl <- clubs[i, ]
  max_rank <- max(c(1L, max_rank_day$max_rank[max_rank_day$day == cl$first_day]))
  counts <- placed %>% filter(club_code == cl$code, day == cl$first_day) %>% count(rank)
  tibble(rank = seq_len(max_rank)) %>%
    left_join(counts, by = "rank") %>%
    mutate(n = coalesce(n, 0L), club_code = cl$code)
}))

# ---------- Look ----------
# Light chart surface, one blue for the places, a neutral gray for
# "Χωρίς όμιλο". Counts are printed on the bars (the chart is static),
# text stays in ink colours, the grid is a hairline.

INK <- "#0b0b0b"
INK_2 <- "#52514e"
MUTED <- "#898781"
GRID <- "#e1e0d9"
AXIS <- "#c3c2b7"
SURFACE <- "#fcfcfb"
BAR <- "#2a78d6"
BAR_UNPLACED <- "#898781"

theme_hist <- function(base = 11) {
  theme_minimal(base_size = base, base_family = "sans") +
    theme(
      plot.background = element_rect(fill = SURFACE, colour = NA),
      panel.background = element_rect(fill = SURFACE, colour = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = GRID, linewidth = 0.3),
      axis.line.x = element_line(colour = AXIS, linewidth = 0.4),
      axis.ticks = element_blank(),
      axis.text = element_text(colour = INK_2, size = rel(0.85)),
      axis.title = element_text(colour = INK_2, size = rel(0.85)),
      axis.title.x = element_text(margin = margin(t = 8)),
      plot.title = element_text(colour = INK, face = "bold", size = rel(1.25), margin = margin(b = 4)),
      plot.subtitle = element_text(colour = INK_2, size = rel(0.9), margin = margin(b = 12), lineheight = 1.15),
      plot.caption = element_text(colour = MUTED, size = rel(0.75), hjust = 0, margin = margin(t = 10)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      strip.text = element_text(colour = INK, face = "bold", hjust = 0, size = rel(0.9), margin = margin(b = 2)),
      panel.spacing.x = unit(18, "pt"),
      panel.spacing.y = unit(14, "pt"),
      plot.margin = margin(16, 20, 12, 16)
    )
}

ordinal <- function(k) paste0(k, "η")
pct <- function(x, total) {
  if (total == 0) return("–")
  if (x > 0 && 100 * x / total < 0.5) return("<1%")
  paste0(round(100 * x / total), "%")
}
num <- function(x) formatC(x, format = "d", big.mark = ".", decimal.mark = ",")

# x position: places at 1..k, "Χωρίς όμιλο" a little apart at k + 1.6
with_x <- function(d, max_rank) {
  d %>% mutate(x = ifelse(rank == UNPLACED, max_rank + 1.6, rank),
               kind = ifelse(rank == UNPLACED, "unplaced", "place"))
}
x_scale <- function(max_rank, unplaced = TRUE, label_every = 1) {
  breaks <- seq_len(max_rank)
  labels <- ifelse(breaks == 1 | breaks %% label_every == 0, ordinal(breaks), "")
  if (unplaced) {
    breaks <- c(breaks, max_rank + 1.6)
    labels <- c(labels, "Χωρίς\nόμιλο")
  }
  scale_x_continuous(breaks = breaks, labels = labels, expand = expansion(add = 0.6))
}

summary_line <- function(counts) {
  places <- counts %>% filter(rank != UNPLACED)
  total <- sum(counts$n)
  first <- sum(places$n[places$rank == 1])
  top3 <- sum(places$n[places$rank <= 3])
  none <- sum(counts$n[counts$rank == UNPLACED])
  mean_rank <- if (sum(places$n) > 0) sum(places$rank * places$n) / sum(places$n) else NA
  sprintf("1η επιλογή: %s · έως 3η: %s · μέση θέση: %s · χωρίς όμιλο: %d",
          pct(first, total), pct(top3, total),
          ifelse(is.na(mean_rank), "–", formatC(mean_rank, format = "f", digits = 2, decimal.mark = ",")), none)
}

hist_layers <- function(d, label_size = 3.2, show_pct = TRUE, total = NULL) {
  d <- d %>% mutate(label = if (show_pct && !is.null(total) && total > 0)
                      ifelse(n > 0, paste0(num(n), "\n", vapply(n, pct, "", total = total)), "0") else num(n))
  list(
    geom_col(data = d, aes(x = x, y = n, fill = kind), width = 0.78, colour = SURFACE, linewidth = 0.5),
    geom_text(data = d, aes(x = x, y = n, label = label, colour = n == 0),
              vjust = -0.35, size = label_size, lineheight = 0.9),
    scale_fill_manual(values = c(place = BAR, unplaced = BAR_UNPLACED), guide = "none"),
    scale_colour_manual(values = c(`FALSE` = INK, `TRUE` = MUTED), guide = "none"),
    scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = function(v) formatC(v, format = "fg", big.mark = ".", decimal.mark = ",")),
    labs(x = "Θέση του ομίλου στη δήλωση", y = "Μαθητές")
  )
}

caption_text <- function() {
  "Θέση = η σειρά του ομίλου στη δήλωση του γονέα. «Χωρίς όμιλο»: υπήρχε δήλωση για την ημέρα, αλλά ο μαθητής δεν τοποθετήθηκε."
}

# ---------- Charts ----------

charts <- list()  # name → list(plot, width, height) in inches

# Whole week (each student counted once per day)
wk_max <- max(max_rank_day$max_rank)
wk <- with_x(week_counts, wk_max)
wk_total <- sum(wk$n)
charts$ebdomada <- list(
  plot = ggplot() + hist_layers(wk, total = wk_total) + x_scale(wk_max) + theme_hist() +
    labs(title = "Όλη η εβδομάδα: σε ποια επιλογή μπήκαν οι μαθητές",
         subtitle = sprintf("%s τοποθετήσεις (μαθητής × ημέρα)\n%s",
                            num(wk_total), summary_line(week_counts)),
         caption = caption_text()),
  width = 11.69, height = 8.27)

# Five days side by side, same y scale so the days compare
dd <- day_counts %>%
  left_join(max_rank_day, by = "day") %>%
  group_by(day) %>%
  mutate(x = ifelse(rank == UNPLACED, max_rank + 1.6, rank),
         kind = ifelse(rank == UNPLACED, "unplaced", "place"),
         total = sum(n)) %>%
  ungroup()
strip <- dd %>% group_by(day) %>%
  summarise(lbl = sprintf("%s · %d μαθητές\n%s", day_labels[first(day)], sum(n), summary_line(tibble(rank = rank, n = n))),
            .groups = "drop")
dd <- dd %>% left_join(strip, by = "day") %>%
  mutate(lbl = factor(lbl, levels = strip$lbl[order(match(strip$day, DAY_KEYS))]),
         label = ifelse(n > 0, paste0(num(n), "\n", mapply(pct, n, total)), "0"))
day_breaks <- function(lims) {
  k <- floor(lims[2] - 0.6 - 1.6 + 0.01)
  c(seq_len(max(1, round(k))), round(k) + 1.6)
}
day_labels_fn <- function(b) ifelse(b == round(b), ordinal(b), "Χωρίς\nόμιλο")
charts$imeres <- list(
  plot = ggplot(dd, aes(x = x, y = n)) +
    geom_col(aes(fill = kind), width = 0.78, colour = SURFACE, linewidth = 0.5) +
    geom_text(aes(label = label, colour = n == 0), vjust = -0.35, size = 2.5, lineheight = 0.9) +
    facet_wrap(~lbl, ncol = 2, scales = "free_x") +
    scale_fill_manual(values = c(place = BAR, unplaced = BAR_UNPLACED), guide = "none") +
    scale_colour_manual(values = c(`FALSE` = INK, `TRUE` = MUTED), guide = "none") +
    scale_x_continuous(breaks = day_breaks, labels = day_labels_fn, expand = expansion(add = 0.6)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
    labs(x = "Θέση του ομίλου στη δήλωση", y = "Μαθητές",
         title = "Ανά ημέρα: σε ποια επιλογή μπήκαν οι μαθητές",
         subtitle = "Ίδια κλίμακα μαθητών σε όλες τις ημέρες· κάθε ημέρα έχει τόσες θέσεις όσες η μεγαλύτερη δήλωσή της.",
         caption = caption_text()) +
    theme_hist(10),
  width = 11.69, height = 11.69)

# Clubs: one chart each, and one sheet per day with all its clubs
club_info <- clubs %>%
  mutate(day_names = vapply(strsplit(days, ";"), function(x) paste(day_labels[x], collapse = " + "), ""),
         first_order = match(first_day, DAY_KEYS)) %>%
  arrange(first_order, as.numeric(code))
club_info$enrolled <- vapply(club_info$code, function(cd) sum(club_counts$n[club_counts$club_code == cd]), 0L)
club_title <- function(cl) sprintf("%s %s", cl$code, cl$name)
club_sub <- function(cl) {
  cc <- club_counts %>% filter(club_code == cl$code)
  multi <- grepl(";", cl$days)
  sprintf("%s%s · %d μαθητές σε %d θέσεις · 1η επιλογή: %d",
          cl$day_names, ifelse(multi, " (κρίνεται την 1η ημέρα)", ""),
          cl$enrolled, cl$capacity, sum(cc$n[cc$rank == 1]))
}

dir.create(file.path(output_dir, "omiloi"), showWarnings = FALSE, recursive = TRUE)
# Images of an earlier run (a club removed or moved to another day) must
# not stay next to the new ones: remove only the files this script writes.
unlink(c(list.files(output_dir, pattern = "^(ebdomada|imeres|imera_[a-z]+)\\.png$", full.names = TRUE),
         list.files(file.path(output_dir, "omiloi"), pattern = "\\.png$", full.names = TRUE)))
club_plots <- list()
for (i in seq_len(nrow(club_info))) {
  cl <- club_info[i, ]
  cc <- club_counts %>% filter(club_code == cl$code) %>% mutate(x = rank, kind = "place")
  mr <- max(cc$rank)
  club_plots[[cl$code]] <- list(
    plot = ggplot() + hist_layers(cc, total = cl$enrolled) + x_scale(mr, unplaced = FALSE) + theme_hist() +
      labs(title = club_title(cl), subtitle = club_sub(cl),
           caption = "Θέση = η σειρά του ομίλου στη δήλωση του γονέα. Κάθε μαθητής μετρά μία φορά, ακόμα κι αν ο όμιλος γίνεται πολλές ημέρες."),
    width = 8, height = 5)
}

for (d in intersect(DAY_KEYS, club_info$first_day)) {
  cd <- club_info %>% filter(first_day == d)
  fc <- club_counts %>% filter(club_code %in% cd$code) %>%
    left_join(cd %>% transmute(club_code = code,
                               lbl = sprintf("%s\n%d μαθητές / %d θέσεις%s", club_title(cd), enrolled, capacity,
                                             ifelse(grepl(";", days), paste0("\n", day_names), ""))),
              by = "club_code") %>%
    group_by(club_code) %>%
    mutate(label = ifelse(n > 0, paste0(n, "\n", vapply(n, pct, "", total = sum(n))), "0")) %>%
    ungroup()
  fc$lbl <- factor(fc$lbl, levels = unique(fc$lbl[order(match(fc$club_code, cd$code))]))
  n_clubs <- nrow(cd)
  ncol <- if (n_clubs <= 4) 2 else 3
  nrow_ <- ceiling(n_clubs / ncol)
  charts[[paste0("imera_", day_files[[d]])]] <- list(
    plot = ggplot(fc, aes(x = rank, y = n)) +
      geom_col(fill = BAR, width = 0.78, colour = SURFACE, linewidth = 0.4) +
      geom_text(aes(label = label, colour = n == 0), vjust = -0.3, size = 2.4, lineheight = 0.9) +
      facet_wrap(~lbl, ncol = ncol, scales = "free") +
      scale_colour_manual(values = c(`FALSE` = INK, `TRUE` = MUTED), guide = "none") +
      scale_x_continuous(breaks = seq_len(max(fc$rank)), labels = ordinal(seq_len(max(fc$rank))),
                         expand = expansion(add = 0.6)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.32)), breaks = function(l) pretty(l, 3)) +
      labs(x = "Θέση του ομίλου στη δήλωση", y = "Μαθητές",
           title = sprintf("%s: οι όμιλοι της ημέρας", day_labels[[d]]),
           subtitle = "Σε ποια επιλογή μπήκαν οι μαθητές κάθε ομίλου. Η κλίμακα των μαθητών διαφέρει από όμιλο σε όμιλο.",
           caption = "Οι όμιλοι πολλών ημερών εμφανίζονται στην 1η τους ημέρα, όπου κρίνονται.") +
      theme_hist(10),
    width = 11.69, height = max(5, 1.2 + 2.6 * nrow_))
}

# ---------- Write ----------

save_png <- function(ch, path) {
  ggsave(path, ch$plot, width = ch$width, height = ch$height, dpi = 200, bg = SURFACE)
}
for (nm in names(charts)) save_png(charts[[nm]], file.path(output_dir, paste0(nm, ".png")))
for (cd in names(club_plots)) save_png(club_plots[[cd]], file.path(output_dir, "omiloi", paste0(cd, ".png")))

pdf_file <- file.path(output_dir, "istogrammata.pdf")
grDevices::cairo_pdf(pdf_file, width = 11.69, height = 8.27, onefile = TRUE, bg = SURFACE)
order_pdf <- c("ebdomada", "imeres", grep("^imera_", names(charts), value = TRUE))
for (nm in order_pdf) print(charts[[nm]]$plot)
for (cd in names(club_plots)) print(club_plots[[cd]]$plot)
invisible(dev.off())

# Tables: the same numbers, one column per place
wide <- function(counts, id_cols) {
  counts %>%
    mutate(col = ifelse(rank == UNPLACED, "Χωρίς όμιλο", ordinal(rank)),
           order = ifelse(rank == UNPLACED, 1e6, rank)) %>%
    arrange(order) %>%
    select(all_of(id_cols), col, n) %>%
    pivot_wider(names_from = col, values_from = n, values_fill = 0L)
}
day_table <- bind_rows(
  wide(day_counts %>% mutate(`Ημέρα` = day_labels[day]), "Ημέρα"),
  wide(week_counts %>% mutate(`Ημέρα` = "Όλη η εβδομάδα"), "Ημέρα")
) %>% mutate(across(-`Ημέρα`, ~ coalesce(.x, 0L)))
day_table$`Σύνολο` <- rowSums(day_table[, -1])
day_table$`Χωρίς δήλωση` <- c(
  vapply(intersect(DAY_KEYS, max_rank_day$day), function(d) sum(no_declaration$n[no_declaration$day == d]), 0L),
  sum(no_declaration$n))
club_table <- wide(club_counts %>%
                     left_join(club_info %>% transmute(club_code = code, `Κωδικός` = code, `Όμιλος` = name,
                                                       `Ημέρες` = day_names, `Θέσεις` = capacity),
                               by = "club_code"),
                   c("Κωδικός", "Όμιλος", "Ημέρες", "Θέσεις")) %>%
  mutate(across(-c(`Κωδικός`, `Όμιλος`, `Ημέρες`, `Θέσεις`), ~ coalesce(.x, 0L)))
club_table$`Μαθητές` <- rowSums(club_table[, -(1:4)])
club_table <- club_table[match(club_info$code, club_table$`Κωδικός`), ]
write_xlsx(list(`Ανά ημέρα` = day_table, `Ανά όμιλο` = club_table), file.path(output_dir, "istogrammata.xlsx"))

# ---------- Console summary ----------

cat("Σε ποια επιλογή μπήκαν οι μαθητές\n\n")
print(as.data.frame(day_table), row.names = FALSE)
cat(sprintf("\n✓ Ιστογράμματα: %s\n  istogrammata.pdf, istogrammata.xlsx, %d εικόνες (εβδομάδα και ημέρες) και %d ομίλων (φάκελος omiloi)\n",
            normalizePath(output_dir), length(charts), length(club_plots)))
