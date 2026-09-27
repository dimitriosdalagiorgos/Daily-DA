# ----------------------------------------------------------
# lottery.R
# ----------------------------------------------------------
# The platform's lottery, computed independently in R: one fixed number
# per student from a published seed. Gives exactly the same numbers as
# platform/src/algorithm/lottery.js.
#
# Procedure:
#   1. Sort the registry numbers (ΑΜ) numerically.
#   2. Seed a random generator (sfc32) from the seed text (cyrb128 hash).
#   3. Shuffle the sorted list (Fisher–Yates).
#   4. The student at position i gets lottery number i (lower wins ties).
#
# Usage from R:
#   source("R/lottery.R")
#   draw_lottery(c("5742", "5414", "5743"), "seed text")
# or from the command line (writes RegistryNr,lottery_number):
#   Rscript R/lottery.R students.csv "seed text" lottery.csv
# ----------------------------------------------------------

# 32-bit unsigned arithmetic on doubles (R integers are signed 32-bit).
.U32 <- 4294967296
.u32 <- function(x) x %% .U32
.xor32 <- function(a, b) {
  hi <- bitwXor(as.integer(a %/% 65536), as.integer(b %/% 65536))
  lo <- bitwXor(as.integer(a %% 65536), as.integer(b %% 65536))
  hi * 65536 + lo
}
.imul32 <- function(a, b) {
  # (a * b) mod 2^32 without losing precision: split b into 16-bit halves
  lo <- b %% 65536
  hi <- b %/% 65536
  .u32(a * lo + ((a * hi) %% 65536) * 65536)
}
.shr32 <- function(a, n) a %/% (2^n)          # a >>> n
.shl32 <- function(a, n) .u32(a * 2^n)        # a << n

# Text as UTF-16 code units (what JavaScript's charCodeAt returns)
.utf16_units <- function(text) {
  cps <- utf8ToInt(enc2utf8(text))
  units <- c()
  for (cp in cps) {
    if (cp > 0xFFFF) {
      cp <- cp - 0x10000
      units <- c(units, 0xD800 + cp %/% 1024, 0xDC00 + cp %% 1024)
    } else {
      units <- c(units, cp)
    }
  }
  units
}

.cyrb128 <- function(text) {
  h1 <- 1779033703; h2 <- 3144134277; h3 <- 1013904242; h4 <- 2773480762
  for (k in .utf16_units(text)) {
    h1 <- .xor32(h2, .imul32(.xor32(h1, k), 597399067))
    h2 <- .xor32(h3, .imul32(.xor32(h2, k), 2869860233))
    h3 <- .xor32(h4, .imul32(.xor32(h3, k), 951274213))
    h4 <- .xor32(h1, .imul32(.xor32(h4, k), 2716044179))
  }
  h1 <- .imul32(.xor32(h3, .shr32(h1, 18)), 597399067)
  h2 <- .imul32(.xor32(h4, .shr32(h2, 22)), 2869860233)
  h3 <- .imul32(.xor32(h1, .shr32(h3, 17)), 951274213)
  h4 <- .imul32(.xor32(h2, .shr32(h4, 19)), 2716044179)
  h1 <- .xor32(h1, .xor32(h2, .xor32(h3, h4)))
  h2 <- .xor32(h2, h1)
  h3 <- .xor32(h3, h1)
  h4 <- .xor32(h4, h1)
  c(h1, h2, h3, h4)
}

# Random generator: returns a function giving numbers in [0, 1)
seeded_random <- function(seed) {
  if (!is.character(seed) || length(seed) != 1 || trimws(seed) == "") {
    stop("Η κλήρωση χρειάζεται μη κενό seed.")
  }
  s <- .cyrb128(seed)
  a <- s[1]; b <- s[2]; c <- s[3]; d <- s[4]
  nxt <- function() {
    t <- .u32(a + b + d)
    d <<- .u32(d + 1)
    a <<- .xor32(b, .shr32(b, 9))
    b <<- .u32(c + .shl32(c, 3))
    c <<- .shl32(c, 21) + .shr32(c, 11)   # rotate left by 21 (disjoint bits)
    c <<- .u32(c + t)
    t / .U32
  }
  for (i in 1:16) nxt()  # discard warm-up outputs
  nxt
}

# ΑΜ → lottery number (named integer vector)
draw_lottery <- function(ams, seed) {
  ams <- as.character(ams)
  if (anyDuplicated(ams)) stop("Διπλότυπος ΑΜ στην κλήρωση.")
  num <- suppressWarnings(as.numeric(ams))
  ord <- if (all(!is.na(num))) order(num) else order(ams)
  list <- ams[ord]
  rng <- seeded_random(seed)
  n <- length(list)
  if (n > 1) {
    for (i in (n - 1):1) {            # JavaScript indices i = n-1 … 1
      j <- floor(rng() * (i + 1))     # 0 … i
      tmp <- list[i + 1]; list[i + 1] <- list[j + 1]; list[j + 1] <- tmp
    }
  }
  setNames(seq_len(n), list)
}

# Command line: Rscript R/lottery.R students.csv "seed" lottery.csv
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 3) stop('Usage: Rscript R/lottery.R students.csv "seed" lottery.csv')
  students <- read.csv(args[1], colClasses = "character", check.names = FALSE, encoding = "UTF-8")
  lot <- draw_lottery(students$RegistryNr, args[2])
  write.csv(data.frame(RegistryNr = names(lot), lottery_number = unname(lot)), args[3], row.names = FALSE)
  cat(sprintf("Κλήρωση για %d μαθητές με seed «%s» → %s\n", length(lot), args[2], args[3]))
}
