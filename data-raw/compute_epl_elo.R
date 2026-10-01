# ==============================================================================
# Reproduces the Elo ratings embedded in vignettes/introduction-to-btrank.Rmd,
# section "External validation: comparison with Elo".
#
# Unlike data/epl_scores.rda, this script's output is NOT saved as package
# data — Elo requires match-level results in chronological order (dates),
# which are not part of epl_scores and are not redistributed with this
# package. The resulting ratings are embedded as a literal vector directly
# in the vignette; this script exists so that embedding is reproducible
# and auditable, not asserted without evidence.
#
# To regenerate: same raw CSVs as data-raw/prepare_epl_scores.R
# (data-raw/epl/*.csv, not tracked in git — see that script for how to
# obtain them from football-data.co.uk).
# ==============================================================================

epl_dir <- "data-raw/epl"

files <- list.files(epl_dir, pattern = "^[0-9]{4}\\.csv$", full.names = TRUE)
if (length(files) == 0) stop("No season CSV files found in `epl_dir`.")

read_one_season_dated <- function(path) {
  season_code <- tools::file_path_sans_ext(basename(path))
  yy1        <- as.integer(substr(season_code, 1, 2))
  start_year <- if (yy1 >= 95) 1900L + yy1 else 2000L + yy1
  
  df <- suppressWarnings(readr::read_csv(
    path, col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  ))
  names(df) <- trimws(sub("^\ufeff", "", names(df)))
  
  # football-data.co.uk uses dd/mm/yy up to ~2018/19, dd/mm/yyyy from then
  # on. Same 2-digit-year rule as prepare_epl_scores.R's season parsing.
  parse_date_col <- function(x) {
    parts <- strsplit(x, "/")
    d <- vapply(parts, `[`, character(1), 1)
    m <- vapply(parts, `[`, character(1), 2)
    y <- vapply(parts, `[`, character(1), 3)
    y_full <- ifelse(nchar(y) == 2,
                     ifelse(as.integer(y) >= 95, paste0("19", y), paste0("20", y)),
                     y)
    as.Date(paste(y_full, m, d, sep = "-"), format = "%Y-%m-%d")
  }
  
  data.frame(
    period     = start_year,
    date       = parse_date_col(df$Date),
    home       = df$HomeTeam,
    away       = df$AwayTeam,
    home_goals = suppressWarnings(as.numeric(df$FTHG)),
    away_goals = suppressWarnings(as.numeric(df$FTAG)),
    stringsAsFactors = FALSE
  )
}

matches_all <- do.call(rbind, lapply(files, read_one_season_dated))
matches_all <- matches_all[
  !is.na(matches_all$date) & !is.na(matches_all$home_goals) &
    !is.na(matches_all$away_goals) & matches_all$home != "" & matches_all$away != "",
]
matches_all <- matches_all[order(matches_all$date), ]

cat("Matches used for Elo computation:", nrow(matches_all), "\n")

# ── Sequential Elo update, standard formula, no home advantage adjustment ──
# Starting rating 1500, K = 20. See vignette for why this is computed
# entirely outside the btrank pipeline (external validation, not a
# score_col input).
teams <- sort(unique(c(matches_all$home, matches_all$away)))
elo   <- setNames(rep(1500, length(teams)), teams)
K     <- 20

for (i in seq_len(nrow(matches_all))) {
  h  <- matches_all$home[i]
  a  <- matches_all$away[i]
  hg <- matches_all$home_goals[i]
  ag <- matches_all$away_goals[i]
  
  E_home <- 1 / (1 + 10^((elo[a] - elo[h]) / 400))
  S_home <- if (hg > ag) 1 else if (hg == ag) 0.5 else 0
  
  delta    <- K * (S_home - E_home)
  elo[h]   <- elo[h] + delta
  elo[a]   <- elo[a] - delta
}

# Ratings for the 20 teams active in the most recent season, as embedded
# in the vignette.
current_teams <- unique(matches_all$home[matches_all$period == max(matches_all$period)])
current_teams <- union(current_teams,
                       matches_all$away[matches_all$period == max(matches_all$period)])

elo_final <- sort(elo[current_teams], decreasing = TRUE)
round(elo_final, 1)
