# ==============================================================================
# Reproduces data/epl_scores.rda from raw football-data.co.uk match CSVs.
#
# Raw CSVs are NOT redistributed with this package (unclear third-party
# redistribution terms; see vignette for rationale). To regenerate:
#
#   1. Download season CSVs from https://www.football-data.co.uk/englandm.php
#      (English Premier League, division "E0"), one per season, 1995/96
#      through the most recent completed season.
#   2. Name them YYZZ.csv (e.g. 9596.csv, 0001.csv, 2526.csv) and place them
#      in a single directory.
#   3. Set `epl_dir` below and run this script from the package root.
#
# NOTE: uses readr::read_csv(), not utils::read.csv(). Two seasons
# (0304.csv, 0405.csv) have rows with more fields than the header further
# into the season (football-data.co.uk added bookmaker columns mid-season
# without normalising earlier rows). utils::read.csv() misparses those
# rows, silently splitting them into extra malformed rows. readr::read_csv()
# handles this correctly. Verified against known results: Arsenal's 2003-04
# "Invincibles" (90 points) and Chelsea's 2004-05 title (95 points).
# ==============================================================================

devtools::load_all()  # exposes internal .matches_to_scores() for this script

epl_dir <- "data-raw/epl"

files <- list.files(epl_dir, pattern = "^[0-9]{4}\\.csv$", full.names = TRUE)
if (length(files) == 0) stop("No season CSV files found in `epl_dir`.")

read_one_season <- function(path) {
  season_code <- tools::file_path_sans_ext(basename(path))
  yy1        <- as.integer(substr(season_code, 1, 2))
  start_year <- if (yy1 >= 95) 1900L + yy1 else 2000L + yy1
  
  df <- suppressWarnings(readr::read_csv(
    path,
    col_types      = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  ))
  names(df) <- trimws(sub("^\ufeff", "", names(df)))  # strip BOM if present
  
  required <- c("HomeTeam", "AwayTeam", "FTHG", "FTAG")
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("Missing column(s) in ", basename(path), ": ", paste(missing, collapse = ", "))
  }
  
  data.frame(
    period     = start_year,
    home       = df$HomeTeam,
    away       = df$AwayTeam,
    home_goals = suppressWarnings(as.numeric(df$FTHG)),
    away_goals = suppressWarnings(as.numeric(df$FTAG)),
    stringsAsFactors = FALSE
  )
}

matches_all <- do.call(rbind, lapply(files, read_one_season))

epl_scores <- .matches_to_scores(
  matches_all,
  period_col = "period", home_col = "home", away_col = "away",
  home_goals_col = "home_goals", away_goals_col = "away_goals"
)

usethis::use_data(epl_scores, overwrite = TRUE)