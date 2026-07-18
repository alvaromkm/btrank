#' English Premier League team scores by season (1995/96-2025/26)
#'
#' Aggregated points and goal difference for every team in every English
#' Premier League season from 1995/96 to 2025/26, derived from match
#' results. Provided as example data for the package vignette, already in
#' the `item/period/score` format expected by [bt_win_matrix()].
#'
#' @format A data frame with one row per team per season and 4 columns:
#' \describe{
#'   \item{item}{Team name.}
#'   \item{period}{Season start year (integer), e.g. `1995` for 1995/96.}
#'   \item{points}{Total league points that season (3 win / 1 draw / 0 loss).}
#'   \item{goal_diff}{Total goal difference that season (goals for minus
#'     goals against).}
#' }
#'
#' @source Match results: \url{https://www.football-data.co.uk/englandm.php}
#'   (English Premier League, division "E0"). Raw match-level CSVs are not
#'   redistributed with this package; see `data-raw/prepare_epl_scores.R`
#'   for full reproduction instructions from the original source.
"epl_scores"
