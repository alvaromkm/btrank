#' Aggregate match-level results into item/period scores
#'
#' Converts one row per match (home item, away item, goals) into one row
#' per item per period, with two aggregate metrics: points (3/1/0 for
#' win/draw/loss) and goal difference. This is the standard bridge between
#' raw match-level results and the `item/period/score` format expected by
#' [bt_win_matrix()].
#'
#' @param matches A data frame with one row per match.
#' @param period_col,home_col,away_col,home_goals_col,away_goals_col
#'   Column names in `matches` identifying the period, home item, away
#'   item, home goals and away goals respectively.
#'
#' @return A data frame with columns `item`, `period`, `points`,
#'   `goal_diff`, one row per item per period that actually occurs in
#'   `matches`. Item/period combinations with no match produce no row —
#'   this function only ever reports observed data, so that absence is
#'   never confused with an observed score of 0 (see `?bt_win_matrix`,
#'   `absent` argument).
#'
#' @noRd
.matches_to_scores <- function(matches,
                               period_col     = "period",
                               home_col       = "home",
                               away_col       = "away",
                               home_goals_col = "home_goals",
                               away_goals_col = "away_goals") {
  
  required <- c(period_col, home_col, away_col, home_goals_col, away_goals_col)
  missing  <- setdiff(required, names(matches))
  if (length(missing) > 0) {
    stop("Column(s) not found in `matches`: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  
  period <- matches[[period_col]]
  home   <- matches[[home_col]]
  away   <- matches[[away_col]]
  hg     <- suppressWarnings(as.numeric(matches[[home_goals_col]]))
  ag     <- suppressWarnings(as.numeric(matches[[away_goals_col]]))
  
  keep <- !is.na(period) & !is.na(home) & !is.na(away) & !is.na(hg) & !is.na(ag)
  if (!all(keep)) {
    warning(sum(!keep), " row(s) dropped due to missing period/team/goals.",
            call. = FALSE)
  }
  period <- period[keep]; home <- home[keep]; away <- away[keep]
  hg <- hg[keep]; ag <- ag[keep]
  
  if (length(period) == 0) {
    stop("No valid match rows after removing missing data.", call. = FALSE)
  }
  
  home_pts <- ifelse(hg > ag, 3, ifelse(hg == ag, 1, 0))
  away_pts <- ifelse(ag > hg, 3, ifelse(ag == hg, 1, 0))
  
  long <- rbind(
    data.frame(period = period, item = home, points = home_pts,
               goal_diff = hg - ag, stringsAsFactors = FALSE),
    data.frame(period = period, item = away, points = away_pts,
               goal_diff = ag - hg, stringsAsFactors = FALSE)
  )
  
  agg <- stats::aggregate(
    long[c("points", "goal_diff")],
    by  = list(period = long$period, item = long$item),
    FUN = sum
  )
  
  agg <- agg[order(agg$period, agg$item), c("item", "period", "points", "goal_diff")]
  rownames(agg) <- NULL
  agg
}
