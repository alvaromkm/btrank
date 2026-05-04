#' Fit a Bradley-Terry ranking in a single call
#'
#' Convenience wrapper that runs the full Bradley-Terry pipeline in one step:
#' builds the win count matrix, fits the model, and returns a ranked data
#' frame. Optionally applies temporal weighting via exponential decay.
#'
#' This function is equivalent to calling [bt_win_matrix()], [bt_fit()], and
#' [bt_rank()] in sequence. For more control over individual steps, use those
#' functions directly.
#'
#' @param data A data frame with one row per item per period. Must contain
#'   at least the columns specified in `item_col`, `period_col`, and
#'   `score_col`.
#' @param score_col Name of the column containing the numeric metric used to
#'   determine winners within each period. Character string. Default: `"score"`.
#' @param item_col Name of the column identifying items. Character string.
#'   Default: `"item"`.
#' @param period_col Name of the column identifying periods. Character string.
#'   Default: `"period"`.
#' @param higher_is_better Logical. If `TRUE` (default), higher values of
#'   `score_col` indicate a better outcome. Set to `FALSE` for metrics where
#'   lower is better (e.g. time, errors).
#' @param top_n Integer. If provided, only the top `top_n` items by ability
#'   are returned. If `NULL` (default), all items are returned.
#' @param half_life Positive number. If provided, exponential decay weights
#'   are computed via [bt_weights()] and passed to [bt_win_matrix()]. The
#'   most recent period receives weight 1; earlier periods receive
#'   progressively lower weights. If `NULL` (default), all periods are
#'   weighted equally.
#' @param digits Integer. Number of decimal places for ability columns.
#'   Default: `4`.
#'
#' @return A data frame as returned by [bt_rank()], with columns `rank`,
#'   `item`, `ability`, and `ability_norm`.
#'
#' @seealso [bt_win_matrix()], [bt_fit()], [bt_rank()], [bt_weights()]
#'   for the individual pipeline steps.
#'
#' @examples
#' data <- data.frame(
#'   item   = c("A", "B", "C", "A", "B", "C"),
#'   period = c(2022, 2022, 2022, 2023, 2023, 2023),
#'   score  = c(80, 65, 50, 75, 85, 60)
#' )
#'
#' # Full pipeline in one call
#' bt_rank_all(data, score_col = "score")
#'
#' # With temporal weighting
#' bt_rank_all(data, score_col = "score", half_life = 1)
#'
#' # Top 2 items only
#' bt_rank_all(data, score_col = "score", top_n = 2)
#'
#' @export
bt_rank_all <- function(data,
                        score_col        = "score",
                        item_col         = "item",
                        period_col       = "period",
                        higher_is_better = TRUE,
                        top_n            = NULL,
                        half_life        = NULL,
                        digits           = 4) {

  # ── Input validation ──────────────────────────────────────────────────────
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!is.null(half_life)) {
    if (!is.numeric(half_life) || length(half_life) != 1 || half_life <= 0) {
      stop("`half_life` must be a single positive number.", call. = FALSE)
    }
  }

  # ── Compute temporal weights (optional) ───────────────────────────────────
  weights <- NULL
  if (!is.null(half_life)) {
    periods <- unique(data[[period_col]])
    weights <- bt_weights(periods = periods, half_life = half_life)
  }

  # ── Run pipeline ──────────────────────────────────────────────────────────
  win_mat <- bt_win_matrix(
    data             = data,
    item_col         = item_col,
    period_col       = period_col,
    score_col        = score_col,
    higher_is_better = higher_is_better,
    weights          = weights
  )

  fit <- bt_fit(win_mat)

  bt_rank(fit, top_n = top_n, digits = digits)
}
