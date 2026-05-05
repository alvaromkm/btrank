#' Extract a ranked data frame from a fitted Bradley-Terry model
#'
#' Takes a fitted Bradley-Terry model (as produced by [bt_fit()]) and returns
#' a tidy data frame with items ranked by their estimated latent ability,
#' optionally limited to the top N items.
#'
#' Note: when `top_n` is specified here, the ranking is simply truncated after
#' fitting — the model is not re-fitted on the subset. If you want the
#' two-pass behaviour (fit on all items, then re-fit on the top subset),
#' use [bt_rank_all()] with its `top_n` argument instead.
#'
#' @param fit A `btfit` object as returned by [bt_fit()].
#' @param top_n Integer. If provided, only the top `top_n` items by ability
#'   are returned. The model is not re-fitted — results are simply truncated.
#'   If `NULL` (default), all items are returned.
#' @param digits Integer. Number of decimal places for the ability column.
#'   Default: `4`.
#'
#' @return A data frame with one row per item and the following columns:
#' \describe{
#'   \item{`rank`}{Integer rank (1 = strongest item).}
#'   \item{`item`}{Item identifier.}
#'   \item{`ability`}{Estimated log-ability (lambda). The reference item
#'     always has ability 0; all others are relative to it.}
#'   \item{`ability_norm`}{Ability normalised to the 0-1 interval, where
#'     1 is the strongest item and 0 is the reference. Useful for comparing
#'     rankings across different model fits.}
#' }
#'
#' @seealso [bt_fit()] to fit the model, [bt_prob_matrix()] to compute
#'   pairwise win probabilities from the same fit, [bt_rank_all()] for the
#'   full pipeline with two-pass top-N re-fitting.
#'
#' @examples
#' data <- data.frame(
#'   item   = c("A", "B", "C", "A", "B", "C"),
#'   period = c(2022, 2022, 2022, 2023, 2023, 2023),
#'   score  = c(80, 65, 50, 75, 85, 60)
#' )
#' mat <- bt_win_matrix(data, score_col = "score")
#' fit <- bt_fit(mat)
#'
#' # Full ranking
#' bt_rank(fit)
#'
#' # Top 2 only (truncation only — no model re-fitting)
#' bt_rank(fit, top_n = 2)
#'
#' @export
bt_rank <- function(fit, top_n = NULL, digits = 4) {

  # ── Input validation ──────────────────────────────────────────────────────
  if (!inherits(fit, "btfit")) {
    stop("`fit` must be a `btfit` object returned by `bt_fit()`.", call. = FALSE)
  }
  if (!is.null(top_n)) {
    if (!is.numeric(top_n) || length(top_n) != 1 || top_n < 1) {
      stop("`top_n` must be a single positive integer.", call. = FALSE)
    }
    top_n <- as.integer(top_n)
  }

  # ── Build ranked data frame ───────────────────────────────────────────────
  ab       <- sort(fit$abilities, decreasing = TRUE)
  max_ab   <- max(ab)
  min_ab   <- min(ab)
  range_ab <- max_ab - min_ab

  df <- data.frame(
    rank         = seq_along(ab),
    item         = names(ab),
    ability      = round(ab, digits),
    ability_norm = if (range_ab > 0) round((ab - min_ab) / range_ab, digits)
    else rep(1, length(ab)),
    row.names    = NULL,
    stringsAsFactors = FALSE
  )

  # ── Apply top_n filter (truncation only) ──────────────────────────────────
  if (!is.null(top_n)) {
    if (top_n > nrow(df)) {
      warning(
        "`top_n` (", top_n, ") exceeds the number of items (", nrow(df), "). ",
        "Returning all items.",
        call. = FALSE
      )
    } else {
      df <- df[seq_len(top_n), ]
    }
  }

  df
}
