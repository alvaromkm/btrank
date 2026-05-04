#' Build a pairwise win count matrix from comparison data
#'
#' For each pair of items and each period, records which item ranked higher
#' according to the chosen metric. Aggregates all pairwise period-level
#' comparisons into a win count matrix suitable for Bradley-Terry estimation.
#' Ties are split as 0.5 wins each.
#'
#' @param data A data frame with one row per item per period. Must contain
#'   at least the columns specified in `item_col`, `period_col`, and
#'   `score_col`.
#' @param item_col Name of the column identifying items (e.g. teams, wines,
#'   candidates). Character string. Default: `"item"`.
#' @param period_col Name of the column identifying periods (e.g. seasons,
#'   years, rounds). Character string. Default: `"period"`.
#' @param score_col Name of the column containing the numeric metric used to
#'   determine winners within each period. Character string. Default: `"score"`.
#' @param higher_is_better Logical. If `TRUE` (default), higher values of
#'   `score_col` indicate a better outcome (e.g. points, goals, ratings).
#'   Set to `FALSE` for metrics where lower is better (e.g. time, errors).
#' @param weights An optional named numeric vector of period weights, where
#'   names match values in `period_col`. If `NULL` (default), all periods
#'   receive equal weight of 1. Use [bt_weights()] to generate exponential
#'   decay weights.
#'
#' @return A square numeric matrix of dimension n_items × n_items, where
#'   entry `[i, j]` is the (possibly weighted) number of periods in which
#'   item `i` outperformed item `j`. Row and column names are item identifiers.
#'
#' @seealso [bt_fit()] to fit a Bradley-Terry model from the win matrix,
#'   [bt_weights()] to compute temporal decay weights.
#'
#' @examples
#' # Simple example: three teams over two seasons
#' data <- data.frame(
#'   item   = c("A", "B", "C", "A", "B", "C"),
#'   period = c(2022, 2022, 2022, 2023, 2023, 2023),
#'   score  = c(80, 65, 50, 75, 85, 60)
#' )
#' mat <- bt_win_matrix(data, score_col = "score")
#' mat
#'
#' # With temporal weighting (more recent periods count more)
#' w <- bt_weights(periods = c(2022, 2023), half_life = 1)
#' mat_w <- bt_win_matrix(data, score_col = "score", weights = w)
#' mat_w
#'
#' @export
bt_win_matrix <- function(data,
                          item_col         = "item",
                          period_col       = "period",
                          score_col        = "score",
                          higher_is_better = TRUE,
                          weights          = NULL) {

  # ── Input validation ──────────────────────────────────────────────────────
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  required_cols <- c(item_col, period_col, score_col)
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Column(s) not found in `data`: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # ── Rename to internal names for clarity ─────────────────────────────────
  data <- data.frame(
    item   = data[[item_col]],
    period = data[[period_col]],
    score  = suppressWarnings(as.numeric(data[[score_col]]))
  )

  items   <- sort(unique(data$item))
  periods <- sort(unique(data$period))
  n       <- length(items)

  if (n < 2) {
    stop("`data` must contain at least 2 distinct items.", call. = FALSE)
  }

  # ── Initialise win count matrix ───────────────────────────────────────────
  win_counts <- matrix(
    0,
    nrow     = n,
    ncol     = n,
    dimnames = list(items, items)
  )

  # ── Accumulate wins per period ────────────────────────────────────────────
  for (p in periods) {

    sub <- data[data$period == p, ]
    sub <- sub[!is.na(sub$score), ]

    if (nrow(sub) < 2) next

    # Period weight (defaults to 1 if not supplied)
    w <- if (!is.null(weights) && as.character(p) %in% names(weights)) {
      weights[[as.character(p)]]
    } else {
      1
    }

    # Sort by metric so row i always beats row j (i < j index)
    if (higher_is_better) {
      sub <- sub[order(-sub$score), ]
    } else {
      sub <- sub[order(sub$score), ]
    }

    items_p  <- sub$item
    scores_p <- sub$score
    n_p      <- nrow(sub)

    # After sorting, position i always ranks above position j (i < j).
    # We only need the raw scores to detect ties — never to decide the winner.
    for (i in seq_len(n_p - 1)) {
      for (j in seq(i + 1, n_p)) {
        wi <- items_p[i]
        wj <- items_p[j]

        if (scores_p[i] != scores_p[j]) {
          # i is the better-ranked item after sorting
          win_counts[wi, wj] <- win_counts[wi, wj] + w
        } else {
          # Tie: split equally
          win_counts[wi, wj] <- win_counts[wi, wj] + 0.5 * w
          win_counts[wj, wi] <- win_counts[wj, wi] + 0.5 * w
        }
      }
    }
  }

  win_counts
}
