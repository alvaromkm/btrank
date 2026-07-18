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
#' @param absent Character. How to treat items that belong to the item
#'   universe (all items appearing anywhere in `data`) but have no row, or
#'   an `NA` score, in a given period. One of `"ignore"` (default: no
#'   comparison is generated for that item that period — the current,
#'   unchanged behaviour) or `"penalize"` (every item present that period
#'   is credited a structural win against the absent item, weighted by
#'   `absent_penalty` and the period's temporal weight).
#' @param absent_penalty Non-negative number. Only used when
#'   `absent = "penalize"`. `1` (default) treats the absence as equivalent
#'   in magnitude to a genuine loss that period. Values in `(0, 1)` soften
#'   the penalty; `0` is equivalent to `"ignore"` but explicit. This
#'   operates directly on win/loss counts, never on `score_col`, so an
#'   absence can never be confused with a genuine observed score value
#'   (e.g. a real score of 0 is not the same event as not playing).
#'
#'   Two effects have been verified empirically and should be considered
#'   before choosing a value: (1) a sufficiently high `absent_penalty` can
#'   change which item [bt_fit()] selects as the reference item, since
#'   accumulated structural losses lower the penalized item's estimated
#'   ability; (2) increasing `absent_penalty` shrinks the standard error of
#'   the penalized item's ability difference against other items, because
#'   the structural comparisons are treated as additional information by
#'   the likelihood even though they are not observed data. There is no
#'   principled default threshold for `absent_penalty`; report results
#'   under a range of values (sensitivity analysis) rather than a single
#'   fixed choice.
#'
#' @return A square numeric matrix of dimension n_items × n_items, where
#'   entry `[i, j]` is the (possibly weighted) number of periods in which
#'   item `i` outperformed item `j` — including, when `absent = "penalize"`,
#'   structural wins credited against items absent that period. Row and
#'   column names are item identifiers.
#'
#' @details
#' The item universe is defined as every distinct value in `item_col`
#' across the *entire* `data` passed in — not just the items present in
#' any single period. This means the caller is responsible for bounding
#' the time window before calling `bt_win_matrix()`: passing an unfiltered,
#' multi-decade dataset with `absent = "penalize"` will accumulate
#' structural losses for every period an item is missing, which may not be
#' the intended comparison window.
#'
#' `absent = "ignore"` is the default because it makes no assumption about
#' why an item is missing from a period. `absent = "penalize"` encodes a
#' specific modelling choice — that absence itself is informative (e.g.
#' relegation from a league) — and should be selected and justified
#' explicitly, not left as an implicit default. For contrast, Liu,
#' Battaglia & Wu (2025, *PLOS One*) handle the same promotion/relegation
#' scenario in the English Premier League by excluding teams without
#' consistent presence across the full period, rather than penalizing
#' their absence.
#'
#' Both `weights` and `absent_penalty` can produce non-integer entries in
#' the returned matrix. See the Details section of [bt_fit()] for how this
#' interacts with the underlying `glm()` fit.
#'
#' @references
#' Liu, H. H., Battaglia, J., & Wu, T. T. (2025). Impact of COVID-19 on
#' English Football Premier League: Analyzing rankings and home advantage
#' using extended Bradley-Terry models. *PLOS One*, 20(10), e0332627.
#' https://doi.org/10.1371/journal.pone.0332627
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
#' # Item C is absent in 2023 (e.g. relegated): penalize instead of ignore
#' data_absent <- data[-6, ]  # drop C's 2023 row
#' mat_ignored   <- bt_win_matrix(data_absent, score_col = "score")
#' mat_penalized <- bt_win_matrix(data_absent, score_col = "score",
#'                                absent = "penalize", absent_penalty = 1)
#'
#' @export
bt_win_matrix <- function(data,
                          item_col         = "item",
                          period_col       = "period",
                          score_col        = "score",
                          higher_is_better = TRUE,
                          weights          = NULL,
                          absent           = c("ignore", "penalize"),
                          absent_penalty   = 1) {
  
  # ── Input validation ──────────────────────────────────────────────────────
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  
  absent <- match.arg(absent)
  
  if (absent == "penalize") {
    if (!is.numeric(absent_penalty) || length(absent_penalty) != 1 ||
        is.na(absent_penalty) || absent_penalty < 0) {
      stop("`absent_penalty` must be a single non-negative number.", call. = FALSE)
    }
  } else if (!missing(absent_penalty) && !identical(absent_penalty, 1)) {
    warning("`absent_penalty` is ignored when `absent = \"ignore\"`.", call. = FALSE)
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
  
  # Item universe: every item appearing anywhere in `data`, not just within
  # a single period. See Details for why the caller must bound the window.
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
    
    # Period weight (defaults to 1 if not supplied)
    w <- if (!is.null(weights) && as.character(p) %in% names(weights)) {
      weights[[as.character(p)]]
    } else {
      1
    }
    
    # ── Real comparisons among items present this period (needs >= 2) ──────
    if (nrow(sub) >= 2) {
      
      sub_ord <- if (higher_is_better) {
        sub[order(-sub$score), ]
      } else {
        sub[order(sub$score), ]
      }
      
      items_p  <- sub_ord$item
      scores_p <- sub_ord$score
      n_p      <- nrow(sub_ord)
      
      # After sorting, position i always ranks above position j (i < j).
      # We only need the raw scores to detect ties — never to decide the winner.
      for (i in seq_len(n_p - 1)) {
        for (j in seq(i + 1, n_p)) {
          wi <- items_p[i]
          wj <- items_p[j]
          
          if (scores_p[i] != scores_p[j]) {
            win_counts[wi, wj] <- win_counts[wi, wj] + w
          } else {
            win_counts[wi, wj] <- win_counts[wi, wj] + 0.5 * w
            win_counts[wj, wi] <- win_counts[wj, wi] + 0.5 * w
          }
        }
      }
    }
    
    # ── Structural penalty for absent items (needs >= 1 present item) ──────
    # Never touches `score` — operates directly on win/loss counts, so an
    # absence can never be mistaken for a genuine observed value. No
    # comparison is generated between two absent items.
    if (absent == "penalize" && nrow(sub) >= 1) {
      present_items <- unique(sub$item)
      absent_items  <- setdiff(items, present_items)
      for (a in absent_items) {
        for (j in present_items) {
          win_counts[j, a] <- win_counts[j, a] + w * absent_penalty
        }
      }
    }
  }
  
  win_counts
}