#' Build a pairwise win count matrix from comparison data
#'
#' For each pair of items and each period, records which item ranked higher
#' according to the chosen metric. Aggregates all pairwise period-level
#' comparisons into a win count matrix suitable for Bradley-Terry estimation.
#' Ties are split as 0.5 wins each.
#'
#' @param data A data frame with at most one row per item per period
#'   (block). Must contain
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
#' @param weights An optional named numeric vector of weights, where names
#'   match values in `time_col` (by default, `period_col`). If `NULL`
#'   (default), all periods receive equal weight of 1. Use [bt_weights()] to
#'   generate exponential decay weights.
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
#' @param cluster_col Optional name of the column identifying the
#'   independent units used for variance estimation (see Details). If
#'   `NULL` (default), each period is its own cluster. Must be constant
#'   within each period.
#' @param time_col Optional name of the column whose values are matched to
#'   the names of `weights`. If `NULL` (default), `period_col` is used. Needed
#'   when periods are not time units (e.g. one period per match, weighted by
#'   season). Must be constant within each period.
#'
#' @return An object of class `"bt_win_matrix"`: a square numeric matrix
#'   (it inherits from `"matrix"`) of dimension n_items × n_items, where
#'   entry `[i, j]` is the (possibly weighted) number of periods in which
#'   item `i` outperformed item `j` — including, when `absent = "penalize"`,
#'   structural wins credited against items absent that period. Row and
#'   column names are item identifiers.
#'
#'   The matrix carries an attribute `"comparisons"`: a data frame with one
#'   row per pairwise comparison that contributes to the matrix, with
#'   columns `item1`, `item2` (character), `y` (outcome for `item1`: `1` for
#'   a win, `0.5` for a tie), `period` (as in `period_col`), `cluster`
#'   (as in `cluster_col`, or `period` if not supplied), `weight`
#'   (the row's contribution to the counts: the period weight, multiplied
#'   by `absent_penalty` for structural rows) and `structural` (logical,
#'   `TRUE` for rows created by `absent = "penalize"`). Rows with zero
#'   weight are omitted. The matrix is exactly the weighted aggregation of
#'   these rows. This period-level record is what allows [bt_fit()] and
#'   its methods to compute standard errors that account for the
#'   dependence between comparisons derived from the same period. The
#'   attribute is dropped by subsetting (e.g. `mat[1:3, 1:3]`); a matrix
#'   without it can still be fitted, but only model-based standard errors
#'   are then available.
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
#' It only makes sense when every period is a full participation unit
#' (e.g. a league season). With one period per match or per consumer,
#' every item not in that period would be penalized.
#' Battaglia & Wu (2025, *PLOS One*) handle the same promotion/relegation
#' scenario in the English Premier League by excluding teams without
#' consistent presence across the full period, rather than penalizing
#' their absence.
#'
#' Both `weights` and `absent_penalty` can produce non-integer entries in
#' the returned matrix. See the Details section of [bt_fit()] for how this
#' interacts with the underlying `glm()` fit.
#'
#' **Periods as comparison blocks.** A "period" is any block within which
#' scores are comparable: a season (items = teams, score = points), a
#' single match (two rows, score = goals), a consumer (items = products,
#' score = rating) or a judge (items = wines, score = mark). Scores are
#' never compared across periods, so each item may appear at most once per
#' period; duplicated item-period rows are an error. For example, ratings
#' from several consumers must use the consumer, not the month, as
#' `period_col`.
#'
#' **Clusters.** All pairwise comparisons derived from the same period are
#' functions of a single vector of scores and are therefore not
#' independent (e.g. if item `i` finishes first, it beats every other item
#' that period). The `"comparisons"` attribute records the cluster of every
#' comparison so that downstream variance estimation can treat clusters as
#' the independent units. By default each period is a cluster. Supply
#' `cluster_col` when several periods share a source of dependence, e.g.
#' one period per pairwise judgement but many judgements per respondent
#' (`cluster_col = "respondent"`).
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
                          absent_penalty   = 1,
                          cluster_col      = NULL,
                          time_col         = NULL) {
  
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
  
  for (arg in c("cluster_col", "time_col")) {
    val <- get(arg)
    if (!is.null(val) && (!is.character(val) || length(val) != 1)) {
      stop("`", arg, "` must be NULL or a single column name.", call. = FALSE)
    }
  }
  
  required_cols <- c(item_col, period_col, score_col, cluster_col, time_col)
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
    item    = as.character(data[[item_col]]),
    period  = data[[period_col]],
    score   = suppressWarnings(as.numeric(data[[score_col]])),
    cluster = if (is.null(cluster_col)) data[[period_col]] else data[[cluster_col]],
    time    = if (is.null(time_col)) data[[period_col]] else data[[time_col]],
    stringsAsFactors = FALSE
  )
  
  # ── Structural validation of periods, clusters and time ──────────────────
  dup <- duplicated(data[, c("item", "period")])
  if (any(dup)) {
    ex <- utils::head(unique(paste0(data$item[dup], " @ ", data$period[dup])), 3)
    stop(
      "Each item may appear at most once per period (scores are only ",
      "compared within a period). Duplicated item-period rows, e.g.: ",
      paste(ex, collapse = "; "),
      ". If rows come from different raters, use the rater as `period_col`.",
      call. = FALSE
    )
  }
  
  for (col in c("cluster", "time")) {
    if (anyNA(data[[col]])) {
      stop("`", col, "_col` must not contain missing values.", call. = FALSE)
    }
    n_per_period <- tapply(data[[col]], as.character(data$period),
                           function(z) length(unique(z)))
    if (any(n_per_period > 1)) {
      stop("`", col, "_col` must be constant within each period; it varies in ",
           "period(s): ",
           paste(utils::head(names(n_per_period)[n_per_period > 1], 3),
                 collapse = ", "),
           ".", call. = FALSE)
    }
  }
  
  # Item universe: every item appearing anywhere in `data`, not just within
  # a single period. See Details for why the caller must bound the window.
  items   <- sort(unique(data$item))
  periods <- sort(unique(data$period))
  n       <- length(items)
  
  if (n < 2) {
    stop("`data` must contain at least 2 distinct items.", call. = FALSE)
  }
  
  # ── Build the period-level comparison records ─────────────────────────────
  # Single source of truth: the win matrix is aggregated from these rows
  # below, so the matrix and the "comparisons" attribute cannot disagree.
  rows <- vector("list", length(periods))
  
  for (k in seq_along(periods)) {
    p <- periods[k]
    
    sub <- data[data$period == p, ]
    cl  <- sub$cluster[1]
    tm  <- as.character(sub$time[1])
    sub <- sub[!is.na(sub$score), ]
    
    # Weight looked up by the period's time value (defaults to 1)
    w <- if (!is.null(weights) && tm %in% names(weights)) {
      weights[[tm]]
    } else {
      1
    }
    
    rows_p <- list()
    
    # ── Real comparisons among items present this period (needs >= 2) ──────
    if (nrow(sub) >= 2) {
      
      sub_ord <- if (higher_is_better) {
        sub[order(-sub$score), ]
      } else {
        sub[order(sub$score), ]
      }
      
      # After sorting, position i always ranks above position j (i < j).
      # Raw scores are used only to detect ties, never to decide the winner.
      idx <- utils::combn(nrow(sub_ord), 2)
      s1  <- sub_ord$score[idx[1, ]]
      s2  <- sub_ord$score[idx[2, ]]
      
      rows_p$real <- data.frame(
        item1      = sub_ord$item[idx[1, ]],
        item2      = sub_ord$item[idx[2, ]],
        y          = ifelse(s1 != s2, 1, 0.5),
        period     = rep(p, ncol(idx)),
        cluster    = rep(cl, ncol(idx)),
        weight     = rep(w, ncol(idx)),
        structural = FALSE,
        stringsAsFactors = FALSE
      )
    }
    
    # ── Structural penalty for absent items (needs >= 1 present item) ──────
    # Never touches `score` — operates directly on win/loss counts, so an
    # absence can never be mistaken for a genuine observed value. No
    # comparison is generated between two absent items.
    if (absent == "penalize" && nrow(sub) >= 1) {
      present_items <- unique(sub$item)
      absent_items  <- setdiff(items, present_items)
      if (length(absent_items) > 0) {
        grid <- expand.grid(
          item1 = present_items,
          item2 = absent_items,
          stringsAsFactors = FALSE
        )
        rows_p$structural <- data.frame(
          item1      = grid$item1,
          item2      = grid$item2,
          y          = 1,
          period     = rep(p, nrow(grid)),
          cluster    = rep(cl, nrow(grid)),
          weight     = rep(w * absent_penalty, nrow(grid)),
          structural = TRUE,
          stringsAsFactors = FALSE
        )
      }
    }
    
    if (length(rows_p) > 0) {
      rows[[k]] <- do.call(rbind, unname(rows_p))
    }
  }
  
  rows <- rows[!vapply(rows, is.null, logical(1))]
  comparisons <- if (length(rows) > 0) {
    do.call(rbind, rows)
  } else {
    data.frame(
      item1 = character(0), item2 = character(0), y = numeric(0),
      period = periods[0], cluster = data$cluster[0], weight = numeric(0),
      structural = logical(0),
      stringsAsFactors = FALSE
    )
  }
  
  # Zero-weight rows contribute nothing to the counts or to any variance
  # computation; dropping them keeps absent_penalty = 0 identical to
  # absent = "ignore".
  comparisons <- comparisons[comparisons$weight != 0, , drop = FALSE]
  rownames(comparisons) <- NULL
  
  # ── Aggregate into the win count matrix ───────────────────────────────────
  win_counts <- .bt_aggregate_comparisons(comparisons, items)
  
  attr(win_counts, "comparisons") <- comparisons
  class(win_counts) <- c("bt_win_matrix", "matrix", "array")
  win_counts
}

# Aggregate comparison rows into a win count matrix (internal) ---------------
# item1 earns weight * y against item2; item2 earns weight * (1 - y)
# against item1 (non-zero only for ties). Shared by bt_win_matrix() and the
# consistency check in bt_fit(), so both use exactly the same arithmetic.
.bt_aggregate_comparisons <- function(comparisons, items) {
  n  <- length(items)
  f1 <- factor(comparisons$item1, levels = items)
  f2 <- factor(comparisons$item2, levels = items)
  
  wins_1 <- tapply(comparisons$weight * comparisons$y, list(f1, f2), sum)
  wins_2 <- tapply(comparisons$weight * (1 - comparisons$y), list(f2, f1), sum)
  wins_1[is.na(wins_1)] <- 0
  wins_2[is.na(wins_2)] <- 0
  
  matrix(
    as.vector(wins_1 + wins_2),
    nrow     = n,
    ncol     = n,
    dimnames = list(items, items)
  )
}

#' Print a win count matrix
#'
#' Prints the win counts only; the period-level `"comparisons"` attribute
#' (see [bt_win_matrix()]) is not shown. Use `attr(x, "comparisons")` to
#' inspect it.
#'
#' @param x An object of class `"bt_win_matrix"`, as returned by
#'   [bt_win_matrix()].
#' @param ... Further arguments passed to [print()].
#'
#' @return `x`, invisibly.
#'
#' @export
print.bt_win_matrix <- function(x, ...) {
  m <- matrix(as.vector(x), nrow = nrow(x), ncol = ncol(x),
              dimnames = dimnames(x))
  print(m, ...)
  invisible(x)
}
