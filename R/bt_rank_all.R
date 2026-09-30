#' Fit a Bradley-Terry ranking in a single call
#'
#' Convenience wrapper that runs the full Bradley-Terry pipeline in one step:
#' builds the win count matrix, fits the model, and returns a ranked data
#' frame. Optionally applies temporal weighting via exponential decay.
#'
#' When `top_n` is specified, the function runs the pipeline in two passes.
#' The first pass fits the model on all items to identify the top `top_n`
#' performers. The second pass re-fits the model using only those items,
#' so that the returned abilities reflect competitive strength within the
#' selected subset rather than the full pool.
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
#' @param top_n Integer. If provided, the model is first fitted on all items
#'   to identify the top `top_n` performers, then re-fitted on that subset.
#'   If `NULL` (default), all items are returned from a single model fit.
#' @param half_life Positive number. If provided, exponential decay weights
#'   are computed via [bt_weights()] and passed to [bt_win_matrix()]. The
#'   most recent period receives weight 1; earlier periods receive
#'   progressively lower weights. Weights are computed over the values of
#'   `time_col` when supplied, and of `period_col` otherwise. If `NULL`
#'   (default), all periods are weighted equally.
#' @param digits Integer. Number of decimal places for ability columns.
#'   Default: `4`.
#' @param absent,absent_penalty,cluster_col,time_col Passed to
#'   [bt_win_matrix()]; see its documentation. Defaults reproduce
#'   [bt_win_matrix()]'s defaults.
#'
#' @return A data frame as returned by [bt_rank()], with columns `rank`,
#'   `item`, `ability`, and `ability_norm`. The fitted model is attached as
#'   the attribute `"fit"` (a `btfit` object; with `top_n`, the second-pass
#'   fit on the selected subset), so standard errors and tests remain
#'   available, e.g. `summary(attr(res, "fit"))`.
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
#' # Top 2 only — model re-fitted on subset
#' bt_rank_all(data, score_col = "score", top_n = 2)
#'
#' # The fitted model is attached for further inference
#' res <- bt_rank_all(data, score_col = "score")
#' class(attr(res, "fit"))
#'
#' @export
bt_rank_all <- function(data,
                        score_col        = "score",
                        item_col         = "item",
                        period_col       = "period",
                        higher_is_better = TRUE,
                        top_n            = NULL,
                        half_life        = NULL,
                        digits           = 4,
                        absent           = c("ignore", "penalize"),
                        absent_penalty   = 1,
                        cluster_col      = NULL,
                        time_col         = NULL) {

  # ── Input validation ──────────────────────────────────────────────────────
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.null(top_n)) {
    if (!is.numeric(top_n) || length(top_n) != 1 || top_n < 1) {
      stop("`top_n` must be a single positive integer.", call. = FALSE)
    }
    top_n <- as.integer(top_n)
  }
  if (!is.null(half_life)) {
    if (!is.numeric(half_life) || length(half_life) != 1 || half_life <= 0) {
      stop("`half_life` must be a single positive number.", call. = FALSE)
    }
  }

  absent <- match.arg(absent)
  
  # ── Compute temporal weights (optional) ───────────────────────────────────
  # Over time_col when supplied (e.g. seasons when periods are matches).
  weights <- NULL
  if (!is.null(half_life)) {
    weight_col <- if (is.null(time_col)) period_col else time_col
    if (!weight_col %in% names(data)) {
      stop("Column(s) not found in `data`: ", weight_col, call. = FALSE)
    }
    weights <- bt_weights(periods = unique(data[[weight_col]]),
                          half_life = half_life)
  }
  
  # Only forward absent_penalty when the caller supplied it, so that
  # bt_win_matrix()'s "ignored with absent = 'ignore'" warning behaves as if
  # it had been called directly.
  win_args <- list(
    item_col         = item_col,
    period_col       = period_col,
    score_col        = score_col,
    higher_is_better = higher_is_better,
    weights          = weights,
    absent           = absent,
    cluster_col      = cluster_col,
    time_col         = time_col
  )
  if (!missing(absent_penalty)) win_args$absent_penalty <- absent_penalty

  # ── First pass: fit on all items ──────────────────────────────────────────
  win_mat_full <- do.call(bt_win_matrix, c(list(data = data), win_args))
  fit_full <- bt_fit(win_mat_full)

  # ── If no top_n, return ranking from full model ───────────────────────────
  if (is.null(top_n) || top_n >= length(fit_full$abilities)) {
    res <- bt_rank(fit_full, digits = digits)
    attr(res, "fit") <- fit_full
    return(res)
  }

  # ── Identify TOP-N items from first pass ──────────────────────────────────
  rk_full    <- bt_rank(fit_full)
  top_items  <- rk_full$item[seq_len(top_n)]

  # ── Second pass: re-fit on TOP-N subset only ──────────────────────────────
  data_sub <- data[data[[item_col]] %in% top_items, ]

  win_mat_sub <- do.call(bt_win_matrix, c(list(data = data_sub), win_args))
  fit_sub <- bt_fit(win_mat_sub)

  res <- bt_rank(fit_sub, digits = digits)
  attr(res, "fit") <- fit_sub
  res
}
