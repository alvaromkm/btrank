#' Pairwise significance matrix for a fitted Bradley-Terry model
#'
#' For every pair of items, tests whether their estimated abilities differ
#' significantly, using a Wald test on the difference `ability_i - ability_j`
#' and the joint variance-covariance matrix from [vcov.btfit()] (by default
#' cluster-robust). With more
#' than two items this involves multiple simultaneous comparisons, so
#' p-values are adjusted by default (see `method`).
#'
#' @param fit A `btfit` object as returned by [bt_fit()].
#' @param level Confidence level, used only to derive the significance
#'   threshold `alpha = 1 - level` for the `significant` matrix. Default
#'   `0.95`.
#' @param method Multiple comparison adjustment applied to the
#'   `n * (n-1) / 2` pairwise p-values, via [stats::p.adjust()]. One of
#'   `"holm"` (default), `"bonferroni"`, `"BH"`, or `"none"`. `"none"` is
#'   provided for comparison only; reporting unadjusted pairwise p-values as
#'   if they were independent tests understates the true false-positive
#'   rate and is not recommended.
#' @param type Standard error type, passed to [vcov.btfit()]. `NULL`
#'   (default) selects the cluster-robust estimate when available.
#'
#' @return An object of class `"bt_significance_matrix"`, a list containing:
#' \describe{
#'   \item{`p`}{Symmetric numeric matrix of adjusted p-values, diagonal `NA`.}
#'   \item{`significant`}{Symmetric logical matrix, `TRUE` where
#'     `p < 1 - level`, diagonal `NA`.}
#'   \item{`method`}{The adjustment method used.}
#'   \item{`level`}{The confidence level used.}
#'   \item{`type`}{Standard error type used (`"cluster"` or `"model"`).}
#'   \item{`df`}{Degrees of freedom of the reference t distribution
#'     (`Inf` means normal).}
#'   \item{`non_estimable`}{Items whose pairs are `NA` (see Details).}
#'   \item{`reliable`}{Single logical. `FALSE` if `fit$separation_items` is
#'     non-empty, in which case no p-value in this matrix is meaningful
#'     (see [summary.btfit()] Details for why this is a fit-wide, not
#'     per-item, concern).}
#' }
#'
#' @details
#' For two non-reference items i, j: `Var(ability_i - ability_j) =
#' Var(ability_i) + Var(ability_j) - 2 * Cov(ability_i, ability_j)`, read
#' directly off [vcov.btfit()]. For an item compared against the reference
#' item, `Var(ability_i - ability_ref) = Var(ability_i)` exactly: the
#' reference's ability is fixed to 0 by construction, not estimated, so its
#' variance and its covariance with every other item are exactly 0 - not an
#' approximation. This is implemented by embedding [vcov.btfit()]'s
#' (n-1) x (n-1) matrix into a full n x n matrix with a zero row/column for
#' the reference, so no special-casing is needed in the comparison formula.
#' The variance of a difference does not depend on which item is the
#' reference, so these tests are exact with respect to that choice.
#'
#' Pairs involving an item whose uncertainty cannot be estimated (see
#' `non_estimable` in [vcov.btfit()]) get `NA` and are excluded from the
#' multiplicity adjustment.
#'
#' The test statistic is referred to a t distribution with `df` degrees of
#' freedom (\eqn{G - 1} for cluster-robust errors with \eqn{G} clusters)
#' or to the normal distribution for model-based errors.
#'
#' As with [summary.btfit()], the underlying Wald test breaks down under
#' quasi-complete separation (see [bt_fit()] Details): p-values can be
#' misleadingly large due to the Hauck-Donner effect. `reliable` surfaces
#' this rather than silently reporting numbers.
#'
#' @examples
#' set.seed(1)
#' data <- data.frame(item = rep(LETTERS[1:5], 10), period = rep(1:10, each = 5))
#' data$score <- rep(c(3, 2.25, 1.5, 0.75, 0), 10) + rnorm(50)
#' fit <- bt_fit(bt_win_matrix(data))
#' bt_significance_matrix(fit)
#'
#' @seealso [vcov.btfit()] for the underlying covariance matrix,
#'   [summary.btfit()] for per-item standard errors and confidence intervals.
#'
#' @importFrom stats pnorm pt p.adjust
#' @export
bt_significance_matrix <- function(fit, level = 0.95,
                                   method = c("holm", "bonferroni", "BH", "none"),
                                   type = NULL) {
  if (!inherits(fit, "btfit")) {
    stop("`fit` must be a `btfit` object returned by `bt_fit()`.", call. = FALSE)
  }
  if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 1) {
    stop("`level` must be a single number strictly between 0 and 1.", call. = FALSE)
  }
  method <- match.arg(method)
  
  items <- fit$items
  n     <- length(items)
  ab    <- fit$abilities[items]
  
  # ── Full n x n covariance, with an exact zero row/column for the ─────────
  # reference item (fixed at 0 by construction).
  inf    <- .bt_inference(fit, type)
  v_full <- inf$vcov_full[items, items]
  
  # ── Pairwise variance of ability differences ──────────────────────────────
  d        <- diag(v_full)
  var_diff <- outer(d, d, "+") - 2 * v_full
  
  diff_ab <- outer(ab, ab, "-")
  stat    <- diff_ab / sqrt(var_diff)
  p_raw   <- if (is.finite(inf$df)) {
    2 * stats::pt(-abs(stat), df = inf$df)
  } else {
    2 * stats::pnorm(-abs(stat))
  }
  diag(p_raw) <- NA_real_
  ne <- intersect(inf$non_estimable, items)
  p_raw[ne, ] <- NA_real_
  p_raw[, ne] <- NA_real_
  
  # ── Multiple comparison adjustment (upper triangle, then mirrored) ───────
  p_adj <- p_raw
  if (method != "none") {
    idx <- upper.tri(p_raw)
    p_adj[idx] <- stats::p.adjust(p_raw[idx], method = method)
    p_adj[lower.tri(p_adj)] <- t(p_adj)[lower.tri(p_adj)]
  }
  
  alpha       <- 1 - level
  significant <- p_adj < alpha
  diag(significant) <- NA
  
  structure(
    list(
      p           = p_adj,
      significant = significant,
      method      = method,
      level       = level,
      type        = inf$type,
      df          = inf$df,
      non_estimable = inf$non_estimable,
      reliable    = length(fit$separation_items) == 0
    ),
    class = "bt_significance_matrix"
  )
}

#' Print method for bt_significance_matrix objects
#'
#' @param x A `bt_significance_matrix` object.
#' @param digits Integer. Decimal places to display. Default `4`.
#' @param ... Further arguments (ignored).
#' @return Invisibly returns `x`.
#' @export
print.bt_significance_matrix <- function(x, digits = 4, ...) {
  cat("Bradley-Terry pairwise significance matrix\n")
  cat("  Adjustment method:", x$method, "\n")
  if (x$type == "cluster") {
    cat(sprintf("  Standard errors: cluster-robust; t reference with %d df\n",
                as.integer(x$df)))
  } else {
    cat("  Standard errors: model-based; normal reference\n")
  }
  cat(sprintf("  Significance level (alpha): %.3g\n\n", 1 - x$level))
  cat("p-values:\n")
  print(round(x$p, digits))
  
  if (length(x$non_estimable) > 0) {
    cat("\nNote: pairs involving item(s) present in a single cluster are NA",
        "(see `?vcov.btfit`):", paste(x$non_estimable, collapse = ", "), "\n")
  }  
  if (!x$reliable) {
    cat(
      "\nWarning: this fit has quasi-complete separation.",
      "No p-value in this matrix is a meaningful measure of significance",
      "(see `?bt_fit`).\n"
    )
  }
  invisible(x)
}