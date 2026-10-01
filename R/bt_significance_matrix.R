#' Pairwise significance matrix for a fitted Bradley-Terry model
#'
#' For every pair of items, tests whether their estimated abilities differ
#' significantly, using a Wald test on the difference `ability_i - ability_j`
#' and the joint variance-covariance matrix from [vcov.btfit()]. With more
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
#'
#' @return An object of class `"bt_significance_matrix"`, a list containing:
#' \describe{
#'   \item{`p`}{Symmetric numeric matrix of adjusted p-values, diagonal `NA`.}
#'   \item{`significant`}{Symmetric logical matrix, `TRUE` where
#'     `p < 1 - level`, diagonal `NA`.}
#'   \item{`method`}{The adjustment method used.}
#'   \item{`level`}{The confidence level used.}
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
#'
#' As with [summary.btfit()], the underlying Wald test breaks down under
#' quasi-complete separation (see [bt_fit()] Details): p-values can be
#' misleadingly large due to the Hauck-Donner effect. `reliable` surfaces
#' this rather than silently reporting numbers.
#'
#' @examples
#' data <- data.frame(
#'   item   = c("A", "B", "C", "A", "B", "C"),
#'   period = c(2021, 2021, 2021, 2022, 2022, 2022),
#'   score  = c(80, 70, 60, 65, 85, 55)
#' )
#' mat <- bt_win_matrix(data, score_col = "score")
#' fit <- bt_fit(mat)
#' bt_significance_matrix(fit)
#'
#' @seealso [vcov.btfit()] for the underlying covariance matrix,
#'   [summary.btfit()] for per-item standard errors and confidence intervals.
#'
#' @importFrom stats pnorm p.adjust
#' @export
bt_significance_matrix <- function(fit, level = 0.95,
                                   method = c("holm", "bonferroni", "BH", "none")) {
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
  
  # ── Embed vcov.btfit()'s (n-1) x (n-1) matrix into a full n x n matrix ────
  # with an exact zero row/column for the reference item (fixed at 0 by
  # construction: zero variance, zero covariance with everything else).
  v_full <- matrix(0, n, n, dimnames = list(items, items))
  v_sub  <- vcov(fit)
  v_full[rownames(v_sub), colnames(v_sub)] <- v_sub
  
  # ── Pairwise variance of ability differences ──────────────────────────────
  var_diff <- outer(items, items, Vectorize(function(i, j) {
    v_full[i, i] + v_full[j, j] - 2 * v_full[i, j]
  }))
  dimnames(var_diff) <- list(items, items)
  
  diff_ab <- outer(ab, ab, "-")
  z <- diff_ab / sqrt(var_diff)
  p_raw <- 2 * stats::pnorm(-abs(z))
  diag(p_raw) <- NA_real_
  
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
  cat(sprintf("  Significance level (alpha): %.3g\n\n", 1 - x$level))
  cat("p-values:\n")
  print(round(x$p, digits))
  
  if (!x$reliable) {
    cat(
      "\nWarning: this fit has quasi-complete separation.",
      "No p-value in this matrix is a meaningful measure of significance",
      "(see `?bt_fit`).\n"
    )
  }
  invisible(x)
}