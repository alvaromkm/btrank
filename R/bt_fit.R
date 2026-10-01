#' Fit a Bradley-Terry model from a pairwise win count matrix
#'
#' Takes a win count matrix (as produced by [bt_win_matrix()]) and fits a
#' Bradley-Terry model using maximum likelihood estimation. The reference item
#' (ability fixed to 0) is automatically set to the weakest item, so that all
#' other ability estimates are non-negative and directly interpretable as
#' strength relative to the worst-performing item in the set.
#'
#' @param win_matrix A square numeric matrix of pairwise win counts, as
#'   returned by [bt_win_matrix()]. Row and column names must be item
#'   identifiers.
#'
#' @return An object of class `"btfit"`, which is a list containing:
#' \describe{
#'   \item{`abilities`}{Named numeric vector of estimated log-ability (lambda)
#'     parameters, one per item. The reference item has ability 0.}
#'   \item{`reference`}{Character string. Name of the item used as reference
#'     (the one with the lowest estimated ability).}
#'   \item{`model`}{The underlying `BTm` model object from the
#'     \pkg{BradleyTerry2} package, for advanced use.}
#'   \item{`items`}{Character vector of all item names, in the order used
#'     internally by the model.}
#'   \item{`separation_items`}{Character vector of item names exhibiting
#'     quasi-complete separation (zero wins or zero losses across all
#'     comparisons). Empty (`character(0)`) if none. See Details.}
#' }
#'
#' @references
#' Turner, H., & Firth, D. (2012). Bradley-Terry models in R: The BradleyTerry2
#' package. *Journal of Statistical Software*, 48(9), 1–21.
#' https://doi.org/10.18637/jss.v048.i09
#' 
#' @details
#' Internally, `bt_fit()` estimates the model via maximum likelihood using
#' `BradleyTerry2::BTm()` (Turner & Firth, 2012).
#' The fitting procedure runs in two internal passes. A first model is fit
#' with an arbitrary reference item to identify the item with the lowest
#' estimated ability. A second model is then re-fit using that weakest item
#' as the reference, ensuring all reported ability estimates are >= 0.
#' These two passes are internal to `bt_fit` and are distinct from the
#' two-pass top-N selection performed by [bt_rank_all()].
#'
#' Items with no wins and no losses in any comparison (i.e. all-zero rows and
#' columns in `win_matrix`) are automatically dropped with a warning, as the
#' model cannot estimate their ability.
#'
#' Separately, items that never win (but do lose) or never lose (but do win)
#' any comparison are **not** dropped, but flagged in `separation_items` and
#' reported via a warning. Such items induce quasi-complete separation in the
#' underlying logistic likelihood: the point estimate returned is finite
#' (bounded by the optimiser's iteration limit) but its standard error is not
#' a meaningful measure of uncertainty. Any standard error, confidence
#' interval, or significance test involving a flagged item should be treated
#' with caution, or omitted, downstream (see `summary.btfit()`).
#'
#' When [bt_win_matrix()] is used with temporal weighting (`weights`) or
#' `absent = "penalize"`, the resulting win counts are generally non-integer.
#' `BradleyTerry2::BTm()`'s underlying `glm()` call emits a
#' `"non-integer counts in a binomial glm!"` warning in this case; this is
#' the same device Turner & Firth (2012, Section 9.1) describe for their
#' tie-splitting adjustment, and they state it is safe to ignore for point
#' estimates. `bt_fit()` suppresses only this specific warning (see
#' `.muffle_noninteger_count_warning()` below); every other warning from the fit, including
#' non-convergence, is left untouched. Standard errors under non-integer
#' counts are not addressed by Turner & Firth and are not separately
#' validated here; see the `absent_penalty` documentation in
#' [bt_win_matrix()] for a known effect on precision.
#'
#' @seealso [bt_win_matrix()] to build the input matrix,
#'   [bt_rank()] to extract a ranked data frame from the fitted model,
#'   [bt_prob_matrix()] to compute win probability matrices,
#'   [bt_rank_all()] for the full pipeline with two-pass top-N re-fitting.
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
#' fit$abilities    # named vector of lambda estimates
#' fit$reference    # item used as reference (ability = 0)
#'
#' @importFrom BradleyTerry2 BTm
#' @importFrom utils combn
#' @importFrom stats coef setNames
#' @export
bt_fit <- function(win_matrix) {
  
  # ── Input validation ──────────────────────────────────────────────────────
  if (!is.matrix(win_matrix) || !is.numeric(win_matrix)) {
    stop("`win_matrix` must be a numeric matrix.", call. = FALSE)
  }
  
  if (nrow(win_matrix) != ncol(win_matrix)) {
    stop("`win_matrix` must be square.", call. = FALSE)
  }
  
  if (is.null(rownames(win_matrix)) || is.null(colnames(win_matrix))) {
    stop("`win_matrix` must have row and column names.", call. = FALSE)
  }
  
  if (nrow(win_matrix) < 2) {
    stop("`win_matrix` must contain at least 2 items.", call. = FALSE)
  }
  
  # ── Drop items with zero activity ─────────────────────────────────────────
  active <- rowSums(win_matrix) + colSums(win_matrix) > 0
  if (any(!active)) {
    dropped <- names(active)[!active]
    warning(
      "The following items have no comparisons and were dropped: ",
      paste(dropped, collapse = ", "),
      call. = FALSE
    )
    win_matrix <- win_matrix[active, active]
  }
  
  items <- rownames(win_matrix)
  
  # ── Detect quasi-complete separation ──────────────────────────────────────
  # An item that never wins (rowSum = 0) or never loses (colSum = 0) a single
  # comparison drives its own coefficient towards +/-Inf under the logistic
  # likelihood used internally by BTm. The optimiser still returns a finite
  # point estimate (bounded by iteration limits), but the associated standard
  # error is not a meaningful measure of uncertainty. This is flagged here,
  # once, so that every downstream consumer (summary.btfit, confint.btfit,
  # bt_significance_matrix) can rely on it instead of re-deriving it.
  separation_items <- .detect_separation(win_matrix)
  if (length(separation_items) > 0) {
    warning(
      "The following items never won or never lost a single comparison: ",
      paste(separation_items, collapse = ", "),
      ". This causes quasi-complete separation: point estimates are ",
      "reported, but standard errors and confidence intervals involving ",
      "these items are not reliable (see `?bt_fit` Details).",
      call. = FALSE
    )
  }
  
  # ── Build long-format data frame for BTm ─────────────────────────────────
  df_bt <- .win_matrix_to_long(win_matrix)
  
  # ── First internal pass: arbitrary reference to find the weakest item ─────
  fit1 <- withCallingHandlers(
    BradleyTerry2::BTm(
      outcome = cbind(wins1, wins2),
      player1 = item1,
      player2 = item2,
      data    = df_bt
    ),
    warning = .muffle_noninteger_count_warning
  )
  
  abilities1 <- .extract_abilities(fit1, items)
  ref_item   <- abilities1$item[which.min(abilities1$ability)]
  
  # ── Second internal pass: re-fit with weakest item as reference ───────────
  new_levels <- c(ref_item, setdiff(items, ref_item))
  
  df_bt2       <- df_bt
  df_bt2$item1 <- factor(as.character(df_bt$item1), levels = new_levels)
  df_bt2$item2 <- factor(as.character(df_bt$item2), levels = new_levels)
  
  fit2 <- withCallingHandlers(
    BradleyTerry2::BTm(
      outcome = cbind(wins1, wins2),
      player1 = item1,
      player2 = item2,
      data    = df_bt2
    ),
    warning = .muffle_noninteger_count_warning
  )
  
  abilities2    <- .extract_abilities(fit2, new_levels)
  abilities_vec <- setNames(abilities2$ability, abilities2$item)
  
  # ── Return btfit object ───────────────────────────────────────────────────
  structure(
    list(
      abilities        = abilities_vec,
      reference        = ref_item,
      model            = fit2,
      items            = new_levels,
      separation_items = separation_items
    ),
    class = "btfit"
  )
}

# =============================================================================
# Internal helpers (not exported)
# =============================================================================

#' Handler that suppresses only the documented non-integer-count warning
#'
#' Turner & Firth (2012, JSS 48(9), Section 9.1) document that fractional
#' win counts (their tie-splitting device) trigger a `glm()` warning about
#' non-integer binomial counts, and state it is safe to ignore for point
#' estimates. `bt_win_matrix()`'s `weights` and `absent_penalty` arguments
#' produce fractional counts by the same mechanism, so the same warning is
#' suppressed here. Every other warning (e.g. non-convergence) is re-raised
#' unchanged via `invokeRestart`, never silently swallowed.
#'
#' Only the handler is factored out, not the `BTm()` call: wrapping the call
#' itself in a helper breaks BTm's NSE resolution of `player1`/`player2`
#' column references. `BTm()` must always be called inline.
#' @noRd
.muffle_noninteger_count_warning <- function(w) {
  if (grepl("non-integer .* binomial", conditionMessage(w))) {
    invokeRestart("muffleWarning")
  }
}

#' Convert a win count matrix to a long data frame for BTm
#' @noRd
.win_matrix_to_long <- function(win_matrix) {
  items <- rownames(win_matrix)
  pairs <- combn(items, 2)
  n     <- ncol(pairs)
  
  data.frame(
    item1 = factor(pairs[1, ], levels = items),
    item2 = factor(pairs[2, ], levels = items),
    wins1 = vapply(seq_len(n), function(k) win_matrix[pairs[1, k], pairs[2, k]], numeric(1)),
    wins2 = vapply(seq_len(n), function(k) win_matrix[pairs[2, k], pairs[1, k]], numeric(1)),
    stringsAsFactors = FALSE
  )
}

#' Identify items with quasi-complete separation (0 wins or 0 losses)
#' @noRd
.detect_separation <- function(win_matrix) {
  zero_wins   <- rownames(win_matrix)[rowSums(win_matrix) == 0]
  zero_losses <- rownames(win_matrix)[colSums(win_matrix) == 0]
  union(zero_wins, zero_losses)
}

#' Extract named ability estimates from a BTm model
#' @noRd
.extract_abilities <- function(bt_model, all_items) {
  coef_mat   <- coef(summary(bt_model))
  item_names <- gsub("^\\.\\.", "", rownames(coef_mat))
  ref_item   <- setdiff(all_items, item_names)
  
  rbind(
    data.frame(item = item_names, ability = coef_mat[, "Estimate"],
               stringsAsFactors = FALSE),
    data.frame(item = ref_item,   ability = 0,
               stringsAsFactors = FALSE)
  )
}

# =============================================================================
# S3 methods for btfit
# =============================================================================

#' Print method for btfit objects
#'
#' @description
#' Displays a compact summary of a fitted Bradley-Terry model: the number of
#' items, the reference item (whose ability is fixed to 0), and the estimated
#' log-ability (lambda) values sorted from strongest to weakest.
#'
#' @param x A `btfit` object.
#' @param ... Further arguments (ignored).
#' @return Invisibly returns `x`.
#' @export
print.btfit <- function(x, ...) {
  cat("Bradley-Terry model fit\n")
  cat("  Items    :", length(x$abilities), "\n")
  cat("  Reference:", x$reference, "(ability = 0)\n\n")
  ab_sorted <- sort(x$abilities, decreasing = TRUE)
  cat("  Abilities (lambda):\n")
  print(round(ab_sorted, 4))
  invisible(x)
}