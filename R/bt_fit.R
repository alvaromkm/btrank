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
#' }
#'
#' @details
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

  # ── Build long-format data frame for BTm ─────────────────────────────────
  df_bt <- .win_matrix_to_long(win_matrix)

  # ── First internal pass: arbitrary reference to find the weakest item ─────
  fit1 <- BradleyTerry2::BTm(
    outcome = cbind(wins1, wins2),
    player1 = item1,
    player2 = item2,
    data    = df_bt
  )

  abilities1 <- .extract_abilities(fit1, items)
  ref_item   <- abilities1$item[which.min(abilities1$ability)]

  # ── Second internal pass: re-fit with weakest item as reference ───────────
  new_levels <- c(ref_item, setdiff(items, ref_item))

  df_bt2       <- df_bt
  df_bt2$item1 <- factor(as.character(df_bt$item1), levels = new_levels)
  df_bt2$item2 <- factor(as.character(df_bt$item2), levels = new_levels)

  fit2 <- BradleyTerry2::BTm(
    outcome = cbind(wins1, wins2),
    player1 = item1,
    player2 = item2,
    data    = df_bt2
  )

  abilities2    <- .extract_abilities(fit2, new_levels)
  abilities_vec <- setNames(abilities2$ability, abilities2$item)

  # ── Return btfit object ───────────────────────────────────────────────────
  structure(
    list(
      abilities = abilities_vec,
      reference = ref_item,
      model     = fit2,
      items     = new_levels
    ),
    class = "btfit"
  )
}

# =============================================================================
# Internal helpers (not exported)
# =============================================================================

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
