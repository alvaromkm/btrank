#' Compute a pairwise win probability matrix from a fitted Bradley-Terry model
#'
#' For every ordered pair of items (i, j), computes the probability that item
#' i beats item j according to the Bradley-Terry model:
#'
#' \deqn{P(i \text{ beats } j) = \frac{e^{\lambda_i}}{e^{\lambda_i} + e^{\lambda_j}}}
#'
#' where \eqn{\lambda_i} and \eqn{\lambda_j} are the estimated log-abilities
#' from the fitted model.
#'
#' @param fit A `btfit` object as returned by [bt_fit()].
#' @param order_by Character string controlling row/column order of the
#'   output matrix. One of `"ability"` (default, strongest item first) or
#'   `"name"` (alphabetical order).
#'
#' @return A square numeric matrix of dimension n_items × n_items. Entry
#'   `[i, j]` is the probability that item i beats item j. Diagonal entries
#'   are `NA`. Row and column names are item identifiers.
#'
#' @seealso [bt_fit()] to fit the model, [bt_rank()] to extract a ranked
#'   data frame from the same fit.
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
#' # Probability matrix ordered by ability (strongest first)
#' bt_prob_matrix(fit)
#'
#' # Probability that item "A" beats item "C"
#' pm <- bt_prob_matrix(fit)
#' pm["A", "C"]
#'
#' @export
bt_prob_matrix <- function(fit, order_by = c("ability", "name")) {

  # ── Input validation ──────────────────────────────────────────────────────
  if (!inherits(fit, "btfit")) {
    stop("`fit` must be a `btfit` object returned by `bt_fit()`.", call. = FALSE)
  }

  order_by <- match.arg(order_by)

  # ── Determine item order ──────────────────────────────────────────────────
  abilities <- fit$abilities

  item_order <- switch(order_by,
                       ability = names(sort(abilities, decreasing = TRUE)),
                       name    = sort(names(abilities))
  )

  abilities <- abilities[item_order]

  # ── Compute probability matrix (vectorised) ───────────────────────────────
  e_lambda <- exp(abilities)
  mat      <- outer(e_lambda, e_lambda, function(a, b) a / (a + b))
  diag(mat) <- NA_real_
  dimnames(mat) <- list(item_order, item_order)

  mat
}
