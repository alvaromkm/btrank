#' Variance-covariance matrix of a fitted Bradley-Terry model
#'
#' Extracts the variance-covariance matrix of the estimated log-ability
#' (lambda) parameters, re-parametrised so that names match the item
#' identifiers used throughout \pkg{btrank} (rather than `BTm`'s internal
#' `"..item"` naming). The reference item (ability fixed to 0 by
#' construction) is not a free parameter and is therefore not included.
#'
#' @param object A `btfit` object as returned by [bt_fit()].
#' @param ... Further arguments (ignored).
#'
#' @return A square numeric matrix of dimension (n_items - 1) x (n_items - 1),
#'   where n_items is `length(object$items)`. Row and column names are item
#'   identifiers, excluding `object$reference`.
#'
#' @seealso [summary.btfit()], [confint.btfit()]
#'
#' @importFrom stats vcov
#' @export
vcov.btfit <- function(object, ...) {
  if (!inherits(object, "btfit")) {
    stop("`object` must be a `btfit` object returned by `bt_fit()`.", call. = FALSE)
  }
  v  <- stats::vcov(object$model)
  nm <- gsub("^\\.\\.", "", rownames(v))
  dimnames(v) <- list(nm, nm)
  v
}

#' Summarise a fitted Bradley-Terry model with standard errors and
#' confidence intervals
#'
#' Produces a per-item table of estimated abilities, standard errors, and
#' Wald confidence intervals, extending the point estimates returned by
#' [bt_fit()] with the uncertainty quantification computed internally by
#' `BradleyTerry2::BTm()`.
#'
#' @param object A `btfit` object as returned by [bt_fit()].
#' @param level Confidence level for the interval. Default `0.95`.
#' @param ... Further arguments (ignored).
#'
#' @return An object of class `"summary.btfit"`, a list containing:
#' \describe{
#'   \item{`table`}{A data frame with columns `item`, `ability`, `se`,
#'     `ci_low`, `ci_high`, and `reliable`, sorted by descending ability.
#'     The reference item has `se`, `ci_low`, and `ci_high` equal to `NA`:
#'     its ability is fixed to 0 by construction, not estimated, so a
#'     standard error of 0 would misleadingly imply perfect certainty.}
#'   \item{`level`}{The confidence level used.}
#'   \item{`reference`}{The reference item.}
#' }
#'
#' @details
#' Confidence intervals are Wald intervals (`estimate +/- qnorm((1+level)/2) *
#' se`).
#'
#' The `reliable` column applies to the **entire fit**, not just to the
#' items listed in `object$separation_items`. This is deliberate, not an
#' approximation: because [bt_fit()] fixes the *weakest* item as reference,
#' and a never-winning item is almost always the weakest, separation most
#' often falls exactly on the reference. Every other item's ability and SE
#' are computed *relative to that reference*, so the reference's broken
#' uncertainty propagates to all of them identically — verified empirically
#' to produce numerically identical (and equally uninformative) standard
#' errors across every non-separated item in the fit, not just the flagged
#' one. Reporting only the separated item(s) as unreliable would therefore
#' be misleading, not conservative. If `object$separation_items` is
#' non-empty, `reliable` is `FALSE` for every row.
#'
#' @examples
#' data <- data.frame(
#'   item   = c("A", "B", "C", "A", "B", "C"),
#'   period = c(2021, 2021, 2021, 2022, 2022, 2022),
#'   score  = c(80, 70, 60, 65, 85, 55)
#' )
#' mat <- bt_win_matrix(data, score_col = "score")
#' fit <- bt_fit(mat)
#' summary(fit)
#'
#' @importFrom stats qnorm
#' @export
summary.btfit <- function(object, level = 0.95, ...) {
  if (!inherits(object, "btfit")) {
    stop("`object` must be a `btfit` object returned by `bt_fit()`.", call. = FALSE)
  }
  if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 1) {
    stop("`level` must be a single number strictly between 0 and 1.", call. = FALSE)
  }
  
  ab    <- object$abilities
  items <- names(ab)
  
  se <- setNames(rep(NA_real_, length(items)), items)
  v  <- vcov(object)
  se[rownames(v)] <- sqrt(diag(v))
  
  z       <- stats::qnorm((1 + level) / 2)
  ci_low  <- ab - z * se
  ci_high <- ab + z * se
  
  # Separation contaminates the whole fit (see Details), not just the
  # flagged item(s): every ability is expressed relative to the reference,
  # so if the reference (or any item) is separated, no SE in this table is
  # trustworthy.
  reliable <- rep(length(object$separation_items) == 0, length(items))
  
  tbl <- data.frame(
    item     = items,
    ability  = as.numeric(ab),
    se       = as.numeric(se),
    ci_low   = as.numeric(ci_low),
    ci_high  = as.numeric(ci_high),
    reliable = reliable,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  tbl <- tbl[order(-tbl$ability), ]
  rownames(tbl) <- NULL
  
  structure(
    list(table = tbl, level = level, reference = object$reference),
    class = "summary.btfit"
  )
}

#' Print method for summary.btfit objects
#'
#' @param x A `summary.btfit` object.
#' @param digits Integer. Number of decimal places to display. Default `4`.
#' @param ... Further arguments (ignored).
#' @return Invisibly returns `x`.
#' @export
print.summary.btfit <- function(x, digits = 4, ...) {
  cat("Bradley-Terry model summary\n")
  cat("  Reference item:", x$reference, "(ability = 0, SE = NA by construction)\n")
  cat(sprintf("  Confidence level: %.0f%%\n\n", x$level * 100))
  
  tbl <- x$table
  tbl$ability <- round(tbl$ability, digits)
  tbl$se      <- round(tbl$se, digits)
  tbl$ci_low  <- round(tbl$ci_low, digits)
  tbl$ci_high <- round(tbl$ci_high, digits)
  print(tbl, row.names = FALSE)
  
  if (any(!tbl$reliable)) {
    cat(
      "\nWarning: item(s) marked `reliable = FALSE` exhibit quasi-complete",
      "separation.\nTheir SE/CI are reported but are not a meaningful",
      "measure of uncertainty (see `?bt_fit`).\n"
    )
  }
  invisible(x)
}

#' Confidence intervals for a fitted Bradley-Terry model
#'
#' @param object A `btfit` object as returned by [bt_fit()].
#' @param parm Character vector of item names to include. If missing,
#'   all items are returned.
#' @param level Confidence level. Default `0.95`.
#' @param ... Further arguments (ignored).
#'
#' @return A numeric matrix with one row per item and two columns giving
#'   the lower and upper confidence limits. The reference item's row is
#'   `NA` in both columns (see [summary.btfit()] Details).
#'
#' @seealso [summary.btfit()] for the full table including standard errors
#'   and the `reliable` flag.
#'
#' @export
confint.btfit <- function(object, parm, level = 0.95, ...) {
  if (!inherits(object, "btfit")) {
    stop("`object` must be a `btfit` object returned by `bt_fit()`.", call. = FALSE)
  }
  s   <- summary(object, level = level)
  tbl <- s$table
  
  if (!missing(parm)) {
    unknown <- setdiff(parm, tbl$item)
    if (length(unknown) > 0) {
      stop("Unknown item(s) in `parm`: ", paste(unknown, collapse = ", "), call. = FALSE)
    }
    tbl <- tbl[match(parm, tbl$item), , drop = FALSE]
  }
  
  out <- as.matrix(tbl[, c("ci_low", "ci_high")])
  rownames(out) <- tbl$item
  colnames(out) <- c(
    sprintf("%.1f %%", (1 - level) / 2 * 100),
    sprintf("%.1f %%", (1 + level) / 2 * 100)
  )
  out
}

#' Plot method for btfit objects
#'
#' Caterpillar plot of estimated abilities with Wald confidence intervals,
#' one row per item, ordenado de más débil (abajo) a más fuerte (arriba).
#'
#' @param x A `btfit` object as returned by [bt_fit()].
#' @param level Confidence level for the intervals. Default `0.95`.
#' @param ... Further arguments passed to [graphics::plot()].
#'
#' @return Invisibly returns the `summary.btfit` table used to draw the plot.
#'
#' @details
#' Items flagged `reliable = FALSE` (see [summary.btfit()]) are drawn in
#' grey, since their confidence intervals are not a meaningful measure of
#' uncertainty. The reference item is drawn as a point at 0 with no
#' interval, since its ability is fixed by construction, not estimated.
#'
#' @examples
#' data_ex <- data.frame(
#'   item   = c("A", "B", "C", "A", "B", "C"),
#'   period = c(2021, 2021, 2021, 2022, 2022, 2022),
#'   score  = c(80, 70, 60, 65, 85, 55)
#' )
#' mat <- bt_win_matrix(data_ex, score_col = "score")
#' fit <- bt_fit(mat)
#' plot(fit)
#'
#' @importFrom graphics plot segments axis abline
#' @export
plot.btfit <- function(x, level = 0.95, ...) {
  if (!inherits(x, "btfit")) {
    stop("`x` must be a `btfit` object returned by `bt_fit()`.", call. = FALSE)
  }
  
  tbl <- summary(x, level = level)$table
  tbl <- tbl[order(tbl$ability), ]
  n   <- nrow(tbl)
  y   <- seq_len(n)
  
  xlim <- range(c(tbl$ci_low, tbl$ci_high, tbl$ability), na.rm = TRUE)
  col  <- ifelse(tbl$reliable, "black", "grey60")
  
  graphics::plot(
    tbl$ability, y,
    xlim = xlim, ylim = c(0.5, n + 0.5),
    yaxt = "n", ylab = "", xlab = "Estimated ability (lambda)",
    pch = 19, col = col
  )
  graphics::axis(2, at = y, labels = tbl$item, las = 1)
  graphics::segments(tbl$ci_low, y, tbl$ci_high, y, col = col)
  graphics::abline(v = 0, lty = 2, col = "grey80")
  
  invisible(tbl)
}