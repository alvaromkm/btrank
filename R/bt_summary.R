#' Variance-covariance matrix of a fitted Bradley-Terry model
#'
#' Returns the variance-covariance matrix of the estimated log-ability
#' (lambda) parameters. By default it is a cluster-robust (sandwich)
#' estimate that accounts for the dependence between comparisons derived
#' from the same cluster; the model-based estimate of
#' `BradleyTerry2::BTm()` is available for comparison. Names match the item
#' identifiers used throughout \pkg{btrank}. The reference item (ability
#' fixed to 0 by construction) is not a free parameter and is not included.
#'
#' @param object A `btfit` object as returned by [bt_fit()].
#' @param type Character or `NULL`. `"cluster"` for the cluster-robust
#'   estimate, `"model"` for the model-based estimate. `NULL` (default)
#'   selects `"cluster"` when the fit carries a comparison-level record (see
#'   [bt_fit()]) with at least 2 clusters, and `"model"` otherwise.
#' @param ... Further arguments (ignored).
#'
#' @return A square numeric matrix of dimension (n_items - 1) x (n_items - 1)
#'   with row and column names equal to the item identifiers, excluding
#'   `object$reference`. It carries the attributes `type`, `n_clusters`,
#'   `effective_clusters`, `df` and `non_estimable` (see Details); for
#'   `type = "model"` these are `"model"`, `NA`, `NA`, `Inf` and
#'   `character(0)`.
#'
#' @details
#' **Why a cluster-robust estimate.** [bt_win_matrix()] decomposes every
#' period into all pairwise comparisons. Comparisons from the same period
#' are functions of a single vector of scores and are not independent, and
#' temporal weights are importance weights, not frequencies. The
#' model-based covariance assumes independent binomial comparisons with
#' frequency weights, so it is not a valid measure of sampling uncertainty
#' in this setting.
#'
#' The fit is treated as a weighted composite-likelihood (M-) estimator.
#' With clusters \eqn{g = 1, \dots, G} (by default the periods; see
#' `cluster_col` in [bt_win_matrix()]) and score contributions \eqn{U_g}
#' summed within each cluster, the estimate is
#' \deqn{V = A^{-1} \left(\sum_g U_g U_g^\top\right) A^{-1} \frac{G}{G - 1},}
#' where \eqn{A^{-1}} is the model-based covariance (Liang & Zeger, 1986;
#' White, 1982; Varin, Reid & Firth, 2011).
#'
#' **Attributes.** `n_clusters` is \eqn{G}. `effective_clusters` is a
#' Kish-type effective number of clusters,
#' \eqn{(\sum_g W_g)^2 / \sum_g W_g^2}, with \eqn{W_g} the total weight of
#' cluster \eqn{g}; it is reported as a descriptive diagnostic of how many
#' clusters effectively carry the information (it drops when temporal
#' decay is strong) and is not used in any computation. `df` is the number
#' of degrees of freedom used for intervals and tests, \eqn{G - 1} for
#' `type = "cluster"` (a common convention for cluster-robust inference)
#' and `Inf` (normal reference) for `type = "model"`.
#'
#' **Items present in a single cluster.** The cluster-robust estimate
#' cannot measure the uncertainty of an item whose comparisons all come from
#' one cluster (e.g. a team present in a single season): at the fitted
#' values its score contributions sum to zero within that cluster, so the
#' estimate is degenerate in its direction. Such items are listed in the
#' attribute `non_estimable`; the rows and columns of the returned matrix
#' that involve them are not meaningful, and [summary.btfit()] and
#' [bt_significance_matrix()] report `NA` for them. Resampling methods
#' (jackknife, cluster bootstrap) share this limitation. Structural
#' comparisons from `absent = "penalize"` count as presence in a cluster.
#'
#' Requesting `type = "model"` explicitly on a fit whose clusters contain
#' more than one comparison gives a warning, since the independence
#' assumption is known to be violated there.
#'
#' @references
#' Liang, K.-Y., & Zeger, S. L. (1986). Longitudinal data analysis using
#' generalized linear models. *Biometrika*, 73(1), 13–22.
#'
#' Varin, C., Reid, N., & Firth, D. (2011). An overview of composite
#' likelihood methods. *Statistica Sinica*, 21(1), 5–42.
#'
#' White, H. (1982). Maximum likelihood estimation of misspecified models.
#' *Econometrica*, 50(1), 1–25.
#'
#' @seealso [summary.btfit()], [confint.btfit()], [bt_significance_matrix()]
#'
#' @importFrom stats vcov
#' @export
vcov.btfit <- function(object, type = NULL, ...) {
  if (!inherits(object, "btfit")) {
    stop("`object` must be a `btfit` object returned by `bt_fit()`.", call. = FALSE)
  }
  .bt_inference(object, type)$vcov
}

# Shared inference engine (internal) -----------------------------------------
# Resolves `type`, computes the covariance for the estimated items, the full
# covariance including the reference, quasi-SEs and degrees of freedom.
.bt_inference <- function(object, type = NULL) {
  if (!is.null(type)) {
    if (!is.character(type) || length(type) != 1 ||
        !type %in% c("cluster", "model")) {
      stop("`type` must be NULL, \"cluster\" or \"model\".", call. = FALSE)
    }
  }
  explicit <- !is.null(type)
  cmp      <- object$comparisons
  G        <- if (is.null(cmp)) 0L else length(unique(cmp$cluster))
  
  v_model <- stats::vcov(object$model)
  nm <- gsub("^\\.\\.", "", rownames(v_model))
  dimnames(v_model) <- list(nm, nm)
  
  if (is.null(type)) {
    type <- if (G >= 2) "cluster" else "model"
  } else if (type == "cluster" && is.null(cmp)) {
    stop("Cluster-robust standard errors need the comparison-level record ",
         "created by bt_win_matrix(), which this fit does not have ",
         "(see `?bt_fit`). Use `type = \"model\"`.", call. = FALSE)
  } else if (type == "cluster" && G < 2) {
    stop("Cluster-robust standard errors require at least 2 clusters.",
         call. = FALSE)
  }
  
  if (type == "cluster") {
    U <- .bt_cluster_scores(cmp, object$abilities, rownames(v_model))
    V <- .bt_vcov_sandwich(v_model, U, small_sample = TRUE)
    non_est    <- .bt_non_estimable_items(cmp, object$items)
    n_clusters <- G
    eff        <- .bt_effective_clusters(cmp)
    df         <- G - 1
  } else {
    if (explicit && !is.null(cmp) && any(table(cmp$cluster) > 1)) {
      warning("Model-based standard errors assume independent comparisons, ",
              "but some clusters contribute several comparisons. They are ",
              "not a valid measure of uncertainty here; use ",
              "`type = \"cluster\"`.", call. = FALSE)
    }
    V <- v_model
    non_est    <- character(0)
    n_clusters <- NA_integer_
    eff        <- NA_real_
    df         <- Inf
  }
  
  attr(V, "type")               <- type
  attr(V, "n_clusters")         <- n_clusters
  attr(V, "effective_clusters") <- eff
  attr(V, "df")                 <- df
  attr(V, "non_estimable")      <- non_est
  
  V_plain <- V
  attributes(V_plain) <- list(dim = dim(V), dimnames = dimnames(V))
  V_full  <- .bt_full_vcov(V_plain, object$items)
  
  list(vcov = V, vcov_full = V_full, type = type, n_clusters = n_clusters,
       effective_clusters = eff, df = df, non_estimable = non_est)
}

.bt_quantile <- function(level, df) {
  p <- (1 + level) / 2
  if (is.finite(df)) stats::qt(p, df) else stats::qnorm(p)
}

#' Summarise a fitted Bradley-Terry model with standard errors and
#' confidence intervals
#'
#' Produces a per-item table of estimated abilities, quasi-standard errors
#' and confidence intervals.
#'
#' @param object A `btfit` object as returned by [bt_fit()].
#' @param level Confidence level for the interval. Default `0.95`.
#' @param type Standard error type, passed to [vcov.btfit()]. `NULL`
#'   (default) selects the cluster-robust estimate when available.
#' @param ... Further arguments (ignored).
#'
#' @return An object of class `"summary.btfit"`, a list containing:
#' \describe{
#'   \item{`table`}{A data frame with columns `item`, `ability`, `se`,
#'     `ci_low`, `ci_high`, and `reliable`, sorted by descending ability.
#'     `se` is the quasi-standard error (see Details); every item, including
#'     the reference, has one.}
#'   \item{`level`}{The confidence level used.}
#'   \item{`reference`}{The reference item.}
#'   \item{`type`}{Standard error type used (`"cluster"` or `"model"`).}
#'   \item{`df`}{Degrees of freedom of the reference distribution (`Inf`
#'     means normal).}
#'   \item{`n_clusters`, `effective_clusters`, `non_estimable`}{As in
#'     [vcov.btfit()].}
#' }
#'
#' @details
#' **Quasi-standard errors.** Abilities are only identified up to an
#' additive constant, so the standard error of a single ability depends on
#' the arbitrary choice of reference item: with the weakest item as
#' reference (see [bt_fit()]), every standard error absorbs the
#' uncertainty of that item. `summary.btfit()` instead reports
#' quasi-standard errors (Firth & de Menezes, 2004), computed with
#' \pkg{qvcalc} from the full covariance matrix of [vcov.btfit()]. They do
#' not depend on the reference, exist for every item, and
#' \eqn{\sqrt{qse_i^2 + qse_j^2}} approximates the standard error of any
#' difference \eqn{\lambda_i - \lambda_j}. The intervals
#' `ability +/- q * se` are therefore comparison intervals: they are meant
#' for comparing items with each other, not as intervals for an absolute
#' ability. For exact pairwise tests, use [bt_significance_matrix()].
#'
#' Quasi-variances can be negative, or not computable, when the covariance
#' matrix is far from the structure they approximate. In practice this
#' happens with very few items or clusters, where the cluster-robust
#' covariance is itself noisy. The affected quasi-standard errors are then
#' `NA`, with a warning; [bt_significance_matrix()] uses the full
#' covariance matrix and is unaffected.
#'
#' `q` is the `(1 + level) / 2` quantile of a t distribution with `df`
#' degrees of freedom (\eqn{G - 1} for cluster-robust errors) or of the
#' normal distribution for model-based errors.
#'
#' **Reliability.** Items whose uncertainty cannot be estimated (see
#' `non_estimable` in [vcov.btfit()]) have `NA` standard errors and
#' intervals and `reliable = FALSE`; quasi-variances are computed from the
#' remaining items only. If `object$separation_items` is non-empty,
#' `reliable` is `FALSE` for every row. Quasi-variances are fitted jointly to the whole
#' covariance matrix, so the diverging variance of a separated item can
#' distort every item's quasi-standard error; flagging only the separated
#' item(s) would not be conservative.
#'
#' @references
#' Firth, D., & de Menezes, R. X. (2004). Quasi-variances. *Biometrika*,
#' 91(1), 65–80.
#'
#' @examples
#' set.seed(1)
#' data <- data.frame(item = rep(LETTERS[1:5], 10), period = rep(1:10, each = 5))
#' data$score <- rep(c(3, 2.25, 1.5, 0.75, 0), 10) + rnorm(50)
#' fit <- bt_fit(bt_win_matrix(data))
#' summary(fit)
#'
#' @importFrom stats qnorm qt
#' @export
summary.btfit <- function(object, level = 0.95, type = NULL, ...) {
  if (!inherits(object, "btfit")) {
    stop("`object` must be a `btfit` object returned by `bt_fit()`.", call. = FALSE)
  }
  if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 1) {
    stop("`level` must be a single number strictly between 0 and 1.", call. = FALSE)
  }
  
  inf   <- .bt_inference(object, type)
  ab    <- object$abilities
  items <- names(ab)
  
  est <- setdiff(items, inf$non_estimable)
  se  <- stats::setNames(rep(NA_real_, length(items)), items)
  if (length(est) >= 2) {
    se[est] <- .bt_quasi_se(inf$vcov_full[est, est, drop = FALSE])
  }
  q <- .bt_quantile(level, inf$df)
  
  reliable <- (length(object$separation_items) == 0) &
    !(items %in% inf$non_estimable)
  
  tbl <- data.frame(
    item     = items,
    ability  = as.numeric(ab),
    se       = as.numeric(se),
    ci_low   = as.numeric(ab - q * se),
    ci_high  = as.numeric(ab + q * se),
    reliable = reliable,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  tbl <- tbl[order(-tbl$ability), ]
  rownames(tbl) <- NULL
  
  structure(
    list(table = tbl, level = level, reference = object$reference,
         type = inf$type, df = inf$df, n_clusters = inf$n_clusters,
         effective_clusters = inf$effective_clusters,
         non_estimable = inf$non_estimable,
         separation_items = object$separation_items),
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
  cat("  Reference item:", x$reference, "(ability = 0)\n")
  if (x$type == "cluster") {
    cat(sprintf("  Standard errors: cluster-robust, %d clusters (effective: %.1f)\n",
                x$n_clusters, x$effective_clusters))
    cat(sprintf("  Intervals: t with %d df, %.0f%% level, quasi-SE\n\n",
                as.integer(x$df), x$level * 100))
  } else {
    cat("  Standard errors: model-based (assume independent comparisons)\n")
    cat(sprintf("  Intervals: normal, %.0f%% level, quasi-SE\n\n", x$level * 100))
  }
  
  tbl <- x$table
  num <- c("ability", "se", "ci_low", "ci_high")
  tbl[num] <- lapply(tbl[num], round, digits)
  print(tbl, row.names = FALSE)
  
  if (length(x$non_estimable) > 0) {
    cat(
      "\nNote: item(s) present in a single cluster, whose uncertainty cannot",
      "be estimated\n(SE/CI = NA, see `?vcov.btfit`):",
      paste(x$non_estimable, collapse = ", "), "\n"
    )
  }
  if (length(x$separation_items) > 0) {
    cat(
      "\nWarning: this fit has quasi-complete separation, so no SE/CI in",
      "this table is a\nmeaningful measure of uncertainty (see `?bt_fit`).\n"
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
#' @param type Standard error type, passed to [vcov.btfit()].
#' @param ... Further arguments (ignored).
#'
#' @return A numeric matrix with one row per item and two columns giving
#'   the lower and upper limits of the comparison intervals described in
#'   [summary.btfit()] Details.
#'
#' @seealso [summary.btfit()] for the full table including standard errors
#'   and the `reliable` flag.
#'
#' @export
confint.btfit <- function(object, parm, level = 0.95, type = NULL, ...) {
  if (!inherits(object, "btfit")) {
    stop("`object` must be a `btfit` object returned by `bt_fit()`.", call. = FALSE)
  }
  s   <- summary(object, level = level, type = type)
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
#' Caterpillar plot of estimated abilities with comparison intervals (see
#' [summary.btfit()]), one row per item, ordered from weakest (bottom) to
#' strongest (top).
#'
#' @param x A `btfit` object as returned by [bt_fit()].
#' @param level Confidence level for the intervals. Default `0.95`.
#' @param type Standard error type, passed to [vcov.btfit()].
#' @param ... Further arguments passed to [graphics::plot()].
#'
#' @return Invisibly returns the `summary.btfit` table used to draw the plot.
#'
#' @details
#' Items flagged `reliable = FALSE` (see [summary.btfit()]) are drawn in
#' grey, since their intervals are not a meaningful measure of uncertainty.
#' Intervals are based on quasi-standard errors, so every item, including
#' the reference, has one. Two items whose intervals do not overlap differ
#' approximately at the chosen level (without multiplicity adjustment); use
#' [bt_significance_matrix()] for adjusted pairwise tests.
#'
#' @examples
#' set.seed(1)
#' data <- data.frame(item = rep(LETTERS[1:5], 10), period = rep(1:10, each = 5))
#' data$score <- rep(c(3, 2.25, 1.5, 0.75, 0), 10) + rnorm(50)
#' fit <- bt_fit(bt_win_matrix(data))
#' plot(fit)
#'
#' @importFrom graphics plot segments axis abline
#' @export
plot.btfit <- function(x, level = 0.95, type = NULL, ...) {
  if (!inherits(x, "btfit")) {
    stop("`x` must be a `btfit` object returned by `bt_fit()`.", call. = FALSE)
  }
  
  tbl <- summary(x, level = level, type = type)$table
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
