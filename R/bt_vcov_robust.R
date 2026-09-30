# =============================================================================
# Cluster-robust (sandwich) variance and quasi-variances for btfit objects
#
# Internal helpers, not exported. They operate on plain inputs (abilities,
# model-based covariance matrix, and the "comparisons" attribute produced by
# bt_win_matrix()) so they can be tested independently of bt_fit().
#
# Rationale
# ---------
# bt_win_matrix() decomposes every period into all pairwise comparisons. All
# comparisons from one period are functions of a single score vector and are
# not independent, so the model-based covariance (which assumes independent
# binomial trials, and treats period weights as frequency weights) is not a
# valid measure of sampling uncertainty.
#
# The Bradley-Terry fit is treated as a weighted M-estimator solving
#
#     sum_g U_g(lambda) = 0,   U_g = sum_{r in cluster g} w_r (y_r - p_r) x_r,
#
# where x_r = e_{item1} - e_{item2} (reference column removed) and
# p_r = plogis(lambda_{item1} - lambda_{item2}). Clusters are the
# independent units declared in bt_win_matrix() (by default, periods).
# Treating them as independent gives the cluster-robust sandwich
#
#     V = A^{-1} ( sum_g U_g U_g' ) A^{-1} * G / (G - 1),
#
# with A^{-1} the model-based covariance returned by BTm (the weighted
# Fisher information of the aggregated fit is exactly A). Period weights
# enter the meat squared, as appropriate for importance (not frequency)
# weights. The target of inference is the pseudo-true Bradley-Terry
# parameter (White, 1982), which is the relevant estimand when the paired
# decomposition of rankings is not the true data-generating process.
#
# References
# White, H. (1982). Maximum likelihood estimation of misspecified models.
#   Econometrica, 50(1), 1-25.
# Firth, D. & de Menezes, R. X. (2004). Quasi-variances. Biometrika, 91(1),
#   65-80.
# =============================================================================


# Cluster-level score contributions ------------------------------------------
#
# comparisons : data frame from attr(bt_win_matrix(...), "comparisons")
# abilities   : named numeric vector of all abilities (reference = 0)
# param_items : names of the estimated (non-reference) items, in the order of
#               the rows/columns of the model-based covariance matrix
#
# Returns a G x p matrix (one row per cluster, one column per parameter).
# At the MLE, colSums() of the result is (numerically) zero.
.bt_cluster_scores <- function(comparisons, abilities, param_items) {
  items_needed <- unique(c(comparisons$item1, comparisons$item2))
  unknown <- setdiff(items_needed, names(abilities))
  if (length(unknown) > 0) {
    stop("Comparisons involve items without an estimated ability: ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  
  eta <- abilities[comparisons$item1] - abilities[comparisons$item2]
  res <- comparisons$weight * (comparisons$y - stats::plogis(eta))
  
  # Design rows x_r restricted to the estimated parameters
  X <- outer(comparisons$item1, param_items, "==") -
       outer(comparisons$item2, param_items, "==")
  
  U <- rowsum(X * res, group = as.character(comparisons$cluster),
              reorder = FALSE)
  colnames(U) <- param_items
  U
}


# Cluster-robust sandwich ------------------------------------------------------
#
# vcov_model : p x p model-based covariance (A^{-1}), dimnames = param_items
# U          : G x p matrix from .bt_cluster_scores()
.bt_vcov_sandwich <- function(vcov_model, U, small_sample = TRUE) {
  G <- nrow(U)
  if (G < 2) {
    stop("Cluster-robust standard errors require at least 2 clusters.",
         call. = FALSE)
  }
  if (!identical(colnames(U), colnames(vcov_model))) {
    stop("Score and covariance parameters do not match.", call. = FALSE)
  }
  
  V <- vcov_model %*% crossprod(U) %*% vcov_model
  if (small_sample) V <- V * G / (G - 1)
  
  V <- (V + t(V)) / 2   # remove floating-point asymmetry
  dimnames(V) <- dimnames(vcov_model)
  V
}


# Effective number of clusters (Kish-type heuristic) --------------------------
#
# Kish's effective sample size applied to each cluster's total contribution
# to the counts, W_g = sum of row weights: (sum W_g)^2 / sum W_g^2. Equals
# the number of clusters when all carry the same weight; shrinks as weights
# concentrate the information in a few clusters. This adaptation is a
# descriptive diagnostic, not a published reliability criterion for
# cluster-robust variances.
#
# Kish, L. (1965). Survey Sampling. Wiley.
.bt_effective_clusters <- function(comparisons) {
  W <- tapply(comparisons$weight, as.character(comparisons$cluster), sum)
  sum(W)^2 / sum(W^2)
}


# Items whose information comes from fewer than 2 clusters -------------------
#
# At the MLE the score of such an item sums to zero within its single
# cluster, so its row of the meat is exactly zero: the sandwich cannot
# estimate its between-cluster variability (e.g. a team present in a single
# season). Jackknife or cluster bootstrap fail for the same reason (removing
# its only cluster removes the item). Structural rows from
# absent = "penalize" count as presence in a cluster.
.bt_non_estimable_items <- function(comparisons, items) {
  long <- unique(data.frame(
    item    = c(comparisons$item1, comparisons$item2),
    cluster = as.character(c(comparisons$cluster, comparisons$cluster)),
    stringsAsFactors = FALSE
  ))
  n_cl <- table(factor(long$item, levels = items))
  names(n_cl)[n_cl < 2]
}


# Expand a p x p covariance to all items (reference row/column = 0) ---------
.bt_full_vcov <- function(V, all_items) {
  full <- matrix(0, length(all_items), length(all_items),
                 dimnames = list(all_items, all_items))
  full[rownames(V), colnames(V)] <- V
  full
}


# Quasi-standard errors (Firth & de Menezes, 2004) ---------------------------
#
# V_full : covariance for all items, including the reference (zeros)
# Returns a named vector of quasi-SEs, one per item, that does not depend on
# the choice of reference. Quasi-variances can be negative, or not computable
# at all, when the covariance is far from the structure they approximate;
# this happens mostly with very few items or clusters, where the robust
# covariance is itself noisy. Affected items get NA, with a warning.
.bt_quasi_se <- function(V_full) {
  items <- rownames(V_full)
  qvar <- tryCatch(
    suppressWarnings(qvcalc::qvcalc(V_full))$qvframe$quasiVar,
    error = function(e) NULL
  )
  if (is.null(qvar)) {
    warning("Quasi-variances could not be computed for this covariance ",
            "matrix; quasi-SEs are reported as NA. Pairwise tests in ",
            "bt_significance_matrix() are unaffected.", call. = FALSE)
    return(stats::setNames(rep(NA_real_, length(items)), items))
  }
  se <- rep(NA_real_, length(qvar))
  ok <- qvar >= 0
  se[ok] <- sqrt(qvar[ok])
  if (any(!ok)) {
    warning("Negative quasi-variance for item(s): ",
            paste(items[!ok], collapse = ", "),
            "; their quasi-SE is reported as NA. Pairwise tests in ",
            "bt_significance_matrix() are unaffected.", call. = FALSE)
  }
  stats::setNames(se, items)
}
