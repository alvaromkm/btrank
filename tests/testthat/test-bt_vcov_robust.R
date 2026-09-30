# tests/testthat/test-bt_vcov_robust.R
#
# Internal helpers are tested on plain glm fits of the disaggregated
# comparisons, so these tests do not depend on how bt_fit() stores things.

# make_data() is defined in helper-data.R

# Weighted BT fit on the comparison rows with a chosen reference item.
fit_rows <- function(cmp, ref) {
  items <- sort(unique(c(cmp$item1, cmp$item2)))
  param <- setdiff(items, ref)
  X <- outer(cmp$item1, param, "==") - outer(cmp$item2, param, "==")
  colnames(X) <- param
  resp <- cbind(cmp$weight * cmp$y, cmp$weight * (1 - cmp$y))
  # Tight convergence: glm() stores working residuals from the last IRLS
  # iteration, so sandwich::estfun() is only accurate to the convergence
  # tolerance. With the default epsilon the comparison below fails at ~1e-4.
  m <- suppressWarnings(stats::glm(
    resp ~ X - 1, family = stats::binomial,
    control = stats::glm.control(epsilon = 1e-14, maxit = 100)
  ))
  ab <- c(stats::setNames(0, ref), stats::setNames(stats::coef(m), param))
  V  <- stats::vcov(m); dimnames(V) <- list(param, param)
  list(model = m, abilities = ab[items], vcov = V, param = param,
       items = items, X = X)
}

robust_full <- function(cmp, ref) {
  f <- fit_rows(cmp, ref)
  U <- .bt_cluster_scores(cmp, f$abilities, f$param)
  .bt_full_vcov(.bt_vcov_sandwich(f$vcov, U), f$items)
}

var_diff <- function(V, i, j) V[i, i] + V[j, j] - 2 * V[i, j]

test_that("period scores sum to zero at the MLE", {
  cmp <- attr(bt_win_matrix(make_data()), "comparisons")
  f   <- fit_rows(cmp, "E")
  U   <- .bt_cluster_scores(cmp, f$abilities, f$param)
  expect_equal(nrow(U), 10)
  expect_equal(unname(colSums(U)), rep(0, length(f$param)), tolerance = 1e-6)
})

test_that("sandwich matches sandwich::vcovCL clustered by period", {
  skip_if_not_installed("sandwich")
  w   <- bt_weights(1:10, half_life = 4)
  cmp <- attr(bt_win_matrix(make_data(), weights = w), "comparisons")
  f   <- fit_rows(cmp, "E")
  U   <- .bt_cluster_scores(cmp, f$abilities, f$param)
  ours <- .bt_vcov_sandwich(f$vcov, U, small_sample = TRUE)
  ref  <- sandwich::vcovCL(f$model, cluster = cmp$period, type = "HC0",
                           cadjust = TRUE)
  expect_equal(unname(ours), unname(ref), tolerance = 1e-6)
})

test_that("variance of ability differences does not depend on the reference", {
  cmp <- attr(bt_win_matrix(make_data()), "comparisons")
  V_E <- robust_full(cmp, "E")
  V_A <- robust_full(cmp, "A")
  for (pr in list(c("A", "B"), c("B", "D"), c("C", "E"))) {
    expect_equal(var_diff(V_E, pr[1], pr[2]), var_diff(V_A, pr[1], pr[2]),
                 tolerance = 1e-6)
  }
})

test_that("quasi-SEs do not depend on the reference and cover all items", {
  cmp  <- attr(bt_win_matrix(make_data()), "comparisons")
  qs_E <- .bt_quasi_se(robust_full(cmp, "E"))
  qs_A <- .bt_quasi_se(robust_full(cmp, "A"))
  expect_equal(qs_E, qs_A, tolerance = 1e-5)
  expect_true(all(is.finite(qs_E)))
  expect_named(qs_E, LETTERS[1:5])
})

test_that("effective number of clusters equals G with equal weights and shrinks with decay", {
  cmp <- attr(bt_win_matrix(make_data()), "comparisons")
  expect_equal(.bt_effective_clusters(cmp), 10)
  w     <- bt_weights(1:10, half_life = 1)
  cmp_w <- attr(bt_win_matrix(make_data(), weights = w), "comparisons")
  expect_lt(.bt_effective_clusters(cmp_w), 5)
})

test_that("sandwich requires at least two clusters", {
  cmp <- attr(bt_win_matrix(make_data(n_periods = 1)), "comparisons")
  f   <- fit_rows(cmp, "E")
  U   <- .bt_cluster_scores(cmp, f$abilities, f$param)
  expect_error(.bt_vcov_sandwich(f$vcov, U), "at least 2 clusters")
})

test_that("scores are aggregated by cluster, not by period", {
  d <- make_data()
  d$league <- ifelse(d$period <= 5, "early", "late")
  cmp <- attr(bt_win_matrix(d, cluster_col = "league"), "comparisons")
  f   <- fit_rows(cmp, "E")
  U   <- .bt_cluster_scores(cmp, f$abilities, f$param)
  expect_equal(sort(rownames(U)), c("early", "late"))
})

test_that("scores error on items without an estimated ability", {
  cmp <- attr(bt_win_matrix(make_data()), "comparisons")
  expect_error(.bt_cluster_scores(cmp, c(A = 0, B = 1), "B"),
               "without an estimated ability")
})

test_that("items present in a single cluster are flagged as non-estimable", {
  d <- make_data()
  d <- rbind(d, data.frame(item = "Z", period = 1, score = 1.5))
  cmp <- attr(bt_win_matrix(d), "comparisons")
  expect_equal(.bt_non_estimable_items(cmp, c(LETTERS[1:5], "Z")), "Z")
})

test_that("a single-cluster item has an exactly zero meat row", {
  d <- make_data()
  d <- rbind(d, data.frame(item = "Z", period = 1, score = 1.5))
  cmp <- attr(bt_win_matrix(d), "comparisons")
  f   <- fit_rows(cmp, "E")
  U   <- .bt_cluster_scores(cmp, f$abilities, f$param)
  expect_equal(unname(U[, "Z"]), rep(0, nrow(U)), tolerance = 1e-8)
})
