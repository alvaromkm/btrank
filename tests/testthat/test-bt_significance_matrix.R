# tests/testthat/test-bt_significance_matrix.R

# Robust fixture: 5 items x 10 periods, no separation (see helper-data.R)
mat_rob <- bt_win_matrix(make_data())

data_sep <- data.frame(
  item   = c("A", "B", "C", "A", "B", "C"),
  period = c(2022, 2022, 2022, 2023, 2023, 2023),
  score  = c(80, 65, 50, 75, 85, 60)
)
mat_sep <- bt_win_matrix(data_sep, score_col = "score")

full_vcov <- function(fit, type = NULL) {
  .bt_inference(fit, type)$vcov_full
}

test_that("bt_significance_matrix returns expected structure", {
  sm <- bt_significance_matrix(bt_fit(mat_rob))
  expect_s3_class(sm, "bt_significance_matrix")
  expect_named(sm, c("p", "significant", "method", "level", "type", "df",
                     "non_estimable", "reliable"))
  expect_equal(sm$type, "cluster")
  expect_equal(sm$df, 9)
})

test_that("p and significant matrices are symmetric with NA diagonal", {
  sm <- bt_significance_matrix(bt_fit(mat_rob))
  expect_equal(sm$p, t(sm$p))
  expect_true(all(is.na(diag(sm$p))))
  expect_true(all(is.na(diag(sm$significant))))
})

test_that("cluster-robust p-value for a pair matches a manual t test", {
  fit <- bt_fit(mat_rob)
  V   <- full_vcov(fit)
  ab  <- fit$abilities
  var_d <- V["A", "A"] + V["B", "B"] - 2 * V["A", "B"]
  t_ab  <- (ab[["A"]] - ab[["B"]]) / sqrt(var_d)
  p_manual <- 2 * stats::pt(-abs(t_ab), df = 9)

  sm <- bt_significance_matrix(fit, method = "none")
  expect_equal(sm$p["A", "B"], p_manual, tolerance = 1e-8)
})

test_that("p-value against the reference uses Var(ability_i) exactly", {
  fit <- bt_fit(mat_rob)
  v   <- vcov(fit)
  i   <- setdiff(fit$items, fit$reference)[1]
  t_i <- fit$abilities[[i]] / sqrt(v[i, i])
  p_manual <- 2 * stats::pt(-abs(t_i), df = 9)
  sm <- bt_significance_matrix(fit, method = "none")
  expect_equal(sm$p[i, fit$reference], p_manual, tolerance = 1e-8)
})

test_that("model-based p-values use the normal reference", {
  fit <- bt_fit(mat_rob)
  V   <- suppressWarnings(full_vcov(fit, "model"))
  ab  <- fit$abilities
  z   <- (ab[["A"]] - ab[["B"]]) / sqrt(V["A", "A"] + V["B", "B"] - 2 * V["A", "B"])
  sm  <- suppressWarnings(bt_significance_matrix(fit, method = "none", type = "model"))
  expect_equal(sm$df, Inf)
  expect_equal(sm$p["A", "B"], 2 * stats::pnorm(-abs(z)), tolerance = 1e-8)
})

test_that("adjustment methods match stats::p.adjust independently", {
  fit <- bt_fit(mat_rob)
  raw <- bt_significance_matrix(fit, method = "none")$p
  raw_pvals <- raw[upper.tri(raw)]
  for (m in c("holm", "bonferroni", "BH")) {
    sm <- bt_significance_matrix(fit, method = m)
    expect_equal(sort(sm$p[upper.tri(sm$p)]),
                 sort(stats::p.adjust(raw_pvals, method = m)), tolerance = 1e-8)
  }
})

test_that("significant reflects p < alpha", {
  sm <- bt_significance_matrix(bt_fit(mat_rob), level = 0.95)
  expect_equal(sm$significant[!is.na(sm$significant)],
               (sm$p < 0.05)[!is.na(sm$p)])
})

test_that("pairs with a single-cluster item are NA and excluded from the adjustment", {
  d   <- rbind(make_data(), data.frame(item = "Z", period = 3, score = 1.5))
  fit <- bt_fit(bt_win_matrix(d))
  sm_raw <- bt_significance_matrix(fit, method = "none")
  sm_b   <- bt_significance_matrix(fit, method = "bonferroni")
  expect_equal(sm_b$non_estimable, "Z")
  expect_true(all(is.na(sm_b$p["Z", ])))
  n_valid <- choose(5, 2)
  expect_equal(sm_b$p["A", "B"], min(1, sm_raw$p["A", "B"] * n_valid))
})

test_that("reliable is TRUE when there is no separation", {
  expect_true(bt_significance_matrix(bt_fit(mat_rob))$reliable)
})

test_that("reliable is FALSE for the whole matrix when any item is separated", {
  sm <- suppressWarnings(bt_significance_matrix(suppressWarnings(bt_fit(mat_sep))))
  expect_false(sm$reliable)
  expect_length(sm$reliable, 1)
})

test_that("bt_significance_matrix errors on invalid input", {
  fit <- bt_fit(mat_rob)
  expect_error(bt_significance_matrix(list()), "must be a `btfit` object")
  expect_error(bt_significance_matrix(fit, level = 0), "strictly between 0 and 1")
  expect_error(bt_significance_matrix(fit, method = "not_a_method"))
  expect_error(bt_significance_matrix(fit, type = "sandwich"), "must be NULL")
})

test_that("print.bt_significance_matrix reports SE type and warns when unreliable", {
  out_ok  <- capture.output(print(bt_significance_matrix(bt_fit(mat_rob))))
  out_sep <- capture.output(print(suppressWarnings(
    bt_significance_matrix(suppressWarnings(bt_fit(mat_sep))))))
  expect_true(any(grepl("cluster-robust; t reference with 9 df", out_ok)))
  expect_false(any(grepl("Warning:", out_ok)))
  expect_true(any(grepl("quasi-complete separation", out_sep)))
})
