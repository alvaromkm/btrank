# tests/testthat/test-bt_significance_matrix.R

data_balanced <- data.frame(
  item   = c("A", "B", "C", "A", "B", "C", "A", "B", "C"),
  period = c(2021, 2021, 2021, 2022, 2022, 2022, 2023, 2023, 2023),
  score  = c(80, 70, 60, 65, 85, 55, 75, 60, 90)
)
mat_balanced <- bt_win_matrix(data_balanced, score_col = "score")

data_sep <- data.frame(
  item   = c("A", "B", "C", "A", "B", "C"),
  period = c(2022, 2022, 2022, 2023, 2023, 2023),
  score  = c(80, 65, 50, 75, 85, 60)
)
mat_sep <- bt_win_matrix(data_sep, score_col = "score")

test_that("bt_significance_matrix returns expected structure", {
  fit <- bt_fit(mat_balanced)
  sm <- bt_significance_matrix(fit)
  expect_s3_class(sm, "bt_significance_matrix")
  expect_named(sm, c("p", "significant", "method", "level", "reliable"))
})

test_that("p and significant matrices are symmetric with NA diagonal", {
  fit <- bt_fit(mat_balanced)
  sm <- bt_significance_matrix(fit)
  expect_equal(sm$p, t(sm$p))
  expect_true(all(is.na(diag(sm$p))))
  expect_true(all(is.na(diag(sm$significant))))
})

test_that("p-value vs reference matches summary.btfit's Wald test exactly", {
  fit <- bt_fit(mat_balanced)
  s   <- summary(fit)
  sm  <- bt_significance_matrix(fit, method = "none")
  
  row_A  <- s$table[s$table$item == "A", ]
  z_A    <- row_A$ability / row_A$se
  p_manual <- 2 * stats::pnorm(-abs(z_A))
  
  expect_equal(unname(sm$p["A", fit$reference]), p_manual, tolerance = 1e-8)
})

test_that("p-value for a non-reference pair matches the full covariance formula", {
  fit <- bt_fit(mat_balanced)
  v   <- vcov(fit)
  ab  <- fit$abilities
  
  var_diff <- v["A", "A"] + v["B", "B"] - 2 * v["A", "B"]
  z_manual <- (ab["A"] - ab["B"]) / sqrt(var_diff)
  p_manual <- 2 * stats::pnorm(-abs(z_manual))
  
  sm <- bt_significance_matrix(fit, method = "none")
  expect_equal(unname(sm$p["A", "B"]), unname(p_manual), tolerance = 1e-8)
})

test_that("adjustment methods match stats::p.adjust independently", {
  fit <- bt_fit(mat_balanced)
  sm_raw <- bt_significance_matrix(fit, method = "none")
  raw_pvals <- sm_raw$p[upper.tri(sm_raw$p)]
  
  for (m in c("holm", "bonferroni", "BH")) {
    sm <- bt_significance_matrix(fit, method = m)
    adj_manual <- stats::p.adjust(raw_pvals, method = m)
    adj_pkg    <- sm$p[upper.tri(sm$p)]
    expect_equal(sort(adj_pkg), sort(adj_manual), tolerance = 1e-8)
  }
})

test_that("significant reflects p < alpha", {
  fit <- bt_fit(mat_balanced)
  sm  <- bt_significance_matrix(fit, level = 0.95)
  expect_equal(sm$significant[!is.na(sm$significant)],
               (sm$p < 0.05)[!is.na(sm$p)])
})

test_that("reliable is TRUE when there is no separation", {
  fit <- bt_fit(mat_balanced)
  sm  <- bt_significance_matrix(fit)
  expect_true(sm$reliable)
})

test_that("reliable is FALSE for the whole matrix when any item is separated", {
  fit <- suppressWarnings(bt_fit(mat_sep))
  sm  <- bt_significance_matrix(fit)
  expect_false(sm$reliable)
  # single fit-wide flag, not per-cell - matches summary.btfit's design
  expect_type(sm$reliable, "logical")
  expect_length(sm$reliable, 1)
})

test_that("bt_significance_matrix errors on invalid input", {
  fit <- bt_fit(mat_balanced)
  expect_error(bt_significance_matrix(list()), "must be a `btfit` object")
  expect_error(bt_significance_matrix(fit, level = 0), "strictly between 0 and 1")
  expect_error(bt_significance_matrix(fit, method = "not_a_method"))
})

test_that("print.bt_significance_matrix runs without error and warns when unreliable", {
  fit_ok  <- bt_fit(mat_balanced)
  fit_sep <- suppressWarnings(bt_fit(mat_sep))
  
  out_ok  <- capture.output(print(bt_significance_matrix(fit_ok)))
  out_sep <- capture.output(print(bt_significance_matrix(fit_sep)))
  
  expect_false(any(grepl("Warning:", out_ok)))
  expect_true(any(grepl("quasi-complete separation", out_sep)))
})