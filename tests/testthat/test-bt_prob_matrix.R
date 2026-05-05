# tests/testthat/test-bt_prob_matrix.R

data_simple <- data.frame(
  item   = c("A", "B", "C", "A", "B", "C"),
  period = c(2022, 2022, 2022, 2023, 2023, 2023),
  score  = c(80, 65, 50, 75, 85, 60)
)

fit_simple <- bt_fit(bt_win_matrix(data_simple, score_col = "score"))

test_that("bt_prob_matrix returns a square numeric matrix", {
  pm <- bt_prob_matrix(fit_simple)
  expect_true(is.matrix(pm))
  expect_true(is.numeric(pm))
  expect_equal(nrow(pm), ncol(pm))
})

test_that("bt_prob_matrix diagonal is NA", {
  pm <- bt_prob_matrix(fit_simple)
  expect_true(all(is.na(diag(pm))))
})

test_that("bt_prob_matrix probabilities are in (0, 1)", {
  pm <- bt_prob_matrix(fit_simple)
  off_diag <- pm[!is.na(pm)]
  expect_true(all(off_diag > 0))
  expect_true(all(off_diag < 1))
})

test_that("bt_prob_matrix satisfies symmetry: P(i,j) + P(j,i) = 1", {
  pm <- bt_prob_matrix(fit_simple)
  items <- rownames(pm)
  for (i in items) {
    for (j in items) {
      if (i != j) {
        expect_equal(pm[i, j] + pm[j, i], 1, tolerance = 1e-10)
      }
    }
  }
})

test_that("bt_prob_matrix order_by ability returns strongest item first", {
  pm <- bt_prob_matrix(fit_simple, order_by = "ability")
  rk <- bt_rank(fit_simple)
  expect_equal(rownames(pm)[1], rk$item[1])
})

test_that("bt_prob_matrix order_by name returns alphabetical order", {
  pm <- bt_prob_matrix(fit_simple, order_by = "name")
  expect_equal(rownames(pm), sort(rownames(pm)))
})

test_that("bt_prob_matrix errors on non-btfit input", {
  expect_error(bt_prob_matrix("not a fit"), "`fit` must be a `btfit` object")
})

test_that("bt_prob_matrix errors on invalid order_by", {
  expect_error(bt_prob_matrix(fit_simple, order_by = "invalid"),
               "should be one of")
})
