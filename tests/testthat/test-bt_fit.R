# tests/testthat/test-bt_fit.R

data_simple <- data.frame(
  item   = c("A", "B", "C", "A", "B", "C"),
  period = c(2022, 2022, 2022, 2023, 2023, 2023),
  score  = c(80, 65, 50, 75, 85, 60)
)

mat_simple <- bt_win_matrix(data_simple, score_col = "score")

test_that("bt_fit returns a btfit object", {
  fit <- bt_fit(mat_simple)
  expect_s3_class(fit, "btfit")
})

test_that("bt_fit returns required components", {
  fit <- bt_fit(mat_simple)
  expect_named(fit, c("abilities", "reference", "model", "items"))
})

test_that("bt_fit reference item has ability 0", {
  fit <- bt_fit(mat_simple)
  expect_equal(fit$abilities[[fit$reference]], 0)
})

test_that("bt_fit all abilities are non-negative", {
  fit <- bt_fit(mat_simple)
  expect_true(all(fit$abilities >= 0))
})

test_that("bt_fit abilities named vector matches items", {
  fit <- bt_fit(mat_simple)
  expect_equal(sort(names(fit$abilities)), sort(fit$items))
})

test_that("bt_fit drops items with no comparisons with warning", {
  mat_orphan <- matrix(
    c(0, 2, 0,
      1, 0, 0,
      0, 0, 0),
    nrow = 3, ncol = 3,
    dimnames = list(c("X", "Y", "Z"), c("X", "Y", "Z"))
  )
  expect_warning(bt_fit(mat_orphan), "dropped")
})

test_that("bt_fit errors on non-matrix input", {
  expect_error(bt_fit("not a matrix"), "`win_matrix` must be a numeric matrix")
})

test_that("bt_fit errors on non-square matrix", {
  expect_error(bt_fit(matrix(1:6, 2, 3)), "`win_matrix` must be square")
})

test_that("bt_fit errors on matrix without names", {
  expect_error(bt_fit(matrix(c(0,1,1,0), 2, 2)),
               "must have row and column names")
})
