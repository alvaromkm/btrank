# tests/testthat/test-bt_rank.R

data_simple <- data.frame(
  item   = c("A", "B", "C", "A", "B", "C"),
  period = c(2022, 2022, 2022, 2023, 2023, 2023),
  score  = c(80, 65, 50, 75, 85, 60)
)

fit_simple <- bt_fit(bt_win_matrix(data_simple, score_col = "score"))

test_that("bt_rank returns a data frame", {
  rk <- bt_rank(fit_simple)
  expect_s3_class(rk, "data.frame")
})

test_that("bt_rank returns correct columns", {
  rk <- bt_rank(fit_simple)
  expect_named(rk, c("rank", "item", "ability", "ability_norm"))
})

test_that("bt_rank returns all items by default", {
  rk <- bt_rank(fit_simple)
  expect_equal(nrow(rk), length(fit_simple$abilities))
})

test_that("bt_rank is sorted by descending ability", {
  rk <- bt_rank(fit_simple)
  expect_true(all(diff(rk$ability) <= 0))
})

test_that("bt_rank rank column is sequential from 1", {
  rk <- bt_rank(fit_simple)
  expect_equal(rk$rank, seq_len(nrow(rk)))
})

test_that("bt_rank reference item has ability 0", {
  rk <- bt_rank(fit_simple)
  ref_ability <- rk$ability[rk$item == fit_simple$reference]
  expect_equal(ref_ability, 0)
})

test_that("bt_rank ability_norm is in [0, 1]", {
  rk <- bt_rank(fit_simple)
  expect_true(all(rk$ability_norm >= 0))
  expect_true(all(rk$ability_norm <= 1))
  expect_equal(max(rk$ability_norm), 1)
  expect_equal(min(rk$ability_norm), 0)
})

test_that("bt_rank top_n limits output correctly", {
  rk <- bt_rank(fit_simple, top_n = 2)
  expect_equal(nrow(rk), 2)
  expect_equal(rk$rank, c(1, 2))
})

test_that("bt_rank warns when top_n exceeds number of items", {
  expect_warning(bt_rank(fit_simple, top_n = 100), "exceeds")
})

test_that("bt_rank errors on non-btfit input", {
  expect_error(bt_rank("not a fit"), "`fit` must be a `btfit` object")
})

test_that("bt_rank errors on invalid top_n", {
  expect_error(bt_rank(fit_simple, top_n = -1), "single positive integer")
  expect_error(bt_rank(fit_simple, top_n = 0),  "single positive integer")
})
