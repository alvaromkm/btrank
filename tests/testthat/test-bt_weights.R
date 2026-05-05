# tests/testthat/test-bt_weights.R

test_that("bt_weights returns a named numeric vector", {
  w <- bt_weights(2019:2023, half_life = 2)
  expect_true(is.numeric(w))
  expect_named(w)
})

test_that("bt_weights most recent period has weight 1", {
  w <- bt_weights(2019:2023, half_life = 2)
  expect_equal(w["2023"], c("2023" = 1))
})

test_that("bt_weights follow exponential decay formula", {
  w <- bt_weights(2019:2023, half_life = 2)
  for (t in 2019:2023) {
    esperado <- (0.5) ^ ((2023 - t) / 2)
    expect_equal(w[as.character(t)], c(setNames(esperado, as.character(t))),
                 tolerance = 1e-10)
  }
})

test_that("bt_weights shorter half_life gives lower weight to older periods", {
  w_aggressive <- bt_weights(2019:2023, half_life = 1)
  w_smooth     <- bt_weights(2019:2023, half_life = 3)
  expect_true(w_aggressive["2019"] < w_smooth["2019"])
})

test_that("bt_weights names are character strings", {
  w <- bt_weights(2019:2023, half_life = 2)
  expect_true(all(is.character(names(w))))
})

test_that("bt_weights works with character period labels", {
  w <- bt_weights(c("2021-22", "2022-23", "2023-24"), half_life = 1)
  expect_equal(w["2023-24"], c("2023-24" = 1))
  expect_equal(w["2022-23"], c("2022-23" = 0.5), tolerance = 1e-10)
})

test_that("bt_weights errors on empty periods", {
  expect_error(bt_weights(c(), half_life = 2), "non-empty vector")
})

test_that("bt_weights errors on non-positive half_life", {
  expect_error(bt_weights(2019:2023, half_life = 0),  "single positive number")
  expect_error(bt_weights(2019:2023, half_life = -1), "single positive number")
})

test_that("bt_weights errors on non-scalar half_life", {
  expect_error(bt_weights(2019:2023, half_life = c(1, 2)), "single positive number")
})
