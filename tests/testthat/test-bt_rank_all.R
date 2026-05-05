# tests/testthat/test-bt_rank_all.R

data_simple <- data.frame(
  item   = c("A", "B", "C", "A", "B", "C"),
  period = c(2022, 2022, 2022, 2023, 2023, 2023),
  score  = c(80, 65, 50, 75, 85, 60)
)

test_that("bt_rank_all returns a data frame", {
  rk <- bt_rank_all(data_simple, score_col = "score")
  expect_s3_class(rk, "data.frame")
})

test_that("bt_rank_all returns correct columns", {
  rk <- bt_rank_all(data_simple, score_col = "score")
  expect_named(rk, c("rank", "item", "ability", "ability_norm"))
})

test_that("bt_rank_all result is identical to manual pipeline", {
  w   <- bt_weights(periods = c(2022, 2023), half_life = 1)
  mat <- bt_win_matrix(data_simple, score_col = "score", weights = w)
  fit <- bt_fit(mat)
  rk_manual  <- bt_rank(fit)
  rk_wrapper <- bt_rank_all(data_simple, score_col = "score", half_life = 1)
  expect_identical(rk_manual, rk_wrapper)
})

test_that("bt_rank_all top_n limits output", {
  rk <- bt_rank_all(data_simple, score_col = "score", top_n = 2)
  expect_equal(nrow(rk), 2)
})

test_that("bt_rank_all ranking is sorted by descending ability", {
  rk <- bt_rank_all(data_simple, score_col = "score")
  expect_true(all(diff(rk$ability) <= 0))
})

test_that("bt_rank_all works with custom column names", {
  data_custom <- data.frame(
    vino  = c("Rioja", "Ribera", "Rioja", "Ribera"),
    cata  = c(2022, 2022, 2023, 2023),
    nota  = c(92, 88, 89, 91)
  )
  rk <- bt_rank_all(data_custom,
                    item_col   = "vino",
                    period_col = "cata",
                    score_col  = "nota")
  expect_s3_class(rk, "data.frame")
  expect_true(all(c("Rioja", "Ribera") %in% rk$item))
})

test_that("bt_rank_all works with higher_is_better = FALSE", {
  data_time <- data.frame(
    item   = c("Ana", "Ben", "Cara", "Ana", "Ben", "Cara"),
    period = c(2022, 2022, 2022, 2023, 2023, 2023),
    score  = c(12.3, 14.1, 11.8, 12.0, 13.5, 11.5)
  )
  rk <- bt_rank_all(data_time, score_col = "score",
                    higher_is_better = FALSE)
  expect_equal(rk$item[1], "Cara")
})

test_that("bt_rank_all errors on non-data-frame input", {
  expect_error(bt_rank_all("not a df"), "`data` must be a data frame")
})

test_that("bt_rank_all errors on invalid half_life", {
  expect_error(bt_rank_all(data_simple, score_col = "score", half_life = -1),
               "single positive number")
  expect_error(bt_rank_all(data_simple, score_col = "score", half_life = 0),
               "single positive number")
})
