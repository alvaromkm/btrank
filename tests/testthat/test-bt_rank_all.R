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
  # The wrapper additionally attaches the fit as attribute "fit"
  expect_identical(rk_manual, rk_wrapper[, names(rk_wrapper)])
  expect_equal(attr(rk_wrapper, "fit")$abilities, fit$abilities)
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

test_that("bt_rank_all with top_n re-fits on subset, not just truncates", {
  # With enough items and clear hierarchy, re-fitting on subset
  # produces different ability values than truncating the full model
  datos_dom <- data.frame(
    item   = c("Top", "Mid", "Bot", "Top", "Mid", "Bot", "Top", "Mid", "Bot"),
    period = c(2021,  2021,  2021,  2022,  2022,  2022,  2023,  2023,  2023),
    score  = c(95,    60,    30,    92,    58,    28,    97,    62,    25)
  )

  # Two-pass result from bt_rank_all
  rk_two_pass <- bt_rank_all(datos_dom, score_col = "score", top_n = 2)

  # Single-pass truncation manually
  mat      <- bt_win_matrix(datos_dom, score_col = "score")
  fit_full <- bt_fit(mat)
  rk_trunc <- bt_rank(fit_full, top_n = 2)

  # Items should be the same but abilities will differ
  expect_equal(sort(rk_two_pass$item), sort(rk_trunc$item))
  expect_false(identical(rk_two_pass$ability, rk_trunc$ability))
})


# ── pass-through of bt_win_matrix() arguments and attached fit ─────────────

test_that("bt_rank_all attaches the fitted btfit object", {
  rk  <- bt_rank_all(make_data())
  fit <- attr(rk, "fit")
  expect_s3_class(fit, "btfit")
  expect_equal(fit$abilities, bt_fit(bt_win_matrix(make_data()))$abilities)
})

test_that("with top_n the attached fit is the second-pass subset fit", {
  rk <- bt_rank_all(make_data(), top_n = 3)
  expect_setequal(names(attr(rk, "fit")$abilities), rk$item)
})

test_that("cluster_col is passed through to bt_win_matrix", {
  d <- make_data()
  d$league <- ifelse(d$period <= 5, "early", "late")
  rk <- bt_rank_all(d, cluster_col = "league")
  expect_setequal(unique(attr(rk, "fit")$comparisons$cluster), c("early", "late"))
})

test_that("half_life weights are computed over time_col when supplied", {
  d <- data.frame(
    item   = c("A", "B", "A", "B", "A", "B"),
    match  = c(1, 1, 2, 2, 3, 3),
    season = c(2020, 2020, 2020, 2020, 2021, 2021),
    goals  = c(2, 1, 0, 3, 1, 0)
  )
  rk  <- bt_rank_all(d, period_col = "match", score_col = "goals",
                     time_col = "season", half_life = 1)
  cmp <- attr(rk, "fit")$comparisons
  expect_equal(cmp$weight, c(0.5, 0.5, 1))
})

test_that("absent and absent_penalty are passed through", {
  d  <- make_data()
  d  <- d[!(d$item == "E" & d$period == 10), ]
  rk <- bt_rank_all(d, absent = "penalize", absent_penalty = 0.5)
  expect_true(any(attr(rk, "fit")$comparisons$structural))
  manual <- bt_fit(bt_win_matrix(d, absent = "penalize", absent_penalty = 0.5))
  expect_equal(attr(rk, "fit")$abilities, manual$abilities)
})

test_that("absent_penalty with absent = 'ignore' warns as in bt_win_matrix", {
  expect_warning(bt_rank_all(make_data(), absent_penalty = 0.5), "ignored")
})

test_that("bt_rank_all errors when the weighting column is missing", {
  expect_error(bt_rank_all(make_data(), half_life = 1, time_col = "season"),
               "not found")
})
