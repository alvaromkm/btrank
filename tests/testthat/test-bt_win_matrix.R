# tests/testthat/test-bt_win_matrix.R

data_simple <- data.frame(
  item   = c("A", "B", "C", "A", "B", "C"),
  period = c(2022, 2022, 2022, 2023, 2023, 2023),
  score  = c(80, 65, 50, 75, 85, 60)
)

test_that("bt_win_matrix returns a square numeric matrix", {
  mat <- bt_win_matrix(data_simple, score_col = "score")
  expect_true(is.matrix(mat))
  expect_true(is.numeric(mat))
  expect_equal(nrow(mat), ncol(mat))
})

test_that("bt_win_matrix diagonal is zero", {
  mat <- bt_win_matrix(data_simple, score_col = "score")
  expect_true(all(diag(mat) == 0))
})

test_that("bt_win_matrix row and column names match items", {
  mat <- bt_win_matrix(data_simple, score_col = "score")
  expect_equal(sort(rownames(mat)), sort(unique(data_simple$item)))
  expect_equal(sort(colnames(mat)), sort(unique(data_simple$item)))
})

test_that("bt_win_matrix win counts are symmetric complements", {
  mat <- bt_win_matrix(data_simple, score_col = "score")
  items <- rownames(mat)
  # data_simple has 2 periods with no ties, so every pair sums to 2
  for (i in items) {
    for (j in items) {
      if (i != j) {
        expect_equal(mat[i, j] + mat[j, i], 2)
      }
    }
  }
})

test_that("bt_win_matrix handles ties with 0.5 split", {
  data_tie <- data.frame(
    item   = c("X", "Y", "Z"),
    period = rep("p1", 3),
    score  = c(100, 100, 80)
  )
  mat <- bt_win_matrix(data_tie, score_col = "score")
  expect_equal(mat["X", "Y"], 0.5)
  expect_equal(mat["Y", "X"], 0.5)
  expect_equal(mat["X", "Z"], 1)
  expect_equal(mat["Y", "Z"], 1)
})

test_that("bt_win_matrix respects higher_is_better = FALSE", {
  data_time <- data.frame(
    item   = c("Ana", "Ben", "Cara"),
    period = rep("r1", 3),
    score  = c(12.3, 14.1, 11.8)
  )
  mat <- bt_win_matrix(data_time, score_col = "score",
                       higher_is_better = FALSE)
  # Cara (11.8) beats Ana (12.3) beats Ben (14.1)
  expect_equal(mat["Cara", "Ana"], 1)
  expect_equal(mat["Ana", "Ben"], 1)
  expect_equal(mat["Cara", "Ben"], 1)
})

test_that("bt_win_matrix errors on non-data-frame input", {
  expect_error(bt_win_matrix("not a df"), "`data` must be a data frame")
})

test_that("bt_win_matrix errors on missing column", {
  expect_error(
    bt_win_matrix(data_simple, score_col = "nonexistent"),
    "Column\\(s\\) not found"
  )
})

test_that("bt_win_matrix errors with fewer than 2 items", {
  data_one <- data.frame(item = "A", period = 2022, score = 80)
  expect_error(bt_win_matrix(data_one, score_col = "score"),
               "at least 2 distinct items")
})
