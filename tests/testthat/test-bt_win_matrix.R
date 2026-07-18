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

# ── absent handling ─────────────────────────────────────────────────────────

# C is absent (no row) in period 2. Note C DOES have real comparisons in
# period 1 (beats/loses to A and B for real) — the expected values below
# are deliberately decomposed as real + structural so the arithmetic is
# auditable, not asserted as opaque constants.
data_absent <- data.frame(
  item   = c("A", "B", "C", "A", "B"),
  period = c(1, 1, 1, 2, 2),
  score  = c(80, 65, 50, 75, 85)
)

test_that("absent = 'ignore' (default) is unchanged from current behaviour", {
  mat_default <- bt_win_matrix(data_absent, score_col = "score")
  mat_ignore  <- bt_win_matrix(data_absent, score_col = "score", absent = "ignore")
  expect_equal(mat_default, mat_ignore)
  # C never beats A: real p1 loss (80 > 50), no p2 comparison at all
  expect_equal(mat_default["C", "A"], 0)
})

test_that("absent = 'penalize' adds structural losses on top of real ones", {
  mat <- bt_win_matrix(data_absent, score_col = "score", absent = "penalize")
  # A beats C: 1 real (p1, 80 > 50) + 1 structural (p2, C absent) = 2
  expect_equal(mat["A", "C"], 1 + 1)
  # B beats C: 1 real (p1, 65 > 50) + 1 structural (p2, C absent) = 2
  expect_equal(mat["B", "C"], 1 + 1)
  # C never beats anyone, real or structural
  expect_equal(mat["C", "A"], 0)
  expect_equal(mat["C", "B"], 0)
})

test_that("absent_penalty scales only the structural component", {
  mat <- bt_win_matrix(data_absent, score_col = "score",
                       absent = "penalize", absent_penalty = 0.5)
  # A beats C: 1 real (p1) + 0.5 structural (p2, penalty = 0.5) = 1.5
  expect_equal(mat["A", "C"], 1 + 0.5)
})

test_that("absent_penalty = 0 is numerically equivalent to 'ignore'", {
  mat_zero   <- bt_win_matrix(data_absent, score_col = "score",
                              absent = "penalize", absent_penalty = 0)
  mat_ignore <- bt_win_matrix(data_absent, score_col = "score", absent = "ignore")
  expect_equal(mat_zero, mat_ignore)
})

test_that("structural penalty is scaled by the period's temporal weight", {
  w <- c("1" = 1, "2" = 0.4)
  mat <- bt_win_matrix(data_absent, score_col = "score", weights = w,
                       absent = "penalize", absent_penalty = 0.5)
  # A beats C: 1 real (p1, weight 1) + 0.4 * 0.5 structural (p2) = 1.2
  expect_equal(mat["A", "C"], 1 + 0.4 * 0.5)
})

test_that("penalty applies even with a single present item that period", {
  # Period 2 has only A present (B absent) — the nrow < 2 guard on real
  # comparisons must not also suppress the structural penalty.
  data_one_present <- data.frame(
    item   = c("A", "B", "A"),
    period = c(1, 1, 2),
    score  = c(80, 65, 90)
  )
  mat <- bt_win_matrix(data_one_present, score_col = "score", absent = "penalize")
  # A beats B: 1 real (p1) + 1 structural (p2, B absent) = 2
  expect_equal(mat["A", "B"], 1 + 1)
})

test_that("no structural penalty is created for a period with zero rows", {
  # Period 2 has no data at all for anyone — nobody "wins" a phantom period.
  data_empty_period <- data.frame(
    item   = c("A", "B"),
    period = c(1, 1)
  )
  data_empty_period$score <- c(80, 65)
  mat <- bt_win_matrix(data_empty_period, score_col = "score", absent = "penalize")
  expect_equal(mat["A", "B"], 1)
})

test_that("invalid absent_penalty errors", {
  expect_error(
    bt_win_matrix(data_absent, absent = "penalize", absent_penalty = -1),
    "non-negative"
  )
  expect_error(
    bt_win_matrix(data_absent, absent = "penalize", absent_penalty = c(1, 2)),
    "non-negative"
  )
})

test_that("invalid absent value errors", {
  expect_error(bt_win_matrix(data_absent, absent = "not_a_mode"))
})

test_that("absent_penalty is ignored (with warning) when absent = 'ignore'", {
  expect_warning(
    bt_win_matrix(data_absent, absent = "ignore", absent_penalty = 0.3),
    "ignored"
  )
})

test_that("an item with zero real wins stays flagged in separation_items after adding absence penalties", {
  # Integration check with bt_fit(), not a claim that 'penalize' uniquely
  # causes this separation — C already has zero real wins in p1/p2 under
  # 'ignore' too. This only verifies the penalize path doesn't break the
  # downstream separation-detection pipeline.
  data_chronic <- data.frame(
    item   = c("A", "B", "C", "A", "B", "C", "A", "B"),
    period = c(1, 1, 1, 2, 2, 2, 3, 3),
    score  = c(80, 65, 50, 75, 85, 60, 70, 90)
  )
  mat <- bt_win_matrix(data_chronic, absent = "penalize")
  fit <- suppressWarnings(bt_fit(mat))
  expect_true("C" %in% fit$separation_items)
})