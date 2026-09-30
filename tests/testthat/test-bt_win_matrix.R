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

# ── "comparisons" attribute (period-level record for robust variance) ──────

# Rebuild the win matrix from the comparison rows, independently of the
# aggregation code inside bt_win_matrix().
rebuild_from_comparisons <- function(cmp, items) {
  m <- matrix(0, length(items), length(items), dimnames = list(items, items))
  for (r in seq_len(nrow(cmp))) {
    i <- cmp$item1[r]; j <- cmp$item2[r]
    m[i, j] <- m[i, j] + cmp$weight[r] * cmp$y[r]
    m[j, i] <- m[j, i] + cmp$weight[r] * (1 - cmp$y[r])
  }
  m
}

test_that("win matrix carries a 'comparisons' attribute with the documented columns", {
  mat <- bt_win_matrix(data_simple, score_col = "score")
  cmp <- attr(mat, "comparisons")
  expect_s3_class(cmp, "data.frame")
  expect_named(cmp, c("item1", "item2", "y", "period", "cluster", "weight",
                      "structural"))
})

test_that("each period contributes choose(n_p, 2) real comparisons", {
  mat <- bt_win_matrix(data_simple, score_col = "score")
  cmp <- attr(mat, "comparisons")
  expect_equal(as.vector(table(cmp$period)), c(3, 3))  # choose(3, 2) per period
  expect_false(any(cmp$structural))
})

test_that("the matrix is exactly the weighted aggregation of 'comparisons'", {
  w <- c("1" = 1, "2" = 0.4)
  mat <- bt_win_matrix(data_absent, score_col = "score", weights = w,
                       absent = "penalize", absent_penalty = 0.5)
  cmp <- attr(mat, "comparisons")
  expect_equal(unclass(mat)[, ], rebuild_from_comparisons(cmp, rownames(mat)),
               ignore_attr = TRUE)
})

test_that("ties are recorded with y = 0.5", {
  data_tie <- data.frame(item = c("X", "Y", "Z"), period = "p1",
                         score = c(100, 100, 80))
  cmp <- attr(bt_win_matrix(data_tie, score_col = "score"), "comparisons")
  tie_row <- cmp[cmp$item1 %in% c("X", "Y") & cmp$item2 %in% c("X", "Y"), ]
  expect_equal(nrow(tie_row), 1)
  expect_equal(tie_row$y, 0.5)
})

test_that("structural rows are flagged and weighted by period weight x penalty", {
  w <- c("1" = 1, "2" = 0.4)
  mat <- bt_win_matrix(data_absent, score_col = "score", weights = w,
                       absent = "penalize", absent_penalty = 0.5)
  cmp <- attr(mat, "comparisons")
  st  <- cmp[cmp$structural, ]
  # Period 2: A and B present, C absent -> two structural rows
  expect_equal(nrow(st), 2)
  expect_setequal(st$item1, c("A", "B"))
  expect_true(all(st$item2 == "C"))
  expect_true(all(st$y == 1))
  expect_equal(st$weight, c(0.4 * 0.5, 0.4 * 0.5))
})

test_that("zero-weight rows are dropped (absent_penalty = 0 leaves no structural rows)", {
  mat <- bt_win_matrix(data_absent, score_col = "score",
                       absent = "penalize", absent_penalty = 0)
  expect_false(any(attr(mat, "comparisons")$structural))
})

test_that("print() shows the counts but not the 'comparisons' attribute", {
  mat <- bt_win_matrix(data_simple, score_col = "score")
  expect_s3_class(mat, "bt_win_matrix")
  out <- capture.output(print(mat))
  expect_false(any(grepl("comparisons", out)))
})


# ── periods as blocks, clusters and time ────────────────────────────────────

test_that("duplicated item within a period is an error", {
  d <- data.frame(item = c("X", "X", "Y", "Y"), period = "07-2020",
                  score = c(7, 5, 6, 8))
  expect_error(bt_win_matrix(d), "at most once per period")
})

test_that("cluster defaults to period", {
  cmp <- attr(bt_win_matrix(data_simple, score_col = "score"), "comparisons")
  expect_equal(cmp$cluster, cmp$period)
})

test_that("cluster_col is recorded and does not change the counts", {
  # One period per pairwise judgement, two respondents
  d <- data.frame(
    item       = c("A", "B", "A", "C", "B", "C", "A", "B"),
    judgement  = c(1, 1, 2, 2, 3, 3, 4, 4),
    respondent = c("r1", "r1", "r1", "r1", "r2", "r2", "r2", "r2"),
    score      = c(1, 0, 0, 1, 1, 0, 1, 0)
  )
  m0 <- bt_win_matrix(d, period_col = "judgement")
  m1 <- bt_win_matrix(d, period_col = "judgement", cluster_col = "respondent")
  expect_equal(unclass(m0)[, ], unclass(m1)[, ], ignore_attr = TRUE)
  cmp <- attr(m1, "comparisons")
  expect_equal(nrow(cmp), 4)
  expect_setequal(unique(cmp$cluster), c("r1", "r2"))
})

test_that("cluster_col must be constant within a period", {
  d <- data.frame(item = c("A", "B"), period = 1, rater = c("r1", "r2"),
                  score = c(1, 0))
  expect_error(bt_win_matrix(d, cluster_col = "rater"),
               "constant within each period")
})

test_that("time_col drives weight lookup when periods are matches", {
  d <- data.frame(
    item   = c("A", "B", "A", "B"),
    match  = c(1, 1, 2, 2),
    season = c(2020, 2020, 2021, 2021),
    goals  = c(2, 1, 0, 3)
  )
  w <- c("2020" = 0.5, "2021" = 1)
  m <- bt_win_matrix(d, period_col = "match", score_col = "goals",
                     time_col = "season", weights = w)
  expect_equal(m["A", "B"], 0.5)   # 2020 win, weight 0.5
  expect_equal(m["B", "A"], 1)     # 2021 win, weight 1
})

test_that("missing values in cluster_col are an error", {
  d <- data.frame(item = c("A", "B"), period = 1, rater = NA, score = c(1, 0))
  expect_error(bt_win_matrix(d, cluster_col = "rater"), "missing values")
})

test_that("cluster_col and time_col must be single names", {
  expect_error(bt_win_matrix(data_simple, cluster_col = c("a", "b")),
               "single column name")
})