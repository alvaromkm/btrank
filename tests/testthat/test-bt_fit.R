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
  fit <- suppressWarnings(bt_fit(mat_simple))
  expect_named(fit, c("abilities", "reference", "model", "items", "separation_items",
                    "comparisons"))
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

test_that("bt_fit abilities reproduce the published citations example (Turner & Firth 2012, JSS 48(9))", {
  # External ground truth, not a self-consistency check against BTm(): the
  # target values are the published coefficients for BradleyTerry2::citations
  # (originally Stigler 1994; see also Agresti 2002, p.448), reported in
  # Turner & Firth (2012, JSS 48(9), Section 2.1) with journalBiometrika = 0.
  citations <- BradleyTerry2::citations
  citations.sf <- BradleyTerry2::countsToBinomial(citations)
  names(citations.sf)[1:2] <- c("journal1", "journal2")
  
  items <- c("Biometrika", "Comm Statist", "JASA", "JRSS-B")
  win_matrix <- matrix(0, 4, 4, dimnames = list(items, items))
  for (i in seq_len(nrow(citations.sf))) {
    r <- citations.sf[i, ]
    win_matrix[as.character(r$journal1), as.character(r$journal2)] <- r$win1
    win_matrix[as.character(r$journal2), as.character(r$journal1)] <- r$win2
  }
  
  fit <- bt_fit(win_matrix)
  
  # Abilities are identified only up to an additive constant; re-reference
  # to Biometrika (the reference used in the published example) before
  # comparing. This is arithmetic on the point estimates, not a re-fit.
  ab <- fit$abilities - fit$abilities["Biometrika"]
  
  published <- c(
    "Biometrika"   = 0,
    "Comm Statist" = -2.9491,
    "JASA"         = -0.4796,
    "JRSS-B"       = 0.2690
  )
  
  expect_equal(ab[names(published)], published, tolerance = 1e-3)
})

test_that("bt_fit errors on matrix without names", {
  expect_error(bt_fit(matrix(c(0,1,1,0), 2, 2)),
               "must have row and column names")
})

test_that("bt_fit warns and flags items with quasi-complete separation", {
  # C never wins a single comparison against A or B
  mat_sep <- matrix(
    c(0, 1, 2,
      1, 0, 2,
      0, 0, 0),
    nrow = 3, ncol = 3,
    dimnames = list(c("A", "B", "C"), c("A", "B", "C"))
  )
  expect_warning(fit <- bt_fit(mat_sep), "quasi-complete separation")
  expect_equal(fit$separation_items, "C")
})

test_that("bt_fit reports no separation for a balanced win matrix", {
  mat_balanced <- matrix(
    c(0, 2, 2,
      1, 0, 2,
      1, 1, 0),
    nrow = 3, ncol = 3,
    dimnames = list(c("A", "B", "C"), c("A", "B", "C"))
  )
  expect_no_warning(fit <- bt_fit(mat_balanced))
  expect_equal(fit$separation_items, character(0))
})

# ── comparison-level record (for cluster-robust variance) ──────────────────

data_ok <- data.frame(
  item   = rep(c("A", "B", "C"), 4),
  period = rep(1:4, each = 3),
  score  = c(3, 2, 1,  1, 3, 2,  2, 1, 3,  3, 1, 2)
)

test_that("bt_fit keeps the comparisons record from bt_win_matrix", {
  mat <- bt_win_matrix(data_ok)
  fit <- bt_fit(mat)
  expect_s3_class(fit$comparisons, "data.frame")
  expect_equal(fit$comparisons, attr(mat, "comparisons"))
})

test_that("bt_fit stores NULL comparisons for a hand-built matrix, silently", {
  mat <- unclass(bt_win_matrix(data_ok))
  attr(mat, "comparisons") <- NULL
  expect_silent(fit <- bt_fit(mat))
  expect_null(fit$comparisons)
})

test_that("bt_fit drops a stale comparisons record with a warning", {
  mat <- bt_win_matrix(data_ok)
  mat["A", "B"] <- mat["A", "B"] + 1   # sub-assignment keeps the attribute
  expect_warning(fit <- bt_fit(mat), "does not match its counts")
  expect_null(fit$comparisons)
})

test_that("comparisons record survives the zero-activity item drop", {
  mat <- bt_win_matrix(data_ok)
  cmp <- attr(mat, "comparisons")
  items <- c(rownames(mat), "Z")
  big <- matrix(0, 4, 4, dimnames = list(items, items))
  big[1:3, 1:3] <- unclass(mat)
  attr(big, "comparisons") <- cmp
  expect_warning(fit <- bt_fit(big), "no comparisons and were dropped")
  expect_equal(fit$comparisons, cmp)
})

test_that("point estimates are unchanged by the comparisons record", {
  mat   <- bt_win_matrix(data_ok)
  plain <- unclass(mat); attr(plain, "comparisons") <- NULL
  expect_equal(bt_fit(mat)$abilities, bt_fit(plain)$abilities)
})
