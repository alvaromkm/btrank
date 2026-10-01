# tests/testthat/test-bt_summary.R

data_balanced <- data.frame(
  item   = c("A", "B", "C", "A", "B", "C", "A", "B", "C"),
  period = c(2021, 2021, 2021, 2022, 2022, 2022, 2023, 2023, 2023),
  score  = c(80, 70, 60, 65, 85, 55, 75, 60, 90)
)
mat_balanced <- bt_win_matrix(data_balanced, score_col = "score")

data_sep <- data.frame(
  item   = c("A", "B", "C", "A", "B", "C"),
  period = c(2022, 2022, 2022, 2023, 2023, 2023),
  score  = c(80, 65, 50, 75, 85, 60)
)
mat_sep <- bt_win_matrix(data_sep, score_col = "score")

# ── vcov.btfit ────────────────────────────────────────────────────────────

test_that("vcov.btfit returns a named square matrix excluding the reference", {
  fit <- bt_fit(mat_balanced)
  v <- vcov(fit)
  expect_true(is.matrix(v))
  expect_equal(nrow(v), length(fit$items) - 1)
  expect_setequal(rownames(v), setdiff(fit$items, fit$reference))
})

test_that("vcov.btfit matches an independently-fit BTm model numerically", {
  fit <- bt_fit(mat_balanced)
  items <- fit$items
  df_indep <- data.frame(
    item1 = factor(c("A", "A", "B"), levels = items),
    item2 = factor(c("B", "C", "C"), levels = items),
    wins1 = c(mat_balanced["A", "B"], mat_balanced["A", "C"], mat_balanced["B", "C"]),
    wins2 = c(mat_balanced["B", "A"], mat_balanced["C", "A"], mat_balanced["C", "B"])
  )
  fit_indep <- BradleyTerry2::BTm(
    outcome = cbind(wins1, wins2), player1 = item1, player2 = item2, data = df_indep
  )
  se_indep <- sqrt(diag(vcov(fit_indep)))
  names(se_indep) <- gsub("^\\.\\.", "", names(se_indep))
  
  v <- vcov(fit)
  se_pkg <- sqrt(diag(v))
  
  expect_equal(se_pkg[names(se_indep)], se_indep, tolerance = 1e-8)
})

test_that("vcov.btfit is not applicable to non-btfit objects", {
  # S3 dispatch never reaches vcov.btfit's internal class check here -
  # the generic itself fails first, which is the expected/correct behaviour.
  expect_error(vcov(list()), "no applicable method")
})

# ── summary.btfit ─────────────────────────────────────────────────────────

test_that("summary.btfit returns expected structure", {
  fit <- bt_fit(mat_balanced)
  s <- summary(fit)
  expect_s3_class(s, "summary.btfit")
  expect_named(s, c("table", "level", "reference"))
  expect_named(s$table, c("item", "ability", "se", "ci_low", "ci_high", "reliable"))
})

test_that("summary.btfit reference item has NA se and NA CI, not 0", {
  fit <- bt_fit(mat_balanced)
  s <- summary(fit)
  ref_row <- s$table[s$table$item == fit$reference, ]
  expect_true(is.na(ref_row$se))
  expect_true(is.na(ref_row$ci_low))
  expect_true(is.na(ref_row$ci_high))
})

test_that("summary.btfit marks all items reliable when there is no separation", {
  fit <- bt_fit(mat_balanced)
  s <- summary(fit)
  expect_true(all(s$table$reliable))
})

test_that("summary.btfit marks ALL items unreliable when any item is separated (not just the separated one)", {
  # Regression test: separation contaminates the shared vcov of the whole
  # fit (verified empirically - all non-separated items get numerically
  # identical, equally uninformative SEs). Flagging only the separated
  # item(s) would be misleading.
  fit <- suppressWarnings(bt_fit(mat_sep))
  expect_equal(fit$separation_items, "C")
  s <- summary(fit)
  expect_true(all(!s$table$reliable))
})

test_that("summary.btfit CI matches ability +/- qnorm * se for non-reference items", {
  fit <- bt_fit(mat_balanced)
  s <- summary(fit)
  non_ref <- s$table[s$table$item != fit$reference, ]
  z <- stats::qnorm(0.975)
  expect_equal(non_ref$ci_low, non_ref$ability - z * non_ref$se, tolerance = 1e-8)
  expect_equal(non_ref$ci_high, non_ref$ability + z * non_ref$se, tolerance = 1e-8)
})

test_that("summary.btfit errors on invalid level", {
  fit <- bt_fit(mat_balanced)
  expect_error(summary(fit, level = 0), "strictly between 0 and 1")
  expect_error(summary(fit, level = 1.5), "strictly between 0 and 1")
})

# ── print.summary.btfit ───────────────────────────────────────────────────

test_that("print.summary.btfit runs without error and mentions separation when relevant", {
  fit_ok  <- bt_fit(mat_balanced)
  fit_sep <- suppressWarnings(bt_fit(mat_sep))
  
  out_ok  <- capture.output(print(summary(fit_ok)))
  out_sep <- capture.output(print(summary(fit_sep)))
  
  expect_false(any(grepl("Warning:", out_ok)))
  expect_true(any(grepl("quasi-complete separation", out_sep)))
})

# ── confint.btfit ─────────────────────────────────────────────────────────

test_that("confint.btfit matches summary.btfit ci_low/ci_high", {
  fit <- bt_fit(mat_balanced)
  s <- summary(fit)
  ci <- confint(fit)
  expect_equal(unname(ci[, 1]), s$table$ci_low[match(rownames(ci), s$table$item)])
  expect_equal(unname(ci[, 2]), s$table$ci_high[match(rownames(ci), s$table$item)])
})

test_that("confint.btfit subsets via parm", {
  fit <- bt_fit(mat_balanced)
  ci_all <- confint(fit)
  ci_sub <- confint(fit, parm = c("A", "B"))
  expect_equal(rownames(ci_sub), c("A", "B"))
  expect_equal(ci_sub, ci_all[c("A", "B"), ])
})

test_that("confint.btfit errors on unknown parm", {
  fit <- bt_fit(mat_balanced)
  expect_error(confint(fit, parm = "Z"), "Unknown item")
})

# ── SE/CI under non-integer win counts (half_life, absent_penalty) ─────────

test_that("SE stays finite and positive under half_life weighting (no separation)", {
  # Round-robin rotation: A>B>C, B>A>C, C>A>B, A>C>B — every item wins and
  # loses against every rival in some period. Verified by hand.
  data3 <- data.frame(
    item   = rep(c("A", "B", "C"), 4),
    period = rep(2020:2023, each = 3),
    score  = c(90, 70, 50,   # 2020: A>B>C
               70, 90, 50,   # 2021: B>A>C
               70, 50, 90,   # 2022: C>A>B
               90, 50, 70)   # 2023: A>C>B
  )
  w   <- bt_weights(periods = 2020:2023, half_life = 1)
  mat <- bt_win_matrix(data3, score_col = "score", weights = w)
  fit <- bt_fit(mat)
  expect_equal(fit$separation_items, character(0))
  non_ref <- summary(fit)$table
  non_ref <- non_ref[non_ref$item != fit$reference, ]
  expect_true(all(is.finite(non_ref$se)))
  expect_true(all(non_ref$se > 0))
})

test_that("absent_penalty shrinks the C-vs-A gap's precision as it grows (reference-invariant)", {
  # Reference-invariant helper: raw se is meaningless once the penalized
  # item itself becomes the reference (se = NA by construction). The
  # variance of an ability *difference* is invariant to which item BTm
  # happens to use as its internal reference.
  ability_diff_se <- function(fit, i, j) {
    v <- vcov(fit)
    if (i == fit$reference || j == fit$reference) {
      other <- if (i == fit$reference) j else i
      sqrt(v[other, other])
    } else {
      sqrt(v[i, i] + v[j, j] - 2 * v[i, j])
    }
  }
  
  # p1: A>B>C ; p2: C>A>B (C has real wins) ; p3: A>B, C absent.
  data_abs <- data.frame(
    item   = c("A", "B", "C", "C", "A", "B", "A", "B"),
    period = c(1, 1, 1, 2, 2, 2, 3, 3),
    score  = c(90, 70, 50, 90, 70, 50, 90, 70)
  )
  
  se_diff <- function(penalty) {
    mat <- bt_win_matrix(data_abs, absent = "penalize", absent_penalty = penalty)
    ability_diff_se(bt_fit(mat), "C", "A")
  }
  
  expect_true(se_diff(5) <= se_diff(0.2))
})

test_that("plot.btfit runs without error and returns table invisibly", {
  df  <- data.frame(item = c("A","B","C","A","B","C"),
                    period = c(2021,2021,2021,2022,2022,2022),
                    score  = c(80,70,60,65,85,55))
  mat <- bt_win_matrix(df, score_col = "score")
  fit <- bt_fit(mat)
  
  out <- withVisible(plot(fit))
  expect_false(out$visible)
  expect_s3_class(out$value, "data.frame")
  expect_true(all(c("item","ability","ci_low","ci_high","reliable") %in% names(out$value)))
})