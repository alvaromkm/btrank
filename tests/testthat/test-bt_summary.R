# tests/testthat/test-bt_summary.R

# Robust fixture: 5 items x 10 periods, no separation (see helper-data.R)
mat_rob <- bt_win_matrix(make_data())

# Small 3-item fixture, used for model-based checks against BTm
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

# Robust fixture plus an item "Z" present in a single period
data_single <- rbind(make_data(), data.frame(item = "Z", period = 3, score = 1.5))
mat_single  <- bt_win_matrix(data_single)

# ── vcov.btfit ────────────────────────────────────────────────────────────

test_that("vcov.btfit returns a named square matrix excluding the reference", {
  fit <- bt_fit(mat_rob)
  v <- vcov(fit)
  expect_true(is.matrix(v))
  expect_equal(nrow(v), length(fit$items) - 1)
  expect_setequal(rownames(v), setdiff(fit$items, fit$reference))
})

test_that("vcov.btfit defaults to cluster-robust when the record is available", {
  fit <- bt_fit(mat_rob)
  v <- vcov(fit)
  expect_equal(attr(v, "type"), "cluster")
  expect_equal(attr(v, "n_clusters"), 10)
  expect_equal(attr(v, "df"), 9)
  expect_equal(attr(v, "effective_clusters"), 10)
  expect_equal(attr(v, "non_estimable"), character(0))
})

test_that("cluster vcov equals the sandwich computed directly", {
  fit <- bt_fit(mat_rob)
  vm  <- suppressWarnings(vcov(fit, type = "model"))
  U   <- .bt_cluster_scores(fit$comparisons, fit$abilities, rownames(vm))
  ref <- .bt_vcov_sandwich(unclass(vm)[, ], U)
  expect_equal(unclass(vcov(fit))[, ], ref, ignore_attr = TRUE)
})

test_that("type = 'model' matches an independently-fit BTm model numerically", {
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

  v <- suppressWarnings(vcov(fit, type = "model"))
  se_pkg <- sqrt(diag(v))
  expect_equal(se_pkg[names(se_indep)], se_indep, tolerance = 1e-8)
})

test_that("explicit type = 'model' warns when clusters hold several comparisons", {
  fit <- bt_fit(mat_rob)
  expect_warning(vcov(fit, type = "model"), "assume independent comparisons")
})

test_that("a hand-built matrix falls back silently to model-based errors", {
  plain <- unclass(mat_rob); attr(plain, "comparisons") <- NULL
  fit <- bt_fit(plain)
  expect_silent(v <- vcov(fit))
  expect_equal(attr(v, "type"), "model")
  expect_error(vcov(fit, type = "cluster"), "comparison-level record")
})

test_that("vcov.btfit errors on an invalid type", {
  fit <- bt_fit(mat_rob)
  expect_error(vcov(fit, type = "sandwich"), "must be NULL")
})

test_that("vcov.btfit is not applicable to non-btfit objects", {
  expect_error(vcov(list()), "no applicable method")
})

# ── summary.btfit ─────────────────────────────────────────────────────────

test_that("summary.btfit returns expected structure", {
  s <- summary(bt_fit(mat_rob))
  expect_s3_class(s, "summary.btfit")
  expect_named(s, c("table", "level", "reference", "type", "df", "n_clusters",
                    "effective_clusters", "non_estimable", "separation_items"))
  expect_named(s$table, c("item", "ability", "se", "ci_low", "ci_high", "reliable"))
})

test_that("every item, including the reference, has a finite quasi-SE", {
  fit <- bt_fit(mat_rob)
  s <- summary(fit)
  expect_true(all(is.finite(s$table$se)))
  expect_true(is.finite(s$table$se[s$table$item == fit$reference]))
})

test_that("summary se are the quasi-SEs of the full cluster-robust covariance", {
  fit <- bt_fit(mat_rob)
  s   <- summary(fit)
  qse <- .bt_quasi_se(.bt_inference(fit)$vcov_full)
  expect_equal(s$table$se, unname(qse[s$table$item]))
})

test_that("cluster-robust intervals use a t quantile with G - 1 df", {
  s <- summary(bt_fit(mat_rob))
  q <- stats::qt(0.975, df = 9)
  expect_equal(s$table$ci_low,  s$table$ability - q * s$table$se)
  expect_equal(s$table$ci_high, s$table$ability + q * s$table$se)
})

test_that("model-based intervals use a normal quantile", {
  s <- suppressWarnings(summary(bt_fit(mat_rob), type = "model"))
  expect_equal(s$df, Inf)
  q <- stats::qnorm(0.975)
  expect_equal(s$table$ci_high, s$table$ability + q * s$table$se)
})

test_that("summary.btfit marks all items reliable when there is no separation", {
  expect_true(all(summary(bt_fit(mat_rob))$table$reliable))
})

test_that("summary.btfit marks ALL items unreliable when any item is separated", {
  fit <- suppressWarnings(bt_fit(mat_sep))
  expect_equal(fit$separation_items, "C")
  s <- suppressWarnings(summary(fit))
  expect_true(all(!s$table$reliable))
})

test_that("single-cluster items get NA se/CI and reliable = FALSE, others are unaffected", {
  fit <- bt_fit(mat_single)
  s   <- summary(fit)
  expect_equal(s$non_estimable, "Z")
  z <- s$table[s$table$item == "Z", ]
  expect_true(is.na(z$se) && is.na(z$ci_low) && is.na(z$ci_high))
  expect_false(z$reliable)
  others <- s$table[s$table$item != "Z", ]
  expect_true(all(is.finite(others$se)))
  expect_true(all(others$reliable))
})

test_that("summary.btfit errors on invalid level", {
  fit <- bt_fit(mat_rob)
  expect_error(summary(fit, level = 0), "strictly between 0 and 1")
  expect_error(summary(fit, level = 1.5), "strictly between 0 and 1")
})

# ── print.summary.btfit ───────────────────────────────────────────────────

test_that("print.summary.btfit reports the SE type and relevant notes", {
  out_ok  <- capture.output(print(summary(bt_fit(mat_rob))))
  out_sep <- capture.output(print(suppressWarnings(summary(suppressWarnings(bt_fit(mat_sep))))))
  out_one <- capture.output(print(summary(bt_fit(mat_single))))
  out_mod <- capture.output(print(suppressWarnings(summary(bt_fit(mat_rob), type = "model"))))

  expect_true(any(grepl("cluster-robust, 10 clusters", out_ok)))
  expect_false(any(grepl("Warning:|Note:", out_ok)))
  expect_true(any(grepl("quasi-complete separation", out_sep)))
  expect_true(any(grepl("single cluster", out_one)))
  expect_true(any(grepl("model-based", out_mod)))
})

# ── confint.btfit ─────────────────────────────────────────────────────────

test_that("confint.btfit matches summary.btfit ci_low/ci_high", {
  fit <- bt_fit(mat_rob)
  s <- summary(fit)
  ci <- confint(fit)
  expect_equal(unname(ci[, 1]), s$table$ci_low[match(rownames(ci), s$table$item)])
  expect_equal(unname(ci[, 2]), s$table$ci_high[match(rownames(ci), s$table$item)])
})

test_that("confint.btfit subsets via parm", {
  fit <- bt_fit(mat_rob)
  ci_all <- confint(fit)
  ci_sub <- confint(fit, parm = c("A", "B"))
  expect_equal(rownames(ci_sub), c("A", "B"))
  expect_equal(ci_sub, ci_all[c("A", "B"), ])
})

test_that("confint.btfit errors on unknown parm", {
  expect_error(confint(bt_fit(mat_rob), parm = "Z"), "Unknown item")
})

# ── SE under non-integer win counts (half_life, absent_penalty) ───────────

test_that("robust SEs stay finite and positive under half_life weighting", {
  w   <- bt_weights(periods = 1:10, half_life = 2)
  fit <- bt_fit(bt_win_matrix(make_data(), weights = w))
  s   <- summary(fit)
  expect_lt(s$effective_clusters, 10)
  expect_true(all(is.finite(s$table$se)))
  expect_true(all(s$table$se > 0))
})

test_that("absent_penalty shrinks the model-based C-vs-A precision as it grows", {
  # Documents the known effect of structural comparisons on model-based
  # precision (see ?bt_win_matrix). Uses the reference-invariant variance of
  # an ability difference.
  var_diff_se <- function(fit, i, j) {
    v <- suppressWarnings(vcov(fit, type = "model"))
    V <- .bt_full_vcov(unclass(v)[, , drop = FALSE], fit$items)
    sqrt(V[i, i] + V[j, j] - 2 * V[i, j])
  }
  # p1: A>B>C ; p2: C>A>B (C has real wins) ; p3: A>B, C absent.
  data_abs <- data.frame(
    item   = c("A", "B", "C", "C", "A", "B", "A", "B"),
    period = c(1, 1, 1, 2, 2, 2, 3, 3),
    score  = c(90, 70, 50, 90, 70, 50, 90, 70)
  )
  se_diff <- function(penalty) {
    mat <- bt_win_matrix(data_abs, absent = "penalize", absent_penalty = penalty)
    var_diff_se(bt_fit(mat), "C", "A")
  }
  expect_true(se_diff(5) <= se_diff(0.2))
})

# ── plot.btfit ────────────────────────────────────────────────────────────

test_that("plot.btfit runs without error and returns table invisibly", {
  fit <- bt_fit(mat_rob)
  pdf(NULL); on.exit(dev.off())
  out <- withVisible(plot(fit))
  expect_false(out$visible)
  expect_s3_class(out$value, "data.frame")
  expect_true(all(c("item", "ability", "ci_low", "ci_high", "reliable") %in% names(out$value)))
})
