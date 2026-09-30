# tests/testthat/helper-data.R
# Shared fixtures, loaded automatically by testthat before the test files.

# Items with decreasing strength, scores = strength + noise, one row per item
# per period. With the defaults (5 items, 10 periods, seed 1) the fit has no
# separation and every item appears in every period.
make_data <- function(seed = 1, n_items = 5, n_periods = 10) {
  set.seed(seed)
  strength <- seq(1, 0, length.out = n_items)
  d <- expand.grid(item = LETTERS[seq_len(n_items)],
                   period = seq_len(n_periods), stringsAsFactors = FALSE)
  d$score <- round(strength[match(d$item, LETTERS)] * 3 +
                     stats::rnorm(nrow(d)), 1)
  d
}
