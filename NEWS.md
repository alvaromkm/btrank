
# btrank 0.1.0

Initial release.

* `bt_win_matrix()`: builds a pairwise win count matrix from panel data,
  with optional temporal weighting, tie handling (0.5 split), and explicit
  handling of items absent from a period (`absent = "ignore"`/`"penalize"`).
* `bt_fit()`: fits a Bradley-Terry model via maximum likelihood, automatically
  setting the weakest item as reference so all ability estimates are >= 0.
  Detects and flags items with quasi-complete separation.
* `bt_rank()`: extracts a ranked data frame from a fitted model, with
  normalised abilities in [0, 1] and optional top-N truncation.
* `bt_rank_all()`: convenience wrapper that runs the full pipeline in one call,
  with optional two-pass top-N re-fitting.
* `bt_prob_matrix()`: computes the matrix of pairwise win probabilities
  P(i beats j) from a fitted model.
* `bt_weights()`: generates exponential decay weights for periods following
  the formula of Ley et al. (2019).
* Uncertainty quantification: `vcov.btfit()`, `summary.btfit()`,
  `confint.btfit()` (Wald intervals), and `bt_significance_matrix()`
  (pairwise Wald tests with Holm-adjusted p-values). Separation-affected
  items are flagged and excluded from reliable inference.
* `plot.btfit()`: caterpillar plot of estimated abilities with confidence
  intervals.
* `epl_scores`: example dataset of English Premier League team results
  by season (1995/96-2025/26), used in the package vignette to demonstrate
  the full pipeline on real, independently verifiable data.