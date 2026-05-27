# btrank 0.1.0

Initial release.

* `bt_win_matrix()`: builds a pairwise win count matrix from panel data,
  with optional temporal weighting and tie handling (0.5 split).
* `bt_fit()`: fits a Bradley-Terry model via maximum likelihood, automatically
  setting the weakest item as reference so all ability estimates are >= 0.
* `bt_rank()`: extracts a ranked data frame from a fitted model, with
  normalised abilities in [0, 1] and optional top-N truncation.
* `bt_rank_all()`: convenience wrapper that runs the full pipeline in one call,
  with optional two-pass top-N re-fitting.
* `bt_prob_matrix()`: computes the matrix of pairwise win probabilities
  P(i beats j) from a fitted model.
* `bt_weights()`: generates exponential decay weights for periods following
  the formula of Ley et al. (2019).
