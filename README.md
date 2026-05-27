
# btrank

**btrank** is an R package for estimating latent ability scores from
paired comparison data using the **Bradley-Terry model**. It is designed
to be domain-agnostic — applicable to sports rankings, preference
experiments, academic evaluations, or any setting where items are
compared pairwise across multiple periods.

## Key features

- Flexible input: accepts any data frame with item, period, and score
  columns
- Temporal weighting via exponential decay (Ley et al., 2019)
- Win probability matrix between all pairs of items
- Modular pipeline and a convenient one-step wrapper
- Full roxygen2 documentation and a worked vignette

## Installation

``` r
# install.packages("remotes")
remotes::install_github("alvaromkm/btrank")
```

## Quick example

``` r
library(btrank)

results <- data.frame(
  item   = c("A", "B", "C", "A", "B", "C"),
  period = c(2022, 2022, 2022, 2023, 2023, 2023),
  score  = c(80, 65, 50, 75, 85, 60)
)

# Full pipeline in one call
bt_rank_all(results, score_col = "score")
#>   rank item ability ability_norm
#> 1    1    B 23.9024            1
#> 2    2    A 23.9024            1
#> 3    3    C  0.0000            0

# With temporal weighting — recent periods count more
bt_rank_all(results, score_col = "score", half_life = 1)
#>   rank item ability ability_norm
#> 1    1    B 24.1255       1.0000
#> 2    2    A 23.4324       0.9713
#> 3    3    C  0.0000       0.0000
```

## Pipeline overview

| Function           | Description                        |
|--------------------|------------------------------------|
| `bt_win_matrix()`  | Build pairwise win count matrix    |
| `bt_fit()`         | Fit Bradley-Terry model            |
| `bt_rank()`        | Extract ranked data frame          |
| `bt_prob_matrix()` | Compute win probability matrix     |
| `bt_weights()`     | Generate exponential decay weights |
| `bt_rank_all()`    | Full pipeline in one call          |

## Vignette

A full worked example with methodological background is available at:

``` r
browseVignettes("btrank")
```

## References

- Bradley, R. A., & Terry, M. E. (1952). Rank analysis of incomplete
  block designs. *Biometrika*, 39(3/4), 324–345.
- Ley, C., et al. (2019). Ranking soccer teams on the basis of their
  current strength. *Statistical Modelling*, 19(1), 55–73.
- Alvo, M., & Yu, P. L. H. (2014). *Statistical Methods for Ranking
  Data*. Springer. <https://doi.org/10.1007/978-1-4939-1471-5>
- Yu, P. L. H., Gu, J., & Xu, H. (2019). Analysis of ranking data.
  *WIREs Computational Statistics*. <https://doi.org/10.1002/wics.1483>

## License

MIT © Álvaro Meca Mondéjar, Miguel Ángel Montero Alonso, Juan de Dios Luna del Castillo
