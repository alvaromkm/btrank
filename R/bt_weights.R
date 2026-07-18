#' Compute exponential decay weights for periods
#'
#' Generates a named numeric vector of weights for each period using an
#' exponential decay function. More recent periods receive higher weights,
#' with the rate of decay controlled by the `half_life` parameter. The
#' most recent period always receives a weight of 1.
#'
#' The weighting formula follows Ley et al. (2019):
#'
#' \deqn{w_t = \left(\frac{1}{2}\right)^{(T - t) \,/\, T_{1/2}}}
#'
#' where \eqn{T} is the most recent period, \eqn{t} is the period being
#' weighted, and \eqn{T_{1/2}} is the half-life.
#'
#' @param periods A numeric or character vector of period identifiers
#'   (e.g. years, season labels). The ordering is determined numerically
#'   for numeric vectors and lexicographically for character vectors.
#' @param half_life Positive number. The number of periods after which a
#'   period's weight is halved. Smaller values concentrate weight on recent
#'   periods (more aggressive decay); larger values produce a smoother,
#'   more uniform weighting.
#'
#' @return A named numeric vector of weights, one per period. Names match
#'   the values in `periods` coerced to character (for compatibility with
#'   the `weights` argument of [bt_win_matrix()]). The most recent period
#'   always has weight 1; all others are in (0, 1].
#'
#' @references
#' Ley, C., Van de Wiele, T., & Van Eetvelde, H. (2019).
#' Ranking soccer teams on the basis of their current strength: A comparison
#' of maximum likelihood approaches. *Statistical Modelling*, 19(1), 55–73.
#'
#' @seealso [bt_win_matrix()] which accepts the output of this function via
#'   its `weights` argument.
#'
#' @examples
#' # Five seasons, half-life of 2 periods
#' bt_weights(periods = 2019:2023, half_life = 2)
#'
#' # More aggressive decay (half-life = 1): recent seasons dominate
#' bt_weights(periods = 2019:2023, half_life = 1)
#'
#' # Smoother decay (half-life = 10): all seasons roughly equal
#' bt_weights(periods = 2019:2023, half_life = 10)
#'
#' # Character period labels (e.g. season codes)
#' bt_weights(periods = c("2021-22", "2022-23", "2023-24"), half_life = 1)
#'
#' @importFrom stats setNames
#' @export
bt_weights <- function(periods, half_life) {

  # ── Input validation ──────────────────────────────────────────────────────
  if (missing(periods) || length(periods) == 0) {
    stop("`periods` must be a non-empty vector.", call. = FALSE)
  }

  if (!is.numeric(half_life) || length(half_life) != 1 || half_life <= 0) {
    stop("`half_life` must be a single positive number.", call. = FALSE)
  }

  # ── Determine period order ────────────────────────────────────────────────
  # Numeric periods: sort numerically. Character periods: sort lexicographically.
  # In both cases the last element after sorting is treated as the most recent.
  if (is.numeric(periods)) {
    periods_ord <- sort(periods)
  } else {
    periods_ord <- sort(as.character(periods))
  }

  t_max <- periods_ord[length(periods_ord)]

  # ── Compute weights ───────────────────────────────────────────────────────
  if (is.numeric(periods_ord)) {
    gaps <- as.numeric(t_max) - as.numeric(periods_ord)
  } else {
    # For character periods, use positional distance from the most recent
    gaps <- rev(seq_along(periods_ord) - 1L)
  }

  weights <- (0.5) ^ (gaps / half_life)

  # ── Return as named vector (names as character for bt_win_matrix compat.) ─
  setNames(weights, as.character(periods_ord))
}
