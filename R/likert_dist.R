#' Likert Density (Probability Mass) Function
#'
#' @description
#' Compute the probability of each Likert scale value under a latent normal
#' distribution.
#'
#' @param x Integer vector of Likert values (in `1:items`).
#' @param items Integer. Number of scale points. Default 5.
#' @param mu Mean of the latent normal. Default 0.
#' @param sd SD of the latent normal. Default 1.
#' @param log Logical. Return log probabilities? Default `FALSE`.
#'
#' @return Numeric vector of probabilities.
#'
#' @examples
#' dlikert(1:5, items = 5)          # equal-width bins, mu=0
#' dlikert(1:5, items = 5, mu = 1)  # positive skew
#'
#' @export
dlikert <- function(x, items = 5, mu = 0, sd = 1, log = FALSE) {
  breaks <- stats::qnorm(seq(0, 1, length.out = items + 1), mu, sd)
  breaks[1]         <- -Inf
  breaks[items + 1] <-  Inf

  p <- vapply(x, function(xi) {
    if (xi < 1 || xi > items || xi != round(xi)) return(0)
    stats::pnorm(breaks[xi + 1], mu, sd) - stats::pnorm(breaks[xi], mu, sd)
  }, numeric(1))

  if (log) log(p) else p
}

#' Likert Cumulative Distribution Function
#'
#' @param q Integer vector of Likert values.
#' @param items Integer. Number of scale points.
#' @param mu,sd Parameters of the latent normal.
#' @param lower.tail Logical.
#' @param log.p Logical.
#' @return Numeric vector of cumulative probabilities.
#' @export
plikert <- function(q, items = 5, mu = 0, sd = 1,
                    lower.tail = TRUE, log.p = FALSE) {
  p <- vapply(q, function(qi) {
    sum(dlikert(seq_len(min(qi, items)), items = items, mu = mu, sd = sd))
  }, numeric(1))
  if (!lower.tail) p <- 1 - p
  if (log.p) log(p) else p
}

#' Likert Quantile Function
#'
#' @param p Probability vector.
#' @param items Integer. Number of scale points.
#' @param mu,sd Parameters of the latent normal.
#' @return Integer vector.
#' @export
qlikert <- function(p, items = 5, mu = 0, sd = 1) {
  vapply(p, function(pi) {
    cum <- cumsum(dlikert(seq_len(items), items = items, mu = mu, sd = sd))
    which(cum >= pi)[1]
  }, integer(1))
}
