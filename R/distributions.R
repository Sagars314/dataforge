# ============================================================
# Distribution conversion functions
# ============================================================
# Pattern: norm2X() converts a standard-normal variate to
#           distribution X; X2norm() does the reverse.
# ============================================================

#' Convert Normal to Beta
#' @param x Numeric vector (standard normal or from \code{\link{rnorm_forge}}).
#' @param shape1,shape2 Beta distribution shape parameters.
#' @return Numeric vector of beta-distributed values.
#' @examples norm2beta(rnorm(100), shape1 = 2, shape2 = 5)
#' @export
norm2beta <- function(x, shape1 = 1, shape2 = 1) {
  p <- stats::pnorm(x)
  stats::qbeta(p, shape1 = shape1, shape2 = shape2)
}

#' Convert Beta to Normal
#' @param x Numeric vector of beta-distributed values.
#' @param shape1,shape2 Beta distribution shape parameters.
#' @return Numeric vector of normal-distributed values.
#' @export
beta2norm <- function(x, shape1 = 1, shape2 = 1) {
  p <- stats::pbeta(x, shape1 = shape1, shape2 = shape2)
  stats::qnorm(p)
}

#' Convert Normal to Binomial
#' @param x Numeric vector.
#' @param size Number of trials.
#' @param prob Probability of success.
#' @return Integer vector.
#' @examples norm2binom(rnorm(100), size = 10, prob = 0.3)
#' @export
norm2binom <- function(x, size = 1, prob = 0.5) {
  p <- stats::pnorm(x)
  stats::qbinom(p, size = size, prob = prob)
}

#' Convert Binomial to Normal
#' @param x Integer vector of binomial counts.
#' @param size Number of trials.
#' @param prob Probability of success.
#' @return Numeric vector.
#' @export
binom2norm <- function(x, size = 1, prob = 0.5) {
  p <- stats::pbinom(x, size = size, prob = prob)
  stats::qnorm(p)
}

#' Convert Normal to Gamma
#' @param x Numeric vector.
#' @param shape Shape parameter.
#' @param rate Rate parameter (1/scale).
#' @return Numeric vector.
#' @examples norm2gamma(rnorm(100), shape = 2, rate = 1)
#' @export
norm2gamma <- function(x, shape = 1, rate = 1) {
  p <- stats::pnorm(x)
  stats::qgamma(p, shape = shape, rate = rate)
}

#' Convert Gamma to Normal
#' @param x Numeric vector.
#' @param shape Shape parameter.
#' @param rate Rate parameter.
#' @return Numeric vector.
#' @export
gamma2norm <- function(x, shape = 1, rate = 1) {
  p <- stats::pgamma(x, shape = shape, rate = rate)
  stats::qnorm(p)
}

#' Convert Normal to Poisson
#' @param x Numeric vector.
#' @param lambda Mean of the Poisson distribution.
#' @return Integer vector.
#' @examples norm2pois(rnorm(100), lambda = 3)
#' @export
norm2pois <- function(x, lambda = 1) {
  p <- stats::pnorm(x)
  stats::qpois(p, lambda = lambda)
}

#' Convert Poisson to Normal
#' @param x Integer vector.
#' @param lambda Mean of the Poisson distribution.
#' @return Numeric vector.
#' @export
pois2norm <- function(x, lambda = 1) {
  p <- stats::ppois(x, lambda = lambda)
  stats::qnorm(p)
}

#' Convert Normal to Uniform
#' @param x Numeric vector.
#' @param min Lower bound.
#' @param max Upper bound.
#' @return Numeric vector.
#' @examples norm2unif(rnorm(100), min = 0, max = 10)
#' @export
norm2unif <- function(x, min = 0, max = 1) {
  p <- stats::pnorm(x)
  stats::qunif(p, min = min, max = max)
}

#' Convert Uniform to Normal
#' @param x Numeric vector (uniform values in `min` and `max`).
#' @param min Lower bound.
#' @param max Upper bound.
#' @return Numeric vector.
#' @export
unif2norm <- function(x, min = 0, max = 1) {
  p <- stats::punif(x, min = min, max = max)
  stats::qnorm(p)
}

#' Convert Normal to Truncated Normal
#'
#' @param x Numeric vector.
#' @param min,max Truncation bounds. `NA` means no truncation on that side.
#' @param mu Mean of the (untruncated) normal.
#' @param sd SD of the (untruncated) normal.
#' @return Numeric vector truncated to `min` and `max`.
#' @examples norm2trunc(rnorm(200), min = 0, max = 5, mu = 2, sd = 1.5)
#' @export
norm2trunc <- function(x, min = -Inf, max = Inf, mu = 0, sd = 1) {
  lo  <- if (is.na(min)) -Inf else min
  hi  <- if (is.na(max))  Inf else max
  p   <- stats::pnorm(x)
  p_lo <- stats::pnorm(lo, mu, sd)
  p_hi <- stats::pnorm(hi, mu, sd)
  p_scaled <- p_lo + p * (p_hi - p_lo)
  stats::qnorm(p_scaled, mu, sd)
}

#' Convert Truncated Normal to Normal
#' @param x Numeric vector (truncated normal values).
#' @param min,max The bounds that were used to truncate.
#' @param mu,sd Parameters of the underlying normal.
#' @return Numeric vector on the standard normal scale.
#' @export
trunc2norm <- function(x, min = -Inf, max = Inf, mu = 0, sd = 1) {
  lo   <- if (is.na(min)) -Inf else min
  hi   <- if (is.na(max))  Inf else max
  p_lo <- stats::pnorm(lo, mu, sd)
  p_hi <- stats::pnorm(hi, mu, sd)
  p    <- stats::pnorm(x, mu, sd)
  p_scaled <- (p - p_lo) / (p_hi - p_lo)
  stats::qnorm(p_scaled)
}

# ============================================================
# Likert distribution
# ============================================================

#' Random Likert-Scale Values
#'
#' @description
#' Generate random responses on a Likert scale by discretising a latent
#' normal variable.
#'
#' @param n Integer. Number of observations.
#' @param items Integer. Number of Likert scale points (default 5).
#' @param mu Mean of the latent normal (maps to scale centre by default).
#' @param sd SD of the latent normal. Default 1.
#'
#' @return Integer vector with values in `1:items`.
#' @examples
#' table(rlikert(200, items = 7, mu = 0, sd = 1))
#' @export
rlikert <- function(n = 100, items = 5, mu = 0, sd = 1) {
  z <- stats::rnorm(n, mu, sd)
  norm2likert(z, items = items)
}

#' Convert Normal to Likert Scale
#'
#' @param x Numeric vector (latent normal values).
#' @param items Integer. Number of scale points.
#' @return Integer vector with values in `1:items`.
#' @export
norm2likert <- function(x, items = 5) {
  breaks <- stats::qnorm(seq(0, 1, length.out = items + 1))
  breaks[1]           <- -Inf
  breaks[items + 1]   <-  Inf
  as.integer(cut(x, breaks = breaks, labels = FALSE))
}

#' Convert Likert to Normal
#' @param x Integer vector of Likert responses.
#' @param items Integer. Number of scale points.
#' @return Numeric vector of approximately normal values.
#' @export
likert2norm <- function(x, items = 5) {
  breaks <- seq(0, 1, length.out = items + 1)
  p <- (breaks[x] + breaks[x + 1]) / 2
  stats::qnorm(p)
}

#' Simulate Multiple Correlated Distributions (NORTA)
#'
#' @description
#' Generate correlated data from arbitrary marginal distributions using the
#' Normal-to-Anything (NORTA) approach: simulate correlated normals, then
#' convert each column's marginal to the desired distribution.
#'
#' @param n Integer. Number of observations.
#' @param dist Named list where each element is a list with elements:
#'   \itemize{
#'     \item \code{dist}: character string naming the marginal distribution,
#'       e.g. \code{"beta"}, \code{"pois"}, \code{"likert"}, \code{"norm"},
#'       \code{"gamma"}, \code{"binom"}, \code{"unif"}, \code{"trunc"}.
#'     \item Additional named arguments passed to the corresponding
#'       \code{norm2X()} function (e.g. \code{shape1 = 2, shape2 = 5}
#'       for \code{"beta"}).
#'   }
#' @param r Correlation specification (scalar, triangle vector, or matrix).
#' @param mu Numeric vector of standard-normal means (default 0 for all).
#' @param empirical Logical. Enforce exact inter-variable correlations in the
#'   latent normal space.
#'
#' @return A data frame with columns named according to `dist`.
#'
#' @examples
#' df <- rmulti(
#'   n    = 200,
#'   dist = list(
#'     score = list(dist = "norm",   mu = 50, sd = 10),
#'     count = list(dist = "pois",   lambda = 5),
#'     prop  = list(dist = "beta",   shape1 = 2, shape2 = 5),
#'     rating = list(dist = "likert", items = 7)
#'   ),
#'   r = 0.4
#' )
#' head(df)
#' @export
rmulti <- function(n = 100, dist, r = 0, mu = 0, empirical = FALSE) {
  if (!is.list(dist)) stop("`dist` must be a named list.", call. = FALSE)
  k <- length(dist)
  if (k < 2) stop("`dist` must have at least 2 elements.", call. = FALSE)

  # Simulate latent normals
  z <- rnorm_forge(n, vars = k, mu = rep_len(mu, k), sd = 1, r = r,
                    var_names = names(dist), empirical = empirical)

  # Convert each column
  out <- as.data.frame(lapply(names(dist), function(nm) {
    spec <- dist[[nm]]
    d    <- spec$dist
    args <- spec[setdiff(names(spec), "dist")]
    args$x <- z[[nm]]

    conv_fn <- switch(d,
      norm   = function(x, mu = 0, sd = 1, ...) x * sd + mu,
      beta   = norm2beta,
      binom  = norm2binom,
      gamma  = norm2gamma,
      pois   = norm2pois,
      unif   = norm2unif,
      likert = norm2likert,
      trunc  = norm2trunc,
      stop(sprintf("Unknown distribution '%s'.", d), call. = FALSE)
    )
    do.call(conv_fn, args)
  }))
  names(out) <- names(dist)
  out
}
