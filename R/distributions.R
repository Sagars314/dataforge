# ============================================================
# Distribution conversion functions (NORTA approach)
# ============================================================
# Each norm2X function converts a standard normal vector to the
# target distribution X by probability-integral transform.
# Each X2norm inverts the process.
# ============================================================

#' Convert a normal vector to a Beta distribution
#' @param x Numeric vector (need not be standard normal; will be rank-based).
#' @param alpha Shape parameter alpha (default `2`).
#' @param beta  Shape parameter beta  (default `5`).
#' @return A numeric vector following a Beta distribution.
#' @export
#' @examples
#' x <- rnorm(500)
#' y <- norm2beta(x, alpha = 2, beta = 5)
#' hist(y, breaks = 30)
norm2beta <- function(x, alpha = 2, beta = 5) {
  p <- pnorm(x)
  qbeta(p, shape1 = alpha, shape2 = beta)
}

#' @rdname norm2beta
#' @export
beta2norm <- function(x, alpha = 2, beta = 5) {
  p <- pbeta(x, shape1 = alpha, shape2 = beta)
  qnorm(p)
}

#' Convert a normal vector to a Binomial distribution
#' @param x Numeric vector.
#' @param size Number of trials.
#' @param prob Probability of success.
#' @return Integer vector.
#' @export
#' @examples
#' norm2binom(rnorm(200), size = 10, prob = 0.4)
norm2binom <- function(x, size = 1, prob = 0.5) {
  p <- pnorm(x)
  qbinom(p, size = size, prob = prob)
}

#' @rdname norm2binom
#' @export
binom2norm <- function(x, size = 1, prob = 0.5) {
  p <- pbinom(x, size = size, prob = prob)
  qnorm(p)
}

#' Convert a normal vector to a Gamma distribution
#' @param x Numeric vector.
#' @param shape Gamma shape parameter.
#' @param rate  Gamma rate parameter.
#' @return Numeric vector.
#' @export
#' @examples
#' norm2gamma(rnorm(300), shape = 2, rate = 0.5)
norm2gamma <- function(x, shape = 1, rate = 1) {
  p <- pnorm(x)
  qgamma(p, shape = shape, rate = rate)
}

#' @rdname norm2gamma
#' @export
gamma2norm <- function(x, shape = 1, rate = 1) {
  p <- pgamma(x, shape = shape, rate = rate)
  qnorm(p)
}

#' Convert a normal vector to a Negative Binomial distribution
#' @param x Numeric vector.
#' @param size Target for number of successful trials.
#' @param prob Probability of success on each trial.
#' @return Integer vector.
#' @export
#' @examples
#' norm2nbinom(rnorm(200), size = 5, prob = 0.4)
norm2nbinom <- function(x, size = 1, prob = 0.5) {
  p <- pnorm(x)
  qnbinom(p, size = size, prob = prob)
}

#' @rdname norm2nbinom
#' @export
nbinom2norm <- function(x, size = 1, prob = 0.5) {
  p <- pnbinom(x, size = size, prob = prob)
  qnorm(p)
}

#' Convert a normal vector to a Poisson distribution
#' @param x Numeric vector.
#' @param lambda Poisson rate parameter.
#' @return Integer vector.
#' @export
#' @examples
#' norm2pois(rnorm(200), lambda = 3)
norm2pois <- function(x, lambda = 1) {
  p <- pnorm(x)
  qpois(p, lambda = lambda)
}

#' @rdname norm2pois
#' @export
pois2norm <- function(x, lambda = 1) {
  p <- ppois(x, lambda = lambda)
  qnorm(p)
}

#' Convert a normal vector to a truncated normal distribution
#' @param x Numeric vector.
#' @param min Lower truncation bound.
#' @param max Upper truncation bound.
#' @param mu  Mean of the underlying normal.
#' @param sd  SD of the underlying normal.
#' @return Numeric vector truncated to \[min, max\].
#' @export
#' @examples
#' norm2trunc(rnorm(200), min = 0, max = 10)
norm2trunc <- function(x, min = -Inf, max = Inf, mu = 0, sd = 1) {
  p <- pnorm(x)
  lo <- pnorm(min, mu, sd)
  hi <- pnorm(max, mu, sd)
  p_scaled <- lo + p * (hi - lo)
  p_scaled <- pmin(pmax(p_scaled, lo + 1e-9), hi - 1e-9)
  qnorm(p_scaled, mu, sd)
}

#' @rdname norm2trunc
#' @export
trunc2norm <- function(x, min = -Inf, max = Inf, mu = 0, sd = 1) {
  p <- pnorm(x, mu, sd)
  lo <- pnorm(min, mu, sd)
  hi <- pnorm(max, mu, sd)
  p_unscaled <- (p - lo) / (hi - lo)
  qnorm(p_unscaled)
}

#' Convert a normal vector to a Uniform distribution
#' @param x Numeric vector.
#' @param min Lower bound (default `0`).
#' @param max Upper bound (default `1`).
#' @return Numeric vector uniform on \[min, max\].
#' @export
#' @examples
#' norm2unif(rnorm(100), min = 1, max = 10)
norm2unif <- function(x, min = 0, max = 1) {
  p <- pnorm(x)
  qunif(p, min = min, max = max)
}

#' @rdname norm2unif
#' @export
unif2norm <- function(x, min = 0, max = 1) {
  p <- punif(x, min = min, max = max)
  qnorm(p)
}

#' Convert a normal vector to normal (identity; for API consistency)
#' @param x Numeric vector.
#' @param mu Target mean.
#' @param sd Target SD.
#' @return Rescaled numeric vector.
#' @export
norm2norm <- function(x, mu = 0, sd = 1) {
  as.numeric(scale(x)) * sd + mu
}

# ============================================================
# Likert distribution
# ============================================================

#' Convert a normal vector to Likert-scale responses
#'
#' Bins a normal vector into ordered integer categories (Likert items) using
#' equal-probability cut-points.
#'
#' @param x Numeric vector (raw or standard-normal scores).
#' @param n Integer. Number of Likert categories (default `5`).
#' @param mu Mean of the latent normal before binning (default `0`).
#' @param sd SD of the latent normal before binning (default `1`).
#' @return An ordered integer vector with values in `1:n`.
#' @export
#' @examples
#' x <- rnorm(200)
#' y <- norm2likert(x, n = 7)
#' table(y)
norm2likert <- function(x, n = 5, mu = 0, sd = 1) {
  breaks <- qnorm(seq(0, 1, length.out = n + 1), mean = mu, sd = sd)
  breaks[c(1, n + 1)] <- c(-Inf, Inf)
  as.integer(cut(x, breaks = breaks, labels = FALSE))
}

#' Simulate random Likert-scale data
#'
#' Generates Likert responses by drawing from a normal distribution and
#' binning via [norm2likert()]. Supports correlated multi-item scales.
#'
#' @param n Integer. Number of observations.
#' @param items Integer. Number of items (default `1`).
#' @param likert_n Integer. Number of Likert categories (default `5`).
#' @param mu Numeric scalar or vector of latent means (one per item).
#' @param sd Numeric scalar or vector of latent SDs.
#' @param r Correlation structure among items (scalar, triangle, or matrix).
#' @param seed Optional integer seed.
#' @return A data frame (or integer vector if `items = 1`) of Likert responses.
#' @export
#' @examples
#' # Single item, 100 respondents
#' rlikert(100, likert_n = 7)
#'
#' # 3-item scale, moderately correlated
#' df <- rlikert(200, items = 3, r = .5, likert_n = 5)
#' head(df)
rlikert <- function(n, items = 1, likert_n = 5, mu = 0, sd = 1, r = 0, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  mu <- recycle_vec(mu, items, "mu")
  sd <- recycle_vec(sd, items, "sd")

  normals <- rnorm_multi(n, vars = items, mu = mu, sd = sd, r = r)
  out     <- lapply(seq_len(items), function(i) norm2likert(normals[[i]], n = likert_n))
  out_df  <- as.data.frame(setNames(out, paste0("item", seq_len(items))))
  if (items == 1) return(out_df[[1]])
  out_df
}

#' Density function for the Likert distribution
#' @param x Integer values at which to evaluate.
#' @param n Number of categories.
#' @param mu Latent mean.
#' @param sd Latent SD.
#' @return Numeric vector of probabilities.
#' @export
dlikert <- function(x, n = 5, mu = 0, sd = 1) {
  breaks <- qnorm(seq(0, 1, length.out = n + 1), mean = mu, sd = sd)
  breaks[c(1, n + 1)] <- c(-Inf, Inf)
  vapply(x, function(xi) {
    if (!xi %in% seq_len(n)) return(0)
    pnorm(breaks[xi + 1], mu, sd) - pnorm(breaks[xi], mu, sd)
  }, numeric(1))
}

#' CDF for the Likert distribution
#' @inheritParams dlikert
#' @return Cumulative probabilities.
#' @export
plikert <- function(x, n = 5, mu = 0, sd = 1) {
  cumsum(dlikert(seq_len(n), n, mu, sd))[pmin(pmax(as.integer(x), 0), n)]
}

#' Quantile function for the Likert distribution
#' @param p Probabilities.
#' @inheritParams dlikert
#' @return Integer quantiles.
#' @export
qlikert <- function(p, n = 5, mu = 0, sd = 1) {
  cum_p <- c(0, cumsum(dlikert(seq_len(n), n, mu, sd)))
  vapply(p, function(pi) {
    idx <- findInterval(pi, cum_p, rightmost.closed = TRUE)
    as.integer(pmin(pmax(idx, 1), n))
  }, integer(1))
}
