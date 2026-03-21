#' Simulate multiple correlated normal variables
#'
#' Generates a data frame of \eqn{n} observations of \eqn{k} correlated normal
#' variables with specified means, standard deviations, and a correlation
#' structure.
#'
#' @param n Integer. Number of observations.
#' @param vars Integer or character vector. Number of variables (if integer) or
#'   their names (if character).
#' @param mu Numeric vector of means (recycled to length \code{vars}).
#' @param sd Numeric vector of standard deviations (recycled to length
#'   \code{vars}).
#' @param r Correlation structure: a scalar (same correlation for all pairs), a
#'   lower-triangle vector, or a \eqn{k \times k} correlation matrix.
#' @param empirical Logical. If \code{TRUE} the sample statistics will exactly
#'   match \code{mu}, \code{sd}, and \code{r} (default \code{FALSE}).
#' @param as_matrix Logical. Return a matrix instead of a data frame
#'   (default \code{FALSE}).
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A data frame (or matrix) with \code{n} rows and \code{k} columns.
#' @export
#' @examples
#' # Three correlated variables
#' df <- rnorm_multi(100, vars = c("x", "y", "z"),
#'                   mu = c(0, 10, 5), sd = c(1, 2, 1.5),
#'                   r = c(.3, .5, .2))
#' cor(df)
#'
#' # Empirical moments
#' df2 <- rnorm_multi(50, vars = 2, mu = c(5, 10), sd = c(1, 2),
#'                    r = .8, empirical = TRUE)
#' cor(df2)
rnorm_multi <- function(n, vars = 3, mu = 0, sd = 1, r = 0,
                        empirical = FALSE, as_matrix = FALSE, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # Resolve variable names / count
  if (is.character(vars)) {
    k    <- length(vars)
    nms  <- vars
  } else {
    k   <- as.integer(vars)
    nms <- paste0("V", seq_len(k))
  }

  mu <- recycle_vec(mu, k, "mu")
  sd <- recycle_vec(sd, k, "sd")

  R <- make_cormat(r, k, nms)

  # Cholesky-based simulation
  Sigma <- diag(sd) %*% R %*% diag(sd)
  raw   <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma, empirical = empirical)
  colnames(raw) <- nms

  if (as_matrix) return(raw)
  as.data.frame(raw)
}

#' Simulate multiple correlated variables from arbitrary distributions
#'
#' Uses the NORTA (Normal-to-Anything) method: generates correlated normals
#' then transforms each margin to the desired distribution.
#'
#' @param n Integer. Number of observations.
#' @param dist Character vector of distribution names. Supported:
#'   `"norm"`, `"beta"`, `"binom"`, `"gamma"`, `"nbinom"`, `"pois"`,
#'   `"unif"`, `"likert"`.
#' @param params A list of lists; each inner list is passed as extra arguments
#'   to the corresponding distribution's parameter resolver (e.g.
#'   `list(list(size=5, prob=.5), list(shape1=2, shape2=5))`).
#' @param mu Numeric vector of means (used only when `dist = "norm"`).
#' @param sd Numeric vector of SDs (used only when `dist = "norm"`).
#' @param r Correlation structure (same semantics as [rnorm_multi()]).
#' @param empirical Logical (default \code{FALSE}).
#' @param seed Optional integer seed.
#' @return A data frame.
#' @export
#' @examples
#' df <- rmulti(200,
#'              dist = c("norm", "pois", "binom"),
#'              params = list(list(mu=5, sd=1), list(lambda=3), list(size=10, prob=.5)),
#'              r = .4)
#' head(df)
rmulti <- function(n, dist = "norm", params = NULL,
                   mu = 0, sd = 1, r = 0,
                   empirical = FALSE, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  k <- length(dist)
  if (is.null(params)) params <- vector("list", k)

  # Build per-variable mu/sd for the normal backbone
  mu_v <- recycle_vec(mu, k, "mu")
  sd_v <- recycle_vec(sd, k, "sd")
  for (i in seq_len(k)) {
    if (dist[i] == "norm") {
      if (!is.null(params[[i]]$mu))  mu_v[i] <- params[[i]]$mu
      if (!is.null(params[[i]]$sd))  sd_v[i] <- params[[i]]$sd
    }
  }

  # Generate normal backbone
  normals <- rnorm_multi(n, vars = k, mu = mu_v, sd = sd_v, r = r,
                         empirical = empirical)

  # Convert each column
  out <- normals
  for (i in seq_len(k)) {
    p <- params[[i]]
    d <- dist[i]
    out[[i]] <- switch(d,
      norm    = normals[[i]],
      beta    = norm2beta(normals[[i]], alpha = p$alpha %||% p$shape1 %||% 2,
                         beta  = p$beta  %||% p$shape2 %||% 5),
      binom   = norm2binom(normals[[i]], size = p$size %||% 1,
                           prob = p$prob %||% 0.5),
      gamma   = norm2gamma(normals[[i]], shape = p$shape %||% 2,
                           rate  = p$rate  %||% 1),
      nbinom  = norm2nbinom(normals[[i]], size = p$size %||% 5,
                            prob = p$prob %||% 0.5),
      pois    = norm2pois(normals[[i]], lambda = p$lambda %||% 1),
      unif    = norm2unif(normals[[i]], min = p$min %||% 0, max = p$max %||% 1),
      likert  = norm2likert(normals[[i]], n = p$n %||% 5),
      stop("Unknown distribution: ", d, call. = FALSE)
    )
  }
  names(out) <- paste0("V", seq_len(k))
  out
}

# null coalescing
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Create a normal vector correlated to one or more existing vectors
#'
#' Generates a new normal variable with specified mean and SD that has (exactly
#' or approximately) a given correlation with existing variables.
#'
#' @param x A numeric vector or data frame of existing variables.
#' @param r Target correlation(s). If `x` has multiple columns, a vector of
#'   the same length specifying the desired correlation with each.
#' @param mu Target mean (default `0`).
#' @param sd Target SD (default `1`).
#' @param empirical Logical (default `TRUE`). Produce exact sample correlation?
#' @param seed Optional integer seed.
#' @return A numeric vector of length `nrow(x)` (or `length(x)`).
#' @export
#' @examples
#' x <- rnorm(100)
#' y <- rnorm_pre(x, r = .6)
#' cor(x, y)
rnorm_pre <- function(x, r = 0, mu = 0, sd = 1, empirical = TRUE, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (is.data.frame(x)) x <- as.matrix(x)
  if (is.vector(x))     x <- matrix(x, ncol = 1)

  n  <- nrow(x)
  k  <- ncol(x)
  r  <- recycle_vec(r, k, "r")

  # Full correlation matrix: existing vars + new var
  R_full <- diag(k + 1)
  R_full[1:k, k + 1] <- r
  R_full[k + 1, 1:k] <- r
  # Fill inter-variable correlations from data
  if (k > 1) {
    existing_r <- cor(x)
    R_full[1:k, 1:k] <- existing_r
  }
  R_full <- make_cormat(R_full, k + 1)

  # Conditional distribution: new | existing
  R11 <- R_full[1:k, 1:k, drop = FALSE]
  r12 <- R_full[1:k, k + 1, drop = FALSE]
  R11_inv <- solve(R11)
  cond_mean_coefs <- t(r12) %*% R11_inv
  cond_var <- 1 - as.numeric(t(r12) %*% R11_inv %*% r12)
  if (cond_var < 0) cond_var <- 0

  # Standardise x
  x_std <- scale(x)
  cond_means <- as.vector(x_std %*% t(cond_mean_coefs))
  noise <- rnorm(n, 0, sqrt(cond_var))
  z <- cond_means + noise

  if (empirical) {
    z <- scale(z)[,1]  # unit normal
  }
  z * sd + mu
}
