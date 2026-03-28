#' Simulate Multiple Correlated Normal Variables
#'
#' @description
#' Generate a data frame of correlated normally distributed variables using the
#' Cholesky decomposition of the correlation matrix.
#'
#' @param n Integer. Number of observations.
#' @param vars Integer. Number of variables. Inferred from `mu`, `sd`, or
#'   `r` when possible.
#' @param mu Numeric vector of means (recycled to length `vars`).
#' @param sd Numeric vector of standard deviations (recycled to length `vars`).
#' @param r Correlation specification — a scalar, lower-triangle vector, or
#'   full correlation matrix. See \code{\link{cormat}}.
#' @param var_names Character vector of column names. Defaults to
#'   `"V1"`, `"V2"`, …
#' @param empirical Logical. If `TRUE`, the generated data will have *exactly*
#'   the specified means, SDs, and correlations (as opposed to expected values).
#' @param as_matrix Logical. If `TRUE`, returns a matrix instead of a
#'   data frame.
#'
#' @return A data frame (or matrix) with `n` rows and `vars` columns.
#'
#' @examples
#' # Three correlated variables, all pairwise r = 0.6
#' df <- rnorm_forge(n = 100, vars = 3, mu = c(10, 20, 30),
#'                   sd = c(1, 2, 3), r = 0.6)
#' cor(df)
#'
#' # Using a lower-triangle vector for correlations
#' df2 <- rnorm_forge(100, vars = 3,
#'                    r = c(0.2, 0.5, 0.3),
#'                    var_names = c("x", "y", "z"))
#' round(cor(df2), 2)
#'
#' @export
rnorm_forge <- function(n = 100, vars = NULL, mu = 0, sd = 1, r = 0,
                        var_names = NULL, empirical = FALSE,
                        as_matrix = FALSE) {
  # Infer number of variables
  if (is.null(vars)) {
    vars <- max(length(mu), length(sd),
                if (is.matrix(r)) nrow(r) else 1L,
                if (length(r) > 1 && !is.matrix(r))
                  round((1 + sqrt(1 + 8 * length(r))) / 2) else 1L)
  }
  if (vars < 2) stop("`vars` must be >= 2.", call. = FALSE)

  mu <- rep_len(mu, vars)
  sd <- rep_len(sd, vars)

  # Build correlation matrix
  cm <- cormat(r, n = vars)

  # Default variable names
  if (is.null(var_names)) {
    var_names <- if (!is.null(rownames(cm))) rownames(cm) else paste0("V", seq_len(vars))
  }
  rownames(cm) <- colnames(cm) <- var_names

  # Cholesky decomposition
  # Use nearPD if not positive definite
  if (!is_pos_def(cm)) {
    cm <- as.matrix(Matrix::nearPD(cm, corr = TRUE)$mat)
    warning("Correlation matrix was adjusted to be positive definite.", call. = FALSE)
  }
  ch <- chol(cm)

  if (empirical) {
    # Generate standard normal then orthogonalise to get exact correlations
    z <- matrix(stats::rnorm(n * vars), nrow = n)
    z <- scale(z)  # centre & scale
    z <- z %*% solve(chol(var(z))) %*% ch
    x <- sweep(sweep(z, 2, sd, "*"), 2, mu, "+")
  } else {
    z <- matrix(stats::rnorm(n * vars), nrow = n)
    x <- sweep(sweep(z %*% ch, 2, sd, "*"), 2, mu, "+")
  }

  colnames(x) <- var_names
  if (as_matrix) return(x)
  as.data.frame(x)
}

#' Make a Normal Vector Correlated with Existing Vectors
#'
#' @description
#' Given one or more existing numeric vectors, generate a new normally
#' distributed vector with specified correlations to each of the existing
#' vectors (and a specified mean and SD).
#'
#' @param x A numeric vector, or a data frame / matrix whose columns serve as
#'   the existing variables.
#' @param r Desired correlation(s) between the new vector and `x`. If `x` has
#'   multiple columns, supply one value per column.
#' @param n Integer. Number of observations. Inferred from `x` when omitted.
#' @param mu Mean of the new variable. Default 0.
#' @param sd SD of the new variable. Default 1.
#' @param empirical Logical. Enforce exact correlation in the sample.
#'
#' @return A numeric vector of length `n`.
#'
#' @examples
#' x <- rnorm(200, mean = 5, sd = 2)
#' y <- rnorm_pre(x, r = 0.7, mu = 10, sd = 3)
#' cor(x, y)
#'
#' @export
rnorm_pre <- function(x, r = 0, n = NULL, mu = 0, sd = 1, empirical = FALSE) {
  if (is.data.frame(x) || is.matrix(x)) {
    x <- as.matrix(x)
    if (is.null(n)) n <- nrow(x)
    r <- rep_len(r, ncol(x))
    # Build block correlation matrix [x_vars | new_var]
    k <- ncol(x)
    existing_cor <- cor(x)
    full_cor <- rbind(
      cbind(existing_cor, matrix(r, nrow = k, ncol = 1)),
      c(r, 1)
    )
    new_data <- rnorm_forge(n, vars = k + 1, mu = c(colMeans(x), mu),
                             sd = c(apply(x, 2, sd), sd),
                             r = full_cor, empirical = empirical)
    return(new_data[[k + 1]])
  }

  if (is.null(n)) n <- length(x)
  if (length(r) != 1) stop("`r` must be a scalar when `x` is a vector.", call. = FALSE)
  if (abs(r) > 1) stop("`r` must be in [-1, 1].", call. = FALSE)

  if (empirical) {
    x_s <- scale(x)
    e <- stats::rnorm(n)
    e_s <- scale(e)
    y_s <- r * x_s + sqrt(1 - r^2) * e_s
    y <- as.vector(y_s) * sd + mu
  } else {
    x_s <- (x - mean(x)) / stats::sd(x)
    e <- stats::rnorm(n)
    y_s <- r * x_s + sqrt(1 - r^2) * e
    y <- y_s * sd + mu
  }
  y
}
