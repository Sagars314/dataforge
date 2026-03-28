#' Build a Correlation Matrix
#'
#' @description
#' Construct a valid symmetric correlation matrix from a scalar, vector, or
#' existing matrix. The result is always positive semi-definite.
#'
#' @param r A scalar (applied to all off-diagonal entries), a vector of the
#'   lower triangle (row-wise), or an existing square matrix.
#' @param n Integer. Number of variables. Required when `r` is a scalar.
#' @param var_names Character vector of variable names. Used as row/column
#'   names of the returned matrix.
#'
#' @return A symmetric `n x n` correlation matrix with ones on the diagonal.
#'
#' @examples
#' # Scalar: all pairs correlated at 0.5
#' cormat(0.5, n = 4)
#'
#' # Vector of lower triangle values
#' cormat(c(0.3, 0.5, 0.4, 0.2, 0.6, 0.1), n = 4)
#'
#' # With variable names
#' cormat(0.4, n = 3, var_names = c("A", "B", "C"))
#'
#' @export
cormat <- function(r = 0, n = NULL, var_names = NULL) {
  # ---- scalar ----
  if (is.numeric(r) && length(r) == 1) {
    if (is.null(n)) stop("`n` must be supplied when `r` is a scalar.", call. = FALSE)
    if (r < -1 || r > 1) stop("`r` must be in [-1, 1].", call. = FALSE)
    m <- matrix(r, nrow = n, ncol = n)
    diag(m) <- 1
  } else if (is.matrix(r)) {
    # ---- already a matrix ----
    n <- nrow(r)
    m <- r
    diag(m) <- 1
    if (!isSymmetric(m)) stop("Supplied matrix is not symmetric.", call. = FALSE)
  } else {
    # ---- lower-triangle vector ----
    expected_n <- (1 + sqrt(1 + 8 * length(r))) / 2
    if (is.null(n)) n <- round(expected_n)
    n_tri <- n * (n - 1) / 2
    if (length(r) != n_tri)
      stop(sprintf("For n = %d variables, the lower triangle needs %d values, but %d were supplied.",
                   n, n_tri, length(r)), call. = FALSE)
    m <- diag(n)
    m[lower.tri(m)] <- r
    m[upper.tri(m)] <- t(m)[upper.tri(m)]
  }

  # Check positive definiteness
  eigs <- eigen(m, only.values = TRUE)$values
  if (any(eigs < -1e-8))
    warning("The correlation matrix is not positive definite. ",
            "Results may be unreliable.", call. = FALSE)

  if (!is.null(var_names)) {
    if (length(var_names) != n)
      stop("Length of `var_names` must equal `n`.", call. = FALSE)
    rownames(m) <- colnames(m) <- var_names
  }

  m
}

#' Check Whether a Matrix is Positive Definite
#'
#' @param m A square matrix.
#' @return Logical scalar.
#' @export
is_pos_def <- function(m) {
  if (!is.matrix(m)) stop("`m` must be a matrix.", call. = FALSE)
  all(eigen(m, only.values = TRUE)$values > 0)
}

#' Get the Limits for a Missing Correlation to Keep a Matrix Positive Definite
#'
#' @description
#' Given a partial correlation matrix with one `NA` entry, returns the range
#' of values that entry could take while keeping the matrix positive definite.
#'
#' @param m A square numeric matrix with exactly one `NA` (and its symmetric
#'   counterpart). Ones on the diagonal are assumed.
#' @return A named numeric vector with elements `min` and `max`.
#'
#' @examples
#' m <- matrix(c(1, 0.5, NA,
#'               0.5, 1, 0.3,
#'               NA, 0.3, 1), nrow = 3)
#' pos_def_limits(m)
#'
#' @export
pos_def_limits <- function(m) {
  if (!is.matrix(m)) stop("`m` must be a matrix.", call. = FALSE)

  # Find NA positions (ignore diagonal)
  na_pos <- which(is.na(m) & row(m) != col(m), arr.ind = TRUE)
  if (nrow(na_pos) == 0)
    stop("No off-diagonal NA found in matrix.", call. = FALSE)

  # Take the first NA (upper or lower triangle)
  i <- na_pos[1, 1]; j <- na_pos[1, 2]

  vals <- seq(-0.9999, 0.9999, by = 0.001)
  ok <- vapply(vals, function(v) {
    mc <- m
    mc[i, j] <- v
    mc[j, i] <- v
    # Replace remaining NAs with 0 for the eigenvalue check
    mc[is.na(mc)] <- 0
    diag(mc) <- 1
    all(eigen(mc, only.values = TRUE)$values > 0)
  }, logical(1))

  if (!any(ok)) {
    warning("No valid correlation found in [-1, 1] for the NA position.", call. = FALSE)
    return(c(min = NA_real_, max = NA_real_))
  }

  c(min = min(vals[ok]), max = max(vals[ok]))
}

# Make unique lower-triangle variable name pairs
.unique_pairs <- function(nms) {
  n <- length(nms)
  pairs <- vector("character", n * (n - 1) / 2)
  k <- 0
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      k <- k + 1
      pairs[k] <- paste0(nms[i], ":", nms[j])
    }
  }
  pairs
}
