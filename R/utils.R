# ============================================================
# Internal utilities and global options
# ============================================================

.dataforge_env <- new.env(parent = emptyenv())
.dataforge_env$options <- list(
  plot  = TRUE,
  verbose = TRUE,
  sep = "_"
)

#' Set or get global dataforge options
#'
#' @param ... Named option values to set. If empty, returns current options.
#'   Valid options:
#'   - `plot` (logical): whether simulation functions auto-plot (default `TRUE`)
#'   - `verbose` (logical): print informational messages (default `TRUE`)
#'   - `sep` (character): separator used to construct column names (default `"_"`)
#'
#' @return A named list of current options (invisibly when setting).
#' @export
#' @examples
#' # Get all options
#' dataforge_options()
#'
#' # Disable auto-plotting
#' dataforge_options(plot = FALSE)
#'
#' # Reset
#' dataforge_options(plot = TRUE, verbose = TRUE, sep = "_")
dataforge_options <- function(...) {
  args <- list(...)
  valid <- c("plot", "verbose", "sep")
  if (length(args) == 0) {
    return(.dataforge_env$options)
  }
  bad <- setdiff(names(args), valid)
  if (length(bad)) {
    stop("Unknown dataforge option(s): ", paste(bad, collapse = ", "),
         ". Valid options: ", paste(valid, collapse = ", "), call. = FALSE)
  }
  for (nm in names(args)) {
    .dataforge_env$options[[nm]] <- args[[nm]]
  }
  invisible(.dataforge_env$options)
}

# ---- internal helpers -------------------------------------------------------

.opt <- function(x) .dataforge_env$options[[x]]

.msg <- function(...) {
  if (isTRUE(.opt("verbose"))) message(...)
}

#' Make a correlation matrix from a vector or scalar
#'
#' @param r Correlation(s). Either a single value (applied to all pairs),
#'   a named vector, or a full \eqn{n \times n} matrix.
#' @param n Number of variables.
#' @param vars Character vector of variable names.
#' @return A symmetric positive-definite correlation matrix.
#' @keywords internal
make_cormat <- function(r, n, vars = NULL) {
  if (is.matrix(r)) {
    if (!all(dim(r) == n)) stop("Correlation matrix must be ", n, "x", n)
    mat <- r
  } else if (length(r) == 1) {
    mat <- matrix(r, n, n)
    diag(mat) <- 1
  } else if (length(r) == n * (n - 1) / 2) {
    mat <- matrix(0, n, n)
    mat[lower.tri(mat)] <- r
    mat <- mat + t(mat)
    diag(mat) <- 1
  } else {
    stop("r must be a scalar, a lower-triangle vector of length ",
         n * (n - 1) / 2, ", or a ", n, "x", n, " matrix.", call. = FALSE)
  }
  if (!is.null(vars)) {
    rownames(mat) <- colnames(mat) <- vars
  }
  # Check positive-definiteness
  ev <- eigen(mat, only.values = TRUE)$values
  if (any(ev < 0)) {
    warning("Correlation matrix is not positive definite; ",
            "nearest PD matrix used.", call. = FALSE)
    mat <- as.matrix(Matrix::nearPD(mat, corr = TRUE, keepDiag = TRUE)$mat)
    if (!is.null(vars)) rownames(mat) <- colnames(mat) <- vars
  }
  mat
}

#' Check whether a matrix is positive definite
#' @param m A square numeric matrix.
#' @return Logical scalar.
#' @export
#' @examples
#' is_pos_def(diag(3))
#' is_pos_def(matrix(c(1,.9,.9,1), 2))
is_pos_def <- function(m) {
  if (!is.matrix(m) || nrow(m) != ncol(m)) return(FALSE)
  ev <- tryCatch(eigen(m, only.values = TRUE)$values, error = function(e) NA)
  all(ev > 0)
}

#' Make unique variable-pair labels for correlations
#'
#' @param vars Character vector of variable names.
#' @param sep Separator between pair names.
#' @return A character vector of pair labels.
#' @export
#' @examples
#' unique_pairs(c("A", "B", "C"))
unique_pairs <- function(vars, sep = ":") {
  pairs <- combn(vars, 2, simplify = FALSE)
  vapply(pairs, paste, character(1), collapse = sep)
}

#' Make a correlation matrix from upper/lower triangle values
#'
#' @param triangle Numeric vector of the upper (or lower) triangle values,
#'   supplied in row-major order.
#' @param n Number of variables. Inferred from `triangle` if not supplied.
#' @return A symmetric correlation matrix with 1s on the diagonal.
#' @export
#' @examples
#' cormat_from_triangle(c(.3, .4, .2))
cormat_from_triangle <- function(triangle, n = NULL) {
  if (is.null(n)) {
    # Solve k = n*(n-1)/2 for n
    k <- length(triangle)
    n <- (1 + sqrt(1 + 8 * k)) / 2
    if (n != round(n)) stop("Length of triangle vector does not match any n.", call. = FALSE)
    n <- as.integer(n)
  }
  make_cormat(triangle, n)
}

#' Make participant / item IDs
#'
#' @param n Number of IDs to generate.
#' @param prefix Character prefix (default `"S"`).
#' @param digits Minimum number of digits (zero-padded).
#' @return A character vector.
#' @export
#' @examples
#' make_id(10)
#' make_id(5, prefix = "item", digits = 3)
make_id <- function(n, prefix = "S", digits = NULL) {
  if (is.null(digits)) digits <- nchar(as.character(n))
  ids <- formatC(seq_len(n), width = digits, flag = "0")
  paste0(prefix, ids)
}

#' Expand a named list of factor levels into all combinations
#' @param factors Named list of factor level vectors.
#' @return A data.frame with one row per combination.
#' @keywords internal
expand_factors <- function(factors) {
  if (length(factors) == 0) return(data.frame())
  do.call(expand.grid, c(rev(factors), stringsAsFactors = FALSE))[ , rev(seq_along(factors)), drop = FALSE]
}

#' Recycle or check length of a vector
#' @keywords internal
recycle_vec <- function(x, n, label = "vector") {
  if (length(x) == 1) return(rep(x, n))
  if (length(x) != n) stop(label, " must have length 1 or ", n, call. = FALSE)
  x
}
