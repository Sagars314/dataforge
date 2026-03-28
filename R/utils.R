#' Get Distributional Parameters from a Data Table
#'
#' @description
#' Compute observed means, standard deviations, and pairwise correlations for
#' numeric columns, optionally stratified by grouping columns.
#'
#' @param data A data frame.
#' @param between Character vector of grouping (between-subject) column names.
#' @param within Character vector of within-subject column names to summarise.
#'   Defaults to all numeric columns not in `between`.
#' @param digits Integer. Number of decimal places for rounding. Default 3.
#' @param usekurtosis Logical. Include skewness and kurtosis? Default `FALSE`.
#'
#' @return A data frame with one row per group (or one row if no grouping) and
#'   columns: `n`, `var`, `mean`, `sd`, plus pairwise correlations.
#'
#' @examples
#' df <- forge_design(
#'   within  = list(time = c("pre","post")),
#'   between = list(group = c("A","B")),
#'   n = 50, mu = c(10,12,10,15), sd = 2, r = 0.6,
#'   plot = FALSE
#' )
#' get_params(df, between = "group")
#'
#' @export
get_params <- function(data, between = NULL, within = NULL,
                       digits = 3, usekurtosis = FALSE) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)

  num_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  if (!is.null(between)) num_cols <- setdiff(num_cols, between)
  if (!is.null(within))  num_cols <- intersect(num_cols, within)
  if ("id" %in% num_cols) num_cols <- setdiff(num_cols, "id")

  summarise_group <- function(g) {
    sub <- g[num_cols]
    cr  <- cor(sub, use = "pairwise.complete.obs")
    pairs <- .unique_pairs(num_cols)

    basic <- data.frame(
      n    = nrow(g),
      var  = num_cols,
      mean = round(colMeans(sub, na.rm = TRUE), digits),
      sd   = round(apply(sub, 2, stats::sd, na.rm = TRUE), digits),
      stringsAsFactors = FALSE
    )

    if (length(num_cols) > 1) {
      cor_df <- as.data.frame(t(round(cr[lower.tri(cr)], digits)))
      names(cor_df) <- pairs
      basic <- cbind(basic, cor_df)
    }
    basic
  }

  if (is.null(between)) {
    return(summarise_group(data))
  }

  if (!all(between %in% names(data)))
    stop("Some `between` columns not found in `data`.", call. = FALSE)

  groups <- split(data, data[between], drop = TRUE)
  out <- lapply(names(groups), function(gn) {
    df_g <- summarise_group(groups[[gn]])
    btw  <- strsplit(gn, "\\.")[[1]]
    prefix <- setNames(as.data.frame(as.list(btw), stringsAsFactors = FALSE), between)
    cbind(prefix[rep(1, nrow(df_g)), , drop = FALSE], df_g,
          row.names = NULL)
  })
  do.call(rbind, out)
}

#' Alias for get_params
#' @inheritParams get_params
#' @export
check_sim_stats <- get_params

#' Make a Unique ID Column
#'
#' @description
#' Generate a vector of padded character IDs suitable for use as a subject or
#' item identifier.
#'
#' @param n Integer. Number of IDs.
#' @param prefix Character. Prefix string. Default `"S"`.
#' @param digits Integer or `NULL`. If an integer, zero-pad to this width.
#'   If `NULL`, pad to the natural width of `n`.
#'
#' @return Character vector of length `n`.
#'
#' @examples
#' make_id(20)
#' make_id(200, prefix = "subj_", digits = 4)
#'
#' @export
make_id <- function(n, prefix = "S", digits = NULL) {
  if (is.null(digits)) digits <- nchar(as.character(n))
  sprintf(paste0(prefix, "%0", digits, "d"), seq_len(n))
}

#' Simulate Missing Data
#'
#' @description
#' Introduce missing values (`NA`) into a data frame at a specified rate,
#' either completely at random (MCAR) or within specified columns.
#'
#' @param data A data frame.
#' @param prop Numeric. Proportion of values to set to `NA`. Default 0.1.
#' @param cols Character vector of column names to apply missingness to.
#'   Defaults to all numeric columns.
#' @param seed Integer or `NULL`. Random seed.
#'
#' @return The data frame with some values replaced by `NA`.
#'
#' @examples
#' df <- forge_design(within = list(t = c("pre","post")), n = 30,
#'                    mu = c(5, 7), sd = 1, r = .5, plot = FALSE)
#' df_miss <- messy(df, prop = 0.1)
#' mean(is.na(df_miss))
#'
#' @export
messy <- function(data, prop = 0.1, cols = NULL, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (is.null(cols)) cols <- names(data)[vapply(data, is.numeric, logical(1))]

  for (col in cols) {
    n_miss <- round(prop * nrow(data))
    idx    <- sample(seq_len(nrow(data)), n_miss)
    data[idx, col] <- NA
  }
  data
}

#' Check / Validate a Design Specification
#'
#' @description
#' Validates that a design specification (within/between lists, n, mu, sd, r)
#' is internally consistent and returns a summary. Issues warnings for
#' potential problems (e.g., non-positive-definite correlation matrix, unequal
#' group sizes).
#'
#' @param within Named list of within factors (see \code{\link{forge_design}}).
#' @param between Named list of between factors.
#' @param n Scalar or vector of group sizes.
#' @param mu Cell means.
#' @param sd Cell standard deviations.
#' @param r Correlation specification.
#'
#' @return Invisibly returns `TRUE` if the design is valid, or throws an error.
#'
#' @export
check_design <- function(within = list(), between = list(),
                         n = 100, mu = 0, sd = 1, r = 0) {
  within  <- if (length(within) == 0) list() else within
  between <- if (length(between) == 0) list() else between

  n_within  <- if (length(within) == 0) 1L else prod(lengths(within))
  n_between <- if (length(between) == 0) 1L else prod(lengths(between))

  # Check mu length
  n_cells <- n_within * n_between
  if (!is.matrix(mu) && !is.data.frame(mu)) {
    if (length(mu) != 1 && length(mu) != n_cells)
      stop(sprintf("`mu` must have 1 or %d values; got %d.", n_cells, length(mu)),
           call. = FALSE)
  }

  # Check correlation matrix
  cm <- tryCatch(cormat(r, n = n_within), error = function(e) e)
  if (inherits(cm, "error"))
    stop("Invalid correlation specification: ", conditionMessage(cm), call. = FALSE)

  if (!is_pos_def(cm))
    warning("Correlation matrix is not positive definite. Results may be unreliable.")

  cat(sprintf(
    "Design OK:\n  Within cells : %d (%s)\n  Between cells: %d (%s)\n  Total cells  : %d\n",
    n_within,
    if (length(within) == 0) "—" else paste(sapply(within, length), collapse = " x "),
    n_between,
    if (length(between) == 0) "—" else paste(sapply(between, length), collapse = " x "),
    n_cells
  ))
  invisible(TRUE)
}
