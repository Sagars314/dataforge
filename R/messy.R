#' Introduce missing or erroneous values into a data frame
#'
#' Randomly replaces values in specified columns with `NA` (or another
#' placeholder), simulating messy real-world data.
#'
#' @param data A data frame.
#' @param cols Character vector of column names to add missingness to.
#'   Defaults to all columns.
#' @param prop Numeric scalar in \[0, 1\]. Proportion of values to make
#'   missing (default `0.1`).
#' @param missing_val Value to substitute for missing data (default `NA`).
#' @param seed Optional integer seed.
#'
#' @return A data frame with some values replaced by `missing_val`.
#' @export
#' @examples
#' df <- forge_design(between = list(g = c("A","B")), n = 20,
#'                    mu = c(0,1), sd = 1, plot = FALSE)
#' make_missing(df, cols = "y", prop = 0.15)
make_missing <- function(data, cols = NULL, prop = 0.1,
                         missing_val = NA, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (is.null(cols)) cols <- names(data)
  cols <- intersect(cols, names(data))
  if (length(cols) == 0) return(data)

  for (col in cols) {
    n_miss <- round(prop * nrow(data))
    idx    <- sample(nrow(data), n_miss, replace = FALSE)
    data[[col]][idx] <- missing_val
  }
  data
}

#' @rdname make_missing
#' @export
messy <- make_missing

#' Simulate duplicate rows
#'
#' Adds a specified number of duplicate rows drawn at random from the data.
#'
#' @param data A data frame.
#' @param n_dups Integer. Number of duplicate rows to add (default `5`).
#' @param seed Optional integer seed.
#' @return The data frame with duplicate rows appended.
#' @export
#' @examples
#' df <- data.frame(x = 1:10, y = rnorm(10))
#' add_duplicates(df, n_dups = 3)
add_duplicates <- function(data, n_dups = 5, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  idx  <- sample(nrow(data), n_dups, replace = TRUE)
  dups <- data[idx, , drop = FALSE]
  rbind(data, dups)
}

#' Add random outliers to numeric columns
#'
#' Replaces a small proportion of values with extreme values drawn from a
#' uniform distribution on \[`outlier_min`, `outlier_max`\].
#'
#' @param data A data frame.
#' @param cols Numeric columns to modify. Defaults to all numeric columns.
#' @param prop Proportion of values to replace with outliers (default `0.02`).
#' @param multiplier Multiplier beyond the column range for outlier magnitude
#'   (default `3`).
#' @param seed Optional integer seed.
#' @return Modified data frame.
#' @export
#' @examples
#' df <- data.frame(x = rnorm(100), y = rnorm(100))
#' df_messy <- add_outliers(df, prop = 0.05)
add_outliers <- function(data, cols = NULL, prop = 0.02,
                         multiplier = 3, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (is.null(cols)) {
    cols <- names(data)[vapply(data, is.numeric, logical(1))]
  }
  for (col in cols) {
    vals  <- data[[col]]
    rng   <- range(vals, na.rm = TRUE)
    span  <- diff(rng)
    n_out <- max(1, round(prop * length(vals)))
    idx   <- sample(length(vals), n_out, replace = FALSE)
    out_vals <- runif(n_out,
                      min = rng[1] - multiplier * span,
                      max = rng[2] + multiplier * span)
    data[[col]][idx] <- out_vals
  }
  data
}
