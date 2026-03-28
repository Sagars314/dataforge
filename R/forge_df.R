#' Simulate New Data from an Existing Data Frame
#'
#' @description
#' Given an existing data frame, `forge_df()` estimates the multivariate
#' normal parameters (means, SDs, correlations) for the numeric columns,
#' stratified by optional grouping (between-subject) columns, and generates a
#' new data frame with the same structure.
#'
#' @param df A data frame containing the source data.
#' @param n Integer. Number of observations to simulate per group. Defaults to
#'   the original group size.
#' @param between Character vector of column names to use as grouping
#'   (between-subject) variables.
#' @param dv Character vector of numeric column names to simulate. Defaults to
#'   all numeric columns not listed in `between`.
#' @param id Character. Name for the generated ID column. Set to `NULL` to
#'   suppress. Default `"id"`.
#' @param empirical Logical. If `TRUE`, simulated data will have exactly the
#'   estimated parameters (means, SDs, correlations).
#' @param seed Integer or `NULL`. Random seed.
#'
#' @return A data frame with `n * n_groups` rows and the same columns as `df`.
#'
#' @examples
#' # Re-simulate iris with 30 obs per species
#' new_iris <- forge_df(iris, n = 30, between = "Species")
#' head(new_iris)
#' nrow(new_iris) # 90
#'
#' # Simulate from mtcars grouped by cylinders
#' new_mt <- forge_df(mtcars, n = 20, between = "cyl",
#'                    dv = c("mpg", "hp", "wt"))
#'
#' @export
forge_df <- function(df, n = NULL, between = NULL, dv = NULL,
                     id = "id", empirical = FALSE, seed = NULL) {
  if (!is.data.frame(df)) stop("`df` must be a data frame.", call. = FALSE)
  if (!is.null(seed)) set.seed(seed)

  # Identify DV columns
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  if (!is.null(between)) num_cols <- setdiff(num_cols, between)
  if (!is.null(dv)) {
    if (!all(dv %in% names(df))) stop("Some `dv` columns not found in `df`.", call. = FALSE)
    num_cols <- dv
  }
  if (length(num_cols) == 0) stop("No numeric columns to simulate.", call. = FALSE)

  # Split by between factors
  if (is.null(between)) {
    groups <- list(all = df)
  } else {
    if (!all(between %in% names(df)))
      stop("Some `between` columns not found in `df`.", call. = FALSE)
    groups <- split(df, df[between], drop = TRUE)
  }

  results <- lapply(seq_along(groups), function(gi) {
    g     <- groups[[gi]]
    gname <- names(groups)[[gi]]
    n_g   <- if (is.null(n)) nrow(g) else as.integer(n)

    # Estimate parameters
    sub   <- g[num_cols]
    mu_g  <- colMeans(sub, na.rm = TRUE)
    sd_g  <- apply(sub, 2, stats::sd, na.rm = TRUE)
    cr_g  <- cor(sub, use = "pairwise.complete.obs")

    # Simulate
    sim <- rnorm_forge(n_g,
                        vars      = length(num_cols),
                        mu        = mu_g,
                        sd        = sd_g,
                        r         = cr_g,
                        var_names = num_cols,
                        empirical = empirical)

    # Re-attach between columns
    if (!is.null(between)) {
      btw_vals <- g[1, between, drop = FALSE]
      sim <- cbind(btw_vals[rep(1, n_g), , drop = FALSE], sim,
                   row.names = NULL, stringsAsFactors = FALSE)
    }

    # ID column
    if (!is.null(id)) {
      sim <- cbind(data.frame(id = seq_len(n_g), stringsAsFactors = FALSE),
                   sim)
    }
    sim
  })

  out <- do.call(rbind, results)
  rownames(out) <- NULL

  if (!is.null(id)) out[[id]] <- seq_len(nrow(out))

  out
}
