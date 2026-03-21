#' Simulate new data that matches the structure of an existing data frame
#'
#' Estimates the empirical means, standard deviations, and correlations of
#' numeric columns (optionally per group defined by `between`), then generates
#' new data with the same statistical structure using [rnorm_multi()].
#'
#' @param df A data frame to use as the template.
#' @param n Integer. Number of observations to simulate per group.
#' @param between Character vector of column names to treat as grouping
#'   (between-subjects) factors. These columns are preserved as-is.
#' @param within Character vector of column names to treat as numeric DV
#'   columns to be simulated. Defaults to all numeric columns not in
#'   `between`.
#' @param empirical Logical. Should simulated data match the template
#'   statistics exactly? (default `FALSE`)
#' @param seed Optional integer seed.
#'
#' @return A data frame with the same column structure as `df`, containing
#'   `n * <number of groups>` rows of simulated data.
#' @export
#'
#' @examples
#' # Simulate 100 obs per species from iris
#' new_iris <- forge_df(iris, n = 100, between = "Species")
#' summary(new_iris)
#'
#' # Simulate from mtcars without grouping
#' new_mt <- forge_df(mtcars[, c("mpg","hp","wt")], n = 50)
#' head(new_mt)
forge_df <- function(df, n = 100, between = NULL, within = NULL,
                     empirical = FALSE, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # Identify numeric columns to simulate
  all_num <- names(df)[vapply(df, is.numeric, logical(1))]
  if (is.null(within)) {
    within <- setdiff(all_num, between)
  }
  if (length(within) == 0) stop("No numeric columns to simulate.", call. = FALSE)

  if (is.null(between) || length(between) == 0) {
    # No grouping
    mu_v <- colMeans(df[, within, drop = FALSE], na.rm = TRUE)
    sd_v <- sapply(df[, within, drop = FALSE], sd, na.rm = TRUE)
    r_v  <- cor(df[, within, drop = FALSE], use = "pairwise.complete.obs")
    sim  <- rnorm_multi(n, vars = within, mu = mu_v, sd = sd_v, r = r_v,
                        empirical = empirical)
    # Re-attach non-numeric / non-simulated columns sampled at random
    extra <- setdiff(names(df), within)
    if (length(extra) > 0) {
      extra_df <- df[sample(nrow(df), n, replace = TRUE), extra, drop = FALSE]
      rownames(extra_df) <- NULL
      sim <- cbind(extra_df, sim)
    }
    return(sim)
  }

  # Grouped simulation
  groups <- unique(df[, between, drop = FALSE])
  results <- vector("list", nrow(groups))
  for (i in seq_len(nrow(groups))) {
    grp_filter <- Reduce(`&`, lapply(between, function(col) {
      df[[col]] == groups[[col]][i]
    }))
    sub <- df[grp_filter, within, drop = FALSE]
    if (nrow(sub) < 2) {
      warning("Group ", paste(unlist(groups[i,]), collapse="/"),
              " has fewer than 2 rows; skipping.", call. = FALSE)
      next
    }
    mu_v <- colMeans(sub, na.rm = TRUE)
    sd_v <- sapply(sub, sd, na.rm = TRUE)
    r_v  <- if (length(within) > 1)
              cor(sub, use = "pairwise.complete.obs")
            else
              matrix(1, 1, 1, dimnames = list(within, within))
    sim_grp <- rnorm_multi(n, vars = within, mu = mu_v, sd = sd_v, r = r_v,
                           empirical = empirical)
    between_df <- groups[rep(i, n), , drop = FALSE]
    rownames(between_df) <- NULL
    results[[i]] <- cbind(between_df, sim_grp)
  }

  out <- do.call(rbind, Filter(Negate(is.null), results))
  rownames(out) <- NULL

  # Re-factor between columns
  for (col in between) {
    if (col %in% names(out) && is.character(out[[col]])) {
      out[[col]] <- factor(out[[col]], levels = levels(factor(df[[col]])))
    }
  }
  out
}
