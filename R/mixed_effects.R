#' Add random factors to a data structure
#'
#' Starting point for building up a mixed effects data structure. Each call
#' adds one random factor (e.g., subjects, items, classes) as rows.
#'
#' @param data A data frame produced by a previous `add_*` call, or `NULL`
#'   (the default) to start a new structure.
#' @param ... Named integer(s) specifying the factor name and the number of
#'   levels (e.g., `subject = 50`). Multiple factors can be specified;
#'   they will be fully crossed.
#' @param .nested_in Character. Name of an existing factor to nest within.
#'   If supplied, the new factor is nested in each level of that factor.
#'
#' @return A data frame with ID columns for the new random factor(s).
#' @export
#' @examples
#' # 20 subjects
#' d <- add_random(subject = 20)
#'
#' # 10 items nested in 3 lists
#' d2 <- add_random(list = 3) |>
#'       add_random(item = 10, .nested_in = "list")
add_random <- function(data = NULL, ..., .nested_in = NULL) {
  args <- list(...)
  if (length(args) == 0) stop("Provide at least one named integer, e.g. subject = 50", call. = FALSE)

  if (is.null(data)) {
    # Start fresh: fully cross all supplied factors
    id_lists <- lapply(seq_along(args), function(i) {
      nm  <- names(args)[i]
      n   <- args[[i]]
      make_id(n, prefix = toupper(substr(nm, 1, 1)))
    })
    names(id_lists) <- names(args)
    out <- do.call(expand.grid, c(rev(id_lists), stringsAsFactors = FALSE))
    out <- out[, rev(seq_along(id_lists)), drop = FALSE]
    return(out)
  }

  # Nested
  if (!is.null(.nested_in)) {
    if (!.nested_in %in% names(data)) {
      stop(".nested_in column '", .nested_in, "' not found in data.", call. = FALSE)
    }
    parent_levels <- unique(data[[.nested_in]])
    nm  <- names(args)[1]
    n_spec <- args[[1]]

    rows <- lapply(parent_levels, function(pl) {
      n_here <- if (length(n_spec) == 1) n_spec
                else n_spec[match(pl, parent_levels)]
      if (is.na(n_here)) n_here <- n_spec[1]
      ids <- make_id(n_here, prefix = toupper(substr(nm, 1, 1)))
      parent_rows <- data[data[[.nested_in]] == pl, , drop = FALSE]
      # Repeat parent rows for each nested id
      parent_rep <- parent_rows[rep(1, n_here), , drop = FALSE]
      parent_rep[[nm]] <- ids
      rownames(parent_rep) <- NULL
      parent_rep
    })
    return(do.call(rbind, rows))
  }

  # Crossing with existing data
  nm <- names(args)[1]
  n  <- args[[1]]
  ids <- make_id(n, prefix = toupper(substr(nm, 1, 1)))
  crossed <- merge(data, data.frame(setNames(list(ids), nm), stringsAsFactors = FALSE),
                   by = NULL)
  # Re-order columns: original first, new last
  orig_cols <- names(data)
  crossed <- crossed[, c(orig_cols, nm), drop = FALSE]
  crossed
}

#' Add within-subject factors to a data structure
#'
#' @param data A data frame (typically from [add_random()]).
#' @param .by Character. Name of the random factor this applies to
#'   (e.g., `"subject"`). If `NULL` uses the last column.
#' @param ... Named character vectors of factor levels.
#' @param .prob Optional numeric vector of probabilities for random assignment.
#'   If `NULL` all combinations appear for every subject (fully crossed).
#'
#' @return A data frame with additional rows/columns for the within factor.
#' @export
#' @examples
#' d <- add_random(subject = 20) |>
#'      add_within(.by = "subject", condition = c("A", "B"))
add_within <- function(data, .by = NULL, ..., .prob = NULL) {
  args <- list(...)
  if (length(args) == 0) stop("Provide at least one named character vector.", call. = FALSE)

  # Expand: for each subject, add all within combinations
  within_df <- do.call(expand.grid, c(rev(args), stringsAsFactors = FALSE))
  within_df <- within_df[, rev(seq_along(args)), drop = FALSE]

  out <- merge(data, within_df, by = NULL)
  # Factor-ise within columns
  for (f in names(args)) {
    out[[f]] <- factor(out[[f]], levels = args[[f]])
  }
  out[order(out[[if (!is.null(.by)) .by else names(data)[1]]]), ]
}

#' Add between-subject factors to a data structure
#'
#' Randomly assigns subjects to between-subject factor levels.
#'
#' @param data A data frame.
#' @param .by Character. Column name of the unit to assign (e.g., `"subject"`).
#' @param ... Named character vectors of factor levels.
#' @param .prob Optional list of probability vectors per factor.
#'
#' @return The input data frame with new between-subject factor column(s).
#' @export
#' @examples
#' d <- add_random(subject = 60) |>
#'      add_between(.by = "subject",
#'                  treatment = c("control", "drug_A", "drug_B"),
#'                  .prob = c(1/3, 1/3, 1/3))
add_between <- function(data, .by = NULL, ..., .prob = NULL) {
  args <- list(...)
  if (length(args) == 0) stop("Provide at least one named character vector.", call. = FALSE)

  # Get unique units to assign
  by_col <- .by %||% names(data)[1]
  units   <- unique(data[[by_col]])
  n_units <- length(units)

  assign_df <- data.frame(setNames(list(units), by_col), stringsAsFactors = FALSE)
  for (i in seq_along(args)) {
    f      <- names(args)[i]
    levs   <- args[[i]]
    probs  <- if (!is.null(.prob)) {
                if (is.list(.prob)) .prob[[i]] else .prob
              } else rep(1/length(levs), length(levs))
    probs  <- probs / sum(probs)
    assign_df[[f]] <- sample(levs, n_units, replace = TRUE, prob = probs)
  }

  out <- merge(data, assign_df, by = by_col)
  # Factor-ise
  for (f in names(args)) {
    out[[f]] <- factor(out[[f]], levels = args[[f]])
  }
  out
}

#' Add random effects (random noise) to a data frame
#'
#' Adds one or more random effect columns with specified standard deviation(s).
#' Useful for building up a mixed-model DGP.
#'
#' @param data A data frame.
#' @param .by Character. Column name of the grouping factor.  Random effects
#'   are drawn once per unique level of this column and then merged in.
#' @param ... Named numeric values specifying `<column_name> = sd`.
#' @param .cors Optional correlation matrix or scalar for correlations among
#'   random effects at the same level.
#' @param seed Optional integer seed.
#'
#' @return The input data frame with new random effect column(s) added.
#' @export
#' @examples
#' d <- add_random(subject = 30) |>
#'      add_within(.by = "subject", item = paste0("I", 1:5)) |>
#'      add_ranef(.by = "subject", subj_intercept = 1.2) |>
#'      add_ranef(.by = "item",    item_intercept = 0.8)
add_ranef <- function(data, .by = NULL, ..., .cors = 0, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  args <- list(...)
  if (length(args) == 0) stop("Provide named sd values, e.g. intercept = 1.5", call. = FALSE)

  by_col <- .by %||% names(data)[1]
  units  <- unique(data[[by_col]])
  n      <- length(units)
  nms    <- names(args)
  sds    <- unlist(args)
  k      <- length(sds)

  if (k == 1) {
    re_df <- data.frame(
      setNames(list(units), by_col),
      setNames(list(rnorm(n, 0, sds[1])), nms[1]),
      stringsAsFactors = FALSE
    )
  } else {
    R   <- make_cormat(.cors, k, nms)
    res <- rnorm_multi(n, vars = nms, mu = rep(0, k), sd = sds, r = R)
    re_df <- cbind(data.frame(setNames(list(units), by_col), stringsAsFactors = FALSE), res)
  }

  merge(data, re_df, by = by_col)
}

#' Recode a factor column in a data frame
#'
#' @param data A data frame.
#' @param col Character. Name of the column to recode.
#' @param new_col Character. Name of the new column (default: appends
#'   `"_recoded"`).
#' @param ... Named character mappings: `"old_level" = "new_level"`.
#' @return The data frame with the recoded column added.
#' @export
#' @examples
#' d <- data.frame(group = c("A","B","A","C"))
#' add_recode(d, "group", "group2", A = "alpha", B = "beta", C = "gamma")
add_recode <- function(data, col, new_col = NULL, ...) {
  map <- c(...)
  if (is.null(new_col)) new_col <- paste0(col, "_recoded")
  data[[new_col]] <- map[as.character(data[[col]])]
  data
}

#' Simulate a cross-classified (subjects x items) dataset
#'
#' Generates a dataset where subjects are crossed with items, each with their
#' own random intercepts.
#'
#' @param subj_n Integer. Number of subjects.
#' @param item_n Integer. Number of items.
#' @param grand_mean Numeric. Overall intercept.
#' @param subj_sd Numeric. SD of subject random intercepts.
#' @param item_sd Numeric. SD of item random intercepts.
#' @param error_sd Numeric. SD of residual error.
#' @param seed Optional integer seed.
#' @return A data frame with columns `subj`, `item`, `subj_re`, `item_re`,
#'   `error`, and `dv`.
#' @export
#' @examples
#' d <- forge_mixed_cc(subj_n = 30, item_n = 20)
#' head(d)
forge_mixed_cc <- function(subj_n = 20, item_n = 20,
                            grand_mean = 0,
                            subj_sd    = 1,
                            item_sd    = 1,
                            error_sd   = 1,
                            seed       = NULL) {
  if (!is.null(seed)) set.seed(seed)
  subj_re <- rnorm(subj_n, 0, subj_sd)
  item_re <- rnorm(item_n, 0, item_sd)

  subj <- rep(make_id(subj_n, "S"), each  = item_n)
  item <- rep(make_id(item_n, "I"), times = subj_n)
  s_re <- rep(subj_re, each  = item_n)
  i_re <- rep(item_re, times = subj_n)
  err  <- rnorm(subj_n * item_n, 0, error_sd)

  dv <- grand_mean + s_re + i_re + err
  data.frame(subj = subj, item = item,
             subj_re = s_re, item_re = i_re,
             error = err, dv = dv,
             stringsAsFactors = FALSE)
}
