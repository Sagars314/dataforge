#' Add Random Grouping Factors to a Design
#'
#' @description
#' Start or extend a data frame representing a mixed effects design by adding
#' new random grouping factors (e.g., subjects, items, schools). Designed to
#' be used with the `%>%` pipe.
#'
#' @param data A data frame to extend. If `NULL` (default), a new design is
#'   started.
#' @param ... Named integer arguments specifying each random factor and the
#'   number of levels, e.g., `subject = 30, item = 20`.
#' @param .nested_in Character. Name of an existing grouping column within
#'   which the new factor is nested. Use `NULL` (default) for crossed factors.
#' @param .sep Character. Separator for ID construction. Default `"_"`.
#'
#' @return A data frame with one row per combination of the new grouping
#'   levels.
#'
#' @examples
#' # 20 subjects, each seeing 10 items (crossed)
#' design <- add_random(subject = 20, item = 10)
#' nrow(design)  # 200
#'
#' # Nested: students within classes
#' design2 <- add_random(class = 5) |>
#'   add_random(student = 8, .nested_in = "class")
#' nrow(design2)  # 40
#'
#' @export
add_random <- function(data = NULL, ..., .nested_in = NULL, .sep = "_") {
  new_factors <- list(...)
  if (length(new_factors) == 0) stop("Provide at least one named random factor.", call. = FALSE)

  # If data already has rows we're extending it; otherwise start fresh
  if (is.null(data)) {
    df <- data.frame(row.names = seq_len(1))
  } else {
    df <- data
  }

  for (fac_name in names(new_factors)) {
    n_levels <- new_factors[[fac_name]]

    if (!is.null(.nested_in)) {
      if (!(.nested_in %in% names(df)))
        stop(sprintf("`.nested_in` column '%s' not found.", .nested_in), call. = FALSE)

      parent_levels <- unique(df[[.nested_in]])
      new_rows <- lapply(parent_levels, function(pl) {
        sub <- df[df[[.nested_in]] == pl, , drop = FALSE]
        n   <- if (length(n_levels) == 1) n_levels else
          sample(n_levels, 1)   # support range
        ids <- paste0(pl, .sep, sprintf("%0*d", nchar(n), seq_len(n)))
        sub2 <- sub[rep(1, n), , drop = FALSE]
        sub2[[fac_name]] <- ids
        sub2
      })
      df <- do.call(rbind, new_rows)
      rownames(df) <- NULL
    } else {
      # Crossed: expand existing rows
      n   <- if (length(n_levels) == 1) n_levels else n_levels[[1]]
      ids <- sprintf("%s%0*d", fac_name, nchar(n), seq_len(n))

      if (nrow(df) == 1 && length(names(df)) == 0) {
        df <- data.frame(setNames(list(ids), fac_name), stringsAsFactors = FALSE)
      } else {
        df_new <- data.frame(setNames(list(ids), fac_name), stringsAsFactors = FALSE)
        df <- merge(df, df_new, by = NULL)
      }
    }
  }
  rownames(df) <- NULL
  df
}

#' Add Within-Subject Factor Levels
#'
#' @description
#' Expand an existing design data frame so that each row is replicated for
#' every level of a within-subject factor.
#'
#' @param data A data frame (typically created by \code{\link{add_random}}).
#' @param ... Named character vector arguments specifying within factors and
#'   their levels, e.g., `time = c("pre", "post")`.
#' @param .by Character. Optional grouping column; the factor is added within
#'   each level of `.by`.
#' @param .prefix Logical. If `TRUE`, within factor names are prepended to
#'   level names in the new column. Default `FALSE`.
#'
#' @return An expanded data frame.
#'
#' @examples
#' df <- add_random(subject = 20) |>
#'   add_within(time = c("pre", "mid", "post"))
#' nrow(df)  # 60
#'
#' @export
add_within <- function(data, ..., .by = NULL, .prefix = FALSE) {
  factors <- list(...)
  if (length(factors) == 0) stop("Provide at least one within factor.", call. = FALSE)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)

  for (fac_name in names(factors)) {
    levs <- as.character(factors[[fac_name]])
    if (.prefix) levs <- paste0(fac_name, "_", levs)

    expand_df <- data.frame(setNames(list(levs), fac_name), stringsAsFactors = FALSE)
    data <- merge(data, expand_df, by = NULL)
  }
  rownames(data) <- NULL

  # Factor-ise
  for (fac_name in names(factors)) {
    levs <- if (.prefix) paste0(fac_name, "_", factors[[fac_name]]) else factors[[fac_name]]
    data[[fac_name]] <- factor(data[[fac_name]], levels = levs)
  }

  data[do.call(order, data[names(factors)]), ]
}

#' Add Between-Subject Factor Assignments
#'
#' @description
#' Randomly (or proportionally) assign factor levels to rows of a design data
#' frame, creating a between-subject variable.
#'
#' @param data A data frame.
#' @param ... Named character vector arguments specifying factors and levels.
#' @param .by Character. Column to group by before assigning. E.g., assign
#'   one level per unique value of `"subject"`.
#' @param .prob Numeric vector. Sampling probabilities for each level
#'   (automatically normalised). Default is equal probability.
#' @param .shuffle Logical. If `TRUE` (default), shuffle assignments randomly.
#'
#' @return The data frame with new factor column(s) added.
#'
#' @examples
#' df <- add_random(subject = 100) |>
#'   add_between(group = c("control", "treatment"))
#' table(df$group)  # ~50 each
#'
#' # Unequal probability (3:1 ratio)
#' df2 <- add_random(subject = 80) |>
#'   add_between(condition = c("A", "B"), .prob = c(3, 1))
#'
#' @export
add_between <- function(data, ..., .by = NULL, .prob = NULL, .shuffle = TRUE) {
  factors <- list(...)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)

  for (fac_name in names(factors)) {
    levs <- as.character(factors[[fac_name]])
    n_levs <- length(levs)

    probs <- if (is.null(.prob)) rep(1 / n_levs, n_levs) else {
      p <- as.numeric(.prob)
      if (length(p) != n_levs)
        stop("`.prob` must have the same length as the number of factor levels.", call. = FALSE)
      p / sum(p)
    }

    if (!is.null(.by)) {
      if (!(.by %in% names(data)))
        stop(sprintf("`.by` column '%s' not found.", .by), call. = FALSE)
      unique_by <- unique(data[[.by]])
      n_unique  <- length(unique_by)
      assigned  <- sample(levs, n_unique, replace = TRUE, prob = probs)
      lut       <- setNames(assigned, unique_by)
      data[[fac_name]] <- lut[as.character(data[[.by]])]
    } else {
      nr <- nrow(data)
      assigned <- sample(levs, nr, replace = TRUE, prob = probs)
      if (.shuffle) assigned <- sample(assigned)
      data[[fac_name]] <- assigned
    }

    data[[fac_name]] <- factor(data[[fac_name]], levels = levs)
  }
  data
}

#' Add Random Effects to a Design Data Frame
#'
#' @description
#' Simulate and add random intercept or slope values drawn from a normal
#' distribution to each unique level of a grouping factor.
#'
#' @param data A data frame.
#' @param ... Named numeric arguments specifying the SD of the random effect
#'   for each grouping column, e.g., `subject = 1.5, item = 0.8`.
#' @param .cors Numeric. Correlation among random effects when multiple are
#'   specified. Default 0.
#' @param .mu Numeric scalar. Mean of the random effect distribution. Default 0.
#'
#' @return The data frame with new random-effect columns added.
#'
#' @examples
#' df <- add_random(subject = 20, item = 10) |>
#'   add_ranef(subject = 1.5, item = 0.8)
#' head(df)
#'
#' @export
add_ranef <- function(data, ..., .cors = 0, .mu = 0) {
  eff_list <- list(...)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)

  for (grp_col in names(eff_list)) {
    if (!(grp_col %in% names(data)))
      stop(sprintf("Grouping column '%s' not found in data.", grp_col), call. = FALSE)
    eff_sd   <- eff_list[[grp_col]]
    uniq_ids <- unique(data[[grp_col]])
    n_ids    <- length(uniq_ids)
    raw      <- stats::rnorm(n_ids, mean = .mu, sd = eff_sd)
    lut      <- setNames(raw, uniq_ids)
    new_col  <- paste0(grp_col, "_ranef")
    data[[new_col]] <- lut[as.character(data[[grp_col]])]
  }
  data
}

#' Recode a Categorical Column
#'
#' @description
#' Apply a named recoding scheme to an existing factor/character column,
#' optionally creating a new column.
#'
#' @param data A data frame.
#' @param col Character. Name of the column to recode.
#' @param ... Named arguments where names are original values and values are
#'   new codes. E.g., `"A" = 1, "B" = 2`.
#' @param .new_col Character. Name for the new column. If `NULL` (default),
#'   overwrites `col`.
#'
#' @return The data frame with the (re)coded column.
#'
#' @examples
#' df <- data.frame(group = c("A","B","A","C"))
#' add_recode(df, "group", A = 1, B = 2, C = 3, .new_col = "group_num")
#'
#' @export
add_recode <- function(data, col, ..., .new_col = NULL) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!(col %in% names(data))) stop(sprintf("Column '%s' not found.", col), call. = FALSE)
  recode_map <- list(...)
  out_col <- if (is.null(.new_col)) col else .new_col
  data[[out_col]] <- recode_map[as.character(data[[col]])]
  data
}
