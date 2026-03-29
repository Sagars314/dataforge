#' Simulate data from a factorial design
#'
#' The primary simulation function in \pkg{dataforge}. Given a description of
#' between- and within-subject factors (their levels, sample sizes, means,
#' standard deviations, and correlation structure), \code{forge_design()} returns a
#' tidy data frame ready for analysis.
#'
#' @param within Named list of within-subject factors. Each element is a
#'   character vector (or named character vector) of level labels. Use
#'   \code{NULL} or omit for a purely between-subjects design.
#' @param between Named list of between-subject factors. Each element is a
#'   character vector (or named character vector) of level labels. Use
#'   \code{NULL} or omit for a purely within-subjects design.
#' @param n Integer scalar or named list/vector specifying the number of
#'   subjects per between-cell. A single integer is recycled across all cells.
#' @param mu Numeric. Cell means. Can be:
#'   \itemize{
#'     \item a scalar (same for all cells),
#'     \item a vector with one value per cell (within levels vary fastest,
#'       then between levels),
#'     \item a data frame with between-groups as columns and within-level
#'       combinations as rows (most explicit).
#'   }
#' @param sd Numeric. Cell standard deviations (same recycling rules as
#'   \code{mu}).
#' @param r Correlation among within-subject measurements. A scalar applies
#'   the same value to all pairs; a lower-triangle vector or full matrix
#'   specifies individual pairwise correlations; a named list supplies a
#'   different specification per between-subject cell.
#' @param long Logical. Return data in long format? Defaults to \code{FALSE}
#'   (wide). Overrides the global \code{dataforge_options(long = ...)} setting
#'   when supplied explicitly.
#' @param dv Character. Name of the dependent variable column in long format
#'   (default \code{"y"}).
#' @param id Character. Name of the subject ID column (default \code{"id"}).
#' @param plot Logical. Auto-plot the design after simulation? Inherits from
#'   \code{\link{dataforge_options}} if \code{NULL}.
#' @param seed Optional integer seed for reproducibility.
#' @param rep Integer. Number of replications (for simulation studies). If
#'   greater than 1, returns a list of \code{rep} independent data frames.
#'
#' @return A data frame (with attribute \code{"dataforge_design"}) containing
#'   the simulated data. If \code{long = TRUE}, the data are in long format
#'   with one column per factor plus the DV column. When \code{rep > 1}, a
#'   list of data frames is returned.
#'
#' @details
#' \strong{Column naming (wide format):}
#' Within-subject columns are named by concatenating factor level names with
#' the separator set via \code{\link{dataforge_options}} (default \code{"_"}).
#' For example, within factors \code{time = c("pre", "post")} and
#' \code{condition = c("A", "B")} produce columns \code{pre_A}, \code{pre_B},
#' \code{post_A}, \code{post_B}.
#'
#' \strong{Cell means order:}
#' When \code{mu} is a flat vector, values are assigned in the order:
#' within-level combinations vary fastest, then between-level combinations.
#' Use a data frame for explicit, readable control.
#'
#' @seealso
#' \code{\link{rnorm_forge}} for direct multivariate normal simulation,
#' \code{\link{forge_df}} for simulating from an existing data frame,
#' \code{\link{wide2long}} and \code{\link{long2wide}} for reshaping,
#' \code{\link{plot_design}} for visualisation,
#' \code{\link{dataforge_options}} for global defaults.
#'
#' @examples
#' # Simple one-way between-subjects
#' df <- forge_design(between = list(group = c("control", "treat")),
#'                    n = 50, mu = c(0, 0.5), sd = 1, plot = FALSE)
#' head(df)
#'
#' # 2 x 2 mixed design — mu as a data frame
#' df2 <- forge_design(
#'   within  = list(time = c("pre", "post")),
#'   between = list(condition = c("A", "B")),
#'   n = 30,
#'   mu = data.frame(A = c(10, 12), B = c(10, 15),
#'                   row.names = c("pre", "post")),
#'   sd = 2, r = 0.6, long = TRUE, plot = FALSE
#' )
#' head(df2)
#'
#' # Purely within-subject (3 levels)
#' df3 <- forge_design(
#'   within = list(time = c("pre", "mid", "post")),
#'   n = 40, mu = c(10, 12, 15), sd = 3, r = 0.6, plot = FALSE
#' )
#'
#' # Simulation study: 500 replications
#' \dontrun{
#' datasets <- forge_design(
#'   within  = list(t = c("pre", "post")),
#'   between = list(g = c("ctrl", "trt")),
#'   n = 30, mu = c(0, 0, 0, 2), sd = 1, r = 0.5,
#'   rep = 500, plot = FALSE
#' )
#' power <- mean(sapply(datasets, function(d) {
#'   t.test(d$post[d$g == "trt"], d$post[d$g == "ctrl"])$p.value < 0.05
#' }))
#' }
#'
#' @export
forge_design <- function(within  = list(),
                         between = list(),
                         n       = 100,
                         mu      = 0,
                         sd      = 1,
                         r       = 0,
                         id      = "id",
                         dv      = "y",
                         long    = NULL,
                         plot    = NULL,
                         seed    = NULL,
                         rep     = 1L,
                         verbose = TRUE) {
  # ---- option defaults ----
  if (is.null(long)) long <- .opt("long")
  if (is.null(plot)) plot <- .opt("plot")
  if (is.null(seed)) seed <- .opt("seed")
  if (!is.null(seed)) set.seed(seed)
  sep <- .opt("sep")

  # ---- input checks ----
  if (!is.list(within))  stop("`within` must be a list.", call. = FALSE)
  if (!is.list(between)) stop("`between` must be a list.", call. = FALSE)

  # Normalise factor level specs:allow unnamed vectors
  within  <- .normalise_factor_list(within)
  between <- .normalise_factor_list(between)

  n_within  <- if (length(within) == 0) 1L else prod(lengths(within))
  n_between <- if (length(between) == 0) 1L else prod(lengths(between))
  n_cells   <- n_within * n_between

  # ---- build design grid ----
  within_grid  <- .expand_grid_named(within)   # nrow = n_within
  between_grid <- .expand_grid_named(between)  # nrow = n_between

  within_col_names <- if (nrow(within_grid) > 0)
    apply(within_grid, 1, paste, collapse = sep) else "y"

  between_cell_names <- if (nrow(between_grid) > 0)
    apply(between_grid, 1, paste, collapse = sep) else "all"

  # ---- parse mu, sd ----
  mu_mat <- .parse_cell_param(mu, n_within, n_between,
                               within_col_names, between_cell_names)
  sd_mat <- .parse_cell_param(sd, n_within, n_between,
                               within_col_names, between_cell_names)

  # ---- parse r (per between-cell or single spec) ----
  if (is.list(r)) {
    if (length(r) != n_between)
      stop("`r` list must have one entry per between-subject cell.", call. = FALSE)
    r_list <- r
  } else {
    r_list <- replicate(n_between, r, simplify = FALSE)
  }

  # ---- parse n per between-cell ----
  n_vec <- .parse_n(n, n_between, between_cell_names)

  # ---- replications ----
  if (rep > 1) {
    return(lapply(seq_len(rep), function(i) {
      forge_design(within = within, between = between,
                   n = n, mu = mu, sd = sd, r = r,
                   id = id, dv = dv, long = long, plot = FALSE,
                   seed = if (!is.null(seed)) seed + i else NULL)
    }))
  }

  # ---- simulate ----
  all_data <- vector("list", n_between)
  id_start <- 1L

  for (b in seq_len(n_between)) {
    nb       <- n_vec[b]
    mu_b     <- mu_mat[, b]
    sd_b     <- sd_mat[, b]
    r_b      <- r_list[[b]]
    cm       <- cormat(r_b, n = n_within, var_names = within_col_names)

    # Simulate multivariate normal
    if (n_within == 1) {
      sim <- data.frame(V1 = stats::rnorm(nb, mu_b, sd_b))
      colnames(sim) <- within_col_names
    } else {
      sim <- rnorm_forge(nb, vars = n_within,
                          mu = mu_b, sd = sd_b, r = cm,
                          var_names = within_col_names)
    }

    # Add ID column
    ids <- sprintf("%s%0*d", id, nchar(as.character(id_start + nb - 1)),
                   seq(id_start, id_start + nb - 1))
    id_start <- id_start + nb

    # Add between-subject factor columns
    btw_row <- if (nrow(between_grid) > 0) between_grid[b, , drop = FALSE] else NULL

    row_df <- data.frame(id = ids, stringsAsFactors = FALSE)
    if (!is.null(btw_row)) {
      for (fac in names(between)) {
        row_df[[fac]] <- btw_row[[fac]]
      }
    }

    all_data[[b]] <- cbind(row_df, sim)
  }

  df <- do.call(rbind, all_data)
  rownames(df) <- NULL

  # Factor-ise between columns
  for (fac in names(between)) {
    df[[fac]] <- factor(df[[fac]], levels = names(between[[fac]]))
  }

  # Attach design metadata
  attr(df, "dataforge_design") <- list(
    within  = within,
    between = between,
    within_col_names = within_col_names,
    mu  = mu_mat,
    sd  = sd_mat
  )
  class(df) <- c("forge_data", "data.frame")

  # ---- verbose ----
  #' @param verbose Logical. Print design summary.
  if (.opt("verbose")) .print_design(within, between, n_vec, mu_mat, sd_mat)

  # ---- long format ----
  if (long) {
    des_save <- attr(df, "dataforge_design")
    df <- wide2long(df, within = names(within), dv = dv)
    attr(df, "dataforge_design") <- des_save
    class(df) <- c("forge_data", "data.frame")
  }

  # ---- plot ----
  # if (plot && length(within) + length(between) > 0) {
  #   p <- plot_design(df, within = if (!long) names(within) else NULL)
  #   print(p)
  # }

  # if (plot && length(within) + length(between) > 0) {
  #
  #   xvar <- if (long) {
  #     names(within)[1]
  #   } else {
  #     names(within)[1]
  #   }
  #
  #   colorvar <- if (length(between) > 0) names(between)[1] else NULL
  #
  #   p <- plot_design(
  #     df,
  #     x = xvar,
  #     color = colorvar,
  #     dv = dv
  #   )
  #
  #   print(p)
  # }

  if (plot && length(within) + length(between) > 0) {

    if (long) {

      xvar <- names(within)[1]

      p <- plot_design(
        df,
        x = xvar,
        color = if (length(between) > 0) names(between)[1] else NULL,
        dv = dv
      )

    } else {

      # let plot_design auto-detect structure from metadata
      p <- plot_design(df)

    }

    print(p)
  }
  df
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.normalise_factor_list <- function(lst) {
  if (length(lst) == 0) return(lst)
  lapply(lst, function(x) {
    if (is.null(names(x))) {
      nms <- as.character(x)
      setNames(nms, nms)
    } else {
      x
    }
  })
}

.expand_grid_named <- function(lst) {
  if (length(lst) == 0) return(data.frame())
  levs <- lapply(lst, names)
  do.call(expand.grid, c(rev(levs), stringsAsFactors = FALSE))[, rev(seq_along(levs)), drop = FALSE]
}

.parse_cell_param <- function(param, n_within, n_between,
                               row_nms, col_nms) {
  n_cells <- n_within * n_between

  if (is.data.frame(param) || is.matrix(param)) {
    m <- as.matrix(param)
    if (!all(dim(m) == c(n_within, n_between)))
      stop(sprintf("mu/sd matrix must be %d x %d (within x between cells).",
                   n_within, n_between), call. = FALSE)
    rownames(m) <- row_nms
    colnames(m) <- col_nms
    return(m)
  }

  param <- as.numeric(param)
  if (length(param) == 1) param <- rep(param, n_cells)
  if (length(param) != n_cells)
    stop(sprintf("mu/sd vector must have 1 or %d values, not %d.",
                 n_cells, length(param)), call. = FALSE)

  m <- matrix(param, nrow = n_within, ncol = n_between)
  rownames(m) <- row_nms
  colnames(m) <- col_nms
  m
}

.parse_n <- function(n, n_between, cell_names) {
  if (is.list(n)) {
    if (length(n) != n_between)
      stop("`n` list must have one entry per between-subject cell.", call. = FALSE)
    as.integer(unlist(n))
  } else if (length(n) == 1) {
    rep(as.integer(n), n_between)
  } else if (length(n) == n_between) {
    as.integer(n)
  } else {
    stop(sprintf("`n` must be length 1 or %d.", n_between), call. = FALSE)
  }
}

.print_design <- function(within, between, n_vec, mu_mat, sd_mat) {
  cat("\n--- dataforge design ---\n")
  if (length(within) > 0) {
    cat("Within factors:\n")
    for (nm in names(within)) {
      cat(sprintf("  %s: %s\n", nm, paste(names(within[[nm]]), collapse = ", ")))
    }
  }
  if (length(between) > 0) {
    cat("Between factors:\n")
    for (nm in names(between)) {
      cat(sprintf("  %s: %s\n", nm, paste(names(between[[nm]]), collapse = ", ")))
    }
  }
  cat(sprintf("n per cell: %s\n", paste(n_vec, collapse = ", ")))
  cat("------------------------\n\n")
}
