#' Simulate data from a factorial design
#'
#' The primary simulation function in `dataforge`. Given a description of
#' between- and within-subject factors (their levels, sample sizes, means,
#' standard deviations, and correlation structure), `forge_design()` returns a
#' tidy data frame ready for analysis.
#'
#' @param within Named list of within-subject factors. Each element is a
#'   character vector (or named character vector) of level labels. Use `NULL`
#'   or omit for a purely between-subjects design.
#' @param between Named list of between-subject factors. Each element is a
#'   character vector (or named character vector) of level labels. Use `NULL`
#'   or omit for a purely within-subjects design.
#' @param n Integer scalar or named list/vector specifying the number of
#'   subjects per between-cell. Recycled if scalar.
#' @param mu Numeric. Cell means. Can be:
#'   - a scalar (same for all cells),
#'   - a named vector (one value per within-level combination),
#'   - a data frame with between-groups as columns and within-levels as rows,
#'   - or a flat vector in the order of cell combinations.
#' @param sd Numeric. Cell standard deviations (same recycling rules as `mu`).
#' @param r Correlation among within-subject measurements. Scalar, lower-
#'   triangle vector, or full matrix.
#' @param long Logical. Return data in long format? (default `FALSE` — wide)
#' @param dv Character. Name of the dependent variable column in long format
#'   (default `"y"`).
#' @param id Character. Name of the subject ID column (default `"id"`).
#' @param plot Logical. Auto-plot the design? Inherits from
#'   [dataforge_options()] if `NULL` (default `NULL`).
#' @param seed Optional integer seed.
#'
#' @return A data frame (with attribute `"design"`) containing the simulated
#'   data. If `long = TRUE`, the data are in long format with columns for each
#'   factor plus the DV.
#' @export
#'
#' @examples
#' # Simple one-way between-subjects
#' df <- forge_design(between = list(group = c("control", "treat")),
#'                    n = 50, mu = c(0, 0.5), sd = 1, plot = FALSE)
#' head(df)
#'
#' # 2x2 mixed design
#' df2 <- forge_design(
#'   within  = list(time = c("pre", "post")),
#'   between = list(condition = c("A", "B")),
#'   n = 30,
#'   mu = data.frame(A = c(10, 12), B = c(10, 15), row.names = c("pre","post")),
#'   sd = 2, r = .6, long = TRUE, plot = FALSE
#' )
#' head(df2)
forge_design <- function(within  = list(),
                         between = list(),
                         n       = 100,
                         mu      = 0,
                         sd      = 1,
                         r       = 0,
                         long    = FALSE,
                         dv      = "y",
                         id      = "id",
                         plot    = NULL,
                         seed    = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (is.null(within))  within  <- list()
  if (is.null(between)) between <- list()

  # ---- normalise factor level lists ----------------------------------------
  within  <- lapply(within,  function(lv) if (is.null(names(lv))) { names(lv) <- lv; lv } else lv)
  between <- lapply(between, function(lv) if (is.null(names(lv))) { names(lv) <- lv; lv } else lv)

  n_within  <- prod(lengths(within))
  n_between <- prod(lengths(between))

  within_combos  <- if (length(within)  > 0) expand_factors(lapply(within,  names)) else data.frame(.r = 1)
  between_combos <- if (length(between) > 0) expand_factors(lapply(between, names)) else data.frame(.r = 1)

  n_w_cells <- nrow(within_combos)
  n_b_cells <- nrow(between_combos)

  # ---- resolve n per between-cell ------------------------------------------
  if (length(n) == 1) {
    n_vec <- rep(as.integer(n), n_b_cells)
  } else {
    n_vec <- as.integer(recycle_vec(n, n_b_cells, "n"))
  }

  # ---- resolve mu matrix [n_w_cells x n_b_cells] ---------------------------
  mu_mat <- .resolve_param_matrix(mu, n_w_cells, n_b_cells,
                                   row_nms = if (n_w_cells > 1) apply(within_combos, 1, paste, collapse = "_") else "y",
                                   col_nms = if (n_b_cells > 1) apply(between_combos, 1, paste, collapse = "_") else "y")

  sd_mat <- .resolve_param_matrix(sd, n_w_cells, n_b_cells,
                                   row_nms = rownames(mu_mat),
                                   col_nms = colnames(mu_mat))

  # ---- correlation matrix for within cells ----------------------------------
  R <- make_cormat(r, n_w_cells)

  # ---- simulate ---------------------------------------------------------------
  all_rows <- vector("list", n_b_cells)

  for (b in seq_len(n_b_cells)) {
    nb <- n_vec[b]
    mu_b <- mu_mat[, b]
    sd_b <- sd_mat[, b]

    # Simulate all within-columns at once (correlated)
    raw <- rnorm_multi(nb, vars = n_w_cells, mu = mu_b, sd = sd_b, r = R)

    # Build between-factor columns
    if (length(between) > 0) {
      bcols <- between_combos[b, , drop = FALSE]
      bdf   <- bcols[rep(1, nb), , drop = FALSE]
      rownames(bdf) <- NULL
    } else {
      bdf <- data.frame(row.names = seq_len(nb))
    }

    # Subject IDs (global within this between-cell)
    start_id <- if (b == 1) 1 else sum(n_vec[seq_len(b - 1)]) + 1
    id_col   <- make_id(nb, prefix = "S", digits = nchar(sum(n_vec)))
    # Adjust for offset
    id_col <- make_id(sum(n_vec), prefix = "S",
                      digits = nchar(sum(n_vec)))[start_id:(start_id + nb - 1)]

    # Within-level column names
    if (n_w_cells > 1) {
      w_nms <- apply(within_combos, 1, paste, collapse = .opt("sep"))
    } else {
      w_nms <- dv
    }
    colnames(raw) <- w_nms

    row_df <- cbind(data.frame(setNames(list(id_col), id), stringsAsFactors = FALSE),
                    bdf, raw)
    all_rows[[b]] <- row_df
  }

  df <- do.call(rbind, all_rows)
  rownames(df) <- NULL

  # Remove dummy ".r" column if present
  df <- df[, !grepl("^\\.r$", names(df)), drop = FALSE]

  # Factor-ise between columns
  for (f in names(between)) {
    if (f %in% names(df)) df[[f]] <- factor(df[[f]], levels = names(between[[f]]))
  }

  # ---- build design attribute -----------------------------------------------
  design <- list(
    within  = within,
    between = between,
    n       = n_vec,
    mu      = mu_mat,
    sd      = sd_mat,
    r       = R,
    dv      = dv,
    id      = id
  )
  attr(df, "design") <- design

  # ---- long format ----------------------------------------------------------
  if (long && n_w_cells > 1) {
    w_nms <- apply(within_combos, 1, paste, collapse = .opt("sep"))
    id_cols <- c(id, names(between))
    df <- tidyr::pivot_longer(df, cols = tidyselect::all_of(w_nms),
                               names_to  = paste(names(within), collapse = .opt("sep")),
                               values_to = dv)
    # Split compound within-factor column back into individual factor columns
    if (length(within) > 1) {
      wkey <- paste(names(within), collapse = .opt("sep"))
      sep  <- .opt("sep")
      df <- tidyr::separate(df, col = tidyselect::all_of(wkey),
                             into = names(within), sep = sep, remove = TRUE)
    }
    for (f in names(within)) {
      if (f %in% names(df)) df[[f]] <- factor(df[[f]], levels = names(within[[f]]))
    }
    attr(df, "design") <- design
  }

  # ---- plot -----------------------------------------------------------------
  do_plot <- if (is.null(plot)) isTRUE(.opt("plot")) else isTRUE(plot)
  if (do_plot) {
    tryCatch(print(plot_design(df)), error = function(e) invisible(NULL))
  }

  df
}

# Helper: resolve a mu/sd spec into an n_w x n_b matrix
.resolve_param_matrix <- function(x, n_w, n_b, row_nms, col_nms) {
  if (is.data.frame(x)) {
    mat <- as.matrix(x)
  } else if (is.matrix(x)) {
    mat <- x
  } else if (is.vector(x)) {
    if (length(x) == 1) {
      mat <- matrix(x, n_w, n_b)
    } else if (length(x) == n_w) {
      mat <- matrix(x, n_w, n_b)
    } else if (length(x) == n_b) {
      mat <- matrix(rep(x, each = n_w), n_w, n_b)
    } else if (length(x) == n_w * n_b) {
      mat <- matrix(x, n_w, n_b)
    } else {
      stop("Cannot match parameter vector of length ", length(x),
           " to design with ", n_w, " within-cells and ", n_b, " between-cells.",
           call. = FALSE)
    }
  } else {
    stop("mu/sd must be numeric scalar, vector, matrix, or data.frame.", call. = FALSE)
  }
  if (!is.null(row_nms) && length(row_nms) == nrow(mat)) rownames(mat) <- row_nms
  if (!is.null(col_nms) && length(col_nms) == ncol(mat)) colnames(mat) <- col_nms
  mat
}

#' Get the design attribute from a dataforge data frame
#'
#' @param df A data frame created by [forge_design()].
#' @return The design list, or `NULL` if absent.
#' @export
get_design <- function(df) attr(df, "design")

#' Validate and summarise a design specification
#'
#' @param within Named list of within factors (same format as [forge_design()]).
#' @param between Named list of between factors.
#' @param n Sample size(s).
#' @param mu Cell means.
#' @param sd Cell SDs.
#' @param r Correlation structure.
#' @return Invisibly returns a list describing the validated design; prints a
#'   summary.
#' @export
#' @examples
#' check_design(within = list(time = c("pre","post")),
#'              between = list(group = c("A","B")),
#'              n = 30, mu = 0, sd = 1)
check_design <- function(within = list(), between = list(),
                         n = 100, mu = 0, sd = 1, r = 0) {
  within  <- within  %||% list()
  between <- between %||% list()

  n_w <- prod(lengths(within))  %||% 1L
  n_b <- prod(lengths(between)) %||% 1L
  if (n_w == 0) n_w <- 1L
  if (n_b == 0) n_b <- 1L

  cat("== dataforge Design Summary ==\n")
  cat(sprintf("  Within factors  : %s\n",
              if (length(within) == 0) "(none)" else paste(names(within), collapse=", ")))
  cat(sprintf("  Between factors : %s\n",
              if (length(between) == 0) "(none)" else paste(names(between), collapse=", ")))
  cat(sprintf("  Within cells    : %d\n", n_w))
  cat(sprintf("  Between cells   : %d\n", n_b))
  cat(sprintf("  Total cells     : %d\n", n_w * n_b))
  invisible(list(within = within, between = between, n_w = n_w, n_b = n_b))
}

#' Get descriptive statistics from a dataforge data frame
#'
#' @param df A data frame (ideally created by [forge_design()]).
#' @param between Character vector of between-subject factor column names.
#' @param within Character vector of within-subject factor column names.
#' @param dv Character. Name of the DV column (for long-format data).
#' @param digits Integer. Number of decimal places to round to (default 4).
#' @return A data frame of summary statistics (n, mean, sd, min, max per cell).
#' @export
#' @examples
#' df <- forge_design(between = list(g = c("A","B")), n = 50,
#'                    mu = c(0,1), sd = 1, plot = FALSE)
#' get_params(df, between = "g", dv = "y")
get_params <- function(df, between = NULL, within = NULL, dv = NULL, digits = 4) {
  group_cols <- c(between, within)
  if (is.null(dv)) {
    # Try to guess: numeric columns that are not grouping cols
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    dv <- setdiff(num_cols, group_cols)
  }

  if (length(group_cols) == 0) {
    out <- lapply(dv, function(v) {
      data.frame(var = v, n = sum(!is.na(df[[v]])),
                 mean = round(mean(df[[v]], na.rm=TRUE), digits),
                 sd   = round(sd(df[[v]], na.rm=TRUE), digits),
                 min  = round(min(df[[v]], na.rm=TRUE), digits),
                 max  = round(max(df[[v]], na.rm=TRUE), digits))
    })
    return(do.call(rbind, out))
  }

  g_syms <- lapply(group_cols, as.symbol)
  out <- lapply(dv, function(v) {
    df %>%
      dplyr::group_by(!!!g_syms) %>%
      dplyr::summarise(var  = v,
                       n    = dplyr::n(),
                       mean = round(mean(.data[[v]], na.rm=TRUE), digits),
                       sd   = round(sd(.data[[v]],   na.rm=TRUE), digits),
                       min  = round(min(.data[[v]],   na.rm=TRUE), digits),
                       max  = round(max(.data[[v]],   na.rm=TRUE), digits),
                       .groups = "drop")
  })
  do.call(rbind, out)
}

#' @rdname get_params
#' @export
check_sim_stats <- get_params
