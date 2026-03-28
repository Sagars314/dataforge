#' Convert Wide Data to Long Format
#'
#' @description
#' Reshape a wide data frame (as produced by \code{\link{forge_design}}) to long format,
#' where within-subject measurements become rows. The column names are parsed
#' to reconstruct factor columns for each within-subject factor.
#'
#' @param data A wide data frame.
#' @param within Character vector of within-factor names. These correspond to
#'   parts of the wide column names separated by `sep`.
#' @param dv Character. Name for the new dependent variable column. Default `"y"`.
#' @param id Character. Name of the subject ID column. Default `"id"`.
#' @param sep Character. Separator used in wide column names. Default `"_"`.
#' @param other_cols Character vector of additional columns to carry forward.
#'   If `NULL` (default), all factor/character columns are treated as fixed.
#'
#' @return A long data frame with one row per subject × measurement combination.
#'
#' @examples
#' wide <- forge_design(
#'   within = list(time = c("pre", "post")),
#'   n = 20, mu = c(5, 8), sd = 1, r = 0.4,
#'   plot = FALSE
#' )
#' long <- wide2long(wide, within = "time")
#' head(long)
#'
#' @export
wide2long <- function(data, within = NULL, dv = "y", id = "id",
                      sep = "_", other_cols = NULL) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)

  # Try to get `within` from design metadata if not supplied
  if (is.null(within)) {
    des <- attr(data, "dataforge_design")
    if (!is.null(des)) within <- names(des$within)
  }

  all_cols <- names(data)

  # Determine fixed (non-measure) columns
  if (!is.null(other_cols)) {
    fixed_cols <- unique(c(id, other_cols))
  } else {
    is_fixed <- vapply(data, function(x) is.factor(x) || is.character(x), logical(1))
    fixed_cols <- all_cols[is_fixed]
    if (!is.null(id) && id %in% all_cols) fixed_cols <- union(id, fixed_cols)
  }

  measure_cols <- setdiff(all_cols, fixed_cols)
  if (length(measure_cols) == 0)
    stop("No measure columns found. Check column types or `other_cols`.", call. = FALSE)

  n_rows    <- nrow(data)
  n_measure <- length(measure_cols)

  # Replicate fixed columns n_measure times (stack vertically)
  long <- data[rep(seq_len(n_rows), times = n_measure), fixed_cols, drop = FALSE]
  rownames(long) <- NULL

  # Stack DV values and record which measure column they came from
  long[[dv]]          <- unlist(lapply(measure_cols, function(mc) data[[mc]]),
                                use.names = FALSE)
  long[[".within_key"]] <- rep(measure_cols, each = n_rows)

  # Parse within column names to reconstruct factor columns
  if (!is.null(within) && length(within) > 0) {
    parts <- strsplit(long[[".within_key"]], sep, fixed = TRUE)
    for (i in seq_along(within)) {
      long[[within[i]]] <- factor(vapply(parts, function(p)
        if (length(p) >= i) p[[i]] else NA_character_,
        character(1)))
    }
  } else {
    long[["within"]] <- long[[".within_key"]]
  }

  long[[".within_key"]] <- NULL
  long
}

#' Convert Long Data to Wide Format
#'
#' @description
#' Reshape a long data frame to wide format, producing one column per
#' combination of within-subject factor levels.
#'
#' @param data A long data frame.
#' @param id Character. Subject ID column. Default `"id"`.
#' @param within Character vector of within-subject factor column names.
#' @param dv Character. Dependent variable column. Default `"y"`.
#' @param sep Character. Separator for new column names. Default `"_"`.
#'
#' @return A wide data frame with one row per subject.
#'
#' @examples
#' long <- data.frame(
#'   id    = rep(1:5, each = 2),
#'   time  = rep(c("pre","post"), 5),
#'   score = rnorm(10, c(10, 15))
#' )
#' long2wide(long, id = "id", within = "time", dv = "score")
#'
#' @export
long2wide <- function(data, id = "id", within, dv = "y", sep = "_") {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!all(within %in% names(data)))
    stop("Some `within` columns not found in `data`.", call. = FALSE)
  if (!(dv %in% names(data)))
    stop(sprintf("DV column '%s' not found.", dv), call. = FALSE)

  # Build a single within-key column from all within factors
  if (length(within) == 1) {
    wkeys <- as.character(data[[within]])
  } else {
    wkeys <- apply(data[within], 1, paste, collapse = sep)
  }

  unique_ids  <- unique(data[[id]])
  unique_keys <- unique(wkeys)

  wide <- data.frame(setNames(list(unique_ids), id), stringsAsFactors = FALSE)

  for (k in unique_keys) {
    idx  <- wkeys == k
    sub  <- data[idx, , drop = FALSE]
    vals <- sub[[dv]][match(unique_ids, sub[[id]])]
    wide[[k]] <- vals
  }

  wide
}
