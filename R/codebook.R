# Similar to `summary()` but with a more structured output and support for metadata.
# improved forge_codebook ≈ str() + summary() + metadata table
# Similar to `summary()` but with a more structured output and support for metadata.

#' Generate a Codebook from a Data Frame
#'
#' @description
#' Creates a structured codebook (data dictionary) summarising each column in a
#' data frame: data type, number and percentage of missing values, number of
#' unique values, and summary statistics appropriate to the column type:
#' - **Numeric**: mean, sd, median, IQR, min, max
#' - **Logical**: TRUE/FALSE counts and percentages
#' - **Date/POSIXt**: min, max, and number of unique dates
#' - **Factor/Character**: top-N most frequent levels with counts and
#'   percentages; truncated with "..." when cardinality exceeds `max_levels`
#'
#' @param data A data frame.
#' @param meta A named list of additional metadata per column. Each element
#'   should be a named list with optional fields: `description`, `units`,
#'   `scale`.
#' @param output One of `"df"` (a tidy data frame), `"json"` (a JSON string),
#'   or `"print"` (formatted console output).
#' @param max_levels Integer. Maximum number of unique levels to display for
#'   factor/character columns before truncating with "...". Default `5`.
#'
#' @return Depends on `output`:
#'   - `"df"`: A data frame with columns `column`, `type`, `n_valid`,
#'     `n_missing`, `pct_missing`, `n_unique`, `description`, `units`,
#'     `summary`.
#'   - `"json"`: A character string (JSON).
#'   - `"print"`: The data frame, invisibly (also prints to console).
#'
#' @examples
#' df <- forge_design(
#'   within = list(time = c("pre", "post")),
#'   n = 30, mu = c(10, 13), sd = 2, r = 0.5, plot = FALSE
#' )
#' forge_codebook(df, output = "print")
#'
#' # With metadata
#' forge_codebook(df,
#'   meta = list(time = list(description = "Measurement occasion", scale = "nominal")),
#'   output = "print"
#' )
#'
#' @export
forge_codebook <- function(data,
                           meta       = list(),
                           output     = c("df", "json", "print"),
                           max_levels = 5L) {

  # --- Input validation -------------------------------------------------------
  output <- match.arg(output)

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame, not a ", class(data)[1], ".", call. = FALSE)
  }
  if (!is.list(meta)) {
    stop("`meta` must be a named list.", call. = FALSE)
  }
  if (!is.numeric(max_levels) || length(max_levels) != 1L || max_levels < 1L) {
    stop("`max_levels` must be a positive integer.", call. = FALSE)
  }
  max_levels <- as.integer(max_levels)

  n_rows <- nrow(data)

  # --- Per-column summary helper ----------------------------------------------
  .summarise_col <- function(col) {
    x       <- data[[col]]
    n_miss  <- sum(is.na(x))
    n_valid <- n_rows - n_miss
    pct_miss <- if (n_rows > 0L) round(100 * n_miss / n_rows, 1) else NA_real_

    # Determine type label (more precise than class()[1])
    type <- if (inherits(x, "POSIXt")) {
      "POSIXt"
    } else if (inherits(x, "Date")) {
      "Date"
    } else {
      class(x)[1]
    }

    # n_unique (on non-missing values)
    x_valid  <- x[!is.na(x)]
    n_unique <- length(unique(x_valid))

    # --- Type-specific summary string ----------------------------------------
    summary_str <- if (is.logical(x)) {
      # Logical: TRUE/FALSE counts and %
      n_true  <- sum(x_valid)
      n_false <- n_valid - n_true
      pct_t   <- if (n_valid > 0L) round(100 * n_true  / n_valid, 1) else NA_real_
      pct_f   <- if (n_valid > 0L) round(100 * n_false / n_valid, 1) else NA_real_
      sprintf("TRUE: %d (%.1f%%), FALSE: %d (%.1f%%)", n_true, pct_t, n_false, pct_f)

    } else if (is.numeric(x)) {
      if (n_valid == 0L) {
        "all NA"
      } else {
        q   <- stats::quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
        iqr <- q[3] - q[1]
        sprintf(
          "mean=%.3g, sd=%.3g, median=%.3g, IQR=%.3g, [%.3g, %.3g]",
          mean(x, na.rm = TRUE),
          stats::sd(x, na.rm = TRUE),
          q[2],
          iqr,
          min(x, na.rm = TRUE),
          max(x, na.rm = TRUE)
        )
      }

    } else if (inherits(x, "Date") || inherits(x, "POSIXt")) {
      if (n_valid == 0L) {
        "all NA"
      } else {
        sprintf("min=%s, max=%s, n_unique=%d",
                format(min(x_valid)), format(max(x_valid)), n_unique)
      }

    } else {
      # Factor / character / other: top-N most frequent levels
      if (n_valid == 0L) {
        "all NA"
      } else {
        # Respect factor level ordering for the sort, otherwise frequency-sort
        tbl <- if (is.factor(x)) {
          table(x_valid)[order(-table(x_valid))]
        } else {
          sort(table(x_valid), decreasing = TRUE)
        }
        n_show   <- min(max_levels, length(tbl))
        top      <- tbl[seq_len(n_show)]
        pct_top  <- round(100 * top / n_valid, 1)
        parts    <- sprintf("%s (%d, %.1f%%)", names(top), top, pct_top)
        suffix   <- if (length(tbl) > max_levels) ", ..." else ""
        paste0(paste(parts, collapse = "; "), suffix)
      }
    }

    # --- Pull metadata --------------------------------------------------------
    extra       <- if (!is.null(meta[[col]])) meta[[col]] else list()
    description <- if (!is.null(extra$description)) extra$description else ""
    units       <- if (!is.null(extra$units))       extra$units       else ""

    # Return as a one-row data frame
    data.frame(
      column      = col,
      type        = type,
      n_valid     = n_valid,
      n_missing   = n_miss,
      pct_missing = pct_miss,
      n_unique    = n_unique,
      description = description,
      units       = units,
      summary     = summary_str,
      stringsAsFactors = FALSE
    )
  }

  # --- Build codebook ---------------------------------------------------------
  rows <- lapply(names(data), .summarise_col)
  cb   <- do.call(rbind, rows)
  rownames(cb) <- NULL

  # --- Output -----------------------------------------------------------------
  switch(output,

         df = cb,

         json = {
           if (requireNamespace("jsonlite", quietly = TRUE)) {
             jsonlite::toJSON(cb, pretty = TRUE, na = "null", dataframe = "rows")
           } else {
             .df_to_json(cb)
           }
         },

         print = {
           cat("\n=== dataforge codebook ===\n")
           cat(sprintf("Rows: %d   Columns: %d\n\n", n_rows, ncol(data)))
           # Flag any high-missingness columns
           hi_miss <- cb$column[cb$pct_missing > 20]
           if (length(hi_miss)) {
             cat(sprintf("Note: high missingness (>20%%) in: %s\n\n",
                         paste(hi_miss, collapse = ", ")))
           }
           print(cb, right = FALSE)
           cat("\n")
           invisible(cb)
         }
  )
}


# ---------------------------------------------------------------------------
# Internal: bare-bones data-frame to JSON (no external deps)
# ---------------------------------------------------------------------------
.df_to_json <- function(df) {
  escape_str <- function(s) {
    s <- gsub("\\\\", "\\\\\\\\", s)
    s <- gsub('"',    '\\\\"',    s)
    s <- gsub("\n",   "\\\\n",    s)
    s
  }

  rows <- lapply(seq_len(nrow(df)), function(i) {
    pairs <- mapply(function(col, val) {
      val_str <- if (is.numeric(val) && !is.na(val)) {
        as.character(val)
      } else if (is.na(val)) {
        "null"
      } else {
        sprintf('"%s"', escape_str(as.character(val)))
      }
      sprintf('    "%s": %s', col, val_str)
    }, names(df), df[i, , drop = FALSE], SIMPLIFY = TRUE)
    paste0("  {\n", paste(pairs, collapse = ",\n"), "\n  }")
  })

  paste0("[\n", paste(rows, collapse = ",\n"), "\n]")
}
