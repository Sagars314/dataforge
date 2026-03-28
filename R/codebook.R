#' Generate a Codebook from a Data Frame
#'
#' @description
#' Creates a structured codebook (data dictionary) summarising each column in a
#' data frame: data type, number of missing values, range or unique values, and
#' summary statistics for numeric variables.
#'
#' @param data A data frame.
#' @param meta A named list of additional metadata per column. Each element
#'   should be a named list with optional fields: `description`, `units`,
#'   `scale`.
#' @param output One of `"df"` (a tidy data frame), `"json"` (a JSON string),
#'   or `"print"` (formatted console output).
#'
#' @return Depends on `output`:
#'   - `"df"`: A data frame.
#'   - `"json"`: A character string (JSON).
#'   - `"print"`: The data frame, invisibly (also prints to console).
#'
#' @examples
#' df <- forge_design(
#'   within = list(time = c("pre","post")),
#'   n = 30, mu = c(10, 13), sd = 2, r = 0.5, plot = FALSE
#' )
#' forge_codebook(df, output = "print")
#'
#' @export
forge_codebook <- function(data, meta = list(), output = c("df", "json", "print")) {
  output <- match.arg(output)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)

  rows <- lapply(names(data), function(col) {
    x      <- data[[col]]
    type   <- class(x)[1]
    n_miss <- sum(is.na(x))
    n_valid <- sum(!is.na(x))

    if (is.numeric(x)) {
      summary_str <- sprintf("mean = %.3g, sd = %.3g, [%.3g, %.3g]",
                             mean(x, na.rm = TRUE),
                             stats::sd(x, na.rm = TRUE),
                             min(x, na.rm = TRUE),
                             max(x, na.rm = TRUE))
    } else {
      levs <- if (is.factor(x)) levels(x) else sort(unique(as.character(x)))
      summary_str <- paste(levs, collapse = ", ")
    }

    extra <- if (!is.null(meta[[col]])) meta[[col]] else list()

    data.frame(
      column      = col,
      type        = type,
      n_valid     = n_valid,
      n_missing   = n_miss,
      description = if (!is.null(extra$description)) extra$description else "",
      units       = if (!is.null(extra$units))       extra$units       else "",
      summary     = summary_str,
      stringsAsFactors = FALSE
    )
  })

  cb <- do.call(rbind, rows)
  rownames(cb) <- NULL

  switch(output,
    df = cb,

    json = {
      # Use jsonlite if available; otherwise fall back to a hand-built JSON
      if (requireNamespace("jsonlite", quietly = TRUE)) {
        jsonlite::toJSON(cb, pretty = TRUE)
      } else {
        .df_to_json(cb)
      }
    },

    print = {
      cat("\n=== dataforge codebook ===\n")
      cat(sprintf("Rows: %d   Columns: %d\n\n", nrow(data), ncol(data)))
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
    s <- gsub('"', '\\\\"', s)
    s <- gsub("\n", "\\\\n", s)
    s
  }

  rows <- lapply(seq_len(nrow(df)), function(i) {
    pairs <- mapply(function(col, val) {
      val_str <- if (is.numeric(val)) {
        if (is.na(val)) "null" else as.character(val)
      } else {
        sprintf('"%s"', escape_str(as.character(val)))
      }
      sprintf('    "%s": %s', col, val_str)
    }, names(df), df[i, ], SIMPLIFY = TRUE)
    paste0("  {\n", paste(pairs, collapse = ",\n"), "\n  }")
  })

  paste0("[\n", paste(rows, collapse = ",\n"), "\n]")
}
