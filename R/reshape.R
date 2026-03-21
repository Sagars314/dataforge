#' Convert data from wide to long format
#'
#' A thin wrapper around [tidyr::pivot_longer()] with sensible defaults for
#' repeated-measures data.
#'
#' @param data A wide-format data frame.
#' @param within_cols Character vector of column names representing within-
#'   subject measurements.
#' @param id_col Character. Name of the subject ID column (default `"id"`).
#' @param values_to Character. Name for the new values column (default `"y"`).
#' @param names_to Character. Name for the new factor column (default
#'   `"condition"`).
#'
#' @return A long-format data frame.
#' @export
#' @examples
#' df <- forge_design(within = list(time = c("pre","post")),
#'                    n = 20, mu = c(5, 7), sd = 1, r = .6, plot = FALSE)
#' long_df <- wide2long(df, within_cols = c("pre","post"))
#' head(long_df)
wide2long <- function(data, within_cols, id_col = "id",
                      values_to = "y", names_to = "condition") {
  tidyr::pivot_longer(data,
                      cols      = tidyselect::all_of(within_cols),
                      names_to  = names_to,
                      values_to = values_to)
}

#' Convert data from long to wide format
#'
#' A thin wrapper around [tidyr::pivot_wider()] with sensible defaults for
#' repeated-measures data.
#'
#' @param data A long-format data frame.
#' @param id_cols Character vector of ID column names (default `"id"`).
#' @param names_from Character. Column whose values become new column names
#'   (default `"condition"`).
#' @param values_from Character. Column to spread (default `"y"`).
#'
#' @return A wide-format data frame.
#' @export
#' @examples
#' df <- forge_design(within = list(time = c("pre","post")),
#'                    n = 20, mu = c(5, 7), sd = 1, r = .6, long = TRUE,
#'                    plot = FALSE)
#' wide_df <- long2wide(df, id_cols = "id", names_from = "time",
#'                      values_from = "y")
#' head(wide_df)
long2wide <- function(data, id_cols = "id", names_from = "condition",
                      values_from = "y") {
  tidyr::pivot_wider(data,
                     id_cols     = tidyselect::all_of(id_cols),
                     names_from  = tidyselect::all_of(names_from),
                     values_from = tidyselect::all_of(values_from))
}
