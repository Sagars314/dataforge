# Internal environment to store package-level options
# .dataforge_env <- new.env(parent = emptyenv())
# .dataforge_env$options <- list(
#   verbose   = TRUE,
#   plot      = TRUE,
#   long      = FALSE,
#   seed      = NULL,
#   sep       = "_"
# )

#' Set or Get Global dataforge Options
#'
#' @description
#' Control package-wide defaults such as verbosity, automatic plotting,
#' output format, and the separator used in column name construction.
#'
#' @param verbose Logical. If `TRUE` (default), functions print informational
#'   messages describing the design.
#' @param plot Logical. If `TRUE` (default), `forge_design()` automatically
#'   plots the design after simulation.
#' @param long Logical. If `TRUE`, `forge_design()` returns data in long
#'   format. Default is `FALSE` (wide format).
#' @param sep Character. Separator used when constructing column names from
#'   factor level combinations. Default is `"_"`.
#' @param seed Integer or `NULL`. If set, a random seed is applied at the
#'   start of every simulation call for reproducibility.
#'
#' @return When called with arguments, invisibly returns the previous option
#'   list. When called with no arguments, returns the current option list.
#'
#' @examples
#' # Turn off automatic plotting and messages
#' dataforge_options(verbose = FALSE, plot = FALSE)
#'
#' # Check current options
#' dataforge_options()
#'
#' # Reset to defaults
#' dataforge_options(verbose = TRUE, plot = TRUE, long = FALSE,
#'                   seed = NULL, sep = "_")
#'
#' @export
dataforge_options <- function(verbose = NULL, plot = NULL, long = NULL,
                               sep = NULL, seed = NULL) {
  prev <- .dataforge_env$options

  if (!is.null(verbose)) .dataforge_env$options$verbose <- verbose
  if (!is.null(plot))    .dataforge_env$options$plot    <- plot
  if (!is.null(long))    .dataforge_env$options$long    <- long
  if (!is.null(sep))     .dataforge_env$options$sep     <- sep
  if (!is.null(seed))    .dataforge_env$options$seed    <- seed

  # Called with no arguments — just return current options
  if (all(c(is.null(verbose), is.null(plot), is.null(long),
            is.null(sep), is.null(seed)))) {
    return(.dataforge_env$options)
  }

  invisible(prev)
}

# Internal accessor used throughout the package
# .opt <- function(name) .dataforge_env$options[[name]]
