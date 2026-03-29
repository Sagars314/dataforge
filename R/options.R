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
  option_names <- c("verbose", "plot", "long", "seed", "sep")
  option_keys <- paste0("dataforge.", option_names)
  names(option_keys) <- option_names

  current <- options()[option_keys]
  prev <- unname(current)
  names(prev) <- option_names

  updates <- list(
    verbose = verbose,
    plot = plot,
    long = long,
    seed = seed,
    sep = sep
  )

  # Called with no arguments — just return current options
  if (all(c(is.null(verbose), is.null(plot), is.null(long),
            is.null(sep), is.null(seed)))) {
    return(prev)
  }

  to_set <- updates[!vapply(updates, is.null, logical(1))]
  if (length(to_set) > 0) {
    names(to_set) <- option_keys[names(to_set)]
    options(to_set)
  }

  invisible(prev)
}
