# Package internal environment (created immediately when sourced)

.dataforge_env <- new.env(parent = emptyenv())

.dataforge_env$options <- list(
  verbose = TRUE,
  plot    = TRUE,
  long    = FALSE,
  seed    = NULL,
  sep     = "_"
)

# internal accessor used across package
# .opt <- function(name) {
#   .dataforge_env$options[[name]]
# }

# ensure function exists during sourcing
# dataforge_options <- function(verbose = NULL, plot = NULL, long = NULL,
#                               sep = NULL, seed = NULL) {
#
#   prev <- .dataforge_env$options
#
#   if (!is.null(verbose)) .dataforge_env$options$verbose <- verbose
#   if (!is.null(plot))    .dataforge_env$options$plot    <- plot
#   if (!is.null(long))    .dataforge_env$options$long    <- long
#   if (!is.null(sep))     .dataforge_env$options$sep     <- sep
#   if (!is.null(seed))    .dataforge_env$options$seed    <- seed
#
#   if (all(c(is.null(verbose), is.null(plot), is.null(long),
#             is.null(sep), is.null(seed)))) {
#     return(.dataforge_env$options)
#   }
#
#   invisible(prev)
# }
