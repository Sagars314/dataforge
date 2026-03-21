#' Plot a simulated factorial design
#'
#' Creates a `ggplot2` visualisation of a data frame produced by
#' [forge_design()]. Supports wide and long format automatically.
#'
#' @param data A data frame (ideally from [forge_design()]).
#' @param x Character. Column to map to the x-axis. If `NULL` auto-detected.
#' @param color Character. Column to map to colour/fill. If `NULL` auto-
#'   detected.
#' @param facet Character. Column to use for faceting. If `NULL` no facets.
#' @param geom Character. One of `"violin"`, `"box"`, `"point"`, `"bar"`,
#'   `"line"` (default `"violin"`).
#' @param dv Character. Name of the DV column (long format). Auto-detected if
#'   `NULL`.
#' @param ... Additional arguments passed to the geom function.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' df <- forge_design(within  = list(time = c("pre","post")),
#'                    between = list(group = c("A","B")),
#'                    n = 40, mu = data.frame(A = c(10,12), B = c(10,15)),
#'                    sd = 2, r = .5, long = TRUE, plot = FALSE)
#' plot_design(df)
#' plot_design(df, geom = "box")
plot_design <- function(data, x = NULL, color = NULL, facet = NULL,
                        geom = "violin", dv = NULL, ...) {
  design <- get_design(data)

  # Auto-detect DV and factor columns
  factor_cols <- names(data)[vapply(data, is.factor, logical(1))]
  char_cols   <- names(data)[vapply(data, is.character, logical(1))]
  id_col      <- if (!is.null(design)) design$id else "id"
  group_cols  <- setdiff(c(factor_cols, char_cols), id_col)

  if (is.null(dv)) {
    num_cols <- names(data)[vapply(data, is.numeric, logical(1))]
    dv <- setdiff(num_cols, c(group_cols, id_col))
    if (length(dv) == 0) stop("No numeric DV column found.", call. = FALSE)
    dv <- dv[1]
  }

  if (is.null(x)) {
    x <- if (length(group_cols) >= 1) group_cols[1] else NULL
  }
  if (is.null(color) && length(group_cols) >= 2) {
    color <- group_cols[2]
  }
  if (is.null(facet) && length(group_cols) >= 3) {
    facet <- group_cols[3]
  }

  aes_args <- list(y = ggplot2::sym(dv))
  if (!is.null(x))     aes_args$x     <- ggplot2::sym(x)
  if (!is.null(color)) {
    aes_args$colour <- ggplot2::sym(color)
    aes_args$fill   <- ggplot2::sym(color)
  }

  p <- ggplot2::ggplot(data, do.call(ggplot2::aes, aes_args))

  geom_layer <- switch(geom,
    violin = ggplot2::geom_violin(alpha = 0.5, ...),
    box    = ggplot2::geom_boxplot(alpha = 0.5, ...),
    point  = ggplot2::geom_jitter(width = 0.2, alpha = 0.6, ...),
    bar    = ggplot2::stat_summary(fun = mean, geom = "bar", position = "dodge",
                                   alpha = 0.7, ...),
    line   = ggplot2::stat_summary(fun = mean, geom = "line",
                                   ggplot2::aes(group = if (!is.null(color))
                                     ggplot2::sym(color) else 1), ...),
    stop("Unknown geom: ", geom, call. = FALSE)
  )

  p <- p + geom_layer +
    ggplot2::labs(title = paste0("dataforge design: ", dv),
                  x = x %||% "", y = dv) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = if (is.null(color)) "none" else "right")

  if (!is.null(facet)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(!!ggplot2::sym(facet)))
  }

  p
}
