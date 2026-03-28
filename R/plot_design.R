#' Plot a Simulated Design
#'
#' @description
#' Generate a `ggplot2` visualization of a simulated data frame produced by
#' \code{\link{forge_design}}. Supports violin + box + point overlays, and automatically
#' maps factorial structure to aesthetics.
#'
#' @param data A data frame (ideally a `forge_data` object from
#'   \code{\link{forge_design}}, or a long data frame).
#' @param x Character. Column to map to the x-axis.
#' @param color Character. Column to map to colour / fill.
#' @param facet Character. Column to use for faceting. Optional.
#' @param dv Character. Dependent variable column (for long format data).
#'   Default `"y"`.
#' @param geom Character vector. One or more of `"violin"`, `"box"`, `"point"`,
#'   `"line"`. Default `c("violin", "box")`.
#' @param alpha Numeric. Transparency for geoms. Default 0.5.
#' @param palette Character. A `ggplot2`-compatible colour palette name, or a
#'   character vector of colours.
#' @param title Character. Plot title. Optional.
#'
#' @return A `ggplot2` object, or `NULL` invisibly if ggplot2 is not installed.
#'
#' @examples
#' df <- forge_design(
#'   within  = list(time = c("pre","post")),
#'   between = list(group = c("control","treat")),
#'   n = 40, mu = c(10,13,10,17), sd = 2, r = .5, plot = FALSE
#' )
#' plot_design(df, x = "time", color = "group")
#'
#' @export
plot_design <- function(data, x = NULL, color = NULL, facet = NULL,
                        dv = "y", geom = c("violin", "box"),
                        alpha = 0.5, palette = NULL, title = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message("ggplot2 is required for plotting. Install it with install.packages('ggplot2').")
    return(invisible(NULL))
  }

  # Get design metadata if available
  des <- attr(data, "dataforge_design")

  # Determine whether data is long or wide
  is_long <- dv %in% names(data)

  if (!is_long) {
    # Wide → melt to long using base R (wide2long)
    within_cols  <- if (!is.null(des)) des$within_col_names else
      names(data)[vapply(data, is.numeric, logical(1))]
    between_cols <- if (!is.null(des)) names(des$between) else
      names(data)[vapply(data, is.factor, logical(1))]

    # Manually stack (base R equivalent of pivot_longer)
    fixed_cols <- setdiff(names(data), within_cols)
    n_rows <- nrow(data)
    long <- data[rep(seq_len(n_rows), times = length(within_cols)),
                 fixed_cols, drop = FALSE]
    # long[[dv]]       <- unlist(lapply(within_cols, function(wc) data[[wc]]), use.names = FALSE)
    # long[["within"]] <- rep(within_cols, each = n_rows)
    # rownames(long)   <- NULL
    # data <- long
    # if (is.null(x))     x     <- "within"
    # if (is.null(color) && length(between_cols) > 0) color <- between_cols[1]

    long[[dv]] <- unlist(lapply(within_cols, function(wc) data[[wc]]),
                         use.names = FALSE)

    within_name <- if (!is.null(des) && length(names(des$within)) == 1) {
      names(des$within)[1]
    } else {
      "within"
    }

    long[[within_name]] <- rep(within_cols, each = n_rows)
    rownames(long) <- NULL
    data <- long

    if (is.null(x)) x <- within_name
    if (!is.null(x) && x == "within" && within_name != "within") x <- within_name
    if (is.null(color) && length(between_cols) > 0) color <- between_cols[1]

  } else {
    within_cols <- names(data)[vapply(data, is.factor, logical(1))]
    if (is.null(x) && length(within_cols) > 0) x <- within_cols[1]
  }

  if (is.null(x)) stop("No suitable x variable found. Specify `x`.", call. = FALSE)

  # Build aes using strings (no rlang)
  base_aes <- ggplot2::aes_string(x = x, y = dv)
  if (!is.null(color)) {
    col_aes <- ggplot2::aes_string(colour = color, fill = color)
    full_aes <- c(base_aes, col_aes)
    class(full_aes) <- "uneval"
  } else {
    full_aes <- base_aes
  }

  p <- ggplot2::ggplot(data, full_aes)

  dodge <- ggplot2::position_dodge(0.8)

  if ("violin" %in% geom) {
    p <- p + ggplot2::geom_violin(alpha = alpha, trim = FALSE, position = dodge)
  }
  if ("box" %in% geom) {
    p <- p + ggplot2::geom_boxplot(width = 0.2, alpha = alpha, position = dodge,
                                    outlier.shape = NA)
  }
  if ("point" %in% geom) {
    p <- p + ggplot2::geom_jitter(alpha = alpha * 0.7,
                                   position = ggplot2::position_jitterdodge(
                                     dodge.width = 0.8, jitter.width = 0.1))
  }
  if ("line" %in% geom) {
    if (!is.null(color)) {
      p <- p + ggplot2::stat_summary(
        fun = mean, geom = "line",
        mapping = ggplot2::aes_string(group = color),
        position = dodge)
    } else {
      p <- p + ggplot2::stat_summary(fun = mean, geom = "line",
                                      mapping = ggplot2::aes(group = 1))
    }
  }

  if (!is.null(facet)) {
    p <- p + ggplot2::facet_wrap(facet)
  }

  if (!is.null(palette)) {
    if (length(palette) == 1) {
      p <- p + ggplot2::scale_colour_brewer(palette = palette) +
               ggplot2::scale_fill_brewer(palette = palette)
    } else {
      p <- p + ggplot2::scale_colour_manual(values = palette) +
               ggplot2::scale_fill_manual(values = palette)
    }
  }

  p + ggplot2::theme_bw(base_size = 13) +
      ggplot2::labs(x = x, y = dv, title = title) +
      ggplot2::theme(legend.position = "top")
}

#' @export
plot.forge_data <- function(x, ...) plot_design(x, ...)
