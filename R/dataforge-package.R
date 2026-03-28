#' dataforge: Simulation Tools for Factorial and Mixed Designs
#'
#' @description
#' The \pkg{dataforge} package provides a suite of functions for simulating
#' structured data, particularly for factorial and mixed experimental designs
#' common in social science and psychology research.
#'
#' \strong{Core capabilities:}
#'
#' \itemize{
#'   \item \strong{Factorial design simulation}: \code{forge_design()} simulates data for
#'     within- and between-subject factorial designs with full control over
#'     means, SDs, and correlations.
#'   \item \strong{Correlated normal variables}: \code{rnorm_forge()} generates multiple
#'     correlated normal variables from a specified correlation matrix.
#'   \item \strong{Simulate from existing data}: \code{forge_df()} resamples from an
#'     existing data frame preserving the distributional structure.
#'   \item \strong{Mixed effects builders}: \code{add_random()}, \code{add_between()},
#'     \code{add_within()}, and \code{add_ranef()} provide a pipe-friendly interface
#'     for building complex nested/crossed designs.
#'   \item \strong{NORTA / distribution conversions}: \code{rmulti()} and the
#'     \code{norm2x()} family convert normal variates to beta, binomial, gamma,
#'     Likert, Poisson, and other distributions.
#'   \item \strong{Likert scale}: \code{rlikert()}, \code{dlikert()}, \code{plikert()},
#'     and \code{qlikert()} form a complete Likert distribution family.
#'   \item \strong{Data reshaping}: \code{long2wide()} and \code{wide2long()} handle
#'     within-subject data with proper naming conventions.
#'   \item \strong{Missing data}: \code{messy()} introduces missing values at a
#'     specified rate.
#'   \item \strong{Visualisation}: \code{plot_design()} produces \pkg{ggplot2}
#'     violin/box plots from simulated data frames.
#'   \item \strong{Codebook generation}: \code{forge_codebook()} creates structured
#'     metadata from data frames.
#' }
#'
#' @section Getting started:
#' \preformatted{
#' library(dataforge)
#'
#' # Simulate a 2 x 3 mixed factorial design
#' df <- forge_design(
#'   within  = list(time = c("pre", "mid", "post")),
#'   between = list(group = c("control", "treatment")),
#'   n   = 50,
#'   mu  = c(10, 12, 14, 10, 15, 20),
#'   sd  = 3,
#'   r   = 0.6
#' )
#' head(df)
#' }
#'
#' @seealso
#' \code{\link{forge_design}}, \code{\link{rnorm_forge}}, \code{\link{add_random}},
#' \code{\link{rmulti}}, \code{\link{plot_design}}, \code{\link{dataforge_options}}
#'
#' @docType package
#' @name dataforge-package
#' @aliases dataforge
"_PACKAGE"
