# stats -------------------------------------------------------------

# Core statistical functions used across simulation functions
#' @importFrom stats cor var setNames
#' cor var sd rnorm qnorm pnorm runif
#' qbeta pbeta qbinom pbinom qgamma pgamma
#' qpois ppois quantile median
#' contr.treatment contr.sum contr.helmert contr.poly
NULL

#' @importFrom rlang .data
NULL



# Matrix ------------------------------------------------------------

# Used to repair non-positive definite correlation matrices
#' @importFrom Matrix nearPD
NULL


# utils -------------------------------------------------------------

# Used internally by R packages
#' @importFrom utils head tail
NULL
