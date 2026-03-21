#' dataforge: Simulate Structured Data for Factorial and Mixed Designs
#'
#' @description
#' The `dataforge` package provides a comprehensive toolkit for simulating
#' structured datasets with user-specified statistical properties. It is designed
#' for researchers, educators, and data scientists who need reproducible synthetic
#' datasets for power analysis, teaching, or testing statistical workflows.
#'
#' ## Core capabilities
#'
#' - **Factorial design simulation** via [forge_design()] — specify between- and
#'   within-subject factors, sample sizes, means, SDs, and correlations.
#' - **Correlated multivariate normals** via [rnorm_multi()] and [rmulti()].
#' - **Simulate from existing data** via [forge_df()].
#' - **Mixed effects structures** via [add_random()], [add_between()],
#'   [add_within()], [add_ranef()].
#' - **Contrast coding** via [add_contrast()] and the `contr_code_*` family.
#' - **Distribution conversions** via the `norm2*` and `*2norm` families.
#' - **Likert-scale data** via [rlikert()].
#' - **Messy data** simulation via [make_missing()].
#' - **Plotting** via [plot_design()].
#'
#' @seealso
#' - [forge_design()] for factorial design simulation
#' - [rnorm_multi()] for correlated normal variables
#' - [add_random()] for mixed effects data building
#'
#' @aliases dataforge-package
#' @keywords internal
"_PACKAGE"
