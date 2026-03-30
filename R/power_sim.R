#' Estimate Statistical Power by Simulation
#'
#' @description
#' Simulate datasets using \code{\link{forge_design}} and estimate statistical
#' power as the proportion of simulated analyses with p-values below `alpha`.
#'
#' @param within Named list of within-subject factors passed to
#'   \code{\link{forge_design}}.
#' @param between Named list of between-subject factors passed to
#'   \code{\link{forge_design}}.
#' @param n Sample size per between-subject cell.
#' @param mu Cell means.
#' @param sd Cell standard deviations.
#' @param r Correlation among within-subject measurements.
#' @param test Statistical test to apply:
#'   \itemize{
#'     \item \code{"t_test"} independent samples t-test
#'     \item \code{"anova"} factorial ANOVA
#'     \item \code{"lm"} linear model
#'   }
#' @param alpha Significance level. Default = 0.05.
#' @param rep Number of simulated datasets.
#' @param seed Optional random seed.
#' @param verbose Print progress messages.
#'
#' @return A list with elements:
#'   \itemize{
#'     \item power Estimated statistical power
#'     \item replications Number of simulations
#'     \item significant Number of significant results
#'   }
#'
#' @examples
#'
#' # Independent t-test (two groups)
#' power_sim(
#'   between = list(group = c("control","treat")),
#'   n = 40,
#'   mu = c(0, 0.5),
#'   sd = 1,
#'   rep = 500,
#'   test = "t_test"
#' )
#'
#' # One-way ANOVA (3 groups)
#' power_sim(
#'   between = list(group = c("A","B","C")),
#'   n = 30,
#'   mu = c(0, 0.3, 0.8),
#'   sd = 1,
#'   rep = 500,
#'   test = "anova"
#' )
#'
#' # Mixed design ANOVA (within + between)
#' power_sim(
#'   within  = list(time = c("pre","post")),
#'   between = list(group = c("control","treat")),
#'   n = 40,
#'   mu = c(10,10,10,13),
#'   sd = 2,
#'   r = 0.6,
#'   rep = 500,
#'   test = "anova"
#' )
#'
#' # Linear model with factorial predictors
#' power_sim(
#'   between = list(group = c("A","B")),
#'   within  = list(time = c("t1","t2","t3")),
#'   n = 30,
#'   mu = c(0,0.2,0.5,0,0.4,0.9),
#'   sd = 1,
#'   r = 0.5,
#'   rep = 500,
#'   test = "lm"
#' )
#'
#' @export
power_sim <- function(within = list(),
                      between = list(),
                      n = 50,
                      mu = 0,
                      sd = 1,
                      r = 0,
                      test = c("t_test", "anova", "lm"),
                      alpha = 0.05,
                      rep = 1000,
                      seed = NULL,
                      verbose = TRUE) {
  call <- match.call()
  test <- match.arg(test)

  if (!is.list(within)) {
    stop("`within` must be a list.", call. = FALSE)
  }
  if (!is.list(between)) {
    stop("`between` must be a list.", call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) ||
      alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a single number between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(rep) || length(rep) != 1L || is.na(rep) || rep < 1 ||
      rep != as.integer(rep)) {
    stop("`rep` must be a positive integer.", call. = FALSE)
  }
  rep <- as.integer(rep)
  if (rep < 100L) {
    warning("`rep` is less than 100; power estimates may be unstable.",
            call. = FALSE)
  }
  if (!is.null(seed) &&
      (!is.numeric(seed) || length(seed) != 1L || is.na(seed))) {
    stop("`seed` must be `NULL` or a single numeric value.", call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be `TRUE` or `FALSE`.", call. = FALSE)
  }

  within  <- .normalise_factor_list(within)
  between <- .normalise_factor_list(between)

  n_within <- if (length(within) == 0) 1L else prod(lengths(within))
  n_between <- if (length(between) == 0) 1L else prod(lengths(between))
  n_cells <- n_within * n_between

  .power_check_cell_param(mu, n_cells, "mu")
  .power_check_cell_param(sd, n_cells, "sd")

  if (test == "t_test") {
    if (length(between) != 1L) {
      stop("`t_test` requires exactly one between-subject factor.", call. = FALSE)
    }
    if (n_between != 2L) {
      stop("`t_test` requires exactly two groups.", call. = FALSE)
    }
    if (n_within != 1L) {
      stop("`t_test` requires a single response per subject.", call. = FALSE)
    }
  }

  factor_names <- c(names(between), names(within))
  if (test %in% c("anova", "lm") && length(factor_names) == 0L) {
    stop("`anova` and `lm` require at least one factor in `within` or `between`.",
         call. = FALSE)
  }

  old_opts <- dataforge_options()
  on.exit(do.call(dataforge_options, old_opts), add = TRUE)
  dataforge_options(verbose = FALSE, plot = FALSE, seed = NULL)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (verbose) {
    cat(sprintf("Running %d simulation%s with %s...\n",
                rep, if (rep == 1L) "" else "s", test))
  }

  sim_data <- forge_design(
    within = within,
    between = between,
    n = n,
    mu = mu,
    sd = sd,
    r = r,
    dv = "y",
    long = TRUE,
    plot = FALSE,
    seed = NULL,
    rep = rep,
    verbose = FALSE
  )

  if (!is.list(sim_data)) {
    sim_data <- list(sim_data)
  }

  p_values <- vapply(sim_data, function(dat) {
    .power_p_value(dat, test = test, between = between, within = within)
  }, numeric(1))

  significant <- sum(p_values < alpha, na.rm = TRUE)
  power <- significant / length(p_values)

  out <- list(
    power = power,
    alpha = alpha,
    replications = length(p_values),
    significant = significant,
    method = test,
    call = call
  )

  class(out) <- "power_sim"
  out
}

#' @export
print.power_sim <- function(x, ...) {
  cat("## Power Simulation Result\n\n")
  cat(sprintf("Test: %s\n", x$method))
  cat(sprintf("Alpha: %s\n", x$alpha))
  cat(sprintf("Replications: %s\n", x$replications))
  cat(sprintf("Significant results: %s\n", x$significant))
  cat(sprintf("Estimated power: %s\n", x$power))
  invisible(x)
}

.power_check_cell_param <- function(x, n_cells, name) {
  if (is.matrix(x) || is.data.frame(x)) {
    dims <- dim(x)
    if (is.null(dims) || prod(dims) != n_cells) {
      stop(sprintf("`%s` must have 1 or %d values.", name, n_cells),
           call. = FALSE)
    }
    return(invisible(TRUE))
  }

  if (!is.numeric(x)) {
    stop(sprintf("`%s` must be numeric.", name), call. = FALSE)
  }

  if (!(length(x) %in% c(1L, n_cells))) {
    stop(sprintf("`%s` must have 1 or %d values.", name, n_cells),
         call. = FALSE)
  }

  invisible(TRUE)
}

.power_p_value <- function(data, test, between, within) {
  if (test == "t_test") {
    group_name <- names(between)[1]
    fit <- stats::t.test(data$y ~ data[[group_name]])
    return(unname(fit$p.value))
  }

  factor_names <- c(names(between), names(within))
  form <- stats::as.formula(paste("y ~", paste(factor_names, collapse = " * ")))

  if (test == "anova") {
    fit <- stats::aov(form, data = data)
    tab <- summary(fit)[[1]]
    p_value <- tab[["Pr(>F)"]][1]
    return(unname(p_value))
  }

  fit <- stats::lm(form, data = data)
  tab <- stats::anova(fit)
  p_value <- tab[["Pr(>F)"]][1]
  unname(p_value)
}
