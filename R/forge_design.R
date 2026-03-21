forge_design <- function(
  between = list(),
  within = list(),
  n = 30,
  mu = 0,
  sd = 1,
  r = 0
){

  design <- expand.grid(
    between,
    within
  )

  design$id <- rep(
    1:n,
    each = nrow(design)
  )

  design$value <- rnorm(
    nrow(design),
    mean = mu,
    sd = sd
  )

  tibble::as_tibble(design)
}