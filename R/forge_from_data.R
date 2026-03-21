forge_from_data <- function(
  data,
  n,
  noise_sd = .1
){

  idx <- sample(
    1:nrow(data),
    n,
    replace = TRUE
  )

  sim <- data[idx,]

  numeric_cols <-
    sapply(sim,is.numeric)

  sim[numeric_cols] <-
    sim[numeric_cols] +
    rnorm(
      sum(numeric_cols)*n,
      0,
      noise_sd
    )

  tibble::as_tibble(sim)
}