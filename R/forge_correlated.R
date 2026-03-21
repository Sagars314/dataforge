forge_correlated <- function(
  n,
  mu,
  sd,
  cor_matrix,
  names = NULL,
  seed = NULL
){

  if(!is.null(seed)) set.seed(seed)

  Sigma <- diag(sd) %*% cor_matrix %*% diag(sd)

  data <- MASS::mvrnorm(
    n = n,
    mu = mu,
    Sigma = Sigma
  )

  data <- as.data.frame(data)

  if(!is.null(names))
    colnames(data) <- names

  tibble::as_tibble(data)
}