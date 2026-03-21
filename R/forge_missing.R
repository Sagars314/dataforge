forge_missing <- function(
  data,
  prop = .1
){

  n <- nrow(data)

  miss_id <- sample(
    1:n,
    size = floor(prop*n)
  )

  data[miss_id,] <- NA

  data
}