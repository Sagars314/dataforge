library(dataforge)

df <- forge_correlated(
  n = 100,
  mu = c(0,5),
  sd = c(1,2),
  cor_matrix = matrix(
    c(1,0.7,
      0.7,1),2
  ),
  names=c("x","y"),
  seed = 123
)

head(df)
cor(df)


data <- data.frame(
  x = rnorm(50),
  y = rnorm(50)
)

df2 <- forge_from_data(
  data,
  n = 100
)

dim(df2)
head(df2)


df_miss <- forge_missing(
  df,
  prop = .2
)

summary(df_miss)
