test_that("rnorm_multi returns correct dimensions", {
  df <- rnorm_multi(100, vars = 3, mu = 0, sd = 1, r = 0, seed = 1)
  expect_equal(nrow(df), 100)
  expect_equal(ncol(df), 3)
  expect_true(is.data.frame(df))
})

test_that("rnorm_multi uses variable names", {
  df <- rnorm_multi(50, vars = c("a", "b", "c"), seed = 1)
  expect_equal(names(df), c("a", "b", "c"))
})

test_that("rnorm_multi empirical matches target stats", {
  df <- rnorm_multi(200, vars = c("x","y"), mu = c(10, 20),
                    sd = c(2, 3), r = 0.7, empirical = TRUE, seed = 42)
  expect_equal(round(colMeans(df), 6), c(x = 10, y = 20))
  expect_equal(round(apply(df, 2, sd), 6), c(x = 2, y = 3))
  expect_equal(round(cor(df)[1,2], 6), 0.7)
})

test_that("rnorm_multi handles scalar r", {
  df <- rnorm_multi(100, vars = 4, r = 0.5, seed = 1)
  expect_equal(dim(df), c(100, 4))
})

test_that("rnorm_multi returns matrix when as_matrix = TRUE", {
  m <- rnorm_multi(50, vars = 2, as_matrix = TRUE, seed = 1)
  expect_true(is.matrix(m))
})

test_that("rnorm_multi seed is reproducible", {
  d1 <- rnorm_multi(30, vars = 2, seed = 99)
  d2 <- rnorm_multi(30, vars = 2, seed = 99)
  expect_equal(d1, d2)
})

test_that("rnorm_pre produces correct correlation", {
  set.seed(1)
  x <- rnorm(500)
  y <- rnorm_pre(x, r = 0.8, empirical = TRUE)
  expect_equal(round(cor(x, y), 1), 0.8)
})

test_that("rmulti supports multiple distributions", {
  df <- rmulti(200,
               dist = c("norm","pois","binom"),
               params = list(list(mu = 5, sd = 1),
                             list(lambda = 3),
                             list(size = 10, prob = 0.5)),
               r = 0.3, seed = 1)
  expect_equal(nrow(df), 200)
  expect_equal(ncol(df), 3)
  # Poisson should be non-negative integers
  expect_true(all(df[[2]] >= 0))
  expect_true(all(df[[2]] == floor(df[[2]])))
})
