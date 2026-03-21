test_that("norm2beta returns values in (0,1)", {
  x <- rnorm(200)
  y <- norm2beta(x, alpha = 2, beta = 5)
  expect_true(all(y > 0 & y < 1))
})

test_that("norm2binom returns non-negative integers <= size", {
  x <- rnorm(200)
  y <- norm2binom(x, size = 10, prob = 0.5)
  expect_true(all(y >= 0 & y <= 10))
  expect_true(all(y == floor(y)))
})

test_that("norm2gamma returns positive values", {
  y <- norm2gamma(rnorm(200), shape = 2, rate = 1)
  expect_true(all(y > 0))
})

test_that("norm2pois returns non-negative integers", {
  y <- norm2pois(rnorm(200), lambda = 3)
  expect_true(all(y >= 0))
  expect_true(all(y == floor(y)))
})

test_that("norm2unif respects bounds", {
  y <- norm2unif(rnorm(300), min = 5, max = 10)
  expect_true(all(y >= 5 & y <= 10))
})

test_that("norm2likert returns integers in 1:n", {
  y <- norm2likert(rnorm(200), n = 5)
  expect_true(all(y %in% 1:5))
  expect_true(is.integer(y))
})

test_that("rlikert returns single vector for items=1", {
  y <- rlikert(100, items = 1, likert_n = 7, seed = 1)
  expect_true(is.integer(y))
  expect_equal(length(y), 100)
  expect_true(all(y %in% 1:7))
})

test_that("rlikert returns data frame for items>1", {
  df <- rlikert(100, items = 4, r = 0.5, seed = 1)
  expect_true(is.data.frame(df))
  expect_equal(ncol(df), 4)
})

test_that("dlikert probabilities sum to 1", {
  probs <- dlikert(1:5, n = 5)
  expect_equal(round(sum(probs), 10), 1)
})

test_that("round-trip norm->beta->norm is identity (approx)", {
  set.seed(1)
  x <- rnorm(500)
  y <- norm2beta(x)
  x2 <- beta2norm(y)
  expect_lt(cor(x, x2), 1.0)  # not perfect but high correlation
  expect_gt(cor(x, x2), 0.99)
})
