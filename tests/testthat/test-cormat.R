## Suppress verbose output and auto-plotting
dataforge_options(verbose = FALSE, plot = FALSE)

test_that("cormat() produces valid correlation matrices", {
  # scalar
  m <- cormat(0.5, n = 4)
  expect_equal(nrow(m), 4)
  expect_equal(ncol(m), 4)
  expect_equal(diag(m), rep(1, 4))
  expect_true(all(m[upper.tri(m)] == 0.5))

  # triangle vector
  m2 <- cormat(c(0.3, 0.5, 0.2), n = 3)
  expect_equal(dim(m2), c(3, 3))
  expect_equal(m2[1, 2], 0.3)

  # var_names
  m3 <- cormat(0.4, n = 3, var_names = c("A", "B", "C"))
  expect_equal(rownames(m3), c("A", "B", "C"))

  # errors
  expect_error(cormat(0.5))          # n required for scalar
  expect_error(cormat(2, n = 3))     # r out of range
  expect_error(cormat(c(0.1, 0.2), n = 4))  # wrong number of values
})

test_that("is_pos_def() works correctly", {
  m_good <- cormat(0.3, n = 3)
  m_bad  <- matrix(c(1, 0.99, 0.99, 0.99, 1, 0.99, 0.99, 0.99, 1), 3)
  expect_true(is_pos_def(m_good))
  # near-singular matrix may or may not be pos def; just test it runs
  expect_type(is_pos_def(m_bad), "logical")
})

test_that("rnorm_forge() returns correct dimensions and structure", {
  df <- rnorm_forge(n = 50, vars = 3, mu = c(0, 5, 10), sd = c(1, 2, 3), r = 0.5)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 50)
  expect_equal(ncol(df), 3)
  expect_equal(names(df), c("V1", "V2", "V3"))

  # custom names
  df2 <- rnorm_forge(100, vars = 2, var_names = c("x", "y"))
  expect_equal(names(df2), c("x", "y"))

  # empirical: should have exact means and SDs
  df3 <- rnorm_forge(200, vars = 2, mu = c(10, 20), sd = c(2, 3),
                      r = 0.6, empirical = TRUE)
  expect_equal(round(colMeans(df3)), c(V1 = 10, V2 = 20))

  # as_matrix
  m <- rnorm_forge(30, vars = 2, as_matrix = TRUE)
  expect_true(is.matrix(m))
})

test_that("rnorm_pre() generates correlated vector", {
  set.seed(99)
  x <- rnorm(300, 5, 2)
  y <- rnorm_pre(x, r = 0.7, mu = 10, sd = 3)
  expect_equal(length(y), 300)
  expect_true(abs(cor(x, y) - 0.7) < 0.1)
  expect_true(abs(mean(y) - 10) < 0.5)
})
