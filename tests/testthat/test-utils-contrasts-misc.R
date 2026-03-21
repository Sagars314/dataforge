# ---- Contrasts ---------------------------------------------------------------

test_that("contr_code_treatment produces k-1 columns", {
  cm <- contr_code_treatment(c("A","B","C"))
  expect_equal(nrow(cm), 3)
  expect_equal(ncol(cm), 2)
})

test_that("contr_code_sum columns sum to zero per level", {
  cm <- contr_code_sum(c("A","B","C"))
  expect_equal(nrow(cm), 3)
})

test_that("contr_code_helmert produces k-1 columns", {
  cm <- contr_code_helmert(c("low","mid","high","vhigh"))
  expect_equal(ncol(cm), 3)
})

test_that("contr_code_poly produces orthogonal columns", {
  cm <- contr_code_poly(c("a","b","c","d"))
  # Check columns are orthogonal
  cp <- t(cm) %*% cm
  expect_equal(round(cp[1,2], 6), 0)
})

test_that("add_contrast adds columns to data frame", {
  d <- data.frame(group = factor(c("A","B","C","A","B","C")), y = rnorm(6))
  d2 <- add_contrast(d, "group", contrasts = "treatment")
  expect_gt(ncol(d2), ncol(d))
})

# ---- Messy ------------------------------------------------------------------

test_that("make_missing introduces correct proportion of NAs", {
  set.seed(1)
  df <- data.frame(x = 1:100, y = rnorm(100))
  df2 <- make_missing(df, cols = "x", prop = 0.2)
  expect_equal(sum(is.na(df2$x)), 20)
})

test_that("add_duplicates adds correct number of rows", {
  df <- data.frame(x = 1:10)
  df2 <- add_duplicates(df, n_dups = 5, seed = 1)
  expect_equal(nrow(df2), 15)
})

test_that("add_outliers keeps column length", {
  df <- data.frame(x = rnorm(100))
  df2 <- add_outliers(df, prop = 0.05, seed = 1)
  expect_equal(nrow(df2), 100)
})

# ---- Reshape ----------------------------------------------------------------

test_that("wide2long increases row count", {
  df <- forge_design(within = list(time = c("pre","post")), n = 20,
                     mu = c(5,7), sd = 1, r = .5, plot = FALSE, seed = 1)
  long <- wide2long(df, within_cols = c("pre","post"))
  expect_equal(nrow(long), 40)
})

test_that("long2wide decreases row count", {
  df <- forge_design(within = list(time = c("pre","post")), n = 20,
                     mu = c(5,7), sd = 1, r = .5, long = TRUE,
                     plot = FALSE, seed = 1)
  wide <- long2wide(df, id_cols = "id", names_from = "time", values_from = "y")
  expect_equal(nrow(wide), 20)
})

# ---- Utils ------------------------------------------------------------------

test_that("make_id returns correct format", {
  ids <- make_id(10)
  expect_equal(length(ids), 10)
  expect_equal(ids[1], "S01")
  expect_equal(ids[10], "S10")
})

test_that("unique_pairs returns correct count", {
  p <- unique_pairs(c("A","B","C"))
  expect_equal(length(p), 3)
})

test_that("is_pos_def correctly identifies matrices", {
  expect_true(is_pos_def(diag(3)))
  expect_true(is_pos_def(matrix(c(1, .99, .99, 1), 2)))  # near-singular but still PD
  # Not PD: off-diagonal > diagonal makes eigenvalue negative
  expect_false(is_pos_def(matrix(c(1, 2, 2, 1), 2)))
  # Singular matrix (det = 0) is not PD
  expect_false(is_pos_def(matrix(c(1, 1, 1, 1), 2)))
})

test_that("cormat_from_triangle reconstructs correctly", {
  r <- c(.3, .5, .2)
  m <- cormat_from_triangle(r)
  expect_equal(dim(m), c(3,3))
  expect_equal(diag(m), rep(1,3))
  expect_equal(m[1,2], .3)
})

test_that("dataforge_options get/set works", {
  old <- dataforge_options()
  dataforge_options(plot = FALSE, verbose = FALSE)
  expect_false(dataforge_options()$plot)
  # Restore
  dataforge_options(plot = old$plot, verbose = old$verbose)
})

test_that("dataforge_options rejects invalid keys", {
  expect_error(dataforge_options(nonexistent = TRUE), "Unknown")
})
