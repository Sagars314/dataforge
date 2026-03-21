test_that("forge_df returns correct number of rows without grouping", {
  df <- forge_df(mtcars[, c("mpg","hp","wt")], n = 100, seed = 1)
  expect_equal(nrow(df), 100)
  expect_equal(ncol(df), 3)
})

test_that("forge_df with between grouping returns n per group", {
  new_iris <- forge_df(iris, n = 50, between = "Species", seed = 1)
  expect_equal(nrow(new_iris), 150)  # 50 per species * 3 species
  expect_true("Species" %in% names(new_iris))
})

test_that("forge_df preserves column names", {
  df <- forge_df(mtcars[, c("mpg","hp")], n = 30, seed = 1)
  expect_equal(names(df), c("mpg","hp"))
})

test_that("forge_df means are roughly correct", {
  set.seed(1)
  big <- forge_df(iris, n = 1000, between = "Species", seed = 1)
  orig_mean <- mean(iris[iris$Species == "setosa", "Sepal.Length"])
  new_mean  <- mean(big[big$Species  == "setosa", "Sepal.Length"])
  expect_lt(abs(orig_mean - new_mean), 0.2)
})
