test_that("forge_design returns correct rows (between only)", {
  df <- forge_design(between = list(g = c("A","B")), n = 30,
                     mu = c(0, 1), sd = 1, plot = FALSE, seed = 1)
  expect_equal(nrow(df), 60)
  expect_true("id" %in% names(df))
  expect_true("g"  %in% names(df))
  expect_true(is.factor(df$g))
})

test_that("forge_design within-only returns wide data", {
  df <- forge_design(within = list(time = c("pre","post")), n = 50,
                     mu = c(5, 7), sd = 1, r = 0.6, plot = FALSE, seed = 1)
  expect_equal(nrow(df), 50)
  expect_true(all(c("pre","post") %in% names(df)))
})

test_that("forge_design long format has correct shape", {
  df <- forge_design(within = list(time = c("pre","post")),
                     between = list(g = c("A","B")),
                     n = 25, mu = 0, sd = 1, r = 0.5,
                     long = TRUE, plot = FALSE, seed = 2)
  expect_true("y" %in% names(df))
  expect_true("time" %in% names(df))
  expect_equal(nrow(df), 25 * 2 * 2)  # 25 subj * 2 between * 2 within
})

test_that("forge_design carries design attribute", {
  df <- forge_design(between = list(g = c("A","B")), n = 10,
                     plot = FALSE, seed = 1)
  d <- get_design(df)
  expect_type(d, "list")
  expect_named(d, c("within","between","n","mu","sd","r","dv","id"))
})

test_that("forge_design check_design runs without error", {
  expect_output(
    check_design(within = list(t = c("a","b")),
                 between = list(g = c("X","Y")),
                 n = 20),
    "dataforge Design Summary"
  )
})

test_that("get_params returns summary statistics", {
  df <- forge_design(between = list(g = c("A","B")), n = 50,
                     mu = c(0,1), sd = 1, plot = FALSE, seed = 3)
  p <- get_params(df, between = "g", dv = "y")
  expect_true(all(c("mean","sd","n") %in% names(p)))
  expect_equal(nrow(p), 2)
})

test_that("forge_design seed is reproducible", {
  d1 <- forge_design(between = list(g = c("A","B")), n = 20,
                     plot = FALSE, seed = 7)
  d2 <- forge_design(between = list(g = c("A","B")), n = 20,
                     plot = FALSE, seed = 7)
  expect_equal(d1$y, d2$y)
})

test_that("forge_design 3-factor mixed design works", {
  df <- forge_design(
    within  = list(time = c("T1","T2"), stimulus = c("S1","S2")),
    between = list(group = c("ctrl","exp")),
    n = 10, mu = 0, sd = 1, r = 0.3, plot = FALSE, seed = 5
  )
  expect_equal(nrow(df), 20)   # 10 per group * 2 groups
  expect_true("group" %in% names(df))
})
