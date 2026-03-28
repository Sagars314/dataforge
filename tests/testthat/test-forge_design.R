## Suppress messages and plots globally for this test file
dataforge_options(verbose = FALSE, plot = FALSE)

test_that("forge_design() handles purely within-subject designs", {
  df <- forge_design(
    within = list(time = c("pre", "post")),
    n = 30, mu = c(5, 8), sd = 2, r = 0.5,
    plot = FALSE
  )
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 30)
  expect_true("id" %in% names(df))
  expect_true(all(c("pre", "post") %in% names(df)))
})

test_that("forge_design() handles purely between-subject designs", {
  df <- forge_design(
    between = list(group = c("A", "B")),
    n = 25, mu = c(10, 15), sd = 3,
    plot = FALSE
  )
  expect_equal(nrow(df), 50)
  expect_true("group" %in% names(df))
  expect_s3_class(df$group, "factor")
  expect_equal(levels(df$group), c("A", "B"))
})

test_that("forge_design() handles mixed 2x3 designs", {
  df <- forge_design(
    within  = list(time = c("pre", "mid", "post")),
    between = list(group = c("ctrl", "trt")),
    n = 40, mu = c(10, 12, 14, 10, 15, 20),
    sd = 2, r = 0.6,
    plot = FALSE
  )
  expect_equal(nrow(df), 80)
  expect_true(all(c("pre", "mid", "post", "group") %in% names(df)))
})

test_that("forge_design() long format works", {
  df <- forge_design(
    within = list(cond = c("A", "B")),
    n = 20, mu = c(5, 7), sd = 1, r = 0.4,
    long = TRUE, plot = FALSE
  )
  expect_true("y" %in% names(df))
  expect_equal(nrow(df), 40)
})

test_that("forge_design() respects seed for reproducibility", {
  df1 <- forge_design(within = list(t = c("a", "b")), n = 10,
                      mu = c(0, 1), sd = 1, r = 0.3, seed = 7,
                      plot = FALSE)
  df2 <- forge_design(within = list(t = c("a", "b")), n = 10,
                      mu = c(0, 1), sd = 1, r = 0.3, seed = 7,
                      plot = FALSE)
  expect_equal(df1$a, df2$a)
  expect_equal(df1$b, df2$b)
})

test_that("forge_design() rep argument returns a list", {
  res <- forge_design(within = list(t = c("x", "y")), n = 10,
                      mu = c(0, 1), sd = 1, r = .3,
                      rep = 5, plot = FALSE)
  expect_type(res, "list")
  expect_length(res, 5)
  expect_s3_class(res[[1]], "data.frame")
})
