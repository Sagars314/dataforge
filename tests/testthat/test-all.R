## Suppress verbose output and auto-plotting for all tests in this file
dataforge_options(verbose = FALSE, plot = FALSE)

# ---- Distribution conversion tests ----

test_that("norm2beta / beta2norm are inverse functions", {
  x <- rnorm(100)
  b <- norm2beta(x, shape1 = 2, shape2 = 5)
  expect_true(all(b >= 0 & b <= 1))
  x2 <- beta2norm(b, shape1 = 2, shape2 = 5)
  expect_equal(round(x, 5), round(x2, 5))
})

test_that("norm2pois returns non-negative integers", {
  x <- rnorm(200)
  p <- norm2pois(x, lambda = 4)
  expect_true(all(p >= 0))
  expect_true(all(p == round(p)))
})

test_that("norm2likert returns values in 1:items", {
  x <- rnorm(500)
  l <- norm2likert(x, items = 7)
  expect_true(all(l >= 1 & l <= 7))
})

test_that("rlikert produces integer values", {
  vals <- rlikert(100, items = 5)
  expect_true(all(vals %in% 1:5))
})

test_that("rmulti returns correct dimensions and types", {
  df <- rmulti(
    n = 100,
    dist = list(
      score  = list(dist = "norm", mu = 50, sd = 10),
      count  = list(dist = "pois", lambda = 3),
      prop   = list(dist = "beta", shape1 = 2, shape2 = 3),
      rating = list(dist = "likert", items = 5)
    ),
    r = 0.3
  )
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 100)
  expect_equal(ncol(df), 4)
  expect_true(all(df$prop >= 0 & df$prop <= 1))
  expect_true(all(df$rating %in% 1:5))
})

# ---- Mixed design builders ----

test_that("add_random creates correct structure (crossed)", {
  df <- add_random(subject = 10, item = 5)
  expect_equal(nrow(df), 50)
  expect_true("subject" %in% names(df))
  expect_true("item" %in% names(df))
})

test_that("add_random creates correct structure (nested)", {
  df <- add_random(class = 3) |>
    add_random(student = 5, .nested_in = "class")
  expect_equal(nrow(df), 15)
  expect_true(all(c("class", "student") %in% names(df)))
})

test_that("add_within expands rows correctly", {
  df <- add_random(subject = 10) |>
    add_within(time = c("pre", "post", "follow"))
  expect_equal(nrow(df), 30)
  expect_s3_class(df$time, "factor")
  expect_equal(levels(df$time), c("pre", "post", "follow"))
})

test_that("add_between assigns factor levels", {
  set.seed(1)
  df <- add_random(subject = 100) |>
    add_between(group = c("A", "B"))
  expect_true("group" %in% names(df))
  expect_true(all(levels(df$group) == c("A", "B")))
})

test_that("add_ranef adds columns of correct length", {
  df <- add_random(subject = 20, item = 10) |>
    add_ranef(subject = 1.5, item = 0.8)
  expect_true("subject_ranef" %in% names(df))
  expect_true("item_ranef" %in% names(df))
  expect_equal(nrow(df), 200)
})

# ---- Reshape tests ----

test_that("wide2long produces correct long structure", {
  wide <- forge_design(
    within = list(time = c("pre","post")),
    n = 20, mu = c(5,8), sd = 1, r = 0.5,
    plot = FALSE
  )
  long <- wide2long(wide, within = "time", dv = "score")
  expect_equal(nrow(long), 40)
  expect_true("score" %in% names(long))
})

test_that("long2wide produces correct wide structure", {
  long <- data.frame(
    id    = rep(1:5, each = 2),
    time  = rep(c("pre", "post"), 5),
    score = c(10, 12, 9, 14, 11, 15, 8, 13, 10, 16)
  )
  wide <- long2wide(long, id = "id", within = "time", dv = "score")
  expect_equal(nrow(wide), 5)
  expect_true(all(c("pre", "post") %in% names(wide)))
})

# ---- Utility tests ----

test_that("make_id generates correct IDs", {
  ids <- make_id(100, prefix = "sub_")
  expect_length(ids, 100)
  expect_equal(ids[1],   "sub_001")
  expect_equal(ids[100], "sub_100")
})

test_that("messy() introduces NAs at approximately correct rate", {
  set.seed(10)
  df <- data.frame(x = rnorm(200), y = rnorm(200))
  df_miss <- messy(df, prop = 0.1)
  na_rate <- mean(is.na(df_miss))
  expect_true(abs(na_rate - 0.1) < 0.05)
})

test_that("get_params returns a data frame with expected columns", {
  df <- forge_design(
    within = list(t = c("a","b")), n = 30,
    mu = c(5,7), sd = 2, r = 0.4,
    plot = FALSE
  )
  p <- get_params(df)
  expect_s3_class(p, "data.frame")
  expect_true(all(c("mean","sd","n") %in% names(p)))
})

test_that("forge_df simulates from existing data", {
  new_iris <- forge_df(iris, n = 20, between = "Species")
  expect_equal(nrow(new_iris), 60)
  expect_true("Species" %in% names(new_iris))
})

# ---- Contrast tests ----

test_that("add_contrast adds correct number of columns", {
  df <- data.frame(grp = factor(rep(c("A","B","C"), each = 5)))
  df2 <- add_contrast(df, "grp", contrast = "treatment")
  expect_equal(ncol(df2), 3)   # original + 2 contrast cols

  df3 <- add_contrast(df, "grp", contrast = "helmert")
  expect_equal(ncol(df3), 3)
})

test_that("contr_code_poly returns orthogonal contrasts", {
  cm <- contr_code_poly(4)
  expect_equal(dim(cm), c(4, 3))
  # Columns should be approximately orthogonal (dot product ~ 0)
  expect_true(abs(sum(cm[,1] * cm[,2])) < 1e-10)
})
