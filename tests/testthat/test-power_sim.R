dataforge_options(verbose = FALSE, plot = FALSE)

test_that("power_sim() returns a structured power_sim object", {
  res <- power_sim(
    between = list(group = c("control", "treat")),
    n = 20,
    mu = c(0, 0.8),
    sd = 1,
    rep = 100,
    seed = 123,
    verbose = FALSE
  )

  expect_s3_class(res, "power_sim")
  expect_true(is.numeric(res$power))
  expect_true(res$power >= 0 && res$power <= 1)
  expect_equal(res$alpha, 0.05)
  expect_equal(res$replications, 100)
  expect_equal(res$method, "t_test")
})

test_that("power_sim() is reproducible when seed is supplied", {
  res1 <- power_sim(
    between = list(group = c("control", "treat")),
    n = 20,
    mu = c(0, 0.5),
    sd = 1,
    rep = 100,
    seed = 99,
    verbose = FALSE
  )

  res2 <- power_sim(
    between = list(group = c("control", "treat")),
    n = 20,
    mu = c(0, 0.5),
    sd = 1,
    rep = 100,
    seed = 99,
    verbose = FALSE
  )

  expect_equal(res1, res2)
})

test_that("power_sim() handles unequal group sizes", {
  res <- power_sim(
    between = list(group = c("control", "treat")),
    n = c(20, 35),
    mu = c(0, 0.7),
    sd = 1,
    rep = 100,
    seed = 1,
    verbose = FALSE
  )

  expect_s3_class(res, "power_sim")
  expect_equal(res$replications, 100)
})

test_that("power_sim() supports anova and lm", {
  res_aov <- power_sim(
    within = list(time = c("pre", "post")),
    between = list(group = c("control", "treat")),
    n = 20,
    mu = c(0, 0.2, 0, 0.8),
    sd = 1,
    r = 0.5,
    test = "anova",
    rep = 100,
    seed = 10,
    verbose = FALSE
  )

  res_lm <- power_sim(
    within = list(time = c("pre", "post")),
    between = list(group = c("control", "treat")),
    n = 20,
    mu = c(0, 0.2, 0, 0.8),
    sd = 1,
    r = 0.5,
    test = "lm",
    rep = 100,
    seed = 10,
    verbose = FALSE
  )

  expect_s3_class(res_aov, "power_sim")
  expect_s3_class(res_lm, "power_sim")
  expect_equal(res_aov$method, "anova")
  expect_equal(res_lm$method, "lm")
})

test_that("power_sim() warns when rep is less than 100", {
  expect_warning(
    power_sim(
      between = list(group = c("control", "treat")),
      n = 20,
      mu = c(0, 0.5),
      sd = 1,
      rep = 20,
      seed = 3,
      verbose = FALSE
    ),
    "`rep` is less than 100"
  )
})

test_that("power_sim() errors on unsupported test", {
  expect_error(
    power_sim(
      between = list(group = c("control", "treat")),
      n = 20,
      mu = c(0, 0.5),
      sd = 1,
      test = "glm",
      rep = 100,
      verbose = FALSE
    )
  )
})

test_that("power_sim() errors on mu length mismatch", {
  expect_error(
    power_sim(
      between = list(group = c("control", "treat")),
      n = 20,
      mu = c(0, 0.5, 1),
      sd = 1,
      rep = 100,
      verbose = FALSE
    ),
    "`mu` must have 1 or 2 values."
  )
})

test_that("print.power_sim() prints the expected summary", {
  res <- power_sim(
    between = list(group = c("control", "treat")),
    n = 20,
    mu = c(0, 0.5),
    sd = 1,
    rep = 100,
    seed = 7,
    verbose = FALSE
  )

  expect_output(print(res), "## Power Simulation Result")
  expect_output(print(res), "Estimated power:")
})
