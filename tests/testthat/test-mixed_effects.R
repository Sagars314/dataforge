test_that("add_random creates correct rows", {
  d <- add_random(subject = 20)
  expect_equal(nrow(d), 20)
  expect_true("subject" %in% names(d))
})

test_that("add_random crosses factors", {
  d <- add_random(subject = 5, item = 3)
  expect_equal(nrow(d), 15)
})

test_that("add_within adds all combinations", {
  d <- add_random(subject = 10) |>
       add_within(.by = "subject", condition = c("A","B","C"))
  expect_equal(nrow(d), 30)
  expect_true("condition" %in% names(d))
  expect_true(is.factor(d$condition))
})

test_that("add_between assigns all subjects", {
  set.seed(1)
  d <- add_random(subject = 60) |>
       add_between(.by = "subject", group = c("ctrl","exp"))
  expect_equal(nrow(d), 60)
  expect_true("group" %in% names(d))
  expect_true(all(d$group %in% c("ctrl","exp")))
})

test_that("add_ranef adds column to data frame", {
  set.seed(1)
  d <- add_random(subject = 30) |>
       add_ranef(.by = "subject", intercept = 1.5)
  expect_true("intercept" %in% names(d))
  expect_equal(nrow(d), 30)
  expect_true(is.numeric(d$intercept))
})

test_that("add_recode works correctly", {
  d <- data.frame(group = c("A","B","C","A"), stringsAsFactors = FALSE)
  d2 <- add_recode(d, "group", "group2", A = "alpha", B = "beta", C = "gamma")
  expect_equal(d2$group2, c("alpha","beta","gamma","alpha"))
})

test_that("forge_mixed_cc returns correct shape", {
  d <- forge_mixed_cc(subj_n = 20, item_n = 15, seed = 1)
  expect_equal(nrow(d), 300)
  expect_true(all(c("subj","item","dv") %in% names(d)))
})

test_that("nested add_random works", {
  d <- add_random(class = 5) |>
       add_random(student = 10, .nested_in = "class")
  expect_equal(nrow(d), 50)
  expect_true(all(c("class","student") %in% names(d)))
})
