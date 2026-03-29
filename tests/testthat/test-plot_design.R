test_that("plot_design respects factor level order", {
  # Create a design with specific order
  df <- forge_design(
    within  = list(time = c("pre", "mid", "post")),
    between = list(group = c("ctrl", "exp")),
    n = 2, mu = 0, sd = 1, r = 0, plot = FALSE, long = TRUE
  )

  # Check original order
  expect_equal(levels(df$time), c("pre", "mid", "post"))
  expect_equal(levels(df$group), c("ctrl", "exp"))

  # Manually mess up the factor levels (e.g. by reordering rows or refactorising)
  df$time <- factor(df$time, levels = c("post", "pre", "mid"))

  # plot_design should restore the order from metadata
  p <- plot_design(df, x = "time")

  # Verify that the data in the plot object has the correct levels
  # ggplot2 stores the data in p$data
  expect_equal(levels(p$data$time), c("pre", "mid", "post"))

  # Test between factor
  df$group <- factor(df$group, levels = c("exp", "ctrl"))
  p2 <- plot_design(df, x = "group")
  expect_equal(levels(p2$data$group), c("ctrl", "exp"))
})
