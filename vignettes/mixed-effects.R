## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      fig.width = 7, fig.height = 4)
library(dataforge)
dataforge_options(plot = FALSE, verbose = FALSE)
set.seed(42)

## ----simple-re----------------------------------------------------------------
set.seed(1)
d <- add_random(subject = 40) |>
  add_between(.by = "subject",
              condition = c("control", "treatment")) |>
  add_ranef(.by = "subject", u0 = 1.2) |>
  within({
    residual <- rnorm(length(subject), 0, 1)
    y <- 5 + 1.5 * (condition == "treatment") + u0 + residual
  })

head(d)

