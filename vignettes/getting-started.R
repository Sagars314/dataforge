## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.width  = 7,
  fig.height = 4
)
library(dataforge)
set.seed(42)

## ----quick--------------------------------------------------------------------
library(dataforge)

## ----rnorm-multi--------------------------------------------------------------
df <- rnorm_multi(
  n    = 200,
  vars = c("reaction_time", "accuracy", "fatigue"),
  mu   = c(350, 0.85, 3.0),
  sd   = c(50,  0.10, 0.8),
  r    = c(0.4, -0.3, -0.5)   # lower-triangle: rt-acc, rt-fat, acc-fat
)

round(cor(df), 2)

## ----rnorm-empirical----------------------------------------------------------
df2 <- rnorm_multi(100, vars = c("x","y"), mu = c(0,5),
                   sd = c(1, 2), r = 0.7, empirical = TRUE)
colMeans(df2)
apply(df2, 2, sd)
cor(df2)[1,2]

