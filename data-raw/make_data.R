# Run this script to regenerate the built-in datasets
# save output to data/ with usethis::use_data()

set.seed(42)

# ---- rt_data ----
rt_data <- data.frame(
  id    = sprintf("S%03d", 1:100),
  group = factor(rep(c("control", "treatment"), each = 50)),
  stringsAsFactors = FALSE
)

# Control group: mild improvement
ctrl_mu <- c(450, 440, 435)
ctrl_sd <- c(50, 48, 47)
ctrl_r  <- 0.65

# Treatment group: large improvement
trt_mu <- c(455, 420, 390)
trt_sd <- c(52, 45, 40)
trt_r  <- 0.60

library(MASS)

make_mvn <- function(n, mu, sd, r, col_names) {
  cm <- matrix(r, 3, 3); diag(cm) <- 1
  sigma <- diag(sd) %*% cm %*% diag(sd)
  df <- as.data.frame(mvrnorm(n, mu = mu, Sigma = sigma))
  names(df) <- col_names
  df
}

ctrl_obs <- make_mvn(50, ctrl_mu, ctrl_sd, ctrl_r, c("pre","mid","post"))
trt_obs  <- make_mvn(50, trt_mu,  trt_sd,  trt_r,  c("pre","mid","post"))

rt_data <- cbind(rt_data, rbind(ctrl_obs, trt_obs))

# ---- cog_data ----
cog_data <- data.frame(
  id        = sprintf("P%03d", 1:120),
  age_group = factor(rep(c("young","middle","older"), each = 40)),
  stringsAsFactors = FALSE
)

young_mu  <- c(memory=85, attention=80, processing_speed=90, executive=82)
middle_mu <- c(memory=80, attention=78, processing_speed=82, executive=81)
older_mu  <- c(memory=70, attention=72, processing_speed=65, executive=74)
cog_sd    <- c(12, 11, 13, 10)
cog_r     <- 0.45

cm_cog <- matrix(cog_r, 4, 4); diag(cm_cog) <- 1
sigma_cog <- diag(cog_sd) %*% cm_cog %*% diag(cog_sd)

young_cog  <- as.data.frame(mvrnorm(40, young_mu,  sigma_cog))
middle_cog <- as.data.frame(mvrnorm(40, middle_mu, sigma_cog))
older_cog  <- as.data.frame(mvrnorm(40, older_mu,  sigma_cog))

cog_obs <- rbind(young_cog, middle_cog, older_cog)
names(cog_obs) <- c("memory","attention","processing_speed","executive")
cog_data <- cbind(cog_data, cog_obs)

save(rt_data,  file = "data/rt_data.rda",  compress = "bzip2")
save(cog_data, file = "data/cog_data.rda", compress = "bzip2")

cat("Datasets saved.\n")
