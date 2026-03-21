library(dataforge)

# Quick smoke test
df <- forge_design(
  between = list(group = c("control", "treatment")),
  n = 30, mu = c(0, 1), sd = 1, plot = TRUE
)

head(df)
get_params(df, between = "group", dv = "y")


library(dataforge)

# rnorm_multi — verify correlation structure
df <- rnorm_multi(1000, vars = c("x","y","z"),
                  mu = c(10, 20, 30),
                  sd = c(1, 2, 3),
                  r  = c(0.8, 0.3, 0.5))
round(colMeans(df), 1)   # should be ~10, 20, 30
round(apply(df, 2, sd), 1)  # should be ~1, 2, 3
round(cor(df), 2)            # should match r = c(.8, .3, .5)

# empirical = TRUE gives exact match
df2 <- rnorm_multi(100, vars = c("a","b"), mu = c(5,10),
                   sd = c(2,4), r = 0.7, empirical = TRUE)
colMeans(df2)   # exactly 5, 10
apply(df2, 2, sd)  # exactly 2, 4
cor(df2)[1,2]      # exactly 0.7


# --- One-way between ---
df1 <- forge_design(
  between = list(drug = c("placebo","10mg","50mg")),
  n  = 50,
  mu = c(5, 6.5, 8),
  sd = 1.5
)
table(df1$drug)
get_params(df1, between = "drug", dv = "y")

# --- Within only, check correlation ---
df2 <- forge_design(
  within = list(time = c("T1","T2","T3","T4")),
  n = 100, mu = c(10,12,14,16), sd = 3, r = 0.7
)
round(cor(df2[,c("T1","T2","T3","T4")]), 2)  # all ~0.7

# --- Mixed 2x3 design ---
df3 <- forge_design(
  between = list(group = c("A","B")),
  within  = list(time  = c("pre","mid","post")),
  n  = 40,
  mu = data.frame(
    A = c(50, 55, 54),
    B = c(50, 60, 70),
    row.names = c("pre","mid","post")
  ),
  sd = 8, r = 0.6, long = TRUE
)
aggregate(y ~ group + time, df3, mean)

# --- Unequal group sizes ---
df4 <- forge_design(
  between = list(site = c("north","south","east")),
  n = c(north=30, south=50, east=20),
  mu = 0, sd = 1
)
table(df4$site)   # should be 30, 50, 20

# --- Heteroscedastic design ---
df5 <- forge_design(
  between = list(g = c("low","med","high")),
  n  = 100,
  mu = c(5, 5, 5),
  sd = c(1, 3, 6)   # very different spreads, same means
)
aggregate(y ~ g, df5, sd)


set.seed(42)

# --- Basic pipeline ---
d <- add_random(subject = 40) |>
  add_within(.by = "subject", cond = c("A","B")) |>
  add_between(.by = "subject", group = c("ctrl","exp")) |>
  add_ranef(.by = "subject", intercept = 1.5) |>
  within({
    y <- 5 + 0.8*(cond=="B") + 1.2*(group=="exp") + intercept + rnorm(80,0,1)
  })

# Check balance
table(d$group)        # ~20 per group
table(d$cond)         # 40 per condition (within, so all subjects)
nrow(d)               # 80 (40 subjects x 2 conditions)

# --- Nested: students within classes ---
school <- add_random(class = 10) |>
  add_random(student = 20, .nested_in = "class") |>
  add_between(.by = "class",
              school_type = c("public","private"),
              .prob = c(.7,.3))
nrow(school)                    # 200
table(school$school_type) / 20  # ~7 public, ~3 private classes

# --- Random slopes ---
d2 <- add_random(subject = 30) |>
  add_within(.by = "subject", trial = as.character(1:10)) |>
  add_ranef(.by = "subject", u0 = 1.0, u1 = 0.3, .cors = 0.5) |>
  within({
    trial_n <- as.numeric(trial)
    y <- 10 + 0.5*trial_n + u0 + u1*trial_n + rnorm(300, 0, 0.8)
  })

# Check random effect correlation
re <- unique(d2[, c("subject","u0","u1")])
cor(re$u0, re$u1)   # should be ~0.5

# --- forge_mixed_cc ---
cc <- forge_mixed_cc(subj_n=50, item_n=30,
                     grand_mean=500, subj_sd=80,
                     item_sd=40, error_sd=60)
nrow(cc)                    # 1500 (50*30)
sd(tapply(cc$subj_re, cc$subj, mean))  # ~80
sd(tapply(cc$item_re, cc$item, mean))  # ~40


d <- data.frame(
  condition = factor(rep(c("low","med","high"), each=30)),
  y = c(rnorm(30,5), rnorm(30,7), rnorm(30,9))
)

# Treatment coding — low is reference
d_t <- add_contrast(d, "condition", contrasts="treatment", base="low")
head(d_t)
# med.vs.low and high.vs.low columns should appear

# Helmert — each level vs mean of previous
d_h <- add_contrast(d, "condition", contrasts="helmert")
head(d_h)

# Sum coding
d_s <- add_contrast(d, "condition", contrasts="sum")
head(d_s)

# Inspect coding matrices directly
contr_code_treatment(c("A","B","C","D"), base="A")
contr_code_helmert(c("none","low","med","high"))
contr_code_poly(c("1","2","3","4","5"))   # 4 orthogonal polynomial columns


set.seed(1)
x <- rnorm(1000)

# Beta: should be in (0,1)
b <- norm2beta(x, alpha=2, beta=5)
range(b); hist(b, breaks=30)

# Poisson: non-negative integers
p <- norm2pois(x, lambda=4)
table(p)     # Poisson-like counts
mean(p)      # ~4

# Truncated normal
tr <- norm2trunc(x, min=0, max=10, mu=5, sd=2)
range(tr)    # should be [0, 10]
hist(tr, breaks=30)

# Round-trip test
x2 <- beta2norm(norm2beta(x, 2, 5), 2, 5)
cor(x, x2)   # should be > 0.999

# Uniform
u <- norm2unif(x, min=100, max=200)
range(u)     # exactly [100, 200]


# Single item distribution
set.seed(1)
y <- rlikert(500, items=1, likert_n=5, mu=0, sd=1)
prop.table(table(y))   # should be roughly equal 20% each

# Skewed: high mu shifts responses up
y_high <- rlikert(500, items=1, likert_n=5, mu=2, sd=1)
prop.table(table(y_high))   # mostly 4s and 5s

# Multi-item scale, check inter-item correlation
scale_df <- rlikert(500, items=6, likert_n=7,
                    mu=c(4,4,5,3,4,4), sd=1, r=0.6)
round(cor(scale_df), 2)   # off-diagonal should be ~0.6

# Alpha (crude reliability check)
alpha_approx <- function(df) {
  k <- ncol(df)
  avg_r <- mean(cor(df)[lower.tri(cor(df))])
  k * avg_r / (1 + (k-1)*avg_r)
}
alpha_approx(scale_df)   # should be ~0.9 for r=0.6, 6 items

# Density and CDF
dlikert(1:5)      # probabilities sum to 1
plikert(1:5)      # cumulative
qlikert(c(.2,.5,.8))  # quantiles



set.seed(1)
df <- forge_design(between=list(g=c("A","B")), n=100,
                   mu=c(0,1), sd=1, plot=FALSE)

# Missing values
df_m <- make_missing(df, cols="y", prop=0.15)
mean(is.na(df_m$y))   # ~0.15

# Missing in multiple columns
df_wide <- forge_design(within=list(t=c("T1","T2","T3")),
                        n=50, mu=0, sd=1, plot=FALSE)
df_m2 <- make_missing(df_wide, cols=c("T1","T2"), prop=0.10)
colSums(is.na(df_m2))

# Duplicates
df_dup <- add_duplicates(df, n_dups=20)
nrow(df_dup)   # 220

# Outliers: check that some values exceed 3*SD
df_out <- add_outliers(df, prop=0.05)
range(df_out$y)   # much wider than range(df$y)


df_wide <- forge_design(
  within  = list(emotion = c("happy","sad","angry","fear")),
  between = list(age = c("young","old")),
  n = 25, mu=0, sd=1, r=.5, plot=FALSE
)

# Wide → Long
df_long <- wide2long(df_wide,
                     within_cols = c("happy","sad","angry","fear"),
                     names_to    = "emotion",
                     values_to   = "rating")
nrow(df_long)   # 200  (50 subjects * 4 emotions)
names(df_long)

# Long → Wide
df_w2 <- long2wide(df_long, id_cols=c("id","age"),
                   names_from="emotion", values_from="rating")
nrow(df_w2)     # back to 50
all.equal(sort(df_wide$happy), sort(df_w2$happy))  # TRUE






df_wide <- forge_design(
  within  = list(emotion = c("happy","sad","angry","fear")),
  between = list(age = c("young","old")),
  n = 25, mu=0, sd=1, r=.5, plot=FALSE
)

# Wide → Long
df_long <- wide2long(df_wide,
                     within_cols = c("happy","sad","angry","fear"),
                     names_to    = "emotion",
                     values_to   = "rating")
nrow(df_long)   # 200  (50 subjects * 4 emotions)
names(df_long)

# Long → Wide
df_w2 <- long2wide(df_long, id_cols=c("id","age"),
                   names_from="emotion", values_from="rating")
nrow(df_w2)     # back to 50
all.equal(sort(df_wide$happy), sort(df_w2$happy))  # TRUE






# Different geoms
df <- forge_design(
  between = list(group = c("ctrl","exp")),
  within  = list(time  = c("pre","post")),
  n=50, mu=data.frame(ctrl=c(10,11), exp=c(10,14)),
  sd=2, r=.6, long=TRUE, plot=FALSE
)

plot_design(df, geom="violin")
plot_design(df, geom="box")
plot_design(df, geom="point")
plot_design(df, geom="line")

# Three-factor design with facets
df3 <- forge_design(
  between = list(sex=c("M","F"), drug=c("A","B")),
  within  = list(time=c("pre","post")),
  n=20, mu=0, sd=1, long=TRUE, plot=FALSE
)
plot_design(df3, x="time", color="drug", facet="sex", geom="line")










# make_id
make_id(5)                          # "S1"..."S5"
make_id(100, prefix="P")            # "P001"..."P100"
make_id(10, prefix="item", digits=4) # "item0001"..."item0010"

# is_pos_def
is_pos_def(diag(4))                 # TRUE
is_pos_def(matrix(c(1,2,2,1),2))    # FALSE

# cormat_from_triangle
m <- cormat_from_triangle(c(.3,.5,.2))
round(m, 2)   # symmetric 3x3, 1s on diagonal

# unique_pairs
unique_pairs(c("A","B","C","D"))    # 6 pairs

# dataforge_options
dataforge_options()                        # see defaults
dataforge_options(plot=FALSE, sep=".")     # change
dataforge_options()                        # confirm change
dataforge_options(plot=TRUE, sep="_")      # reset









# Simulate a power curve for a 2x2 mixed ANOVA interaction
run_sim <- function(n, interaction_effect, seed) {
  set.seed(seed)
  d <- forge_design(
    between = list(group = c("ctrl","exp")),
    within  = list(time  = c("pre","post")),
    n  = n,
    mu = data.frame(
      ctrl = c(50, 50),
      exp  = c(50, 50 + interaction_effect),
      row.names = c("pre","post")
    ),
    sd=10, r=.6, long=TRUE, plot=FALSE
  )
  # Compute difference scores
  wide <- long2wide(d, id_cols=c("id","group"),
                    names_from="time", values_from="y")
  wide$diff <- wide$post - wide$pre
  t.test(diff ~ group, data=wide)$p.value
}

# Power at different sample sizes for effect = 5 points
ns      <- c(20, 30, 40, 50, 75, 100)
n_sims  <- 200
power   <- sapply(ns, function(n) {
  mean(sapply(1:n_sims, function(s) run_sim(n, 5, s) < .05))
})

data.frame(n=ns, power=round(power,2))



