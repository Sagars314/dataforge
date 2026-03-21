# dataforge <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/example/dataforge/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/example/dataforge/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/dataforge)](https://CRAN.R-project.org/package=dataforge)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

> **Forge the data you need — structured, correlated, reproducible.**

`dataforge` is a comprehensive toolkit for simulating structured datasets with user-specified statistical properties. It is designed for researchers, educators, and data scientists who need reproducible synthetic data for:

- 🔬 **Power analysis** — simulate data under your exact design before collecting
- 📚 **Teaching** — create datasets that perfectly illustrate statistical concepts  
- 🧪 **Pipeline testing** — unit-test analyses on data with known properties
- 🎲 **Exploring methods** — study estimator behaviour under controlled conditions

## Installation

```r
# CRAN (once published)
install.packages("dataforge")

# Development version from GitHub
# install.packages("devtools")
devtools::install_github("example/dataforge")
```

## Quick overview

### Correlated multivariate normals

```r
library(dataforge)

df <- rnorm_multi(
  n    = 200,
  vars = c("RT", "accuracy", "fatigue"),
  mu   = c(350, 0.85, 3.0),
  sd   = c(50,  0.10, 0.8),
  r    = c(0.4, -0.3, -0.5)   # lower-triangle
)
round(cor(df), 2)
```

### Factorial design simulation

```r
# 2 (group: control/treatment) × 3 (time: T1/T2/T3) mixed design
df <- forge_design(
  between = list(group = c("control", "treatment")),
  within  = list(time  = c("T1", "T2", "T3")),
  n  = 40,
  mu = data.frame(
    control   = c(50, 51, 52),
    treatment = c(50, 55, 63),
    row.names = c("T1","T2","T3")
  ),
  sd = 8, r = 0.65, long = TRUE
)
```

![Mixed design plot](man/figures/mixed-design-example.png)

### Simulate from existing data

```r
# Match the structure of iris — 100 obs per species
new_iris <- forge_df(iris, n = 100, between = "Species")
```

### Build mixed effects structures

```r
# 30 subjects × 20 items, with random effects, using pipes
data <- add_random(subject = 30) |>
  add_random(item = 20) |>
  add_between(.by = "subject",
              condition = c("A","B","C"),
              .prob     = c(1/3, 1/3, 1/3)) |>
  add_ranef(.by = "subject", u0 = 1.5) |>
  add_ranef(.by = "item",    w0 = 0.8)
```

### Non-normal distributions (NORTA)

```r
# Correlated Poisson + Beta + Binomial
df <- rmulti(
  n      = 300,
  dist   = c("pois", "beta", "binom"),
  params = list(list(lambda=3), list(alpha=2,beta=5), list(size=10,prob=.4)),
  r      = 0.4
)
```

### Likert-scale data

```r
# 5-item 7-point scale, correlated r = 0.5
survey <- rlikert(n = 200, items = 5, likert_n = 7,
                  mu = c(4.5, 3.8, 5.0, 4.2, 3.5),
                  sd = 1.0, r = 0.5)
```

### Contrast coding

```r
d <- add_contrast(d, "condition", contrasts = "helmert")
d <- add_contrast(d, "group",     contrasts = "treatment", base = "control")
```

---

## Full function reference

### Simulation

| Function | Description |
|----------|-------------|
| `forge_design()` | Simulate data for factorial designs (between, within, mixed) |
| `forge_df()` | Simulate new data matching an existing data frame |
| `rnorm_multi()` | Correlated multivariate normal variables |
| `rnorm_pre()` | Normal vector correlated to existing variable(s) |
| `rmulti()` | Correlated multi-distribution data (NORTA) |

### Mixed effects

| Function | Description |
|----------|-------------|
| `add_random()` | Add random factors (crossed or nested) |
| `add_within()` | Add within-subject factor levels |
| `add_between()` | Randomly assign between-subject factors |
| `add_ranef()` | Add normally distributed random effects |
| `add_recode()` | Recode a factor column |
| `forge_mixed_cc()` | Cross-classified subjects × items dataset |

### Contrasts

| Function | Description |
|----------|-------------|
| `add_contrast()` | Add contrast-coded columns to a data frame |
| `contr_code_treatment()` | Treatment (dummy) coding |
| `contr_code_sum()` | Sum (deviation) coding |
| `contr_code_helmert()` | Helmert coding |
| `contr_code_difference()` | Successive difference coding |
| `contr_code_poly()` | Polynomial (orthogonal) coding |
| `contr_code_anova()` | ANOVA coding |

### Distributions

| Function | Description |
|----------|-------------|
| `rlikert()` | Random Likert-scale responses |
| `norm2beta()` / `beta2norm()` | Beta ↔ Normal conversion |
| `norm2binom()` / `binom2norm()` | Binomial ↔ Normal |
| `norm2gamma()` / `gamma2norm()` | Gamma ↔ Normal |
| `norm2pois()` / `pois2norm()` | Poisson ↔ Normal |
| `norm2nbinom()` / `nbinom2norm()` | Negative Binomial ↔ Normal |
| `norm2unif()` / `unif2norm()` | Uniform ↔ Normal |
| `norm2trunc()` / `trunc2norm()` | Truncated Normal ↔ Normal |
| `norm2likert()` | Normal to Likert |
| `norm2norm()` | Rescale to target mean/SD |

### Utilities

| Function | Description |
|----------|-------------|
| `make_missing()` / `messy()` | Introduce missing values |
| `add_duplicates()` | Add duplicate rows |
| `add_outliers()` | Add extreme values |
| `wide2long()` | Wide → long format |
| `long2wide()` | Long → wide format |
| `plot_design()` | Plot a simulated design |
| `get_params()` / `check_sim_stats()` | Descriptive stats per cell |
| `check_design()` | Validate and summarise a design |
| `get_design()` | Retrieve design attribute |
| `make_id()` | Generate participant/item IDs |
| `is_pos_def()` | Check positive definiteness |
| `unique_pairs()` | Variable-pair labels |
| `cormat_from_triangle()` | Build correlation matrix |
| `dataforge_options()` | Set/get global options |

### Built-in datasets

| Dataset | Description |
|---------|-------------|
| `load_exam_scores()` | Simulated pre/post exam scores (240 students) |
| `load_survey_responses()` | Simulated 5-item Likert survey (300 respondents) |

---

## Comparison with `faux`

`dataforge` was inspired by the excellent [`faux`](https://github.com/scienceverse/faux)
package by Lisa DeBruine. Key similarities and differences:

| Feature | `dataforge` | `faux` |
|---------|-------------|--------|
| Factorial design sim | ✅ `forge_design()` | ✅ `sim_design()` |
| Simulate from data | ✅ `forge_df()` | ✅ `sim_df()` |
| Correlated normals | ✅ `rnorm_multi()` | ✅ `rnorm_multi()` |
| Mixed effects pipeline | ✅ `add_random()` etc. | ✅ `add_random()` etc. |
| Contrasts | ✅ 6 coding schemes | ✅ 6 coding schemes |
| Distribution conversions | ✅ `norm2*` family | ✅ `norm2*` family |
| Likert scale | ✅ `rlikert()` | ✅ `rlikert()` |
| Messy data | ✅ `make_missing()` | ✅ `messy()` |
| NORTA multi-dist | ✅ `rmulti()` | ✅ `rmulti()` |
| Codebook generation | ❌ | ✅ `codebook()` |
| Interactive design | ❌ | ✅ `interactive_design()` |
| Outlier injection | ✅ `add_outliers()` | ❌ |

---

## Citation

If you use `dataforge` in published work, please cite it:

```r
citation("dataforge")
```

## License

MIT © dataforge contributors. See [LICENSE](LICENSE) for details.
