# dataforge (development version)

# dataforge 0.1.0

## New features

### Core simulation
* `forge_design()` — simulate data for between, within, and mixed factorial 
  designs with flexible `mu`/`sd` specification (scalar, vector, data frame, 
  or matrix).
* `forge_df()` — simulate new data matching the empirical structure of an 
  existing data frame, optionally grouped by between-subject factors.
* `rnorm_multi()` — generate multiple correlated normal variables with 
  specified means, SDs, and correlation structure.
* `rnorm_pre()` — create a normal vector correlated to one or more existing 
  vectors.
* `rmulti()` — NORTA-based simulation of correlated variables from 
  arbitrary marginal distributions.

### Mixed effects
* `add_random()` — add fully-crossed or nested random factors to a data 
  structure.
* `add_within()` — add within-subject factor levels.
* `add_between()` — randomly assign between-subject factor levels with 
  optional probability weights.
* `add_ranef()` — add normally distributed random effects per grouping factor.
* `add_recode()` — recode a factor column.
* `forge_mixed_cc()` — shortcut for cross-classified (subjects × items) 
  datasets.

### Contrast coding
* `add_contrast()` — add contrast-coded numeric columns to a data frame.
* `contr_code_treatment()` — treatment (dummy) coding.
* `contr_code_sum()` — sum (deviation) coding.
* `contr_code_helmert()` — Helmert coding.
* `contr_code_difference()` — successive difference coding.
* `contr_code_poly()` — polynomial (orthogonal) coding.
* `contr_code_anova()` — ANOVA coding.

### Distribution conversions
* `norm2beta()` / `beta2norm()` — Beta ↔ Normal.
* `norm2binom()` / `binom2norm()` — Binomial ↔ Normal.
* `norm2gamma()` / `gamma2norm()` — Gamma ↔ Normal.
* `norm2nbinom()` / `nbinom2norm()` — Negative Binomial ↔ Normal.
* `norm2pois()` / `pois2norm()` — Poisson ↔ Normal.
* `norm2unif()` / `unif2norm()` — Uniform ↔ Normal.
* `norm2trunc()` / `trunc2norm()` — Truncated Normal ↔ Normal.
* `norm2norm()` — rescale to target mean/SD.
* `norm2likert()` — Normal to Likert categories.

### Likert distributions
* `rlikert()` — simulate correlated Likert-scale items.
* `dlikert()`, `plikert()`, `qlikert()` — density, CDF, and quantile 
  functions for the Likert distribution.

### Data quality / messy data
* `make_missing()` / `messy()` — introduce missing values.
* `add_duplicates()` — inject duplicate rows.
* `add_outliers()` — inject extreme values.

### Reshaping
* `wide2long()` — wide to long format.
* `long2wide()` — long to wide format.

### Visualisation
* `plot_design()` — ggplot2-based design plot supporting violin, box, 
  point, bar, and line geoms.

### Utilities
* `make_id()` — generate zero-padded participant/item IDs.
* `is_pos_def()` — test whether a matrix is positive definite.
* `unique_pairs()` — generate variable-pair labels.
* `cormat_from_triangle()` — build a correlation matrix from a triangle 
  vector.
* `get_design()` — retrieve the `design` attribute from a data frame.
* `check_design()` — print a design summary.
* `get_params()` / `check_sim_stats()` — descriptive statistics per cell.
* `dataforge_options()` — set/get global options (`plot`, `verbose`, `sep`).

### Built-in datasets
* `load_exam_scores()` — 240-row simulated exam scores dataset.
* `load_survey_responses()` — 300-row simulated Likert survey dataset.
