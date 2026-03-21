# Run this script to regenerate the built-in datasets.
# Place in data-raw/ and call: source("data-raw/generate_datasets.R")

library(dataforge)

set.seed(42)

# ---- exam_scores ------------------------------------------------------------
df_wide <- forge_design(
  between = list(
    condition = c(control = "Control",
                  method_A = "Method A",
                  method_B = "Method B"),
    cohort = c(morning = "Morning", afternoon = "Afternoon")
  ),
  within = list(time = c("pre", "post")),
  n  = 20,
  mu = data.frame(
    control.morning   = c(50, 52),
    control.afternoon = c(48, 50),
    method_A.morning  = c(50, 62),
    method_A.afternoon= c(49, 60),
    method_B.morning  = c(50, 70),
    method_B.afternoon= c(48, 68),
    row.names = c("pre", "post")
  ),
  sd  = 10, r = .6, plot = FALSE
)
# Re-shape to have pre/post as separate columns
exam_scores <- df_wide
usethis::use_data(exam_scores, overwrite = TRUE)

# ---- survey_responses -------------------------------------------------------
set.seed(99)
items_A <- rlikert(150, items = 5, likert_n = 5,
                   mu = c(3.5, 3.2, 3.8, 3.0, 3.6), sd = 0.8, r = .5)
items_B <- rlikert(150, items = 5, likert_n = 5,
                   mu = c(2.8, 2.5, 3.0, 2.7, 2.9), sd = 0.9, r = .5)

survey_responses <- rbind(
  data.frame(id    = make_id(150, "R"),
             group = factor(rep("group_A", 150)),
             items_A,
             score = rowMeans(items_A)),
  data.frame(id    = make_id(150, "R", digits = 3)[151:300],
             group = factor(rep("group_B", 150)),
             items_B,
             score = rowMeans(items_B))
)
usethis::use_data(survey_responses, overwrite = TRUE)
