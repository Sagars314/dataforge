# Run once to convert data-raw CSVs into R data objects.
# Requires the dataforge package to be loadable (devtools::load_all()).

exam_scores <- read.csv("data-raw/exam_scores.csv", stringsAsFactors = FALSE)
exam_scores$condition <- factor(exam_scores$condition,
                                levels = c("control","method_A","method_B"))
exam_scores$cohort    <- factor(exam_scores$cohort,
                                levels = c("morning","afternoon"))
save(exam_scores, file = "data/exam_scores.rda", compress = "bzip2")

survey_responses <- read.csv("data-raw/survey_responses.csv", stringsAsFactors = FALSE)
survey_responses$group <- factor(survey_responses$group,
                                 levels = c("group_A","group_B"))
save(survey_responses, file = "data/survey_responses.rda", compress = "bzip2")

message("Datasets saved to data/")
