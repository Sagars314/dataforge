# NOTE: Because the package ships data as CSVs in inst/extdata (for portability),
# the datasets are loaded on demand via the helper functions below.
# Once you have R available, run data-raw/save_datasets.R to generate proper
# .rda files and add LazyData: true to DESCRIPTION.

#' Load the built-in exam_scores dataset
#'
#' @return A data frame (see `?exam_scores_data` for column descriptions).
#' @export
#' @examples
#' df <- load_exam_scores()
#' head(df)
load_exam_scores <- function() {
  path <- system.file("extdata", "exam_scores.csv", package = "dataforge")
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  df$condition <- factor(df$condition, levels = c("control","method_A","method_B"))
  df$cohort    <- factor(df$cohort,    levels = c("morning","afternoon"))
  df
}

#' Load the built-in survey_responses dataset
#'
#' @return A data frame (see `?survey_responses_data` for column descriptions).
#' @export
#' @examples
#' df <- load_survey_responses()
#' head(df)
load_survey_responses <- function() {
  path <- system.file("extdata", "survey_responses.csv", package = "dataforge")
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  df$group <- factor(df$group, levels = c("group_A","group_B"))
  df
}

#' Simulated exam scores dataset
#'
#' A synthetic dataset of 240 students' exam scores across two time points
#' (pre- and post-intervention) in three teaching conditions. Generated with
#' `dataforge` for use in examples and testing.
#'
#' @format A data frame with 240 rows and 5 variables:
#' \describe{
#'   \item{id}{Character. Student ID.}
#'   \item{condition}{Factor. Teaching condition: `"control"`, `"method_A"`,
#'     `"method_B"`.}
#'   \item{cohort}{Factor. Student cohort: `"morning"` or `"afternoon"`.}
#'   \item{pre}{Numeric. Pre-test score (0–100).}
#'   \item{post}{Numeric. Post-test score (0–100).}
#' }
#' @source Generated via `dataforge::forge_design()`.
"exam_scores"

#' Simulated survey responses dataset
#'
#' A synthetic dataset of 300 respondents' answers to a 5-item Likert survey
#' across two demographic groups.
#'
#' @format A data frame with 300 rows and 8 variables:
#' \describe{
#'   \item{id}{Character. Respondent ID.}
#'   \item{group}{Factor. Demographic group: `"group_A"` or `"group_B"`.}
#'   \item{item1}{Integer. Response to item 1 (1–5).}
#'   \item{item2}{Integer. Response to item 2 (1–5).}
#'   \item{item3}{Integer. Response to item 3 (1–5).}
#'   \item{item4}{Integer. Response to item 4 (1–5).}
#'   \item{item5}{Integer. Response to item 5 (1–5).}
#'   \item{score}{Numeric. Mean of item1–item5.}
#' }
#' @source Generated via `dataforge::rlikert()`.
"survey_responses"
