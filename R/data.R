#' Simulated Reaction Time Dataset
#'
#' @description
#' A simulated dataset representing a 2 (Group: control vs treatment) x 3
#' (Time: pre, mid, post) mixed factorial design, with 50 subjects per group.
#' The dependent variable is reaction time (ms).
#'
#' @format A data frame with 100 rows and 5 variables:
#' \describe{
#'   \item{id}{Subject identifier}
#'   \item{group}{Between-subject factor: `"control"` or `"treatment"`}
#'   \item{pre}{Reaction time at pre-test (ms)}
#'   \item{mid}{Reaction time at mid-test (ms)}
#'   \item{post}{Reaction time at post-test (ms)}
#' }
#'
#' @examples
#' data("rt_data", package = "dataforge")
#' head(rt_data)
#'
#' \dontrun{
#' plot_design(rt_data, x = "pre", dv = "pre")
#' }
"rt_data"

#' Simulated Cognitive Score Dataset
#'
#' @description
#' A simulated dataset with multiple cognitive measures for 120 participants
#' across three age groups.
#'
#' @format A data frame with 120 rows and 6 variables:
#' \describe{
#'   \item{id}{Participant identifier}
#'   \item{age_group}{Between-subject factor: `"young"`, `"middle"`, `"older"`}
#'   \item{memory}{Memory score}
#'   \item{attention}{Attention score}
#'   \item{processing_speed}{Processing speed score}
#'   \item{executive}{Executive function score}
#' }
"cog_data"
