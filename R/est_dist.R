#' Estimate statistical distribution
#'
#' Determines whether grouped data are best represented by a
#' normal, lognormal, or other candidate distribution.
#'
#' The returned object retains the original grouped observations in a
#' nested `data` list-column. This allows downstream functions such as
#' prediction interval, tolerance interval, and control chart
#' calculations to access the observations used for distribution
#' estimation.
#'
#' @param df Tidy groundwater data frame. Grouping determines the
#'   level at which distributions are estimated.
#' @param return_result TRUE/FALSE.
#' @param alpha Significance level.
#' @param method Goodness-of-fit method passed to EnvStats::distChoose().
#' @param choices Candidate distributions.
#' @param min_n Minimum sample size required for estimation.
#'
#' @return A tibble with one row per group containing:
#' \describe{
#'   \item{distribution}{Selected distribution.}
#'   \item{n}{Number of non-missing observations used in estimation.}
#'   \item{data}{Nested data frame containing the observations
#'   used for the group.}
#'   \item{result}{EnvStats result object stored as a
#'   list-column when return_result = TRUE.}
#' }
#'
#' @examples
#' data("gw_data")
#'
#' gw_data %>%
#'   dplyr::group_by(
#'     location_id,
#'     param_name,
#'     default_unit
#'   ) %>%
#'   percent_lt() %>%
#'   est_dist()
#'
#' @export

est_dist <- function(
  df,
  return_result = FALSE,
  alpha = 0.05,
  method = "sw",
  choices = c("norm", "lnorm"),
  min_n = 4
) {
  required_cols <- "analysis_result"

  missing_cols <- setdiff(
    required_cols,
    names(df)
  )

  if (length(missing_cols) > 0) {
    rlang::abort(
      paste(
        "Missing required column(s):",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  .validate_est_dist_args(
    alpha = alpha,
    method = method,
    choices = choices,
    return_result = return_result
  )

  .est_dist_worker(
    df = df,
    return_result = return_result,
    min_n = min_n,
    alpha = alpha,
    method = method,
    choices = choices
  )
}
