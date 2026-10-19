#' Function to return either normal, lognormal, gamma, or non-parametric
#' distribution estimate
#'
#' @param df groundwater data frame in tidy format
#' @param left_censored logical vector.
#' @param alpha alpha
#' @param choices vector of distributions to check. Default is c("norm, "lnorm")
#' @param method default is "sf"
#' @param combine_locations TRUE/FALSE to estimate distribution by individual
#' location, or grouped together. Default is FALSE.
#' @param keep_data_object Default is FALSE
#'
#' @examples
#' data("gw_data")
#' wells <- c("MW-1", "MW-2", "MW-3")
#' params <- c("Fluoride, total",
#'             "Arsenic, dissolved",
#'             "Nickel, dissolved")
#'
#' gw_data <- gw_data %>%
#' to_censored() %>%
#' filter(location_id %in% wells, param_name %in% params) %>%
#' percent_lt() %>%
#' filter(percent_lt >=15, percent_lt <= 50)
#'
#' gw_data %>%
#' group_by(location_id, param_name, default_unit) %>%
#' est_dist_censored()
#'
#'
#' gw_data %>%
#' group_by(param_name, default_unit) %>%
#' est_dist_censored()
#'
#' @export

est_dist_censored <- function(
  df,
  return_result = FALSE,
  alpha = 0.05,
  method = "sf",
  choices = c("norm", "lnorm"),
  min_n = 4
) {
  required_cols <- c(
    "analysis_result",
    "left_censored"
  )

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

  .est_dist_censored_worker(
    df = df,
    return_result = return_result,
    min_n = min_n,
    alpha = alpha,
    method = method,
    choices = choices
  )
}
