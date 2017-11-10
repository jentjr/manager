#' Function to return either normal, lognormal, gamma, or non-parametric 
#' distribution estimate
#' 
#' @param df groundwater data frame in tidy format
#' @param left_censored logical vector.
#' @param alpha alpha 
#' @param group_by_location TRUE/FALSE to estimate distribution by individual
#' location, or grouped together. Default is FALSE.
#' @param method default is "sf"
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
#' est_dist_censored(., alpha = 0.05, keep_data_object = FALSE)
#'
#'
#' gw_data %>%
#' group_by(param_name, default_unit) %>%
#' est_dist_censored(., group_by_location = TRUE, keep_data_object = FALSE)
#' @export

est_dist_censored <- function(df, left_censored, alpha = 0.05,
                              group_by_location = FALSE,
                              method = "sf",
                              keep_data_object = FALSE) {

  if (isTRUE(group_by_location)) {
    nested_df <- df %>%
      group_by(param_name, default_unit) %>%
      nest()
    
  } else {
    
    nested_df <- df %>%
      group_by(location_id, param_name, default_unit) %>%
      nest() 
    
  }

  dist_est <- nested_df %>%
    mutate(dist_est = map(.x = data, ~EnvStats::distChooseCensored(
      x = .x$analysis_result, .x$left_censored, censoring.side ='left',
      choices = c("norm", "lnorm"), method = method, alpha = alpha))
      )

  if (isTRUE(keep_data_object)) {

    dist_est %>%
      mutate(distribution = map(.x = dist_est, ~ .x$decision)) %>%
      select(-dist_est) %>%
      unnest(distribution)

  } else {

    dist_est %>%
      mutate(distribution = map(.x = dist_est, ~ .x$decision)) %>%
      select(-data, -dist_est) %>%
      unnest()

  }

}

