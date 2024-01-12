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
#' est_dist_censored(., alpha = 0.05, keep_data_object = FALSE)
#'
#'
#' gw_data %>%
#' group_by(param_name, default_unit) %>%
#' est_dist_censored(., combine_locations = TRUE, keep_data_object = FALSE)
#' 
#' @export

est_dist_censored <- function(df, 
                              left_censored,
                              method = "sf",
                              alpha = 0.05,
                              choices = c("norm", "lnorm"),
                              combine_locations = FALSE,
                              keep_data_object = FALSE) {

  if (isTRUE(combine_locations)) {
    nested_df <- df %>%
      to_censored() %>%
      group_by(param_name, default_unit) %>%
      nest()
    
  } else {
    
    nested_df <- df %>%
      to_censored() %>%
      group_by(location_id, param_name, default_unit) %>%
      nest() 
    
  }

  cen_dist_est <- nested_df %>%
    mutate(cen_dist_est = map(.x = data, ~distChooseCensored(
      x = .x$analysis_result, .x$left_censored, censoring.side ='left',
      choices = choices, method = method, alpha = alpha))
      )

  if (isTRUE(keep_data_object)) {

    cen_dist_est %>%
      mutate(distribution = map(.x = cen_dist_est, ~ .x$decision)) %>%
      unnest(distribution)

  } else {

    cen_dist_est %>%
      mutate(distribution = map(.x = cen_dist_est, ~ .x$decision)) %>%
      select(-data) %>%
      unnest(distribution)

  }

}
