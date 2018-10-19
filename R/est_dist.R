#' Function to return either normal, lognormal, or non-parametric 
#' distribution.
#' 
#' @param df groundwater data frame in tidy format
#' @param alpha alpha
#' @param method default is "sw"
#' @param choices vector of distributions to check. Default is c("norm, "lnorm")
#' @param group_by_location TRUE/FALSE to estimate distribution by individual
#' location, or grouped together. Default is FALSE.
#' @param keep_data_object Default is FALSE
#' 
#' @examples 
#' data("gw_data")
#' wells <- c("MW-1", "MW-2", "MW-3", "MW-4")
#' params <- c("Sulfate, total",
#'             "Arsenic, dissolved",
#'            "Boron, dissolved")
#' 
#' gw_data <- gw_data %>%
#' filter(location_id %in% wells, param_name %in% params) %>%
#' percent_lt() %>%
#' filter(percent_lt <=15)
#' 
#' gw_data %>%
#' group_by(location_id, param_name, default_unit) %>%
#' est_dist(., alpha = 0.05, keep_data_object = FALSE)
#'
#'
#' gw_data %>%
#' group_by(param_name, default_unit) %>%
#' est_dist(., group_by_location = TRUE, keep_data_object = FALSE)
#' 
#' @export

est_dist <- function(df, 
                     alpha = 0.05, 
                     method = "sw", 
                     choices = c("norm", "lnorm"),
                     group_by_location = FALSE,
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
    mutate(dist_est = map(.x = data, ~distChoose(
      y = .x$analysis_result,
      choices = choices, method = method, alpha = alpha))
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
