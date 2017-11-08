#' Function to return either normal, lognormal, or non-parametric 
#' distribution.
#' 
#' @param df groundwater data frame in tidy format
#' @param alpha alpha
#' @param method default is "sw"
#' @param choices default is c("norm, "lnorm")
#' 
#' @export

est_dist <- function(df, alpha = 0.05, method = "sw",
                     keep_data_object = FALSE) {

  nested_df <- df %>%
    group_by(location_id, param_name, default_unit) %>%
    nest() 
  
  dist_est <- nested_df %>%
    mutate(dist_est = map(.x = data, ~EnvStats::distChoose(
      y = .x$analysis_result,
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
