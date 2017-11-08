#' Function to return either normal, lognormal, gamma, or non-parametric 
#' distribution estimate
#' 
#' @param df groundwater data frame in tidy format
#' @param left_censored logical vector.
#' @param alpha alpha 
#' @param method default is "sf"
#' @export

est_dist_censored <- function(df, left_censored, alpha = 0.05,
                              method = "sf", keep_object = FALSE) {
  nested_df <- df %>%
    group_by(location_id, param_name, default_unit) %>%
    nest() 

  dist_est <- nested_df %>%
    mutate(dist_est = map(.x = data, ~EnvStats::distChooseCensored(
      x = .x$analysis_result, .x$left_censored, censoring.side ='left',
      choices = c("norm", "lnorm"), method = method, alpha = alpha))
      )

  if (isTRUE(keep_object)) {

    dist_est %>%
      select(dist_est)

  } else {

    dist_est %>%
      mutate(distribution = map(.x = dist_est, ~ .x$decision)) %>%
      select(-data, -dist_est) %>%
      unnest()

  }

}

