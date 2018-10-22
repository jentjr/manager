#' Boxcox transformation 
#' 
#' @param df data frame
#' @param combine_locations
#' @param keep_data_object
#' 
#' @export

boxcox_transform <- function(df,
                             combine_locations = FALSE,
                             keep_data_object = FALSE) {
  
  if (combine_locations) {
    df <- df %>%
      group_by(param_name, default_unit) %>%
      nest()
  } else {
    df <- df %>%
      group_by(location_id, param_name, default_unit) %>%
      nest()
  }

  if (keep_data_object) {
    df <- df %>%  
      mutate(
        boxcox = map(.x = data,
                     ~EnvStats::boxcox(.x$analysis_result, optimize = TRUE)
        )
      )
  } else {
    df <- df %>%  
      mutate(
        boxcox = map(.x = data,
                     ~EnvStats::boxcox(.x$analysis_result, optimize = TRUE)
        )
      ) %>%
      mutate(
        lambda = map(.x = boxcox,
                     ~ .x$lambda),
        objective = map(.x = boxcox,
                        ~ .x$objective)
      ) %>%
      select(-data, -boxcox) %>%
      unnest()
  }

 df

}