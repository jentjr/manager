#' Boxcox transformation 
#'
#' @param df data frame
#' @param combine_locations logical
#' @param objective_name character string indicating what objective to use.
#' The possible values are "PPCC" (probability plot correlation coefficient;
#' the default), "Shapiro-Wilk" (the Shapiro-Wilk goodness-of-fit statistic),
#' and "Log-Likelihood" (the log-likelihood function).
#'
#' @export

boxcox_transform <- function(df,
                             combine_locations = FALSE,
                             objective_name = "PPCC") {

  if (combine_locations) {
    
    df <- df %>%
      group_by(param_name, default_unit) %>%
      nest()%>%
      mutate(
        boxcox = map(.x = data,
                     ~EnvStats::boxcox(.x$analysis_result,
                                       optimize = TRUE,
                                       objective.name = objective_name)
        )
      ) %>%
      mutate(
        lambda = map(.x = boxcox,
                     ~.x$lambda)
      ) %>%
      select(-boxcox) %>%
      unnest(lambda)%>%
      unnest(data) %>%
      group_by(param_name, default_unit) %>%
      nest() %>%
      mutate(
        analysis_result = map(.x = data,
                              ~boxcoxTransform(.x$analysis_result,
                                               lambda = .x$lambda[1])
        )
      ) %>%
      unnest()

  } else {

    df <- df %>%
      group_by(location_id, param_name, default_unit) %>%
      nest()%>%
      mutate(
        boxcox = map(.x = data,
                     ~EnvStats::boxcox(.x$analysis_result,
                                       optimize = TRUE,
                                       objective.name = objective_name)
        )
      ) %>%
      mutate(
        lambda = map(.x = boxcox,
                     ~.x$lambda)
      ) %>%
      select(-boxcox) %>%
      unnest(lambda)%>%
      unnest(data) %>%
      group_by(location_id, param_name, default_unit) %>%
      nest() %>%
      mutate(
        analysis_result = map(.x = data,
                              ~boxcoxTransform(.x$analysis_result,
                                               lambda = .x$lambda[1])
        )
      ) %>%
      unnest()
  }

 df

}