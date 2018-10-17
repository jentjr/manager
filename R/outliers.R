#' Dixon test for outliers
#' 
#' @param df data frame
#' @param x column of analysis results
#' @param opposite  logical indicating whether you want to check not the value with
#' largest difference from the mean, but opposite (lowest, if most suspicious
#' is highest etc.)
#' @param type an integer specyfying the variant of test to be performed.
#' Possible values are compliant with these given by
#' Dixon (1950): 10, 11, 12, 20, 21. If this value is set to zero, a variant
#' of the test is chosen according to sample size
#' (10 for 3-7, 11 for 8-10, 21 for 11-13, 22 for 14 and more).
#' The lowest or highest value is selected automatically, and can be reversed
#' used opposite parameter.
#' @param two_sided treat test as two-sided. Default is TRUE.
#' @param group_by_location TRUE/FALSE
#' @param keep_data_object TRUE/FALSE to return entire htest object
#' 
#' @export

dixon_test <- function(df,
                       x = "analysis_result",
                       type = 0,
                       opposite = FALSE,
                       two_sided = TRUE,
                       group_by_location = FALSE,
                       keep_data_object = FALSE) {
  
  if (isTRUE(group_by_location)) {
    df <- df %>%
      group_by(param_name, default_unit) %>%
      nest()
  } else {
    df <- df %>%
      group_by(location_id, param_name, default_unit) %>%
      nest()
  }
  
  df <- df %>%
    mutate(dixon = map(.x = data, ~outliers::dixon.test(
                                               .x$analysis_result,
                                               type = type, 
                                               opposite = opposite,
                                               two.sided = two_sided
                                               )
                   )
    )
  
  if (isTRUE(keep_data_object)) {
    outliers <- df %>%
      mutate(alternative = map(.x = dixon, ~ .x$alternative),
             p_value = map(.x = dixon, ~ .X$p.value)) %>%
      select(-data) 
  } else {
    outliers <- df %>%
      mutate(alternative = map(.x = dixon, ~ .x$alternative),
             p_value = map(.x = dixon, ~ .x$p.value)) %>%
      select(-data, -dixon) %>%
      unnest()
  }

  return(outliers)

}

#' Grubb's test for outliers
#' 
#' @param df data frame
#' @param x column of analysis results
#' @param type integer value indicating test variant. 10 is a test for one
#' outlier (side is detected automatically and can be reversed by opposite
#' parameter). 11 is a test for two outliers on opposite tails, 20 is test
#' for two outliers in one tail. Default is 10
#' @param opposite a logical indicating whether you want to check not the
#' value with largest difference from the mean, but opposite (lowest, if
#' most suspicious is highest etc.). Default is FALSE
#' @param two_sided Logical value indicating if there is a need to treat
#' this test as two-sided. Default is FALSE
#' @param group_by_location TRUE/FALSE default is FALSE
#' @param keep_data_object TRUE/FALSE default is FALSE
#' @export

grubbs_test <- function(df,
                        x = "analysis_result",
                        type = 10,
                        opposite = FALSE,
                        two_sided = FALSE,
                        group_by_location = FALSE,
                        keep_data_object = FALSE
                        ) {
  
  if (isTRUE(group_by_location)) {
    df <- df %>%
      group_by(param_name, default_unit) %>%
      nest()
  } else {
    df <- df %>%
      group_by(location_id, param_name, default_unit) %>%
      nest()
  }
  
  df <- df %>%
    mutate(grubbs = map(.x = data, ~outliers::grubbs.test(
                                                .x$analysis_result,
                                                type = type,
                                                opposite = opposite,
                                                two.sided = two_sided
                                                )
                        )
           )
  
  if (isTRUE(keep_data_object)) {
    outliers <- df %>%
      mutate(alternative = map(.x = grubbs, ~ .x$alternative),
             p_value = map(.x = grubbs, ~ .X$p.value)) %>%
      select(-data) 
  } else {
    outliers <- df %>%
      mutate(alternative = map(.x = grubbs, ~ .x$alternative),
             p_value = map(.x = grubbs, ~ .x$p.value)) %>%
      select(-data, -grubbs) %>%
      unnest()
  }

  return(outliers)

}

#' Rosner's test for outliers
#' 
#' @param df data.frame of groundwater data 
#' @param x column of analysis results
#' @param k number of suspected outliers. Default is 3.
#' @param alpha numerical scalar bewteen 0 and 1 indicating the Type I error
#' @param warn logical scalar indicating whether to issue a warning
#' (warn=TRUE; the default) when the number of non-missing, finite values
#' in x and the value of k are such that the assumed Type I error level might
#' not be maintained. 
#' @param group_by_location TRUE/FALSE
#' @param keep_data_object TRUE/FALSE to return entire gofOutlier object
#' @export

rosner_test <- function(df,
                        x = "analysis_result",
                        k = 3,
                        alpha = 0.05,
                        warn = TRUE,
                        group_by_location = FALSE,
                        keep_data_object = FALSE) {

  if (isTRUE(group_by_location)) {
    df <- df %>%
      group_by(param_name, default_unit) %>%
      nest()
  } else {
    df <- df %>%
      group_by(location_id, param_name, default_unit) %>%
      nest()
  }

  df <- df %>%
    mutate(rosner = map(.x = data, ~rosnerTest(.x$analysis_result,
                                               k = k, 
                                               alpha = alpha, 
                                               warn = warn
                                               )
                        )
           )
  
  if (isTRUE(keep_data_object)) {
    outliers <- df %>%
      mutate(value = map(.x = rosner, ~ .x$all.stats[c("Value", "Outlier")])) %>%
      select(-data) 
  } else {
    outliers <- df %>%
      mutate(value = map(.x = rosner, ~ .x$all.stats[c("Value", "Outlier")])) %>%
      select(-data, -rosner) %>%
      unnest()
  }

  return(outliers)

}

#' Tukey's test for outliers
#' 
#' @param df data.frame of groundwater data 
#' @param x column of analysis results
#' @param k multiplier for IQR k = 1.5 indicates "outlier", k = 3 indicates "far out"
#' @param group_by_location TRUE/FALSE
#' @export

tukey_outlier <- function(df, x = "analysis_result", k = 3, 
                          group_by_location = FALSE) {
  
  if (isTRUE(group_by_location)) {
    df <- df %>%
      group_by(param_name, default_unit) %>%
      nest()
  } else {
    df <- df %>%
      group_by(location_id, param_name, default_unit) %>%
      nest()
  }
  
  df <- df %>%
    mutate(outlier = map(.x = data, ~case_when(
      .x$analysis_result > .tukey_high_cutoff(.x$analysis_result, k = k) ~ TRUE,
      .x$analysis_result < .tukey_low_cutoff(.x$analysis_result, k = k) ~ TRUE,
      TRUE ~ FALSE
      )
     )
    ) %>%
    unnest()
  
  return(df)
}

.tukey_low_cutoff <- function(x, k = 3) {
  low <- quantile(x)[["25%"]] - k*IQR(x)
  return(low)
}

.tukey_high_cutoff <- function(x, k = 3) {
  high <- quantile(x)[["75%"]] + k*IQR(x)
  return(high)
}