#' Function to calculate confidence intervals
#'
#' @param df df data frame of groundwater data in tidy format
#' @param analysis_result the analysis result column
#' @param distribution the distribution column
#' @param ci_type character string indicating what kind of confidence interval
#' to compute. The possible values are ci_type="two-sided" (the default),
#' ci_type="lower", and ci_type = "upper
#' @param conf_level a scalar between 0 and 1 indicating the confidence level
#' of the prediction interval. The default value is conf_level = 0.95
#' @param method character string specifying the method of estimation.
#' Possible values are "mvue" (minimum variance unbiased; the default),
#' and "mle/mme" (maximum likelihood/method of moments).
#' @export

conf_int <- function(df,
                     analysis_result = "analysis_result",
                     distribution = "distribution",
                     method = "mvue",
                     ci_type = "two-sided",
                     conf_level = 0.95) {
  
  if (df$distribution[1] == "norm") {
    int <- EnvStats::enorm(
      df$analysis_result,
      method = method,
      ci = TRUE,
      ci.type = ci_type,
      conf.level = conf_level
    )
    
  } else if (df$distribution[1] == "lnorm") {
    int <- EnvStats::elnorm(
      df$analysis_result,
      method = method,
      ci = TRUE,
      ci.type = ci_type,
      conf.level = conf_level
    )
  } else {
    int <- EnvStats::eqnpar(
      ci = TRUE,
      df$analysis_result,
      ci.type = ci_type
    )
  }
  
  # int["data.name"] <- paste(df$location_id, df$param_name, sep = " ")
  
  return(int)
  
}