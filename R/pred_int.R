#' Function to calculate prediction interval
#'
#' @param df df data frame of groundwater data in tidy format
#' @param analysis_result the analysis result column
#' @param distribution the distribution column
#' @param n.mean n.mean positive integer specifying the sample size associated
#' with the future averages.
#' The default value is n.mean=1 (i.e., individual observations).
#' Note that all future averages must be based on the same sample size.
#' @param k k positive integer specifying the number of future observations or
#' averages the prediction interval should contain with confidence level
#' conf.level. The default value is k=1.
#' @param m m if m = 1, then same results as predInt
#' @param r r
#' @param rule rule "k.of.m"
#' @param pi.type character string indicating what kind of prediction interval
#' to compute. The possible values are pi.type="upper" (the default),
#' and pi.type="lower".
#' @param conf.level a scalar between 0 and 1 indicating the confidence level
#' of the prediction interval. The default value is conf.level=0.95
#' @export

pred_int <- function(df,
                     analysis_result = "analysis_result",
                     distribution = "distribution",
                     n_mean = 1,
                     k = 1,
                     m = 1,
                     r = 1,
                     rule = "k.of.m",
                     pi_type = "upper",
                     conf_level = 0.95) {

  if (df$distribution[1] == "norm") {
    int <- EnvStats::predIntNormSimultaneous(
                                df$analysis_result,
                                n.mean = n_mean,
                                k = k,
                                m = m,
                                r = r,
                                rule = rule,
                                pi.type = pi_type,
                                conf.level = conf_level
                              )

  } else if (df$distribution[1] == "lnorm") {
     int <- EnvStats::predIntLnormSimultaneous(
                                 df$analysis_result,
                                 n.geomean = n_mean,
                                 k = k,
                                 m = m,
                                 r = r,
                                 rule = rule,
                                 pi.type = pi_type,
                                 conf.level = conf_level
                              )
  } else {
    int <- EnvStats::predIntNparSimultaneous(
                                 df$analysis_result,
                                 k = k,
                                 m = m,
                                 pi.type = pi_type
                               )
  }

  # int["data.name"] <- paste(df$location_id, df$param_name, sep = " ")

  return(int)

}