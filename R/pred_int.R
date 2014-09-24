#' Function to calculate simultaneous prediction interval 
#' @param x analysis_result of gw data.frame
#' @param left_censored column of TRUE/FALSE for censored variables
#' @param percent_left column of percent left censored variables
#' @param non_detect vector of cut-off values for non-detect processing
#' @param cen_method method for handlin non-detected between the cut-off values
#' @param ... other variabled passed to EnvStats predInt... functions

#' @export

pred_int_sim <- function(x, left_censored, percent_left, non_detect = c(15, 50), 
                         cen_method = "mle", dist = NULL, conf.level = 0.95,
                         ...) {
  
  if(is.null(dist)){
    dist <- dist(x)
  }
  
  if (dist == "norm") {
    if (percent_left > non_detect[1] && percent_left < non_detect[2]) {
      params <- enormCensored(x, left_censored, method = cen_method)
      out <- predIntNormSimultaneous(params, conf.level = conf.level, ...)
      x <- data.frame(
        variable = c("distribution", "count", "lower_limit", 
                     "upper_limit", "conf_level"),
        result = c(out$distribution, out$sample.size, 
                   round(out$interval$limits[["LPL"]], digits = 6),
                   round(out$interval$limits[["UPL"]], digits = 6), 
                   round(out$interval$conf.level, digits = 6))
      )
    } else {
    out <- EnvStats::predIntNormSimultaneous(x, conf.level = conf.level, ...)
    x <- data.frame(
      variable = c("distribution", "count", "lower_limit", 
                   "upper_limit", "conf_level"),
      result = c(out$distribution, out$sample.size, 
                 round(out$interval$limits[["LPL"]], digits = 6),
                 round(out$interval$limits[["UPL"]], digits = 6), 
                 round(out$interval$conf.level, digits = 6)) 
      )
    }
  }
  
  if (dist == "lnorm") {
    if (percent_left > non_detect[1] && percent_left < non_detect[2]) {
      params <- elnormCensored(x, left_censored, method = cen_method)
      out <- predIntLnormSimultaneous(params, conf.level = conf.level, ...)
      x <- data.frame(
        variable = c("distribution", "count", "lower_limit", 
                     "upper_limit", "conf_level"),
        result = c(out$distribution, out$sample.size, 
                   round(out$interval$limits[["LPL"]], digits = 6),
                   round(out$interval$limits[["UPL"]], digits = 6), 
                   round(out$interval$conf.level, digits = 6))
      )
    } else {
      out <- EnvStats::predIntLnormSimultaneous(x, conf.level = conf.level, ...) 
      x <- data.frame(
        variable = c("distribution", "count", "lower_limit", 
                     "upper_limit", "conf_level"),
        result = c(out$distribution, out$sample.size, 
                   round(out$interval$limits[["LPL"]], digits = 6),
                   round(out$interval$limits[["UPL"]], digits = 6), 
                   round(out$interval$conf.level, digits = 6)) 
       )
     }
  }
  
  if (dist == "none" || percent_left >= non_detect[2]) {
    out <- EnvStats::predIntNparSimultaneous(x, ...) 
    x <- data.frame(
      variable = c("distribution", "count", "lower_limit", "upper_limit", 
                   "conf_level"),
      result = c(out$distribution, out$sample.size, 
                 round(out$interval$limits[["LPL"]], digits = 6), 
                 round(out$interval$limits[["UPL"]], digits = 6), 
                 round(out$interval$conf.level, digits = 6))
    )
  }
  
  return(x)
  
}

#' Function to calculate prediction interval 
#' @param x analysis_result of gw data.frame
#' @param n.mean positive integer specifying the sample size associated 
#' with the future averages. 
#' The default value is n.mean=1 (i.e., individual observations). 
#' Note that all future averages must be based on the same sample size.
#' @param k positive integer specifying the number of future observations or
#' averages the prediction interval should contain with confidence level 
#' conf.level. The default value is k=1.
#' @param pi.type character string indicating what kind of prediction interval 
#' to compute. The possible values are pi.type="upper" (the default), 
#' and pi.type="lower".
#' @param conf.level a scalar between 0 and 1 indicating the confidence level 
#' of the prediction interval. The default value is conf.level=0.95
#' @param dist override distribution. Default is NULL, which will check for 
#' distribution.
#' @export

pred_int <- function(x, n.mean = 1, k = 1, m = 1, method = "Bonferroni", 
                     pi.type = "upper", conf.level = 0.95, dist = NULL) {
  
  if(is.null(dist)){
    dist <- dist(x)
  }
  
  if (dist == "norm") {
    out <- EnvStats::predIntNorm(x, n.mean = n.mean, k = k, method = method,
                                 pi.type = pi.type, conf.level = conf.level)
    x <- data.frame(
      variable = c("distribution", "count", "lower_limit", 
                   "upper_limit", "conf_level"),

      result = c(out$distribution, out$sample.size, 
                 round(out$interval$limits[["LPL"]], digits = 6),
                 round(out$interval$limits[["UPL"]], digits = 6), 
                 round(out$interval$conf.level, digits = 6)) 
    )
  }
  
  if (dist == "lnorm") {
    out <- EnvStats::predIntLnorm(x, n.geomean = n.mean, k = k, 
                                  method = method, pi.type = pi.type,
                                  conf.level = conf.level) 
    x <- data.frame(
      variable = c("distribution", "count", "lower_limit", 
                   "upper_limit", "conf_level"),

      result = c(out$distribution, out$sample.size, 
                 round(out$interval$limits[["LPL"]], digits = 6),
                 round(out$interval$limits[["UPL"]], digits = 6), 
                 round(out$interval$conf.level, digits = 6)) 
    )
  }
  
  if (dist == "none") {
    out <- EnvStats::predIntNpar(x, k = k, m = m) 
    x <- data.frame(
      variable = c("distribution", "count", "lower_limit", "upper_limit", 
                   "conf_level"),
      result = c(out$distribution, out$sample.size, 
                 round(out$interval$limits[["LPL"]], digits = 6), 
                 round(out$interval$limits[["UPL"]], digits = 6), 
                 round(out$interval$conf.level, digits = 6))
    )
  }
  
  return(x)
  
}