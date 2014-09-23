#' Function to calculate simultaneous prediction interval 
#' @param x analysis_result of gw data.frame
#' @param n.mean positive integer specifying the sample size associated 
#' with the future averages. 
#' The default value is n.mean=1 (i.e., individual observations). 
#' Note that all future averages must be based on the same sample size.
#' @param k for the k-of-m rule (rule="k.of.m"), a positive integer specifying 
#' the minimum number of observations (or averages) out of m observations 
#' (or averages) (all obtained on one future sampling “occassion”) the 
#' prediction interval should contain with confidence level conf.level. 
#' The default value is k=1. This argument is ignored when the argument rule 
#' is not equal to "k.of.m".
#' @param m positive integer specifying the maximum number of future 
#' observations (or averages) on one future sampling “occasion”. 
#' The default value is m = 2, except when rule="Modified.CA", in which case 
#' this argument is ignored and m is automatically set equal to 4.
#' @param r positive integer specifying the number of future sampling 
#' “occasions”. The default value is r=2.
#' @param rule character string specifying which rule to use. 
#' The possible values are "k.of.m" (k-of-m rule; the default), 
#' "CA" (California rule), and "Modified.CA" (modified California rule).
#' @param pi.type character string indicating what kind of prediction interval 
#' to compute. The possible values are pi.type="upper" (the default), 
#' and pi.type="lower".
#' @param conf.level a scalar between 0 and 1 indicating the confidence level 
#' of the prediction interval. The default value is conf.level=0.95
#' @param dist override distribution. Default is NULL, which will check for 
#' distribution.
#' @param K.tol numeric scalar indicating the tolerance to use in the nonlinear 
#' search algorithm to compute K. The default value is 
#' K.tol=.Machine$double.eps^(1/2). For many applications, the value of K needs 
#' to be known only to the second decimal place, in which case 
#' setting K.tol=1e-4 will speed up computation a bit.
#' @export

 pred_int_sim <- function(x, left_censored, non_detect = c(15, 50), 
                          cen_method = "mle", 
                          n.mean = 1, k = 1, 
                          m = 2, r = 2, 
                          rule = "k.of.m", pi.type = "upper",
                          conf.level = 0.95, dist = NULL, 
                          K.tol = .Machine$double.eps^0.5) {
  
  if(is.null(dist)){
    dist <- dist(x)
  }
  
  if (dist == "norm") {
    if (percent_left > non_detect[1] & percent_left < non_detect[2]){
      params <- enormCensored(x, left_censored, method = cen_method)
      out <- predIntNormSimultaneous(x, left_censored, k = k, m = m, r = r, 
                                     rule = "k.of.m", 
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
    out <- EnvStats::predIntNormSimultaneous(x, k = k, m = m, r = r, 
                                             rule = "k.of.m", 
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
  
  if (dist == "lnorm") {
    if (percent_left > non_detect[1] & percent_left < non_detect[2]){
      params <- elnormCensored(x, left_censored, method = cen_method)
      out <- predIntLnormSimultaneous(x, left_censored, k = k, m = m, r = r, 
                                     rule = "k.of.m", 
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
    out <- EnvStats::predIntLnormSimultaneous(x, k = k, m = m, r = r, 
                                              rule = "k.of.m", 
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
  
  if (dist == "none" | percent_left >= non_detect[2]) {
    out <- EnvStats::predIntNparSimultaneous(x, k = k, m = m, r = r, 
                                             rule = "k.of.m") 
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