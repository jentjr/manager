#' Function to calculate prediction interval 
#' @param x analysis_result of gw data.frame
#' @param location_id name of well
#' @param param_name name of parameter
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

 pred_int <- function(x, location_id = NULL, param_name = NULL, 
                      n.mean = 1, k = 1, m = 2, r = 2, 
                      rule = "k.of.m", pi.type = "upper",
                      conf.level = 0.95, dist = NULL, 
                      K.tol = .Machine$double.eps^0.5) {
  if(is.null(dist)){
    dist <- dist(x)
  }
  
  if (dist == "norm") {
    out <- EnvStats::predIntNormSimultaneous(x, k = k, m = m, r = r, 
                                             rule = "k.of.m", 
                                             conf.level = conf.level)
    out["data.name"] <- paste(location_id[1], param_name[1], sep = " ")    
  }
  
  if (dist == "lnorm") {
    out <- EnvStats::predIntLnormSimultaneous(x, k = k, m = m, r = r, 
                                              rule = "k.of.m", 
                                              conf.level = conf.level)
    out["data.name"] <- paste(location_id[1], param_name[1], sep = " ") 
  }
  
  if (dist == "none") {
    out <- EnvStats::predIntNparSimultaneous(x, k = k, m = m, r = r, 
                                             rule = "k.of.m")
    out["data.name"] <- paste(location_id[1], param_name[1], sep = " ") 
  }
  
  return(out)
  
}