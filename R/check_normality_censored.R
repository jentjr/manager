#' Function to return either normal, lognormal, or non-parametric multiple 
#' groundwater data locations and parameters.
#' 
#' @param df groundwater data frame in tidy format
#' @param p_value p-value 
#' @export

est_dist_censored <- function(df, p_value = 0.05) {  
  dist_result <- df %>%
    group_by(location_id, param_name, default_unit) %>%
    summarise(distribution = .dist_censored(analysis_result, left_censored,
                                           p = p_value))
  return(dist_result)
}

#' Helper function to check if censored groundwater data has a
#' Normal distribution

.is_normal_censored <- function(x, censored, p = 0.01, ...) {
  gof <- EnvStats::gofTestCensored(x, censored, dist = "norm", ...)
  p <- gof$p.value
  if (p >= p){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#'Helper function to check if censored groundwater data has a
#'Lognormal distribution

.is_lognormal_censored <- function(x, censored, p = 0.01, ...) {
  lgof <- EnvStats::gofTestCensored(x, censored, dist = "lnorm")
  p <- lgof$p.value
  if (p >= p){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Helper function to return distribution based on using p-value 

.dist_censored <- function(x, censored, p = 0.01, ...) {
  n <- .is_normal_censored(x, censored, p, ...)
  if (isTRUE(n >= p)) {
    return("norm_cen")
  }
  if (isTRUE(n < p)) {
    ln <- .is_lognormal_censored(x, censored, p, ...)
    if (isTRUE(ln >= p)) {
      return("lnorm_cen")
    } else {
      return("none")
    }
  } 
}