#' Function to check if censored groundwater data has a Normal distribution
#' @param x analysis_result of groundwater data
#' @param censored logical vector of censored values
#' @export

is_normal_censored <- function(x, censored, ...) {
  gof <- gofTestCensored(x, censored, dist = "norm")
  p <- gof$p.value
  if (p >= 0.01){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Function to check if censored groundwater data has a Lognormal distribution
#' @param x analysis_result of groundwater data
#' @param censored logical vector of censored values
#' @export

is_lognormal_censored <- function(x, censored, ...) {
  lgof <- gofTestCensored(x, censored, dist = "lnorm")
  p <- lgof$p.value
  if (p >= 0.01){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Function to return distribution based on using p-value 
#' @param x analysis_result of groundwater data
#' @export

dist_censored <- function(x, censored) {
  n <- is_normal_censored(x, censored)
  if (isTRUE(n >= 0.01)) {
    return("norm_cen")
  }
  if (isTRUE(n < 0.01)) {
    ln <- is_lognormal_censored(x, censored)
    if (isTRUE(ln >= 0.01)) {
      return("lnorm_cen")
    } else {
      return("none")
    }
  } 
}

#' Function to return either normal, lognormal, or non-parametric multiple 
#' groundwater data locations and parameters.
#' @param df groundwater data frame
#' @export

est_dist_censored <- function(df) {  
  dist_result <- df %>%
    group_by(location_id, param_name, default_unit) %>%
    summarise(distribution = dist_censored(analysis_result, left_censored))
  return(dist_result)
}