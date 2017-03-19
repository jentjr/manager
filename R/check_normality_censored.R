#' Function to check if censored groundwater data has a Normal distribution
#' 
#' @param x column of analysis result for groundwater data in tidy format
#' @param censored logical vector of censored values
#' @param p the p-value 
#' @param ... other variable inputs passed to gofTestCensored
#' @export

is_normal_censored <- function(x, censored, p = 0.01, ...) {
  gof <- gofTestCensored(x, censored, dist = "norm", ...)
  p <- gof$p.value
  if (p >= p){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Function to check if censored groundwater data has a Lognormal distribution
#' 
#' @param x column of analysis result for groundwater data in tidy format
#' @param censored logical vector of censored values
#' @param p the p-value
#' @param ... other variable inputs passed to gofTestCensored
#' @export

is_lognormal_censored <- function(x, censored, p = 0.01, ...) {
  lgof <- gofTestCensored(x, censored, dist = "lnorm")
  p <- lgof$p.value
  if (p >= p){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Function to return distribution based on using p-value 
#' 
#' @param x column of analysis result for groundwater data in tidy format
#' @param censored logical vector of censored values
#' @param p the p-value
#' @param ... other variable inputs passed to gofTestCensored
#' @export

dist_censored <- function(x, censored, p = 0.01, ...) {
  n <- is_normal_censored(x, censored, p, ...)
  if (isTRUE(n >= p)) {
    return("norm_cen")
  }
  if (isTRUE(n < p)) {
    ln <- is_lognormal_censored(x, censored, p, ...)
    if (isTRUE(ln >= p)) {
      return("lnorm_cen")
    } else {
      return("none")
    }
  } 
}

#' Function to return either normal, lognormal, or non-parametric multiple 
#' groundwater data locations and parameters.
#' 
#' @param df groundwater data frame in tidy format
#' @export

est_dist_censored <- function(df) {  
  dist_result <- df %>%
    group_by(location_id, param_name, default_unit) %>%
    summarise(distribution = dist_censored(analysis_result, left_censored))
  return(dist_result)
}