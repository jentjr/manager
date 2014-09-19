#' Function to check if groundwater data has a Normal distribution
#' @param x analysis_result of groundwater data
#' @export

is_normal <- function(x) {
  gof <- gofTest(x, dist = "norm")
  p <- gof$p.value
  if (p >= 0.01){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Function to check if groundwater data has a Lognormal distribution
#' @param x analysis_result of groundwater data
#' @export

is_lognormal <- function(x) {
  lgof <- gofTest(x, dist = "lnorm")
  p <- lgof$p.value
  return(p)
}

#' Function to return distribution based on using p-value 
#' @param x analysis_result of groundwater data
#' @export

dist <- function(x) {
  n <- is_normal(x)
  if (isTRUE(n >= 0.01)) {
    return("norm")
  }
  if (isTRUE(n < 0.01)) {
    ln <- is_lognormal(x)
    if (isTRUE(ln >= 0.01)) {
      return("lnorm")
    } else {
      return("none")
    }
  } 
}

#' Function to return either normal, lognormal, or non-parametric multiple 
#' groundwater data locations and parameters.
#' @param df groundwater data frame
#' @export

est_dist <- function(df) {  
  dist_result <- df %>%
    group_by(location_id, param_name, default_unit) %>%
    summarise(distribution = dist(analysis_result))
  return(dist_result)
}