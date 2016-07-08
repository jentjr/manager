#' Function to check if groundwater data has a Normal distribution
#' @param x analysis_result of groundwater data
#' @export

is_normal <- function(x, p = 0.01) {
  gof <- gofTest(x, dist = "norm")
  p_test <- gof$p.value
  if (p_test >= p) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Function to check if groundwater data has a Lognormal distribution
#' @param x analysis_result of groundwater data
#' @export

is_lognormal <- function(x, p = 0.01) {
  lgof <- gofTest(x, dist = "lnorm")
  p_test <- lgof$p.value
  if (p_test >= p) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Function to return distribution based on using p-value 
#' @param x analysis_result of groundwater data
#' @export

dist <- function(x, p = 0.01) {
  n <- is_normal(x, p = p)
  if (isTRUE(n >= p)) {
    return("norm")
  }
  if (isTRUE(n < p)) {
    ln <- is_lognormal(x, p = p)
    if (isTRUE(ln >= p)) {
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

est_dist <- function(df, p = 0.01) {  
  dist_result <- df %>%
    group_by(location_id, param_name, default_unit) %>%
    summarise(distribution = dist(analysis_result, p = p))
  return(dist_result)
}