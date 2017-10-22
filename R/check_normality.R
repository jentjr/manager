#' Function to return either normal, lognormal, or non-parametric multiple 
#' groundwater data locations and parameters.
#' 
#' @param df groundwater data frame in tidy format
#' @param p p-value for gofTest
#' @export

est_dist <- function(df, p = 0.01) {  
  df %>%
    group_by(location_id, param_name) %>%
    mutate(distribution = dist(analysis_result, p = p)) %>%
    ungroup()
}

#' Helper function to check if data has a Normal distribution

is_normal <- function(x, p = 0.01) {
  gof <- gofTest(x, dist = "norm")
  p_test <- gof$p.value
  if (p_test >= p) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Helper function to check if data has a Lognormal distribution

is_lognormal <- function(x, p = 0.01) {
  lgof <- gofTest(x, dist = "lnorm")
  p_test <- lgof$p.value
  if (p_test >= p) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#'Helper function to return distribution based on using p-value 

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