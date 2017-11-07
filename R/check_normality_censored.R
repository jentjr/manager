#' Function to return either normal, lognormal, or non-parametric multiple 
#' groundwater data locations and parameters.
#' 
#' @param df groundwater data frame in tidy format
#' @param p_value p-value 
#' @export

est_dist_censored <- function(df, left_censored, p_value = 0.05) {
  df %>%
    group_by(location_id, param_name, default_unit) %>%
    mutate(distribution = .dist_censored(analysis_result, left_censored,
                                           p = p_value, ...)) %>%
    ungroup()
}

#' Helper function to check if censored groundwater data has a
#' Normal distribution

.is_normal_censored <- function(x, left_censored, p = 0.05, ...) {
  gof <- EnvStats::gofTestCensored(x, left_censored, dist = "norm", ...)
  p <- gof$p.value
  if (p >= p) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#'Helper function to check if censored groundwater data has a
#'Lognormal distribution

.is_lognormal_censored <- function(x, left_censored, p = 0.05, ...) {
  lgof <- EnvStats::gofTestCensored(x, left_censored, dist = "lnormAlt", ...)
  p <- lgof$p.value
  if (p >= p) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Helper function to return distribution based on using p-value 

.dist_censored <- function(x, left_censored, p = 0.05, ...) {
  n <- .is_normal_censored(x, left_censored, p, ...)
  if (isTRUE(n >= p)) {
    return("norm_cen")
  }
  if (isTRUE(n < p)) {
    ln <- .is_lognormal_censored(x, left_censored, p, ...)
    if (isTRUE(ln >= p)) {
      return("lnorm_cen")
    } else {
      return("none")
    }
  } 
}