#' Function to check if groundwater data has a Normal distribution
#' 
#' @param df data frame of groundwater data
#' @export

is_normal <- function(df) {
  gof <- gofTest(df$analysis_result, dist = "norm")
  gof["data.name"] <- paste(df$location_id[1], df$param_name[1], sep = " ")
  p <- gof$p.value
  return(p)
}

#' Function to check if groundwater data has a Lognormal distribution
#' 
#' @param df data frame of groundwater data
#' @export

is_lognormal <- function(df) {
  lgof <- gofTest(df$analysis_result, dist = "lnorm")
  lgof["data.name"] <- paste(df$location_id[1], df$param_name[1], sep = " ")
  p <- lgof$p.value
  return(p)
}

#' Function to return distribution based on using p-value 
#' @param df
#' @export

dist <- function(df) {
  n <- is_normal(df)
  if (isTRUE(n >= 0.01)) {
    print("norm")
  }
  if (isTRUE(n < 0.01)) {
    ln <- is_lognormal(df)
    if (isTRUE(ln >= 0.01)) {
      print("lnorm")
    }
  } else {
    print("none")
  }
}

#' Function to return either normal, lognormal, or non-parametric multiple 
#' groundwater data locations and parameters.
#' @param df groundwater data frame
#' @export

est_dist <- function(df) {  
  dist_result <- df %>%
    group_by(location_id, param_name, default_unit) %>%
    do(distribution = dist(.))
  return(dist_result)
}