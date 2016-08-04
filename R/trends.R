#' Function to get slope and intercept from Theil-Sen slope for plotting
#' 
#' 

get_theilsen <- function(df, x = "analysis_result", y = "sample_date", ...) {
  
  x <- df[, x]
  y <- df[, y]
  
  kendall <- EnvStats::kendallTrendTest(x, y, ...)
  est <- kendall$estimate
  pv <- kendall$p.value
  
  return(c(est, pv))
}