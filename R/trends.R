#' Function to get slope and intercept from Theil-Sen slope for plotting
#' 
#' @param df dataframe of groundwater data in tidy format
#' @param x column for analysis result
#' @param y column for sample date
#' @param ... other arguements passed to \code{\link{kendallTrendTest}}
#' @export 

get_theilsen <- function(df, x = "analysis_result", y = "sample_date", ...) {
  
  x <- df[, x]
  y <- df[, y]
  
  kendall <- EnvStats::kendallTrendTest(x, y, ...)
  est <- kendall$estimate
  pv <- kendall$p.value
  
  return(c(est, pv))
}