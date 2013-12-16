#' calculate the percentage of non-detects
#' 
#' @param df data frame of groundwater monitoring data in long format
#' @param lt column of non-detects. Default is set to lt_measure which has 
#' the "<" symbol.


percent_lt <- function(lt) {
  yes <- length(lt[lt == "<"])
  total <- length(lt)
  p <- (yes / total) * 100
  return(p)
}


#' function to remove duplicate well names
#'
#'@param df data frame of groundwater data in long format with location_id as the column 
#' name for monitoring wells
#' 

remove_dup <- function(df){
  df_nodup <- df[-grep("*Dup", df$location_id), ]
  return(df_nodup)
}


#' Function to summarize the number of samples, mean, sd, and percentage of 
#' non-detects. This is useful for calculating the upper prediction limit.
#' 
#' @param df data frame of groundwater monitoring network data 

groundwater_summary <- function(df){
  
  gw <- ddply(df, .(location_id, param_name, default_unit), summarise, 
              n = length(analysis_result),
              mean = round(mean(analysis_result), digits = 3), 
              sd = round(sd(analysis_result), digits = 3),
              percent_lt = round(percent_lt(lt_measure), digits = 3))
  
  return(gw)
  
}


