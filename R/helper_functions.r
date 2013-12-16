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

#' Function to summarize the number of samples, mean, sd, and percentage of 
#' non-detects. This is useful for calculating the upper prediction limit.
#' 
#' @param df data frame of groundwater monitoring network data 

groundwater_summary <- function(df){
  
  gw <- ddply(df, .(location_id, param_name), summarise, 
        mean = mean(analysis_result), 
        sd = sd(analysis_result), 
        n = length(analysis_result),
        percent_lt = percent_lt(lt_measure))
  
  return(gw)
  
}


