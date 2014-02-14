#' calculate the percentage of non-detects
#' 
#' @param df data frame of groundwater monitoring data in long format
#' @param lt column of non-detects. Default is set to lt_measure which has 
#' the "<" symbol.
#' @export


percent_lt <- function(lt) {
  yes <- length(lt[lt == "<"])
  total <- length(lt)
  p <- (yes / total) * 100
  return(p)
}


#' function to remove duplicate samples
#' Example: If you have wells named MW-1 and another named MW-1 Duplicate
#' this function will remove the MW-1 Duplicate sample
#'
#'@param df data frame of groundwater data in long format with location_id as the column 
#' name for monitoring wells
#' @export

remove_dup <- function(df){
  df_nodup <- df[-grep("*Dup", df$location_id), ]
  return(df_nodup)
}

#' Calculate intrawell prediction limit
#'
#' @param df data frame of groundwater monitoring data
#' @param back_datas vector of background dates
#' @param comp_dates vector of compliance dates
#' @param num_wells number of wells
#' @param num_params number of parameters 
#' @param m type of 1-of-m retesting scheme (usually m= 1, 2, 3, or 4)
#' @param swfpr site-wide-false-positive-rate, default is 0.05
#' @param ne number of yearly evaluations (4 = quarterly, 2 = semi-anually, 1 = annually)
#' @param ord order of the mean to be predicted (for tests on observations, set ord = 1)
#' @export 

intrawell_prediction <- function(df, back_dates, comp_dates, num_wells, num_params, m, 
                                 swfpr = 0.05, ne = 2, ord = 1){
  back_data <- subset(df, sample_date >= back_dates[1] & sample_date <= back_dates[2])
  back_mean <- mean(df$analysis_result, na.rm = TRUE)
  back_sd <- sd(df$analysis_result, na.rm = TRUE)
  
  n_back <- nrow(back_data)
  
  kappa <- calc_kappa(n = n_back, w = num_wells, coc = num_params, m = m, 
                      swfpr = swfpr, ne = ne, ord = ord)$kappa
  
  upl <- back_mean + kappa * sqrt(1 + 1 / n_back) * back_sd
    
  return(upl)

}


#' Function to summarize the number of samples, mean, sd, and percentage of 
#' non-detects. This is useful for calculating the upper prediction limit.
#' 
#' @param df data frame of groundwater monitoring network data 
#' @export

groundwater_summary <- function(df){
  
  gw <- plyr::ddply(df, .(location_id, param_name, default_unit), summarise, 
              n = length(analysis_result),
              mean = round(mean(analysis_result), digits = 3), 
              sd = round(sd(analysis_result), digits = 3),
              percent_lt = round(percent_lt(lt_measure), digits = 3))
  
  return(gw)
}

#' Function to export data from manages to different formats in excel
#' 
#' @param df data frame
#' @param file full file path name with extension for export
#' @export

export_manages <- function(df, file){
  
  wells <- unique(df$location_id)
  
  wb <- XLConnect::loadWorkbook(file, create=TRUE)
  
  for (i in 1:length(wells)){
    temp <- subset(df, location_id == wells[i])
    temp <- temp[order(df$param_name, df$sample_date),]
    XLConnect::createSheet(wb, name = paste(wells[i]))
    XLConnect::writeWorksheet(wb, temp, sheet = paste(wells[i]), 
                              startRow = 1, startCol = 1, header = TRUE )
  }
  XLConnect::saveWorkbook(wb)
}