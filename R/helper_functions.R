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

gw_summary <- function(df, bkgd_start, bkgd_end){
  
  df$sampling_period <- ifelse(df$sample_date >= bkgd_start & 
                               df$sample_date <= bkgd_end, "background", 
                               "compliance")
  detection <- dplyr::group_by(df, location_id, param_name, default_unit,
                               sampling_period)

  gw <- dplyr::summarise(detection,
                  count = n(),
                  mean = round(mean(analysis_result, na.rm = TRUE), digits = 3),
                  sd = round(sd(analysis_result, na.rm = TRUE), digits = 3),
                  percent_lt = round(percent_lt(lt_measure), digits = 2))

  gw <- as.data.frame(gw)
  gw
}

#' Function to export data from manages to excel format required by OEPA
#' 
#' @param df data frame
#' @param wells list of wells to be exported
#' @param constituents list of constituents to be exported
#' @param file full file path name with extension for export
#' @export

export_OEPA <- function(df, wells, constituents, file, plant, 
                        export_date = date()){
  
  # create a data frame from plant name and date in order to paste into excel
  h <- data.frame(Facility = plant, Date = export_date)
  
  df <- df[df$location_id %in% wells &
           df$param_name %in% constituents, ]
  
  join_lt <- function() {
    paste(df$lt_measure, df$analysis_result, sep = " ")
  }
  
  df$result <- ifelse(df$lt_measure == "<" | df$lt_measure == ">", 
                      join_lt(), df$analysis_result)
  
  id <- dplyr::group_by(df, location_id, sample_date, param_name)
  id <- dplyr::mutate(id, group = n())
  
  wb <- XLConnect::loadWorkbook(file, create=TRUE)
  
  for (i in 1:length(wells)){
    temp <- id[id$location_id == wells[i], ]
    temp <- reshape2::dcast(temp, value.var = "result", 
                            param_name + group + default_unit ~ sample_date, 
                            margins = FALSE)[-2]
    XLConnect::createSheet(wb, name = paste(wells[i]))
    XLConnect::writeWorksheet(wb, temp, sheet=paste(wells[i]), 
                              startRow = 4, startCol = 1, header = TRUE,
                              rownames = FALSE)
    XLConnect::writeWorksheet(wb, h, sheet = paste(wells[i]), 
                              startRow = 1, startCol = 1, header = TRUE,
                              rownames = FALSE)
  }
  XLConnect::saveWorkbook(wb)
}

#' Function to export summary table for a sampling event
#' 
#' @param df data frame in the format location_id, sample_date, param_name,
#'  lt_measure, default_unit
#'  @param wells list of wells
#'  @param constituents list of constituents
#'  @param start start date
#'  @param end end date 
#' 

event_summary <- function(df, wells, constituents, start, end){
  df <- df[df$location_id %in% wells &
           df$param_name %in% constituents &
           df$sample_date >= start &
           df$sample_date <= end, ]
  
  df$param_name <- paste(df$param_name, " (", df$default_unit, ")", sep = "")
  
  join_lt <- function() {
    paste(df$lt_measure, df$analysis_result, sep = " ")
  }
  
  df$result <- ifelse(df$lt_measure == "<" | df$lt_measure == ">", 
                      join_lt(), df$analysis_result)
  
  id <- dplyr::group_by(df, location_id, sample_date, param_name)
  id <- dplyr::mutate(id, group = n())
    
  out <- reshape2::dcast(id, value.var = "result", 
                          group + location_id + sample_date ~ param_name, 
                          margins = FALSE)[-1]
  return(out)
}
