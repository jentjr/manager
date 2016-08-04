#' calculate the percentage of left censored data
#' 
#' @param df data frame of groundwater monitoring data in long format
#' @param lt column of data less than detection limit.
#' @export

percent_lt <- function(lt) {
  yes <- length(lt[lt == "<"])
  total <- length(lt)
  p <- (yes/total)*100
  return(p)
}

#' calculate the percentage of right censored data
#' 
#' @param df data frame of groundwater monitoring data in long format
#' @param gt column of data greater than detection limit. 
#' @export

percent_gt <- function(gt) {
  yes <- length(gt[gt == ">"])
  total <- length(gt)
  p <- (yes/total)*100
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

replace_missing <- function(df){
  df$analysis_result <- ifelse(df$analysis_result == -999.9, NA, 
                                 df$analysis_result)
  return(df)
}

#' Function to convert gwdata frame to censored data frame
#' @param df data frame of groundwater data
#' @export

to_censored <- function(df) {
  
  df <- df %>%
    group_by(location_id, param_name, default_unit) %>%
    mutate(
      left_censored = ifelse(lt_measure == "<", TRUE, FALSE),
      right_censored = ifelse(lt_measure == ">", TRUE, FALSE)
    )
  
  df <- as.data.frame(df)
  
  return(df)
}


#' Function to summarize the number of samples and percentage of 
#' non-detects. This is useful for calculating the upper prediction limit.
#' 
#' @param df data frame of groundwater monitoring network data 
#' @param start_date beginning of time period to be evaluated
#' @param end_date end of time period to be evaluated
#' @export

lt_summary <- function(df, start_date, end_date){
  
  df$sampling_period <- ifelse(df$sample_date >= start_date & 
                               df$sample_date <= end_date, "background", 
                               "compliance")
  detection <- dplyr::group_by(df, location_id, param_name, default_unit,
                               sampling_period)
  
  lt <- dplyr::summarise(detection,
                  count = n(),
                  percent_lt = round(percent_lt(lt_measure), digits = 2))

  lt <- as.data.frame(lt)
  return(lt)
}

#' Function to export data from manages to excel format required by OEPA
#' 
#' @param df data frame
#' @param wells list of wells to be exported
#' @param constituents list of constituents to be exported
#' @param file full file path name with extension for export
#' @export

export_OEPA <- function(df, wells, constituents, file, ...){
  
  df <- df %>% 
    dplyr::filter(param_name %in% constituents, location_id %in% wells)
  
  join_lt <- function() {
    paste(df$lt_measure, df$analysis_result, sep = " ")
  }
  
  df$result <- ifelse(df$lt_measure == "<" | df$lt_measure == ">", 
                      join_lt(), df$analysis_result)
 
  df$param_unit <- paste0(df$param_name, " (",df$default_unit, ")")
  
  df <- df %>%
    dplyr::select(lab_id, location_id, sample_date, param_unit, result) %>%
    tidyr::spread(param_unit, result)

  wb <- openxlsx::createWorkbook()
  
  oepa_cast <- function(x){
    openxlsx::addWorksheet(wb, paste(x$location_id[1]))
    openxlsx::writeData(wb, x, sheet = paste(x$location_id[1]),
                        startRow = 1, startCol = 1, rowNames = FALSE)
  }
  
  plyr::d_ply(df, .(location_id), oepa_cast)

  openxlsx::saveWorkbook(wb, file = file, ...)
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
