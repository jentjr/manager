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

#' Function to summarize the number of samples and percentage of 
#' non-detects. This is useful for calculating the upper prediction limit.
#' 
#' @param df data frame of groundwater monitoring network data 
#' @export

lt_summary <- function(df, bkgd_start, bkgd_end){
  
  df$sampling_period <- ifelse(df$sample_date >= bkgd_start & 
                               df$sample_date <= bkgd_end, "background", 
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
