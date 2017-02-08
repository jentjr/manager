#' Function to return a list of all the location IDs
#' 
#' @param df data frame of groundwater data in the format with column names
#' location_id, param_name, default_unit, lt_measure, analysis_result
#' @export
get_wells <- function(df){
  wells <- unique(df$location_id)
  return(wells)
}

#' Function to return a list of all the constituents
#' 
#' @param df data frame of groundwater data in the format with column names
#' location_id, param_name, default_unit, lt_measure, analysis_result
#' @export
get_constituents <- function(df){
  constituents <- unique(df$param_name)
  return(constituents)
}

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
#' @param df groundwater data frame
#' @export

remove_dup <- function(df){
  
  df_nodup <- df[-grep("*Dup", df$location_id), ]
  
  return(df_nodup)
  
}

#' function to include duplicate samples
#'
#' @param df groundwater data frame
#' @export

include_dup <- function(df, wells) {
  
  pattern <- paste(wells, "[:space:]*Dup")
  
  dups <- unique(df[grep(pattern, df$location_id), ]$location_id)
  
  dups <- as.character(droplevels(dups))
  
  wells <- append(wells, dups)
  
  df_dup <- df %>%
    filter(location_id %in% wells)
  
  df_dup <- droplevels(df_dup)
  
  return(df_dup)
  
}

#' function to replace missing values
#' @param df groundwater data frame
#' @export

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

#' Function to join columns of lt_measure and sample results
#' 
#' @param df groundwater data frame
#' @param col_name qouted column name for the result
#' @export

join_lt <- function(df, col_name) {
  
  .join_lt <- function() {
    paste(df$lt_measure, df$analysis_result, sep = " ")
  }
  
  df$result <- ifelse(df$lt_measure == "<" | df$lt_measure == ">", 
                        .join_lt(), df$analysis_result)
  
  names(df)[names(df) == "result"] <- col_name
  
  return(df)
  
}

#' Function to return a column of parameter name with units
#' @param df groundwater data frame
#' @param col_name quoted column name to return
#' @export

name_units <- function(df, col_name, short_name = TRUE) {
  
  if (short_name == TRUE) {
    
    df$param_unit <- paste0(df$short_name, " (", df$default_unit, ")")
    
  }
  
  else {
    
    df$param_unit <- paste0(df$param_name, " (", df$default_unit, ")")
    
  }
 
  names(df)[names(df) == "param_unit"] <- col_name
  
  return(df)

}