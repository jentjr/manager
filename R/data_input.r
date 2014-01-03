#' Connect to MANAGES database
#' 
#' This function connects to the MANAGES database for the path supplied.
#' R must be in 32-bit mode. 
#' 
#' @param manages_path Path to MANAGES Site.mdb file
#' @export

connect_manages <- function(manages_path) {
  
  manages_conn <- RODBC::odbcConnectAccess(manages_path)
  
  data <- RODBC::sqlQuery(manages_conn, paste("SELECT sample_results.location_id, sample_results.sample_date, sample_results.analysis_result, sample_results.lt_measure, site_parameters.default_unit, site_parameters.param_name, site_parameters.short_name FROM sample_results LEFT JOIN site_parameters ON sample_results.storet_code = site_parameters.storet_code"))

  close(manages_conn)
  
  return(data)
}

#' function to connect to MANAGES database and read spatial data
#' 
#' @param manages_path path to MANAGES database
#' @export
connect_manages_spatial <- function(manages_path){
  
  manages_conn <- RODBC::odbcConnectAccess(manages_path)
  
  sp_data <- RODBC::sqlQuery(manages_conn, paste("SELECT location_id, description, long_pos, lat_pos, install_date, depth_top, depth_bottom, well_top, well_bottom FROM locations"))
  
  close(manages_conn)
  
  return(sp_data)
}

#' Function to return a list of all the location IDs
#' 
#' @param df data frame of groundwater data in the format with column names
#' location_id, param_name, default_unit, lt_measure, analysis_result
#' @export
get_well_names <- function(df){
  wells <- unique(df$location_id)
  return(wells)
}

#' Function to return a list of all the analytes
#' 
#' @param df data frame of groundwater data in the format with column names
#' location_id, param_name, default_unit, lt_measure, analysis_result
#' @export
get_analytes <- function(df){
  analytes <- unique(df$param_name)
  return(analytes)
}

#' function to read data in csv format and convert date to POSIXct with lubridate
#' 
#' @param path path to the csv file of groundwater data in the format with column names
#' location_id, param_name, default_unit, lt_measure, analysis_result
#' @export
from_csv <- function(path){
  csv_data <- read.csv(path, header = TRUE)
  csv_data$sample_date <- lubridate::mdy(csv_data$sample_date)
  csv_data$analysis_result <- as.numeric(csv_data$analysis_result)
  csv_data$lt_measure <- factor(csv_data$lt_measure, exclude = NULL)
  return(csv_data)
}

#' function to read data in excel format 
#' 
#' @param path path to the csv file of groundwater data in the format with column names
#' location_id, param_name, default_unit, lt_measure, analysis_result
#' @export
from_excel <- function(path){
  excel_data <- XLConnect::readWorksheet(XLConnect::loadWorkbook(path), sheet = "Sheet1",
                                         forceConversion = TRUE, dateTimeFormat = "%Y-%m-%d %H:%M:%S")
  excel_data$analysis_result <- as.numeric(excel_data$analysis_result)
  excel_data$lt_measure <- factor(excel_data$lt_measure, exclude = NULL)
  excel_data$param_name <- as.factor(excel_data$param_name)
  return(excel_data)
}