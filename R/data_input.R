#' Connect to MANAGES database
#' 
#' This function connects to the MANAGES database for the path supplied.
#' R must be in 32-bit mode. 
#' 
#' @param manages_path Path to MANAGES Site.mdb file
#' @export

connect_manages <- function(manages_path) {
  
  manages_conn <- RODBC::odbcDriverConnect(
    paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)}",
                    ";DBQ=", manages_path)
    )
  
  manages_query <- paste0("SELECT sample_results.location_id, ",
                "sample_results.lab_id, ", 
                "sample_results.sample_date, ",
                "sample_results.lt_measure, ",
                "sample_results.analysis_result, ", 
                "sample_results.detection_limit, ",
                "sample_results.PQL, ",
                "site_parameters.default_unit, ", 
                "site_parameters.param_name, ",
                "site_parameters.short_name ",
                "FROM ", 
                "sample_results LEFT JOIN site_parameters ON ", 
                "sample_results.storet_code = site_parameters.storet_code")
  
  data <- RODBC::sqlQuery(channel = manages_conn, query = manages_query)

  close(manages_conn)
  
  return(data)
}

#' function to connect to MANAGES database and read spatial data
#' 
#' @param manages_path path to MANAGES database
#' @export
connect_manages_spatial <- function(manages_path){
  
  manages_conn <- RODBC::odbcDriverConnect(
    paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)}",
           ";DBQ=", manages_path)
  )
  
  manages_query <- paste0("SELECT * FROM locations")
  
  sp_data <- RODBC::sqlQuery(manages_conn, manages_query)
  
  close(manages_conn)
  
  return(sp_data)
}

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

#' function to read data in csv format and convert date to POSIXct with lubridate
#' 
#' @param path path to the csv file of groundwater data in the format 
#' with column names
#' location_id, param_name, default_unit, lt_measure, analysis_result
#' @param date_format date format as either mdy, or ymd passed to lubridate
#' @export
from_csv <- function(path, date_format = "mdy", ...){
  csv_data <- readr::read_csv(path, ...)
  if (date_format == "ymd") {
    csv_data$sample_date <- lubridate::ymd(csv_data$sample_date, 
                                           tz = Sys.timezone())
  }
  if (date_format == "mdy") {
    csv_data$sample_date <- lubridate::mdy(csv_data$sample_date,
                                           tz = Sys.timezone())
  }
  csv_data$analysis_result <- as.numeric(csv_data$analysis_result)
  csv_data$lt_measure <- factor(csv_data$lt_measure, exclude = NULL)
  csv_data$param_name <- as.character(csv_data$param_name)
  csv_data$default_unit <- as.character(csv_data$default_unit)
  return(csv_data)
}

#' function to read data in excel format 
#' 
#' @param path path to the excel  file of groundwater data in the format
#'  with column names
#' location_id, param_name, default_unit, lt_measure, analysis_result
#' @param sheet sheet name in spreadhsheet
#' @export
from_excel <- function(path, sheet = "Sheet1"){
  
  excel_data <- readxl::read_excel(path, sheet = sheet)
  
  excel_data$analysis_result <- as.numeric(excel_data$analysis_result)
  excel_data$lt_measure <- factor(excel_data$lt_measure, exclude = NULL)
  excel_data$param_name <- as.character(excel_data$param_name)
  excel_data$default_unit <- as.character(excel_data$default_unit)
  
  return(excel_data)
}
