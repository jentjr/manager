#' Connect to a MANAGES 3.x Site.mdb database
#' 
#' This function connects to the MANAGES database for the path supplied.
#' R must be in 32-bit mode. 
#' 
#' @param manages3_path manages_path Path to MANAGES Site.mdb file
#' @export

connect_manages3 <- function(manages3_path) {
  
  con <- DBI::dbConnect(odbc::odbc(), 
          .connection_string = paste0(
            "Driver={Microsoft Access Driver (*.mdb, *.accdb)}",
            ";DBQ=", manages3_path)
    )
}

#' Function return a MANAGES 3.x Site.mdb database in memory
#' 
#' This function connects to the MANAGES database for the path supplied, and 
#' returns data in memory. R must be in 32-bit mode. 
#' 
#' @param manages3_path manages_path Path to MANAGES Site.mdb file
#' @export

read_manages3 <- function(manages3_path) {
  
  con <- connect_manages3(manages3_path)
  
  query <- paste0(
    "SELECT sample_results.location_id, ",
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
    "sample_results.storet_code = site_parameters.storet_code"
  )
  
  data <- DBI::dbGetQuery(con, query)
  
  DBI::dbDisconnect(con)
  
  return(data)
  
}


#' Function to connect to MANAGES 4.0 database
#' 
#' @param server server name
#' @param database database name
#' @export

connect_manages4 <- function(driver = "SQL Server", server, database) {
  
  con <- DBI::dbConnect(odbc::odbc(), 
                   driver = driver, 
                   server = server, 
                   database = database)
  
}

#' function to read data in csv format and convert date to POSIXct with lubridate
#' 
#' @param path path to the csv file of groundwater data in the format 
#' with column names
#' location_id, param_name, default_unit, lt_measure, analysis_result
#' @param date_format date format as either mdy, or ymd passed to lubridate
#' @export

from_csv <- function(path, date_format = "mdy"){
  
  if (date_format == "ymd") {
    csv_data <- readr::read_csv(path, 
                                col_types = readr::cols(
                                  analysis_result = readr::col_double(),
                                  sample_date = readr::col_datetime(format = "%Y/%m/%d")
                                ))
  }
  if (date_format == "mdy") {
    csv_data <- readr::read_csv(path,
                                col_types = readr::cols(
                                  analysis_result = readr::col_double(),
                                  sample_date = readr::col_datetime(format = "%m/%d/%Y")
                                ))
  }
  return(csv_data)
}

#' function to read data in excel format 
#' 
#' @param path path to the excel  file of groundwater data in the format
#'  with column names
#' location_id, param_name, default_unit, lt_measure, analysis_result
#' @param sheet sheet name in spreadsheet
#' @export

from_excel <- function(path, sheet = "Sheet1"){
  
  excel_data <- readxl::read_excel(path, sheet = sheet)
  
  excel_data$analysis_result <- as.numeric(excel_data$analysis_result)
  excel_data$lt_measure <- factor(excel_data$lt_measure, exclude = NULL)
  excel_data$param_name <- as.character(excel_data$param_name)
  excel_data$default_unit <- as.character(excel_data$default_unit)
  
  return(excel_data)
}