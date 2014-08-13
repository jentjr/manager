get_well_names <- function(df){
  wells <- unique(df$location_id)
  return(wells)
}

get_analytes <- function(df){
  analytes <- unique(df$param_name)
  return(analytes)
}

from_csv <- function(path){
  csv_data <- read.csv(path, header = TRUE)
  csv_data$sample_date <- lubridate::ymd(csv_data$sample_date)
  csv_data$analysis_result <- as.numeric(csv_data$analysis_result)
  csv_data$lt_measure <- factor(csv_data$lt_measure, exclude = NULL)
  return(csv_data)
}

from_excel <- function(path){
  excel_data <- XLConnect::readWorksheet(XLConnect::loadWorkbook(path), 
                                         sheet = "Sheet1",
                                         forceConversion = TRUE, 
                                         dateTimeFormat = "%Y-%m-%d %H:%M:%S")
  excel_data$analysis_result <- as.numeric(excel_data$analysis_result)
  excel_data$lt_measure <- factor(excel_data$lt_measure, exclude = NULL)
  excel_data$param_name <- as.factor(excel_data$param_name)
  return(excel_data)
}

connect_manages <- function(manages_path) {
  
  manages_conn <- RODBC::odbcConnectAccess(manages_path)
  
  data <- RODBC::sqlQuery(manages_conn, paste("SELECT sample_results.location_id, sample_results.sample_date, sample_results.analysis_result, sample_results.lt_measure, site_parameters.default_unit, site_parameters.param_name, site_parameters.short_name FROM sample_results LEFT JOIN site_parameters ON sample_results.storet_code = site_parameters.storet_code"))
  
  close(manages_conn)
  
  return(data)
}