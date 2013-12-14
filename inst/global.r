library(shiny)
library(ggplot2)
library(plyr)
library(RODBC)

# function to connect to MANAGES database
connect_manages <- function(manages_path) {
  
  manages_conn <- odbcConnectAccess(manages_path)
  
  data <- sqlQuery(manages_conn, paste("SELECT sample_results.location_id, sample_results.sample_date, sample_results.analysis_result, sample_results.lt_measure, site_parameters.default_unit, site_parameters.param_name FROM sample_results LEFT JOIN site_parameters ON sample_results.storet_code = site_parameters.storet_code"))
  
  close(manages_conn)
  
  return(data)
}

# global functions
getWellNames <- function(df){
  wells <- unique(df$location_id)
  return(wells)
}

getAnalytes <- function(df){
  analytes <- unique(df$param_name)
  return(analytes)
}

