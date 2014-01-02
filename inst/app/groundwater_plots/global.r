library(shiny)
library(ggplot2)
library(ggmap)
library(plyr)
library(lubridate)
library(RODBC)
library(googleVis)
library(XLConnect)
library(groundwater)

# global functions
getWellNames <- function(df){
  wells <- unique(df$location_id)
  return(wells)
}

getAnalytes <- function(df){
  analytes <- unique(df$param_name)
  return(analytes)
}

# function to read data in csv fromat and convert date to POSIXct with lubridate
from_csv <- function(path){
  csv_data <- read.csv(path, header = TRUE, stringsAsFactors = FALSE)
  csv_data$sample_date <- mdy(csv_data$sample_date)
  csv_data$analysis_result <- as.numeric(csv_data$analysis_result)
  csv_data$lt_measure <- as.factor(csv_data$lt_measure)
  return(csv_data)
}

from_excel <- function(path){
  excel_data <- readWorksheet(loadWorkbook(path), sheet = "Sheet1",
                              forceConversion = TRUE, dateTimeFormat = "%Y-%m-%d %H:%M:%S")
  excel_data$analysis_result <- as.numeric(excel_data$analysis_result)
  excel_data$lt_measure <- as.factor(excel_data$lt_measure)
  return(excel_data)
}