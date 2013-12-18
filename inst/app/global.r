library(shiny)
library(ggplot2)
library(ggmap)
library(plyr)
library(RODBC)
library(googleVis)
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

