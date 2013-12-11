library(ggplot2)
library(plyr)
library(xts)
library(rCharts)
library(googleVis)

source("geomchemistry_plots.r")

# file input
# TODO: change to read MANAGES database, or read Dolan Lab data format excel files 
data <- read.csv("/Users/justinjent/Dropbox/groundwater_app/cardinal.csv", header=TRUE)
# data$result <- as.numeric(as.character(tc$result))
# data$location <- as.factor(tc$location)
data$date <- as.Date(data$date, format="%m/%d/%y")


# global functions
getWellNames <- function(){
  wells <- unique(data$location)
  return(wells)
}

well_names <- getWellNames()

getAnalytes <- function(){
  analytes <- unique(data$analyte)
  return(analytes)
}

analyte_names <- getAnalytes()

# return start and end date of series selected
start_date <- min(data$date)
end_date <- max(data$date)

# # find non-detects
# getNonDetects<- function(){
#   tc$lt == "<"
# }

# # function to compare the results to MCL and GWPS
# compare_MCL <- function(){
#   
#   MCL <- as.data.frame(matrix(c("Antimony, Sb", 0.0, 6, "ug/L",
#                                 "Arsenic, As", 0.0, 10, "ug/L",
#                                 "Barium, Ba", 0.0, 2000, "ug/L",
#                                 "Beryllium, Be", 0.0, 4, "ug/L",
#                                 "Cadmium, Cd", 0, 5, "ug/L",
#                                 "Chromium, Cr",0 ,100, "ug/L",
#                                 "Copper, Cu", 0, 1300, "ug/L",
#                                 "Lead, Pb", 0, 15, "ug/L",
#                                 "Mercury, Hg", 0, 2, "ug/L",
#                                 "Selenium, Se", 0, 50, "ug/L",
#                                 "Thallium, Tl", 0, 2, "ug/L",
#                                 "Uranium, U", 0, 30, "ug/L",
#                                 "Zinc, Zn", 0, 5000, "ug/L",
#                                 "Aluminum, Al", 0.05, 0.2, "mg/L",
#                                 "Iron, Fe", 0, 0.3, "mg/L",
#                                 "Manganese, Mn", 0, 0.05, "mg/L",
#                                 "Chloride, Cl", 0, 250, "mg/L",
#                                 "Fluoride, F", 0, 4, "mg/L",
#                                 "Nitrate, NO3 as N", 0, 10, "mg/L",
#                                 "Residue, Filterable, TDS", 0, 500, "mg/L",
#                                 "Sulfate, SO4", 0, 250, "mg/L",
#                                 "Nitrite, NO2 as N", 0, 1, "mg/L"),
#                               ncol=4, nrow=22, byrow=TRUE))
#   
#   colnames(MCL) <- c("analyte", "MCL_lower", "MCL_upper", "units")
#   # R created factor columns and they must be converted to numeric
#   # must use as.character to make sure as.numeric converts to number,
#   # and not an ordered number from the factor
#   MCL$MCL_upper <- as.numeric(as.character(MCL$MCL_upper))
#   MCL$MCL_lower <- as.numeric(as.character(MCL$MCL_lower))
#   
#   # create a new data frame joining the MCL with Dolan Lab 
#   out <- join(MCL, data, by="analyte", type="full")
#   
#   out$exceedance <- ifelse(out$result >= out$MCL_lower & out$result < out$MCL_upper, "no", "yes")
#   
#   # create a subset of the data to only show the constituent, well, and
#   # whether there was an exceedance.
#   out_f <- out[,c("analyte","location", "lt", "result", "units", "MCL_lower", "MCL_upper", "exceedance")]
#   
#   out_f <- out_f[order(out_f$exceedance, out_f$location, decreasing=TRUE),]
#   return(out_f)
# }

