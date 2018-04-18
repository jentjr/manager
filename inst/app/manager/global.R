source("selectData.R")
source("selectDataUI.R")

library(EnvStats)
library(manager)
library(shiny)
library(DBI)
library(pool)
library(sf)
library(mapview)
library(ggdendro)
library(factoextra)
library(dendextend)

# change options to handle large file size
options(shiny.maxRequestSize = -1)
# force numbers to be decimal instead of scientific
options(scipen = 6, digits = 8)

# Configure MANAGES 4 database--------------------------------------------------
dw <- config::get("datawarehouse")

pool <- dbPool(
  drv = odbc::odbc(),
  Driver = dw$driver,
  Server = dw$server,
  Database = dw$database
)

sites <- pool %>% tbl("SITE")

site_list <- sites %>%
  select(NAME) %>%
  collect() %>%
  first()

locations <- pool %>% tbl("LOCATIONS")

location_list <- locations %>%
  select(LOCATION_ID) %>%
  collect() %>%
  first()

site_parameters <- pool %>% tbl("SITE_PARAMETERS")

constituent_list <- site_parameters %>%
  select(PARAM_NAME) %>%
  collect() %>%
  first()

sample_results <- pool %>% tbl("SAMPLE_RESULTS")

query <- sample_results %>%
  left_join(site_parameters, by = c("SITE_ID", "STORET_CODE")) %>%
  left_join(locations, by = c("SITE_ID", "LOCATION_ID")) %>%
  left_join(sites, by = "SITE_ID") %>%
  select(SITE_ID,
         NAME,
         LAB_ID,
         LOCATION_ID,
         PARAM_NAME,
         SAMPLE_DATE,
         LT_MEASURE,
         FLAGS,
         ANALYSIS_RESULT,
         DETECTION_LIMIT,
         RL,
         DEFAULT_UNIT,
         SHORT_NAME,
         NORTH_COORDINATE,
         EAST_COORDINATE,
         COORDINATE_REFERENCE,
         WATER_CLASS,
         LOCATION_CLASS,
         WELL_TYPE
  ) %>%
  select_all(., tolower)

onStop(function() {
  poolClose(pool)
})

# Map data
wells <- st_read("H:/INTERNAL/JRJ/GIS/AEP/aep_monitoring_wells/aep_monitoring_wells.shp")