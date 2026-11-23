source("selectData.R")
source("selectDataUI.R")

suppressPackageStartupMessages(library(EnvStats))
suppressPackageStartupMessages(library(manager))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(DBI))
suppressPackageStartupMessages(library(pool))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(mapview))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(factoextra))
suppressPackageStartupMessages(library(dendextend))

options(shiny.maxRequestSize = 100 * 1024^2) # 100 MB
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

sites <- pool |> tbl("SITE")
locations <- pool |> tbl("LOCATIONS")
site_parameters <- pool |> tbl("SITE_PARAMETERS")
sample_results <- pool |> tbl("SAMPLE_RESULTS")


site_list <- sites |>
  distinct(NAME) |>
  pull(NAME) |>
  sort()

location_list <- locations |>
  select(LOCATION_ID) |>
  collect()

constituent_list <- site_parameters |>
  select(PARAM_NAME) |>
  collect()


query <- sample_results |>
  left_join(site_parameters, by = c("SITE_ID", "STORET_CODE")) |>
  left_join(locations, by = c("SITE_ID", "LOCATION_ID")) |>
  left_join(sites, by = "SITE_ID") |>
  select(
    SITE_ID,
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
    ACHIEVED_MDC,
    DEFAULT_UNIT,
    SHORT_NAME,
    NORTH_COORDINATE,
    EAST_COORDINATE,
    COORDINATE_REFERENCE,
    WATER_CLASS,
    LOCATION_CLASS,
    WELL_TYPE
  ) |>
  rename_with(tolower)

onStop(function() {
  poolClose(pool)
})

# Map data
# wells <- st_read("H:/INTERNAL/JRJ/GIS/AEP/aep_monitoring_wells/aep_monitoring_wells.shp")
