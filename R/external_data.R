#' Connect to a MANAGES 3.x Site.mdb database
#'
#' This function connects to the MANAGES database for the path supplied.
#' R must be in 32-bit mode.
#'
#' @param manages3_path Path to MANAGES Site.mdb file
#'
#' @export

connect_manages3 <- function(manages3_path) {
  if (!file.exists(manages3_path)) {
    stop(
      "File does not exist: ",
      manages3_path,
      call. = FALSE
    )
  }

  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = paste0(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)}",
      ";DBQ=",
      manages3_path
    )
  )
  return(con)
}

#' Function return a MANAGES 3.x Site.mdb database in memory
#'
#' This function connects to the MANAGES database for the path supplied, and
#' returns data in memory. R must be in 32-bit mode.
#'
#' @param manages3_path Path to MANAGES Site.mdb file
#'
#' @export

read_manages3 <- function(manages3_path) {
  con <- connect_manages3(manages3_path)

  on.exit(DBI::dbDisconnect(con), add = TRUE)

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

  data <- tibble::as_tibble(data)

  return(data)
}


#' Function to connect to MANAGES 4.0 database
#'
#' @param driver default is "SQL Server"
#' @param server server name
#' @param database database name
#'
#' @export

connect_manages4 <- function(driver = "SQL Server", server, database) {
  con <- DBI::dbConnect(
    odbc::odbc(),
    driver = driver,
    server = server,
    database = database
  )
  return(con)
}

#' Function to read MANAGES 4.0 database
#'
#' @param driver default is "SQL Server"
#' @param server server name
#' @param database database name
#' @param site_names Character vector of site names
#'
#' @export

read_manages4 <- function(driver = "SQL Server", server, database, site_names) {
  if (missing(site_names) || length(site_names) == 0) {
    stop(
      "`site_names` must contain at least one site name.",
      call. = FALSE
    )
  }

  manages_conn <- connect_manages4(driver, server, database)

  on.exit(DBI::dbDisconnect(manages_conn), add = TRUE)

  sites <- dplyr::tbl(manages_conn, "SITE")
  locations <- dplyr::tbl(manages_conn, "LOCATIONS")
  site_parameters <- dplyr::tbl(manages_conn, "SITE_PARAMETERS")
  sample_results <- dplyr::tbl(manages_conn, "SAMPLE_RESULTS")

  sample_results %>%
    dplyr::left_join(site_parameters, by = c("SITE_ID", "STORET_CODE")) %>%
    dplyr::left_join(locations, by = c("SITE_ID", "LOCATION_ID")) %>%
    dplyr::left_join(sites, by = "SITE_ID") %>%
    dplyr::select(
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
      PQL,
      ACHIEVED_MDC,
      DEFAULT_UNIT,
      SHORT_NAME,
      NORTH_COORDINATE,
      EAST_COORDINATE,
      COORDINATE_REFERENCE,
      WATER_CLASS,
      LOCATION_CLASS,
      WELL_TYPE,
      DESCRIPTION
    ) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::filter(name %in% site_names) %>%
    dplyr::collect() %>%
    tibble::as_tibble()
}

#' function to read data in csv format and convert date to POSIXct with lubridate
#'
#' @param path path to the csv file of groundwater data in the format
#' with column names
#' location_id, param_name, default_unit, lt_measure, analysis_result
#' @param date_format date format as either mdy, or ymd passed to lubridate
#'
#' @export

from_csv <- function(path, date_format = c("mdy", "ymd")) {
  if (!file.exists(path)) {
    stop(
      "File does not exist: ",
      path,
      call. = FALSE
    )
  }

  date_format <- match.arg(date_format)

  format_string <- switch(
    date_format,
    mdy = "%m/%d/%Y",
    ymd = "%Y/%m/%d"
  )

  readr::read_csv(
    path,
    col_types = readr::cols(
      analysis_result = readr::col_double(),
      sample_date = readr::col_date(format = format_string),
      report_date = readr::col_date(format = format_string)
    )
  )
}

#' function to read data in excel format
#'
#' @param path path to the excel  file of groundwater data in the format
#'  with column names
#' location_id, param_name, default_unit, lt_measure, analysis_result
#' @param sheet sheet name in spreadsheet
#'
#' @export

from_excel <- function(path, sheet = "Sheet1") {
  if (!file.exists(path)) {
    stop(
      "File does not exist: ",
      path,
      call. = FALSE
    )
  }
  excel_data <- readxl::read_excel(path, sheet = sheet)

  excel_data <- excel_data %>%
    dplyr::mutate(
      analysis_result = as.numeric(analysis_result),
      lt_measure = factor(lt_measure, exclude = NULL),
      param_name = as.character(param_name),
      default_unit = as.character(default_unit)
    )

  return(excel_data)
}
