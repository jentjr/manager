#' Import data into env_schema
#'
#' Internal helper used by all importers.
#'
#' @keywords internal
.import_env_data <- function(
  data,
  ...,
  qualifier_table = NULL,
  analyte_lookup = analyte_tbl
) {
  env_schema(
    df = data,
    ...,
    qualifier_table = qualifier_table,
    analyte_lookup = analyte_lookup
  )
}

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

#' Read a MANAGES 3.x database
#'
#' @param manages3_path Path to MANAGES Site.mdb
#' @param analyte_lookup Optional analyte lookup.
#'
#' @returns env_schema
#'
#' @export
read_manages3 <- function(
  manages3_path,
  analyte_lookup = analyte_tbl
) {
  con <- connect_manages3(
    manages3_path
  )

  on.exit(
    DBI::dbDisconnect(con),
    add = TRUE
  )

  query <- paste0(
    "SELECT ",
    "sample_results.location_id, ",
    "sample_results.lab_id, ",
    "sample_results.sample_date, ",
    "sample_results.lt_measure, ",
    "sample_results.analysis_result, ",
    "sample_results.detection_limit, ",
    "sample_results.PQL, ",
    "site_parameters.default_unit, ",
    "site_parameters.param_name, ",
    "site_parameters.short_name ",
    "FROM sample_results ",
    "LEFT JOIN site_parameters ON ",
    "sample_results.storet_code = ",
    "site_parameters.storet_code"
  )

  data <- DBI::dbGetQuery(
    con,
    query
  ) |>
    tibble::as_tibble()

  data <- data |>
    dplyr::mutate(
      reporting_flag = .resolve_reporting_flag(
        flags = NULL,
        lt_measure = lt_measure
      )
    )

  .import_env_data(
    data = data,
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    sample_id = lab_id,
    reporting_flag = reporting_flag,
    detection_limit = detection_limit,
    quantitation_limit = PQL,
    analyte_lookup = analyte_lookup
  )
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
read_manages4 <- function(
  driver = "SQL Server",
  server,
  database,
  site_names,
  analyte_lookup = analyte_tbl
) {
  if (
    missing(site_names) ||
      length(site_names) == 0
  ) {
    stop(
      "`site_names` must contain at least one site name.",
      call. = FALSE
    )
  }

  manages_conn <- connect_manages4(
    driver,
    server,
    database
  )

  on.exit(
    DBI::dbDisconnect(manages_conn),
    add = TRUE
  )

  sites <- dplyr::tbl(
    manages_conn,
    "SITE"
  )

  locations <- dplyr::tbl(
    manages_conn,
    "LOCATIONS"
  )

  site_parameters <- dplyr::tbl(
    manages_conn,
    "SITE_PARAMETERS"
  )

  sample_results <- dplyr::tbl(
    manages_conn,
    "SAMPLE_RESULTS"
  )

  raw_data <-
    sample_results |>
    dplyr::left_join(
      site_parameters,
      by = c(
        "SITE_ID",
        "STORET_CODE"
      )
    ) |>
    dplyr::left_join(
      locations,
      by = c(
        "SITE_ID",
        "LOCATION_ID"
      )
    ) |>
    dplyr::left_join(
      sites,
      by = "SITE_ID"
    ) |>
    dplyr::filter(
      NAME %in% site_names
    ) |>
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
    ) |>
    dplyr::rename_with(
      tolower
    ) |>
    dplyr::collect() |>
    tibble::as_tibble()

  raw_data <- raw_data |>
    dplyr::mutate(
      reporting_flag = .resolve_reporting_flag(
        flags,
        lt_measure
      )
    )

  out <- .import_env_data(
    data = raw_data,
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    sample_id = lab_id,
    reporting_flag = reporting_flag,

    reporting_limit = rl,
    detection_limit = detection_limit,
    quantitation_limit = pql,

    achieved_mdc = achieved_mdc,

    analyte_lookup = analyte_lookup
  ) |>
    dplyr::mutate(
      .site_id = site_id,
      .site_name = name,

      .latitude = north_coordinate,
      .longitude = east_coordinate,
      .coordinate_system = coordinate_reference,

      .water_class = water_class,
      .location_class = location_class,
      .well_type = well_type,
      .location_description = description,

      .source_flag = flags,
      .source_lt_measure = lt_measure
    )

  attr(out, "source_metadata") <- c(
    ".site_id",
    ".site_name",
    ".latitude",
    ".longitude",
    ".coordinate_system",
    ".water_class",
    ".location_class",
    ".well_type",
    ".location_description",
    ".source_flag",
    ".source_lt_measure"
  )

  validate_env_schema(out)

  out
}

#' Import environmental data from CSV
#'
#' Reads a CSV file and converts it directly to an
#' `env_schema` object.
#'
#' Column mappings use tidy evaluation and may reference
#' any column names present in the source file.
#'
#' @param path Path to csv file.
#' @param location Location identifier column.
#' @param date Sample date column.
#' @param analyte Analyte column.
#' @param result Result column.
#' @param unit Unit column.
#' @param sample_id Sample identifier column.
#' @param reporting_flag Reporting qualifier column.
#' @param reporting_limit Reporting limit column.
#' @param detection_limit Detection limit column.
#' @param quantitation_limit Quantitation limit column
#'   (PQL, LOQ, SQL, etc.).
#' @param achieved_mdc Achieved minimum detectable concentration
#'   (radiochemistry).
#' @param measurement_uncertainty Measurement uncertainty column.
#' @param method Analytical method column.
#' @param analysis_date Analysis date column.
#' @param matrix Matrix column.
#' @param lab_name Laboratory name column.
#' @param metadata Additional metadata columns.
#' @param date_format Date format.
#' @param qualifier_table Optional qualifier lookup.
#' @param analyte_lookup Optional analyte lookup.
#'
#' @returns env_schema
#'
#' @export
from_csv <- function(
  path,
  location,
  date,
  analyte,
  result,
  unit = NULL,
  sample_id = NULL,
  reporting_flag = NULL,
  reporting_limit = NULL,
  detection_limit = NULL,
  quantitation_limit = NULL,
  achieved_mdc = NULL,
  measurement_uncertainty = NULL,
  method = NULL,
  analysis_date = NULL,
  matrix = NULL,
  lab_name = NULL,
  metadata = NULL,
  date_format = c(
    "guess",
    "mdy",
    "dmy",
    "ymd"
  ),
  qualifier_table = NULL,
  analyte_lookup = analyte_tbl
) {
  if (!file.exists(path)) {
    stop(
      "File does not exist: ",
      path,
      call. = FALSE
    )
  }

  date_format <- match.arg(
    date_format
  )

  dat <- readr::read_csv(
    path,
    show_col_types = FALSE
  )

  if (date_format != "guess") {
    date_col <- rlang::as_name(
      rlang::ensym(date)
    )

    parse_fun <- switch(
      date_format,
      mdy = lubridate::mdy,
      dmy = lubridate::dmy,
      ymd = lubridate::ymd
    )

    dat[[date_col]] <-
      parse_fun(
        dat[[date_col]]
      )
  }

  .import_env_data(
    data = dat,
    location = location,
    date = date,
    analyte = analyte,
    result = result,
    unit = unit,
    sample_id = sample_id,
    reporting_flag = reporting_flag,
    reporting_limit = reporting_limit,
    detection_limit = detection_limit,
    quantitation_limit = quantitation_limit,
    achieved_mdc = achieved_mdc,
    measurement_uncertainty = measurement_uncertainty,
    method = method,
    analysis_date = analysis_date,
    matrix = matrix,
    lab_name = lab_name,
    metadata = metadata,
    qualifier_table = qualifier_table,
    analyte_lookup = analyte_lookup
  )
}

#' Import environmental data from Excel
#'
#' Reads an Excel worksheet and converts it directly to an
#' `env_schema` object.
#'
#' @param path Path to workbook.
#' @param sheet Worksheet name.
#' @param location Location identifier column.
#' @param date Sample date column.
#' @param analyte Analyte column.
#' @param result Result column.
#' @param unit Unit column.
#' @param sample_id Sample identifier column.
#' @param reporting_flag Reporting qualifier column.
#' @param reporting_limit Reporting limit column.
#' @param detection_limit Detection limit column.
#' @param quantitation_limit Quantitation limit column
#'   (PQL, LOQ, SQL, etc.).
#' @param achieved_mdc Achieved minimum detectable concentration
#'   (radiochemistry).
#' @param measurement_uncertainty Measurement uncertainty column.
#' @param method Analytical method column.
#' @param analysis_date Analysis date column.
#' @param matrix Matrix column.
#' @param lab_name Laboratory name column.
#' @param metadata Additional metadata columns.
#' @param qualifier_table Optional qualifier lookup.
#' @param analyte_lookup Optional analyte lookup.
#'
#' @returns env_schema
#'
#' @export
from_excel <- function(
  path,
  sheet = 1,
  location,
  date,
  analyte,
  result,
  unit = NULL,
  sample_id = NULL,
  reporting_flag = NULL,
  reporting_limit = NULL,
  detection_limit = NULL,
  quantitation_limit = NULL,
  achieved_mdc = NULL,
  measurement_uncertainty = NULL,
  method = NULL,
  analysis_date = NULL,
  matrix = NULL,
  lab_name = NULL,
  metadata = NULL,
  qualifier_table = NULL,
  analyte_lookup = analyte_tbl
) {
  if (!file.exists(path)) {
    stop(
      "File does not exist: ",
      path,
      call. = FALSE
    )
  }

  dat <- readxl::read_excel(
    path,
    sheet = sheet
  )

  .import_env_data(
    data = dat,
    location = location,
    date = date,
    analyte = analyte,
    result = result,
    unit = unit,
    sample_id = sample_id,
    reporting_flag = reporting_flag,
    reporting_limit = reporting_limit,
    detection_limit = detection_limit,
    quantitation_limit = quantitation_limit,
    achieved_mdc = achieved_mdc,
    measurement_uncertainty = measurement_uncertainty,
    method = method,
    analysis_date = analysis_date,
    matrix = matrix,
    lab_name = lab_name,
    metadata = metadata,
    qualifier_table = qualifier_table,
    analyte_lookup = analyte_lookup
  )
}
