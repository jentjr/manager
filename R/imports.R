#' Connect to a MANAGES 3.x Site.mdb database
#'
#' This function connects to the MANAGES database for the path supplied.
#' R must be in 32-bit mode.
#'
#' @param manages3_path Path to MANAGES Site.mdb file
#'
#' @keywords internal

.connect_manages3 <- function(manages3_path) {
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
  con <- .connect_manages3(
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

  env_schema(
    df = data,
    location = location_id,
    sample_date = sample_date,
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
#' @keywords internal

.connect_manages4 <- function(driver = "SQL Server", server, database) {
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

  manages_conn <- .connect_manages4(
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

  out <- env_schema(
    df = raw_data,
    location = location_id,
    sample_date = sample_date,
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

  out |>
    set_source_metadata(
      c(
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
    )
}

#' Function to export data from manages to excel. Each sampling location
#' is saved to a tab.
#'
#' @param df groundwater data frame in tidy format
#' @param wells list of wells to be exported
#' @param constituents list of constituents to be exported
#' @param file full file path name with extension for export
#' @param short_name TRUE/FALSE to abbreviate constituent name
#' @param lab_id TRUE/FALSE to include the lab id
#' @param overwrite TRUE/FALSE overwrite existing spreadsheet if it exists
#'
#' @export

write_excel <- function(
  df,
  wells,
  constituents,
  file,
  short_name = TRUE,
  lab_id = FALSE,
  overwrite = FALSE
) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop(
      "openxlsx needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  if (!requireNamespace("plyr", quietly = TRUE)) {
    stop(
      "plyr needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  df <- df %>%
    dplyr::filter(param_name %in% constituents, location_id %in% wells)

  df <- join_lt(df)

  df <- df %>%
    name_units(short_name = short_name)

  if (lab_id == TRUE) {
    df <- df %>%
      dplyr::select(
        location_id,
        lab_id,
        sample_date,
        param_name,
        analysis_result
      ) %>%
      tidyr::spread(param_name, analysis_result) %>%
      arrange(location_id, sample_date) %>%
      rename(
        `Location ID` = location_id,
        `Lab ID` = lab_id,
        `Sample Date` = sample_date
      )
  } else {
    df <- df %>%
      dplyr::select(location_id, sample_date, param_name, analysis_result) %>%
      tidyr::spread(param_name, analysis_result) %>%
      arrange(location_id, sample_date) %>%
      rename(`Location ID` = location_id, `Sample Date` = sample_date)
  }

  hs <- openxlsx::createStyle(
    halign = "center",
    valign = "center",
    textDecoration = "Bold",
    border = "TopBottomLeftRight",
    wrapText = TRUE
  )

  df$`Sample Date` <- as.Date(df$`Sample Date`)

  wb <- openxlsx::createWorkbook()

  data_cast <- function(x) {
    openxlsx::addWorksheet(wb, paste(x$`Location ID`[1]))
    openxlsx::pageSetup(
      wb,
      sheet = paste(x$`Location ID`[1]),
      orientation = "landscape",
      paperSize = 3,
      fitToWidth = TRUE,
      fitToHeight = TRUE
    )
    openxlsx::writeData(
      wb,
      x,
      sheet = paste(x$`Location ID`[1]),
      startRow = 1,
      startCol = 1,
      rowNames = FALSE,
      borders = "all",
      headerStyle = hs
    )
  }

  if (!requireNamespace("plyr", quietly = TRUE)) {
    stop(
      "plyr needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  plyr::d_ply(df, plyr::.(`Location ID`), data_cast)

  openxlsx::saveWorkbook(wb, file = file, overwrite = overwrite)
}


#' Function to export summary table for a sampling event
#'
#' @param df groundwater data frame in tidy format
#' @param start start date
#' @param end end date
#' @param  gw_elev if TRUE, list date well sampled and annotate date for GW Elev
#' @param short_name TRUE/FALSE to abbreviate constituent name
#'
#' @export

write_event_summary <- function(
  df,
  start,
  end,
  gw_elev = TRUE,
  short_name = TRUE,
  file
) {
  df <- df %>%
    filter(sample_date >= start & sample_date <= end)

  df <- df %>%
    name_units()

  df <- join_lt(df)

  df <- df %>%
    select(location_id, param_name, analysis_result) %>%
    spread(location_id, analysis_result)

  readr::write_csv(df, path = file)
}
