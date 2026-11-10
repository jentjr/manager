#' Attach nested group data
#'
#' @param result Result tibble.
#' @param data Nested group data.
#'
#' @return Tibble with data list-column.
#'
#' @noRd
.attach_nested_data <- function(result, data) {
  result$data <- list(data)

  result
}

#' Return analyte display labels
#'
#' @param df env_schema object.
#' @param short_name Use abbreviated names.
#'
#' @returns character vector.
#'
#' @export
name_units <- function(
  df,
  short_name = FALSE
) {
  validate_env_schema(df)

  if (
    short_name &&
      ".short_analyte_label" %in% names(df)
  ) {
    return(df$.short_analyte_label)
  }

  df$.analyte_label
}

#' Convert Long Data to Wide Format
#'
#' Converts environmental monitoring data from long format to
#' wide format using analytes as columns.
#'
#' `to_wide()` is an S3 generic supporting both `data.frame`
#' and `env_schema` objects.
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional information.

#' @returns
#' A data frame in wide format.
#'
#' @details
#' When `result = "display"`, analyte columns contain
#' display-ready character values derived from
#' `.display_result`, making them suitable for reporting
#' tables and exports.
#'
#' When `result = "analysis"`, analyte columns contain
#' standardized numeric values from `.analysis_result`
#' suitable for statistical and geochemical calculations.
#'
#' @export
to_wide <- function(df, ...) {
  UseMethod("to_wide")
}

#' @rdname to_wide
#'
#' @param location_id Location column name.
#' @param sample_date Sample date column name.
#' @param param_name Analyte column name.
#' @param analysis_result Analytical result column name.
#' @param lab_id Optional laboratory identifier column.
#'
#' @export
to_wide.data.frame <- function(
  df,
  location_id = "location_id",
  sample_date = "sample_date",
  param_name = "param_name",
  analysis_result = "analysis_result",
  lab_id = NULL
) {
  id_cols <- c(
    location_id,
    sample_date
  )

  if (!is.null(lab_id)) {
    id_cols <- c(
      id_cols,
      lab_id
    )
  }

  .to_wide_core(
    df = df,
    id_cols = id_cols,
    names_from = param_name,
    values_from = analysis_result
  )
}

#' @rdname to_wide
#'
#' @param label Use analyte display labels as column names.
#' @param result Result representation to use.
#' Defaults to `"analysis"`.
#'
#' \describe{
#'   \item{"analysis"}{
#'     Uses standardized analytical values stored in
#'     `.analysis_result`.
#'   }
#'
#'   \item{"display"}{
#'     Uses display-ready reporting values stored in
#'     `.display_result`.
#'   }
#' }
#'
#' @section env_schema Method:
#'
#' For `env_schema` objects, location, sample_date, analyte,
#' and result fields are obtained automatically from
#' schema mappings.
#'
#' @export
to_wide.env_schema <- function(
  df,
  label = TRUE,
  result = c(
    "analysis",
    "display"
  )
) {
  validate_env_schema(df)

  result <- rlang::arg_match(
    result
  )

  names_from <- if (
    label &&
      ".analyte_label" %in% names(df)
  ) {
    ".analyte_label"
  } else {
    analyte_var(df)
  }

  values_from <- switch(
    result,

    analysis = ".analysis_result",

    display = ".display_result"
  )

  .to_wide_core(
    df = df,
    id_cols = c(
      location_var(df),
      sample_date_var(df)
    ),
    names_from = names_from,
    values_from = values_from
  )
}

#' @keywords internal
.to_wide_core <- function(
  df,
  id_cols,
  names_from,
  values_from
) {
  df |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(
          c(id_cols, names_from)
        )
      )
    ) |>
    dplyr::summarise(
      value = dplyr::first(
        .data[[values_from]]
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = dplyr::all_of(names_from),
      values_from = value
    )
}

#' function to convert data to sf object
#'
#' @param df data frame
#' @param crs coordinate reference system to convert the sf object to
#'
#' @export

to_spatial <- function(df, crs) {
  df$coordinate_reference <- as.integer(df$coordinate_reference)

  l <- lapply(unique(df$location_id), function(x) {
    df <- df[df$location_id == x, ]
    epsg <- df$coordinate_reference[1]
    df <- sf::st_as_sf(
      df,
      coords = c('east_coordinate', 'north_coordinate'),
      crs = epsg
    )
  })

  out <- do.call(rbind, lapply(l, function(x) x <- sf::st_transform(x, crs)))

  return(out)
}

#' Helper function to split parameter names
#'
#' @param df data frame in long format
#' @param param_name parameter name column
#'
#' @export
# TODO: make sure units are the same
simplify_param_name <- function(df, param_name) {
  df %>%
    mutate(
      param_name = str_split(.$param_name, pattern = ",", simplify = TRUE)[, 1]
    )
}

#' Standardize ion names
#'
#' @param x Character vector.
#'
#' @return Character vector.
#' @keywords internal
.standardize_ion_name <- function(x) {
  x <- x %>%
    stringr::str_to_lower() %>%

    # normalize punctuation
    stringr::str_replace_all("[,_()/-]", " ") %>%

    # remove charge notation
    stringr::str_replace_all("\\+\\d*", "") %>%
    stringr::str_replace_all("-\\d*", "") %>%

    # remove "as ..."
    stringr::str_remove("\\s+as\\s+.*$") %>%

    stringr::str_squish()

  dplyr::case_when(
    # cations
    stringr::str_detect(x, "^(calcium|ca)\\b") ~ "calcium",
    stringr::str_detect(x, "^(magnesium|mg)\\b") ~ "magnesium",
    stringr::str_detect(x, "^(sodium|na)\\b") ~ "sodium",
    stringr::str_detect(x, "^(potassium|k)\\b") ~ "potassium",

    # anions
    stringr::str_detect(x, "^(chloride|cl)\\b") ~ "chloride",
    stringr::str_detect(x, "^(fluoride|f)\\b") ~ "fluoride",
    stringr::str_detect(x, "^(bromide|br)\\b") ~ "bromide",
    stringr::str_detect(x, "^(iodide|i)\\b") ~ "iodide",

    stringr::str_detect(x, "^(sulfate|so4)\\b") ~ "sulfate",

    stringr::str_detect(x, "^(nitrate|no3)\\b") ~ "nitrate",
    stringr::str_detect(x, "^(nitrite|no2)\\b") ~ "nitrite",

    stringr::str_detect(x, "^(phosphate|po4)\\b") ~ "phosphate",

    stringr::str_detect(x, "^(bicarbonate|hco3)\\b") ~ "bicarbonate",
    stringr::str_detect(x, "^(carbonate|co3)\\b") ~ "carbonate",

    stringr::str_detect(x, "\\balkalinity\\b") ~ "bicarbonate",

    TRUE ~ NA_character_
  )
}

#' Return the first sample date
#'
#' @param df Data frame.
#' @param sample_date Sample date column.
#'
#' @export
start_date <- function(df, sample_date = sample_date) {
  sample_date <- rlang::as_name(
    rlang::ensym(sample_date)
  )

  df %>%
    dplyr::arrange(.data[[sample_date]]) %>%
    dplyr::pull(.data[[sample_date]]) %>%
    dplyr::first()
}
#' Function to find the date for next n samples
#'
#' @param df dataframe of groundwater data in tidy format
#' @param start the date to start from
#' @param n the next n samples to get the date for
#' @param sample_date the column for the sampling dates
#'
#' @export

nth_date <- function(df, start, n = 4, sample_date) {
  nth_date <- df %>%
    arrange_(~sample_date) %>%
    filter_(~ sample_date > start) %>%
    select_(~sample_date) %>%
    first() %>%
    nth(., n)

  return(nth_date)
}

#' Function to return a column of background data
#'
#' @param df dataframe of groundwater data in tidy format
#' @param sample_date the column for the sampling dates
#' @param start the start date for background
#' @param end the end date for background
#' @param return_all logical condition to return all values, or only background
#'
#' @export

set_background <- function(df, sample_date, start, end, return_all = FALSE) {
  df <- df %>%
    arrange_(~sample_date) %>%
    mutate(
      background = if_else(
        sample_date >= start &
          sample_date <= end,
        TRUE,
        FALSE
      )
    )
  if (!return_all) {
    df <- df %>%
      filter(background == TRUE)
  }

  df
}

#' Function to return a column of the proposed background data
#'
#' @param df dataframe of groundwater data in tidy format
#' @param sample_date the column for the sampling dates
#' @param start the start date for background
#' @param end the end date for background
#' @param return_all TRUE/FALSE to return all values,
#'    or only proposed background
#'
#' @export

set_proposed_background <- function(
  df,
  sample_date,
  start,
  end,
  return_all = FALSE
) {
  df <- df %>%
    arrange_(~sample_date) %>%
    mutate(
      proposed_background = if_else(
        sample_date > start &
          sample_date <= end,
        TRUE,
        FALSE
      )
    )

  if (!return_all) {
    df <- df %>%
      filter(proposed_background == TRUE)
  }

  df
}

#' Helper funtion to filter data
#'
#' @noRd
.filter_manager_data <- function(
  data,
  wells = NULL,
  params = NULL,
  start_date = NULL,
  end_date = NULL
) {
  if (!is.null(wells)) {
    data <- data %>%
      filter(location_id %in% wells)
  }

  if (!is.null(params)) {
    data <- data %>%
      filter(param_name %in% params)
  }

  if (!is.null(start_date)) {
    data <- data %>%
      filter(sample_date >= start_date)
  }

  if (!is.null(end_date)) {
    data <- data %>%
      filter(sample_date <= end_date)
  }

  data
}

#' Helper function to get date ranges
#'
#' @noRd
.get_date_range <- function(date_range) {
  list(
    start = min(as.Date(date_range), na.rm = TRUE),
    end = max(as.Date(date_range), na.rm = TRUE)
  )
}

#' Safe row sum
#'
#' Returns NA when all inputs are missing.
#'
#' @noRd
.safe_rowsum <- function(data) {
  vals <- as.matrix(data)

  out <- rowSums(
    vals,
    na.rm = TRUE
  )

  out[
    rowSums(!is.na(vals)) == 0
  ] <- NA_real_

  out
}
