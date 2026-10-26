#' Function to return a sorted list of all the location IDs
#'
#' @param df data frame of groundwater data in the format with column names
#' @param location_id column for the sample location id
#'
#' @export

sample_locations <- function(df, location_id = "location_id") {
  sort(unique(as.character(df[[location_id]])))
}

#' Function to return a sorted list of all the constituents
#'
#' @param df data frame of groundwater data in the format with column names
#' @param param_name the column for the constituents
#'
#' @export

constituents <- function(df, param_name = "param_name") {
  sort(unique(as.character(df[[param_name]])))
}

#' Calculate the percentage of left censored data
#'
#' @param df df data frame of groundwater monitoring data in long format
#' @param lt_measure lt_measure column of less than detection limit symbol.
#'
#' @export

percent_lt <- function(df, lt_measure) {
  df %>%
    dplyr::mutate(
      percent_lt = sum({{ lt_measure }} == "<", na.rm = TRUE) /
        dplyr::n() *
        100
    )
}

#' Calculate the percentage of right censored data
#'
#' @param df df data frame of groundwater monitoring data in long format
#' @param lt_measure lt_measure column of greater than detection limit symbol.
#' @param percent_gt percent_gt new column name
#'
#' @export

percent_gt <- function(
  df,
  lt_measure = "lt_measure",
  percent_gt = "percent_gt"
) {
  mutate_call <- lazyeval::interp(
    ~ sum(x == ">") / n() * 100,
    x = as.name(lt_measure)
  )

  df %>%
    mutate_(.dots = setNames(list(mutate_call), percent_gt))
}

#' Function to remove duplicate samples
#' Example: If you have wells named MW-1 and another named MW-1 Duplicate
#' this function will remove the MW-1 Duplicate sample
#'
#' @param df groundwater data frame
#' @export

remove_dup <- function(df) {
  dplyr::filter(
    df,
    !stringr::str_detect(location_id, "\\bDup$")
  )
}

#' Function to include duplicate samples
#'
#' @param df groundwater data frame
#' @param wells list of wells to included duplicates for
#'
#' @export

include_dup <- function(df, wells) {
  dup_pattern <- paste0(
    "^(",
    paste(wells, collapse = "|"),
    ")\\s*Dup$"
  )

  dups <- unique(
    df$location_id[
      stringr::str_detect(df$location_id, dup_pattern)
    ]
  )
}

#' Function to replace missing values
#'
#' @param df groundwater data frame
#' @param analysis_result column containing the analysis results
#' @param value value to replace missing value with
#'
#' @export

replace_missing <- function(df, analysis_result, value) {
  df %>%
    mutate(
      {{ analysis_result }} := dplyr::na_if({{ analysis_result }}, value)
    )
}

#' Function to convert character column of less than, or greater than
#' symbols to logical vectors and return a data frame
#'
#' @param df data frame of groundwater data
#' @export

to_censored <- function(df) {
  df %>%
    group_by(location_id, param_name, default_unit) %>%
    mutate(
      left_censored = if_else(lt_measure == "<", TRUE, FALSE, missing = FALSE),
      right_censored = if_else(lt_measure == ">", TRUE, FALSE, missing = FALSE)
    )
}

#' Function to join columns of lt_measure and sample results
#'
#' @param df groundwater data frame
#' @param lt_measure column name for non-detect symbol
#' @param analysis_result column name for analysis results
#'
#' @export

join_lt <- function(df, lt_measure, analysis_result) {
  df %>%
    tidyr::replace_na(list(lt_measure = "")) %>%
    tidyr::unite(analysis_result, lt_measure, analysis_result, sep = " ")
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

#' Helper function to cast data frame to wide format
#'
#' @param df data frame in long format
#' @param lab_id logical to include lab_id. Default is FALSE.
#' @param join_lt logical to join the non-detect colum with analysis result.
#'
#' @export
#'
#' @examples
#' data(gw_data)
#'
#' gw_data %>%
#'   filter(param_name %in% c("Arsenic, dissolved", "Boron, dissolved")) %>%
#'   to_wide()

to_wide <- function(df, lab_id = FALSE, join_lt = TRUE) {
  if (isTRUE(join_lt)) {
    if (isTRUE(lab_id)) {
      df <- df %>%
        join_lt() %>%
        name_units() %>%
        dplyr::group_by(location_id, lab_id, sample_date, param_name) %>%
        dplyr::summarise(analysis_result) %>%
        tidyr::spread(param_name, analysis_result)
    } else {
      df <- df %>%
        join_lt() %>%
        name_units() %>%
        dplyr::group_by(location_id, sample_date, param_name) %>%
        dplyr::summarise(analysis_result) %>%
        tidyr::spread(param_name, analysis_result)
    }
  } else {
    if (isTRUE(lab_id)) {
      df <- df %>%
        name_units() %>%
        dplyr::group_by(location_id, lab_id, sample_date, param_name) %>%
        dplyr::summarise(analysis_result) %>%
        tidyr::spread(param_name, analysis_result)
    } else {
      df <- df %>%
        name_units() %>%
        dplyr::group_by(location_id, sample_date, param_name) %>%
        dplyr::summarise(analysis_result) %>%
        tidyr::spread(param_name, analysis_result)
    }
  }

  df
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


#' Validate required columns
#'
#' Internal helper used throughout the package to check that
#' required columns are present.
#'
#' @param data A data frame.
#' @param cols Character vector of required column names.
#'
#' @return Invisibly returns TRUE.
#'
#' @noRd
.validate_required_cols <- function(
  data,
  cols
) {
  missing_cols <- setdiff(
    cols,
    names(data)
  )

  if (length(missing_cols) > 0) {
    rlang::abort(
      paste(
        "Missing required column(s):",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
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

#' Validate choices
#'
#' @param x Character vector.
#' @param choices Character vector
#' @param arg Character vector
#'
#' @keywords internal
.validate_choice <- function(x, choices, arg) {
  if (!x %in% choices) {
    stop(
      sprintf(
        "`%s` must be one of: %s",
        arg,
        paste(choices, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(x)
}

#' @keywords internal
.check_flag <- function(x, name) {
  if (
    !is.logical(x) ||
      length(x) != 1 ||
      is.na(x)
  ) {
    stop(
      name,
      " must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
