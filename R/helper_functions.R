#' Function to return a list of all the location IDs
#'
#' @param df data frame of groundwater data in the format with column names
#' @param location_id column for the sample location id
#'
#' @export

sample_locations <- function(df, location_id = location_id){

  wells <- df %>%
    pull({{location_id}}) %>%
    unique()

  return(wells)

}

#' Function to return a list of all the constituents
#'
#' @param df data frame of groundwater data in the format with column names
#' @param param_name the column for the constituents
#'
#' @export

constituents <- function(df, param_name = param_name){

  constituents <- df %>%
    pull({{param_name}}) %>%
    unique()

  return(constituents)

}

#' Calculate the percentage of left censored data
#'
#' @param df df data frame of groundwater monitoring data in long format
#' @param lt_measure lt_measure column of less than detection limit symbol.
#'
#' @export

percent_lt <- function(df, lt_measure) {

  df %>%
    mutate(percent_lt = sum(lt_measure == "<",
                        na.rm = TRUE) / n() * 100)

}

#' Calculate the percentage of right censored data
#'
#' @param df df data frame of groundwater monitoring data in long format
#' @param lt_measure lt_measure column of greater than detection limit symbol.
#' @param percent_gt percent_gt new column name
#'
#' @export

percent_gt <- function(df,
                       lt_measure = "lt_measure",
                       percent_gt = "percent_gt") {

  mutate_call <- lazyeval::interp(~ sum(x == ">") / n() * 100,
                                  x = as.name(lt_measure))

  df %>%
    mutate_(.dots = setNames(list(mutate_call), percent_gt))
}

#' Function to remove duplicate samples
#' Example: If you have wells named MW-1 and another named MW-1 Duplicate
#' this function will remove the MW-1 Duplicate sample
#'
#' @param df groundwater data frame
#' @export

remove_dup <- function(df){

  df_nodup <- df[-grep("*Dup", df$location_id), ]

  return(df_nodup)

}

#' Function to include duplicate samples
#'
#' @param df groundwater data frame
#' @param wells list of wells to included duplicates for
#'
#' @export

include_dup <- function(df, wells) {

  pattern <- paste(wells, "[:space:]*Dup")

  dups <- unique(df[grep(pattern, df$location_id), ]$location_id)

  dups <- as.character(droplevels(dups))

  wells <- append(wells, dups)

  df_dup <- df %>%
    filter(location_id %in% wells)

  df_dup <- droplevels(df_dup)

  return(df_dup)

}

#' Function to replace missing values
#'
#' @param df groundwater data frame
#' @param value value to replace missing value with
#'
#' @export

replace_missing <- function(df, value){

  df$analysis_result <- ifelse(df$analysis_result == value, NA,
                                 df$analysis_result)
  return(df)

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
    replace_na(list(lt_measure = "")) %>%
    unite(analysis_result, lt_measure, analysis_result, sep = " ")

}

#' Function to return a column of parameter name with units
#'
#' @param df groundwater data frame
#' @param short_name If TRUE will use abbreviated constituent names
#'
#' @export

name_units <- function(df, short_name = FALSE) {

  if (short_name == TRUE) {

    df <- df %>%
      mutate(param_name = paste0(.$short_name, " (", .$default_unit, ")"))

  }

  else {

    df <- df %>%
      mutate(param_name  = paste0(.$param_name, " (", .$default_unit, ")"))

  }

  return(df)

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
        group_by(location_id, lab_id, sample_date, param_name) %>%
        summarise(analysis_result) %>%
        spread(param_name, analysis_result)

    } else {
      df <- df %>%
        join_lt() %>%
        name_units() %>%
        group_by(location_id, sample_date, param_name) %>%
        summarise(analysis_result) %>%
        spread(param_name, analysis_result)
    }

  } else {

    if (isTRUE(lab_id)) {
      df <- df %>%
        name_units() %>%
        group_by(location_id, lab_id, sample_date, param_name) %>%
        summarise(analysis_result) %>%
        spread(param_name, analysis_result)
    } else {
      df <- df %>%
        name_units() %>%
        group_by(location_id, sample_date, param_name) %>%
        summarise(analysis_result) %>%
        spread(param_name, analysis_result)
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
    mutate(param_name = str_split(.$param_name, pattern = ",", simplify = TRUE)[, 1])
}
