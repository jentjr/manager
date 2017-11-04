#' Function to return a list of all the location IDs
#'
#' @param df data frame of groundwater data in the format with column names
#' @param location_id column for the sample location id
#' @export

sample_locations <- function(df, location_id){

  wells <- df %>%
    group_by_(~location_id) %>%
    select_(~location_id) %>%
    first() %>%
    unique()

  return(wells)

}

#' Function to return a list of all the constituents
#'
#' @param df data frame of groundwater data in the format with column names
#' @param param_name the column for the constituents
#' @export

constituents <- function(df, param_name){

  constituents <- df %>%
    group_by_(~param_name) %>%
    select_(~param_name) %>%
    first() %>%
    unique()

  return(constituents)

}

#' Calculate the percentage of left censored data
#'
#' @param df df data frame of groundwater monitoring data in long format
#' @param lt_measure lt_measure column of less than detection limit symbol.
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
#' @export

replace_missing <- function(df){

  df$analysis_result <- ifelse(df$analysis_result == -999.9, NA,
                                 df$analysis_result)
  return(df)

}

#' Function to convert character column of less than, or greater than
#' symbols to logical vectors and return a data frame
#'
#' @param df data frame of groundwater data
#' @export

to_censored <- function(df) {

  df <- df %>%
    group_by(location_id, param_name, default_unit) %>%
    mutate(
      left_censored = ifelse(lt_measure == "<", TRUE, FALSE),
      right_censored = ifelse(lt_measure == ">", TRUE, FALSE)
    )

  df <- as.data.frame(df)

  return(df)
}

#' Function to join columns of lt_measure and sample results
#'
#' @param df groundwater data frame
#' @param col_name qouted column name for the result
#' @export

join_lt <- function(df, col_name) {

  .join_lt <- function() {
    paste(df$lt_measure, df$analysis_result, sep = " ")
  }

  df$result <- ifelse(df$lt_measure == "<" | df$lt_measure == ">",
                        .join_lt(), df$analysis_result)

  names(df)[names(df) == "result"] <- col_name

  return(df)

}

#' Function to return a column of parameter name with units
#'
#' @param df groundwater data frame
#' @param short_name If TRUE will use abbreviated constituent names
#' @export

name_units <- function(df, short_name = TRUE) {

  if (short_name == TRUE) {

    df <- df %>% 
      mutate(param_unit = paste0(.$short_name, " (", .$default_unit, ")"))

  }

  else {

    df <- df %>%
      mutate(param_unit  = paste0(.$param_name, " (", .$default_unit, ")"))

  }

  return(df)

}