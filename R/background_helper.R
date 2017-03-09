#' Function to find the start of sampling for a well constituent pair
#' 
#' @param df df dataframe of groundwater data in tidy format
#' @param location_id location_id column for the well
#' @param well well the well to selected
#' @param param_name param_name the column for the constituent
#' @param param param the constituent to be selected
#' @param sample_date sample_date the column for the sampling dates
#' @export

start_date <- function(df, 
                 location_id = "location_id", 
                 well, 
                 param_name = "param_name", 
                 param,  
                 sample_date = "sample_date") {
  
  start <- df %>%
    arrange_(~sample_date) %>%
    filter_(~location_id == paste(well), ~param_name == paste(param)) %>%
    select_(~sample_date) %>%
    first() %>%
    first()
  
  return(start)

}

#' Function to find the date for next n samples
#' 
#' @param df df dataframe of groundwater data in tidy format
#' @param start start the date to start from
#' @param n n the next n samples to get the date for
#' @param location_id location_id column for the well
#' @param well well the well to selected
#' @param param_name param_name the column for the constituent
#' @param param param the constituent to be selected
#' @param sample_date sample_date the column for the sampling dates
#' @export

nth_date <- function(df,
                     start,
                     n = 4,
                     location_id = "location_id",
                     well,
                     param_name = "param_name",
                     param,
                     sample_date = "sample_date") {
  
  nth_date <- df %>%
    arrange_(~sample_date) %>%
    filter_(~location_id == paste(well), 
           ~param_name == paste(param),
           ~sample_date > start_date) %>%
    select_(~sample_date) %>%
    first() %>%
    nth(., n)

  return(nth_date)
  
}

#' Function to return a column of background data 
#' 
#' @param df df dataframe of groundwater data in tidy format
#' @param location_id location_id column for the well
#' @param well well the well to selected
#' @param param_name param_name the column for the constituent
#' @param param param the constituent to be selected
#' @param sample_date sample_date the column for the sampling dates
#' @param start start the start date for background
#' @param end end the end date for background
#' @export

set_background <- function(df, 
                           location_id = "location_id",
                           well,
                           param_name = "param_name",
                           param,
                           sample_date = "sample_date",
                           start, 
                           end) {

  df %>%
    arrange_(~sample_date) %>%
    mutate_(background = if_else(~location_id == paste(well) & 
           ~param_name == paste(param) &
           ~sample_date >= start &
             ~sample_date <= end, TRUE, FALSE))
    
}

