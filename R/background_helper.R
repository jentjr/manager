#' Function to find the start of sampling for a well constituent pair
#' 
#' @param df dataframe of groundwater data in tidy format
#' @param sample_date the column for the sampling dates
#' @export

start_date <- function(df, sample_date) {
  
  start <- df %>%
    arrange_(~sample_date) %>%
    select_(~sample_date) %>%
    first() %>%
    first()
  
  return(start)

}

#' Function to find the date for next n samples
#' 
#' @param df dataframe of groundwater data in tidy format
#' @param start the date to start from
#' @param n the next n samples to get the date for
#' @param sample_date the column for the sampling dates
#' @export

nth_date <- function(df,
                     start,
                     n = 4,
                     sample_date) {
  
  nth_date <- df %>%
    arrange_(~sample_date) %>%
    filter_(~sample_date > start) %>%
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
#' @export

set_background <- function(df, 
                           sample_date,
                           start, 
                           end) {

  df %>%
    arrange_(~sample_date) %>%
    mutate(background = if_else(sample_date >= start &
                                 sample_date <= end, 
                                 TRUE, FALSE))
    
}

#' Function to return a column of the proposed background data 
#' 
#' @param df dataframe of groundwater data in tidy format
#' @param sample_date the column for the sampling dates
#' @param start the start date for background
#' @param end the end date for background
#' @export

set_proposed_background <- function(df, 
                                    sample_date, 
                                    start, 
                                    end) {
  
  df %>%
    arrange_(~sample_date) %>%
    mutate(proposed_background = if_else(sample_date > start &
                                         sample_date <= end, 
                                         TRUE, FALSE))
  
}