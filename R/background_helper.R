#' Function to find the start of sampling for a well constituent pair
#' 
#' @param df dataframe of groundwater data in tidy format
#' @param SAMPLE_DATE the column for the sampling dates
#' @export

start_date <- function(df, SAMPLE_DATE) {
  
  start <- df %>%
    arrange_(~SAMPLE_DATE) %>%
    select_(~SAMPLE_DATE) %>%
    first() %>%
    first()
  
  return(start)

}

#' Function to find the date for next n samples
#' 
#' @param df dataframe of groundwater data in tidy format
#' @param start the date to start from
#' @param n the next n samples to get the date for
#' @param SAMPLE_DATE the column for the sampling dates
#' @export

nth_date <- function(df,
                     start,
                     n = 4,
                     SAMPLE_DATE) {
  
  nth_date <- df %>%
    arrange_(~SAMPLE_DATE) %>%
    filter_(~SAMPLE_DATE > start) %>%
    select_(~SAMPLE_DATE) %>%
    first() %>%
    nth(., n)

  return(nth_date)
  
}

#' Function to return a column of background data 
#' 
#' @param df dataframe of groundwater data in tidy format
#' @param SAMPLE_DATE the column for the sampling dates
#' @param start the start date for background
#' @param end the end date for background
#' @param return_all logical condition to return all values, or only background
#' @export

set_background <- function(df, 
                           SAMPLE_DATE,
                           start, 
                           end, 
                           return_all = FALSE) {

  df <- df %>%
    arrange_(~SAMPLE_DATE) %>%
    mutate(background = if_else(SAMPLE_DATE >= start &
                                 SAMPLE_DATE <= end, 
                                 TRUE, FALSE))
  if (!return_all) {
    df <- df %>%
      filter(background == TRUE)
  }
  
  df
  
}

#' Function to return a column of the proposed background data 
#' 
#' @param df dataframe of groundwater data in tidy format
#' @param SAMPLE_DATE the column for the sampling dates
#' @param start the start date for background
#' @param end the end date for background
#' @param return_all TRUE/FALSE to return all values, 
#'    or only proposed background
#' @export

set_proposed_background <- function(df, 
                                    SAMPLE_DATE, 
                                    start, 
                                    end,
                                    return_all = FALSE) {
  
  df <- df %>%
    arrange_(~SAMPLE_DATE) %>%
    mutate(proposed_background = if_else(SAMPLE_DATE > start &
                                         SAMPLE_DATE <= end, 
                                         TRUE, FALSE))
  
  if (!return_all) {
    df <- df %>%
      filter(proposed_background == TRUE)
  }
  
  df
  
}