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
