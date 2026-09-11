#' Function to assign Standards
#'
#' @param df dataframe of data in tidy format
#' @param column column containing the constituent names
#'
#' @export

assign_limits <- function(df,
                          column = "param_name"
                          ) {
    
    df <- df %>%
      dplyr::mutate(
        param_base = sub(",.*$", "", .data[[column]])
      ) %>%
      dplyr::left_join(
        standards_df,
        by = c("param_base" = "param_name")
      ) %>%
      .convert_units() %>%
      select(-standard_unit, -param_base)
  
  df
  
}

#' Function to compare parameter units to standards_df units 
#'
#' @param df dataframe of data
#'
#' @noRd

.convert_units <- function(df){

  if (!requireNamespace("udunits2", quietly = TRUE)) {
    stop(
      "Package udunits2 is required.",
         call. = FALSE
      )
  }

  rows_to_convert <- which(
    !is.na(df$standard_unit) &
      !is.na(df$default_unit) &
      df$standard_unit != df$default_unit
  )
  
  for (i in rows_to_convert) {

    converted_value <- tryCatch(
      {
        udunits2::ud.convert(
          x = df$standard_value[i],
          from = df$standard_unit[i],
          to = df$default_unit[i]
        )
      },
      error = function(e) NA_real_
    )
    
    if (!is.na(converted_value)) {
      
      df$standard_value[i] <- converted_value
      df$standard_unit[i] <- df$default_unit[i]
      
    }
  }

  df

}