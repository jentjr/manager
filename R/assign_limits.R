#' Function to assign default Groundwater Protection Standards
#' 
#' @param df dataframe of groundwater data in tidy format
#' @param column column to search for constituents
#' @param method used to assign GWPS. Options c("default", "tolerance")
#' @param locations locations to be used
#' 
#' @export

assign_limits <- function(df, 
                          column = "param_name", 
                          method = c("default", "tolerance"),
                          locations = NULL)
  {
  
  method <- match.arg(method)
  
  if (method == "default") {
    for (i in 1:nrow(default_gwps)) {
      element <- default_gwps$param_name[i]
      rws <- grepl(paste(element), df[[paste(column)]])
      df[rws, "gwps_unit"] <- default_gwps$gwps_unit[i]
      df[rws, "gwps"] <- default_gwps$gwps[i]
    }
    
    df<- df %>%
      .convert_gwps_units() %>%
      select(-gwps_unit)
  }
  
  if (method == "tolerance") {
    
  }

 df

}

#' Function to compare groundwater parameter units to GWPS data
#' 
#' @param df dataframe of groundwater data
#' 
#' @noRd

.convert_gwps_units <- function(df){

  if (!requireNamespace("udunits2", quietly = TRUE)) {
    stop("udunits2 needed for this function to work. Please install it.", 
         call. = FALSE)
  }

  for (i in 1:nrow(df)) {

    if (df[i, "default_unit"] != df[i, "gwps_unit"] &
        !is.na(df[i, "gwps_unit"])) {

      df[i, "gwps"] <- udunits2::ud.convert(
        df[i, "gwps"], 
        paste(df[[i, "gwps_unit"]]), 
        paste(df[[i, "default_unit"]])
      )

      df[i, "gwps_unit"] <- paste(df[i, "default_unit"])

    } else{

      next

    }

  }

  df

}