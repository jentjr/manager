#' Function to assign EPA primary and seconday Maximum Contaminant Levels
#' 
#' @param df dataframe of groundwater data in tidy format
#' @param column column to search for constituents
#' @export

assign_limits <- function(df, column = "param_name"){
  
  for (i in 1:nrow(ccr_mcl)) {
    element <- ccr_mcl$param_name[i]
    rws <- grepl(paste(element), df[[paste(column)]])
    df[rws, "mcl_unit"] <- ccr_mcl$default_unit[i]
    df[rws, "mcl"] <- ccr_mcl$mcl[i]
  }

  df <- .convert_mcl_units(df)
  
  return(df)
  
}

#' Function to compare groundwater parameter units to EPA MCL data
#' 
#' @param df dataframe of groundwater data

.convert_mcl_units <- function(df){
  
  if (!requireNamespace("udunits2", quietly = TRUE)) {
    stop("udunits2 needed for this function to work. Please install it.", 
         call. = FALSE)
  }
  
  for (i in 1:nrow(df)) {
    
    if (df[i, "default_unit"] != df[i, "mcl_unit"] &
        !is.na(df[i, "mcl_unit"])) {
      
      df[i, "mcl"] <- udunits2::ud.convert(
        df[i, "mcl"], 
        paste(df[[i, "mcl_unit"]]), 
        paste(df[[i, "default_unit"]])
      )
      
      df[i, "mcl_unit"] <- paste(df[i, "default_unit"])
      
    } else{
       
      next
      
    }
    
  }
  
  return(df)
}
