#' Function to assign EPA primary and seconday Maximum Contaminant Levels
#' 
#' @param df dataframe of groundwater data in tidy format
#' @param column column to search for constituents
#' @param type can be all, primary, or secondary 
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


#' Function to compare result to EPA's MCL
#' 
#' @param df dataframe of groundwater data that has been unit checked
#' @param format default summary
#' @param type default is all
#' @export


compare_mcl <- function(df, format = "summary", type = "all") {
  
  data(mcl)
  
  df$exceedance <- NA
  
  for (i in 1:nrow(mcl)) {
    element <- mcl$param_name[i]
    rws <- grepl(paste(element), df$param_name)
    df[rws,"exceedance"] <- ifelse(df[rws,"analysis_result"] > 
                                   mcl$upper_limit[i] | 
                                   df[rws,"analysis_result"] < 
                                   mcl$lower_limit[i],1,0) 
  }
  # dangerous to use na.omit
  if (isTRUE(format == "summary")) {
    mcl_perc <- plyr::ddply(na.omit(df), .(location_id, param_name), 
                      function(x) sum(x$exceedance, na.rm = TRUE)/nrow(x)*100)
    colnames(mcl_perc) <- c("location_id", "param_name", "pct_mcl_exceed")
    out <- reshape2::dcast(mcl_perc, value.var = "pct_mcl_exceed", 
                           location_id ~ param_name)
  } else {
    out <- na.omit(df)
  }
  return(out)
  rm(mcl)
}

#' Plot for each mcl with location on x axis, MCL parameters on x axis
#' 
#' @param df dataframe of mcl summary
#' @param title title of plot
#' @export

mcl_heatmap <- function(df, title){
  mcl_heat <- reshape2::melt(df, id.vars = "location_id")
  ggplot(mcl_heat, aes(location_id, variable, fill = value)) + geom_tile() +
    scale_fill_gradient(low = "white", high = "steelblue") + 
    theme(axis.text.x = element_text(angle = 90), axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(), panel.background = element_blank()) + 
    guides(fill = guide_legend(title = "Percent\nExceedance")) + 
    xlab("") + ylab("")  + ggtitle(paste(title))
}