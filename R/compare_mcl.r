#' Function to find EPA primary and seconday water constituents
#' 
#' @param df dataframe of groundwater data
#' @export

get_mcl_params <- function(df, type="all"){
  data(mcl)
  
  if(isTRUE(type=="primary")){
    mcl <- subset(mcl, type=="primary")
  }
  
  if(isTRUE(type=="secondary")){
    mcl <- subset(mcl, type=="secondary")
  }
  
  mcl_data <- data.frame()
  for (i in 1:nrow(mcl)){
    element <- mcl$param_name[i]
    rws <- grepl(paste(element), df$param_name)
    mcl_data <- rbind(df[rws,], mcl_data)
  }
  return(mcl_data)
}

#' Function to compare groundwater parameter units to EPA MCL data
#' 
#' @param df dataframe of groundwater data
#' @export 

convert_mcl_units <- function(df){
  for(i in 1:nrow(df)){
    if(df[i,"default_unit"]=="ug/L"){
      df[i,"analysis_result"] <- udunits2::ud.convert(df[i,"analysis_result"],
                                                      "ug/L", "mg/L")
      df[i,"default_unit"] <- "mg/L"
    }
    else{
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
  df$exceedance <- NA
  for (i in 1:nrow(mcl)){
    element <- mcl$param_name[i]
    rws <- grepl(paste(element), df$param_name)
    df[rws,"exceedance"] <- ifelse(df[rws,"analysis_result"] > 
                                   mcl$upper_limit[i] | 
                                   df[rws,"analysis_result"] < 
                                   mcl$lower_limit[i],1,0) 
  }
  # dangerous to use na.omit
  if(isTRUE(format == "summary")){
    mcl_perc <- ddply(na.omit(df), .(location_id, param_name), 
                      function(x) sum(x$exceedance, na.rm=TRUE)/nrow(x)*100)
    colnames(mcl_perc) <- c("location_id", "param_name", "pct_mcl_exceed")
    out <- reshape2::dcast(mcl_perc, value.var = "pct_mcl_exceed", 
                           location_id ~ param_name)
  } else {
    out <- na.omit(df)
  }
  return(out)
  rm(mcl)
}



# Spacetime plot for each mcl with location on y axis, time on x axis
# solid color if exceeds mcl
mcl_heatmap <- function(df, title){
  mcl_heat <- reshape2::melt(df, id.vars = "location_id")
  ggplot(mcl_heat, aes(location_id, variable, fill=value)) + geom_tile() +
    scale_fill_gradient(low = "white", high = "steelblue") + 
    theme(axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(), panel.background = element_blank()) + 
    guides(fill=guide_legend(title="Percent\nExceedance")) + 
    xlab("") + ylab("")  + ggtitle(paste(title))
}

gw_heatmap_multi <- function(df) {
  d_ply(df, .(param_name), gw_heatmap, .print=TRUE)
}

# # example exceedance table output
# 
# location_id | arsenic | antimony | boron 
# ----------- | ------- | -------- | ----- 
# MW-1        | 10      |   1      |  5