#' Function to find EPA primary and seconday water constituents
#' 
#' @param df dataframe of groundwater data
#' @export

get_MCL_params <- function(df, type="all"){
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

check_units <- function(df){
  data(mcl)
  for(i in 1:nrow(mcl)){
    element <- mcl$param_name[i]
    unit <- mcl$default_unit[i]
    rws <- grepl(paste(element), df$param_name)
    tmp <- df[rws,]
    for(j in 1:nrow(tmp)){
      if(tmp[j, "default_unit"] != unit){
        cat(paste("Convert units units for", element, "\n"))
        cat(paste("Data frame units are", tmp[j, "default_unit"], "\n"))
        cat(paste("MCL units are", unit, "\n"))
        break
      } 
    }
  }
}

#' Function to convert units
#' 
#' @param df dataframe of groundwater data
#' @param element element name to convert
#' @param from original units
#' @param to new units
#' @export

convert_units <- function(df, element, from, to){
  rws <- grepl(paste(element), df$param_name)
  df[rws, "analysis_result"] <- udunits2::ud.convert(df[rws, "analysis_result"],
                                                     from, to)
  df[rws, "default_unit"] <- to
  return(df)
}

#' Function to compare result to EPA's MCL
#' 
#' @param df dataframe of groundwater data that has been unit checked
#' @param format default summary
#' @param type default is all
#' @export

compare_MCL <- function(df, format = "summary", type = "all") {
  data(mcl)
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
    out <- reshape2::dcast(na.omit(df), value.var = "exceedance", 
                           location_id ~ param_name, sum)

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
          axis.ticks.y=element_blank()) + 
    guides(fill=guide_legend(title="Number of\nExceedances")) + 
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