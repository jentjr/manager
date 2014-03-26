#' Function to compare groundwater results to EPA primary and seconday standards
#' 

compare_MCL <- function(df, format = "summary") {
  data(mcl)
  df$exceedance <- NA
  for (i in 1:nrow(mcl)){
    element <- mcl$param_name[i]
    rws <- grepl(paste(element), df$param_name)
    # check units before proceeding
    # TODO
    df[rws,"exceedance"] <- ifelse(df[rws,"analysis_result"] > mcl$upper_limit[i] | 
                              df[rws,"analysis_result"] < mcl$lower_limit[i],1,0)
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
gw_heatmap <- function(df){
  df$exceedance <- ifelse(df$exceedance == 1, 1, NA)
  ggplot(df, aes(sample_date, location_id, 
                 fill = exceedance)) + 
    facet_wrap(~param_name) + 
    geom_tile(size=4) 
}

gw_heatmap_multi <- function(df) {
  d_ply(df, .(param_name), gw_heatmap, .print=TRUE)
}

# # example exceedance table output
# 
# location_id | arsenic | antimony | boron 
# ----------- | ------- | -------- | ----- 
# MW-1        | 10      |   1      |  5