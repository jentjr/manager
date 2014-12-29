#' Function to find EPA primary and seconday water constituents
#' 
#' @param df dataframe of groundwater data
#' @param type can be all, primary, or secondary 

get_mcl_params <- function(df, type = "all"){
  
  data(mcl)
  
  if(isTRUE(type == "primary")){
    mcl <- mcl[mcl$type == "primary", ]
  }
  
  if(isTRUE(type == "secondary")){
    mcl <- mcl[mcl$type == "secondary", ]
  }
  
  mcl_data <- data.frame()
  for (i in 1:nrow(mcl)){
    element <- mcl$param_name[i]
    rws <- grepl(paste(element), df$param_name)
    df[rws, "mcl_lower_limit"] <- mcl$lower_limit[i]
    df[rws, "mcl_upper_limit"] <- mcl$upper_limit[i]
    mcl_data <- rbind(df[rws,], mcl_data)
  }
  
  return(mcl_data)
}

#' Function to compare groundwater parameter units to EPA MCL data
#' 
#' @param df dataframe of groundwater data


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


compare_mcl <- function(df, format = "summary", type = "all") {
  
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
    mcl_perc <- plyr::ddply(na.omit(df), .(location_id, param_name), 
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

#' MCL Comparison Plot
#' 

mcl_lineplot <- function(df, title) {
  
  df$non_detect <- ifelse(
    df$lt_measure == "<", "non-detect", "detected"
  )
  
  df$param_name <- paste(df$param_name, " (", 
                             df$default_unit, ")", sep = "")
  
  ggplot(df, aes(x = sample_date, y = analysis_result, color = location_id)) + 
  geom_point(data = df, aes(shape = factor(non_detect)), size = 3) + 
  scale_shape_manual(values = c("non-detect" = 1, "detected" = 16)) +
  facet_wrap(~param_name, scale = "free") + 
  geom_hline(data = df, 
             aes(yintercept = mcl_upper_limit, linetype="upper mcl"),
             size = 1, show_guide=TRUE) + 
  geom_hline(data = df, 
             aes(yintercept = mcl_lower_limit, linetype="lower mcl"), 
             size=1, show_guide=TRUE) +
  scale_linetype_manual(values=c("solid", "dotdash")) +
  ggtitle(paste(title)) +
  ylab("Analysis Result\n") +
  xlab("\nSample Date") +
  scale_x_datetime(labels = scales::date_format("%Y")) +
  theme(plot.margin = grid::unit(c(1, 1, 1, 1), "lines")) + 
  theme_bw() +  
  theme(axis.title.x = element_text(size = 15, vjust = -0.3)) +
  theme(axis.title.y = element_text(size = 15, vjust = 0.3)) +
  guides(linetype = guide_legend("MCL"),
         shape = guide_legend("Legend", override.aes = list(linetype = 0)),
         color = guide_legend("Location", override.aes = list(linetype = 0)))
}

#' Plot for each mcl with location on x axis, MCL parameters on x axis
#' 
#' @param df dataframe of mcl summary
#' @param title title of plot


mcl_heatmap <- function(df, title){
  mcl_heat <- reshape2::melt(df, id.vars = "location_id")
  ggplot(mcl_heat, aes(location_id, variable, fill=value)) + geom_tile() +
    scale_fill_gradient(low = "white", high = "steelblue") + 
    theme(axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(), panel.background = element_blank()) + 
    guides(fill=guide_legend(title="Percent\nExceedance")) + 
    xlab("") + ylab("")  + ggtitle(paste(title))
}

# gw_heatmap_multi <- function(df) {
#   d_ply(df, .(param_name), gw_heatmap, .print=TRUE)
# }

# # example exceedance table output
# 
# location_id | arsenic | antimony | boron 
# ----------- | ------- | -------- | ----- 
# MW-1        | 10      |   1      |  5
