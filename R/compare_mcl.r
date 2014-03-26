#' Function to compare groundwater results to EPA primary and seconday standards
#' 

compare_MCL <- function(df) {
  data(mcl)
  df$exceedance <- NA
  for (i in 1:nrow(mcl)){
    element <- mcl$param_name[i]
    rws <- grepl(paste(element), df$param_name)
    # check units before proceeding
    # TODO
    df[rws,"exceedance"] <- ifelse(df[rws,"analysis_result"] > mcl$upper_limit[i] | 
                              df[rws,"analysis_result"] < mcl$lower_limit[i], 1, 0)
  }
  # dangerous to use na.omit
  return(na.omit(df))
  rm(mcl)
}
  

# Spacetime plot for each mcl with location on y axis, time on x axis
#  solid color if exceeds mcl
gw_heatmap <- function(df){
  tmp <- reshape2::dcast(df, value.var = "exceedance", location_id + param_name ~ 
                           sample_date)
  row.names(tmp) <- tmp$location_id
  tmp <- tmp[, -1]
  tmp <- data.matrix(tmp)
  heatmap(tmp, Rowv=NA, Colv=NA, main = df$param_name[1])
}

gw_heatmap_multi <- function(df) {
  d_ply(df, .(param_name), gw_heatmap, .print=TRUE)
}

# # example exceedance table output
# 
# location_id | arsenic | antimony | boron 
# ----------- | ------- | -------- | ----- 
# MW-1        | 10      |   1      |  5