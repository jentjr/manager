#' Function to compare groundwater results to EPA primary and seconday standards
#' 

compare_MCL <- function(df) {
  ddply(df, .(param_name), )
}
  

parse_mcl <- function(df) {
  element <- mcl$param_name[1]
  tmp <- gw_data[grepl(paste(element), gw_data$param_name)==1,]
  tmp$exceedances <- ifelse(tmp$analysis_result > mcl$upper_limit[1] | 
                              tmp$analysis_result < mcl$lower_limit[1], 1, 0)
  ddply(tmp, .(location_id), summarise, total_exceed = sum(exceedances))
}


# for each param in mcl
#   match to data param
#   compare mcl to results
#     if greater or less than limit exceedance = 1
#   count exceedances and summarize (1 row)
#   combine rows
# return new data frame

# Spacetime plot for each mcl with location on y axis, time on x axis
#  solid color if exceeds mcl
gw_heatmap <- function(df){
  tmp <- dcast(df, value.var = "analysis_result", location_id ~ sample_date)
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