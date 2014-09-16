#' Function to calculate intrawell prediction interval
#' @param df data frame of groundwater data
#' @param analysis_result name of column containing analysis results
#' @param wells vector of wells to be included
#' @param params vector of constituents to be included
#' @param bkgd_dates background data date range
#' @param SWFPR Site-Wide False Positive Rate
#' @export

intra_pred_int <- function(df, analysis_result, wells, params, bkgd_dates, 
                           comp_dates, SWFPR = 0.1, ...){
  nw <- length(wells)
  nc <- length(params)
  SWFPR <- SWFPR
  conf.level <- (1 - SWFPR)^(1/(nc*nw))
  
  df <- filter(df, location_id %in% wells, param_name %in% params, 
               sample_date >= bkgd_dates[1], sample_date <= bkgd_dates[2]) %>%
    tbl_df()

  df <- df %>% group_by(location_id, param_name, default_unit) 
  
  dist <- df %>% mutate(dist = dist(analysis_result))
  
  limits <- dist %>% 
    do(pred_int = pred_int(.$analysis_result, dist = .$dist[1]))
  
  return(limits)

}


