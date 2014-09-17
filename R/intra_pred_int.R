#' Function to calculate intrawell prediction interval
#' @param df data frame of groundwater data
#' @param analysis_result name of column containing analysis results
#' @param wells vector of wells to be included
#' @param params vector of constituents to be included
#' @param bkgd_dates background data date range
#' @param comp_dates compliance data date range
#' @param SWFPR Site-Wide False Positive Rate
#' @export

intra_pred_int <- function(df, analysis_result, wells, params, bkgd_dates, 
                           comp_dates, intra.conf.level = 0.95, 
                           SWFPR = NULL, ...){
  if(!is.null(SWFPR)){
    nw <- length(wells)
    nc <- length(params)
    SWFPR <- SWFPR
    intra.conf.level <- (1 - SWFPR)^(1/(nc*nw)) 
  }

  df <- filter(df, location_id %in% wells, param_name %in% params) 

  bkgd <- filter(
    df, 
    sample_date >= bkgd_dates[1], 
    sample_date <= bkgd_dates[2]
    ) %>% group_by(location_id, param_name, default_unit) 
  
  comp <- filter(
    df, 
    sample_date >= comp_dates[1], 
    sample_date <= comp_dates[2]
    ) %>% group_by(location_id, param_name, default_unit) 
  
  dist <- bkgd %>% 
    mutate(dist = dist(analysis_result))
  
  limits <- dist %>% 
    do(pred_int = pred_int(.$analysis_result, dist = .$dist[1], 
                           conf.level = intra.conf.level, ...))
  
  limits <- limits %>% 
    do(data.frame(.))
  
  limits <- reshape2::dcast(
    limits, 
    location_id + param_name + default_unit ~ pred_int.variable, 
    value.var = "pred_int.result"
    )
  
  out <- left_join(
    comp, 
    limits, 
    by = c("location_id", "param_name", "default_unit")
    )
  
  out <- out %>%
    mutate(exceed = ifelse(analysis_result > upper_limit, "yes", "no"))
  
  out <- as.data.frame(out)
  
  return(out)

}


