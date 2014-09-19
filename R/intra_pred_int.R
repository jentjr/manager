#' Function to calculate intrawell prediction interval
#' @param df data frame of groundwater data
#' @param analysis_result name of column containing analysis results
#' @param wells vector of wells to be included
#' @param params vector of constituents to be included
#' @param bkgd_dates background data date range
#' @param comp_dates compliance data date range
#' @param intra.conf.level confidence level 
#' @param SWFPR Site-Wide False Positive Rate. If supplied, then 
#' intra.conf.level is computed using (1 - SWFPR)^(1/(nc*nw)) where nc is the 
#' number of constituents and nw is the number of wells
#' @export

intra_pred_int <- function(df, analysis_result, wells, params, bkgd_dates, 
                           comp_dates, intra.conf.level = 0.95, 
                           simultaneous = TRUE, SWFPR = NULL, ...){
  if(!is.null(SWFPR)){
    nw <- length(wells)
    nc <- length(params)
    SWFPR <- SWFPR
    intra.conf.level <- (1 - SWFPR)^(1/(nc*nw)) 
  }

  df <- filter(df, location_id %in% wells, param_name %in% params) 
  
  df <- df %>% 
    mutate(
      sample_type = ifelse(sample_date >= bkgd_dates[1] &
      sample_date <= bkgd_dates[2], "background", "")
      ) %>%
    mutate(
      sample_type = ifelse(sample_date >= comp_dates[1] &
      sample_date <= comp_dates[2], "compliance", sample_type)
      )

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
  
  if(isTRUE(simultaneous)) {
    limits <- dist %>% 
      do(pred_int = pred_int_sim(.$analysis_result, dist = .$dist[1], 
                                 conf.level = intra.conf.level, ...))
  } else {
    limits <- dist %>% 
      do(pred_int = pred_int(.$analysis_result, dist = .$dist[1], 
                                 conf.level = intra.conf.level, ...))
  }
  
  limits <- limits %>% 
    do(data.frame(.))
  
  limits <- reshape2::dcast(
    limits, 
    location_id + param_name + default_unit ~ pred_int.variable, 
    value.var = "pred_int.result"
    )
  
  out <- left_join(
    df, 
    limits, 
    by = c("location_id", "param_name", "default_unit")
    )
  
  out <- as.data.frame(out)
  out$conf_level <- as.numeric(out$conf_level)
  out$lower_limit <- as.numeric(out$lower_limit)
  out$upper_limit <- as.numeric(out$upper_limit)
  
  out <- out %>%
    mutate(exceed = ifelse(analysis_result > upper_limit, "yes", "no"))
  
  return(out)

}


