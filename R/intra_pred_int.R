#' Function to calculate intrawell prediction interval
#' 
#' @param df data frame of groundwater data
#' @param ANALYSIS_RESULT name of column containing analysis results
#' @param wells vector of wells to be included
#' @param params vector of constituents to be included
#' @param bkgd_dates background data date range
#' @param comp_dates compliance data date range
#' @param intra.conf.level confidence level 
#' @param SWFPR Site-Wide False Positive Rate. If supplied, then 
#' intra.conf.level is computed using (1 - SWFPR)^(1/(nc*nw)) where nc is the 
#' number of constituents and nw is the number of wells
#' @export

intra_pred_int <- function(df, ANALYSIS_RESULT, wells, params, bkgd_dates, 
                           comp_dates, non_detect = c(15, 50), 
                           cen_method = "mle", intra.conf.level = 0.95, 
                           simultaneous = TRUE, SWFPR = NULL, ...) {
  
  if(!is.null(SWFPR)){
    nw <- length(wells)
    nc <- length(params)
    SWFPR <- SWFPR
    intra.conf.level <- (1 - SWFPR)^(1/(nc*nw)) 
  }

  df <- filter(df, LOCATION_ID %in% wells, PARAM_NAME %in% params) 
  
  df <- to_censored(df)
  
  df <- df %>% 
    mutate(
      sample_type = ifelse(SAMPLE_DATE >= bkgd_dates[1] &
      SAMPLE_DATE <= bkgd_dates[2], "background", "")
      ) %>%
    mutate(
      sample_type = ifelse(SAMPLE_DATE >= comp_dates[1] &
      SAMPLE_DATE <= comp_dates[2], "compliance", sample_type)
      )

  bkgd <- filter(
    df, 
    SAMPLE_DATE >= bkgd_dates[1], 
    SAMPLE_DATE <= bkgd_dates[2]
    ) %>% group_by(LOCATION_ID, PARAM_NAME, DEFAULT_UNIT) 
  
  comp <- filter(
    df, 
    SAMPLE_DATE >= comp_dates[1], 
    SAMPLE_DATE <= comp_dates[2]
    ) %>% group_by(LOCATION_ID, PARAM_NAME, DEFAULT_UNIT) 
  
  dist <- bkgd %>% 
    mutate(dist = dist(ANALYSIS_RESULT))
  
  if(isTRUE(simultaneous)) {
    limits <- dist %>% 
      do(pred_int = pred_int_sim(x = .$ANALYSIS_RESULT,  
                                 left_censored =  .$left_censored,
                                 percent_left = .$percent_left,
                                 non_detect = non_detect, 
                                 cen_method = cen_method,
                                 dist = .$dist[1],
                                 conf.level = intra.conf.level, ...))
  } else {
    limits <- dist %>% 
      do(pred_int = pred_int(.$ANALYSIS_RESULT, dist = .$dist[1], 
                             left_censored =  .$left_censored,
                             percent_left = .$percent_left,
                             non_detect = non_detect, 
                             cen_method = cen_method,
                             conf.level = intra.conf.level, ...))
  }
  
  limits <- limits %>% 
    do(data.frame(.))
  
  limits <- reshape2::dcast(
    limits, 
    LOCATION_ID + PARAM_NAME + DEFAULT_UNIT ~ pred_int.variable, 
    value.var = "pred_int.result"
    )
  
  out <- left_join(
    df, 
    limits, 
    by = c("LOCATION_ID", "PARAM_NAME", "DEFAULT_UNIT")
    )
  
  out <- as.data.frame(out)
  out$conf_level <- as.numeric(out$conf_level)
  out$lower_limit <- as.numeric(out$lower_limit)
  out$upper_limit <- as.numeric(out$upper_limit)
  
  out <- out %>%
    mutate(exceed = ifelse(ANALYSIS_RESULT > upper_limit, "yes", "no"))
  
  return(out)

}