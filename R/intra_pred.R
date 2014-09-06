#' Function to calculate intrawell prediction interval
#' @param df data frame of groundwater data
#' @param wells vector of wells to be included
#' @param params vector of constituents to be included
#' @param bkgd_dates background data date range
#' @param SWFPR Site-Wide False Positive Rate
#' @export

intra_pred <- function(df, wells, params, bkgd_dates, SWFPR = 0.1){
  df <- df[df$location_id %in% wells & df$param_name %in% params & 
           df$sample_date >= bkgd_dates[1] & df$sample_date <= bkgd_dates[2], ]
  
  nw <- length(wells)
  nc <- length(params)
  SWFPR <- SWFPR
  conf.level <- (1 - SWFPR)^(1/(nc*nw))
  
  norm_lim_sim <- function(x){
    out <- EnvStats::predIntNormSimultaneous(x$analysis_result, 
                                             k = 1, m = 2, r = 2, 
                                             rule = "k.of.m", 
                                             conf.level = conf.level)
    x <- data.frame(location_id = x$location_id[1], 
                    param_name = x$param_name[1],
                    assumed_dist = out$distribution,
                    count = out$sample.size,
                    mean = out$parameters["mean"],
                    sd = out$parameters["sd"],
                    conf_level = out$interval$conf.level,
                    UPL = out$interval$limits["UPL"])
    return(x)
  }
  limits <- plyr::ddply(df, .(location_id, param_name), norm_lim_sim)
  return(limits)
}