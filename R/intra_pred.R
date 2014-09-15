#' Function to check for normal, lognormal, gamma, or non-parametric distribution
#' @param df data frame of groundwater data
#' @export

gw_gof <- function(df, nd_percent = 10, non_par_perc = 50) {
  
  lt <- percent_lt(df$lt_measure)
  dat <- df$analysis_result
  censored <- ifelse(df$lt_measure == "<", TRUE, FALSE)
  
  if (lt <= nd_percent) {
    gof <- gofTest(dat, dist = "norm")
    gof["data.name"] <- paste(df$location_id[1], df$param_name[1], sep = " ")
    
    lgof <- gofTest(dat, dist = "lnorm")
    lgof["data.name"] <- paste(df$location_id[1], df$param_name[1], sep = " ")
    
    if (gof$p.value >= 0.01) {
      out <- gof
    }
    if (gof$p.value < 0.01 & lgof$p.value >= 0.01) {
      out <- lgof
    }
    if (gof$p.value < 0.01 & lgof$p.value < 0.01) {
      non_par <- eqnpar(dat, p = 1)
      non_par["data.name"] <- paste(df$location_id[1], df$param_name[1], 
                                    sep = " ")
      out <- non_par
    }
  }
  if (lt > nd_percent & lt < non_par_perc) {
    gof <- gofTestCensored(dat, censored, dist = "norm")
    gof["data.name"] <- paste(df$location_id[1], df$param_name[1], sep = " ")
    
    lgof <- gofTestCensored(dat, censored, dist = "lnorm")
    lgof["data.name"] <- paste(df$location_id[1], df$param_name[1], sep = " ")
    
    if (gof$p.value >= 0.01) {
      out <- gof
    }
    if (gof$p.value < 0.01 & lgof$p.value >= 0.01) {
      out <- lgof
    }
    if (gof$p.value < 0.01 & lgof$p.value < 0.01) {
      non_par <- eqnpar(dat, p = 1)
      non_par["data.name"] <- paste(df$location_id[1], df$param_name[1], 
                                    sep = " ")
      out <- non_par
    }
  }
  if(lt >= non_par_perc) {
    non_par <- eqnpar(dat, p = 1)
    non_par["data.name"] <- paste(df$location_id[1], df$param_name[1], 
                                  sep = " ")
    out <- non_par
  }
  return(out)
}

#' Function to calculate intrawell prediction interval
#' @param df data frame of groundwater data
#' @param wells vector of wells to be included
#' @param params vector of constituents to be included
#' @param bkgd_dates background data date range
#' @param SWFPR Site-Wide False Positive Rate
#' @export

lim_sim <- function(x, nd_percent = 10, non_par_perc = 50, nd_method = "ROS") {
  
  lt <- percent_lt(x$lt_measure)
  dist <- gw_gof(x)$distribution
  
  if (lt < nd_percent & dist == "Normal") {
    out <- EnvStats::predIntNormSimultaneous(x$analysis_result, 
                                             k = 1, m = 2, r = 2, 
                                             rule = "k.of.m")
    out["data.name"] <- paste(x$location_id[1], x$param_name[1], sep = " ")
  }
  if (lt < nd_percent & dist == "None") {
    out <- EnvStats::predIntNparSimultaneous(x$analysis_result, 
                                             k = 1, m = 2, r = 2, 
                                             rule = "k.of.m")
    out["data.name"] <- paste(x$location_id[1], x$param_name[1], sep = " ")
  }
  if (lt < nd_percent & dist == "Lognormal") {
    out <- EnvStats::predIntLnormSimultaneous(x$analysis_result, 
                                              k = 1, m = 2, r = 2, 
                                              rule = "k.of.m")
    out["data.name"] <- paste(x$location_id[1], x$param_name[1], sep = " ")
  }
  if (lt > nd_percent & lt < non_par_perc & nd_method == "ROS") {
    
  }
  if (lt > nd_percent & lt < non_par_perc & nd_method == "Kaplan-Meier") {
    
  }
  if (lt > non_par_perc) {
    out <- EnvStats::predIntNparSimultaneous(x$analysis_result, 
                                             k = 1, m = 2, r = 2, 
                                             rule = "k.of.m")
    out["data.name"] <- paste(x$location_id[1], x$param_name[1], sep = " ")
  }
  return(out)
}

#' Function to calculate intrawell prediction interval
#' @param df data frame of groundwater data
#' @param wells vector of wells to be included
#' @param params vector of constituents to be included
#' @param bkgd_dates background data date range
#' @param SWFPR Site-Wide False Positive Rate
#' @export

intra_pred <- function(df, wells, params, bkgd_dates, comp_dates, SWFPR = 0.1){
  
  df <- df[df$location_id %in% wells & df$param_name %in% params & 
           df$sample_date >= bkgd_dates[1] & df$sample_date <= bkgd_dates[2], ]
  
  nw <- length(wells)
  nc <- length(params)
  SWFPR <- SWFPR
  conf.level <- (1 - SWFPR)^(1/(nc*nw))
  
  
#   limits <- plyr::ddply(df, .(location_id, param_name), lim_sim)
  limits <- plyr::dlply(df, .(location_id, param_name), lim_sim, 
                        conf.level = conf.level)
  return(limits)
}


