#' calculate the percentage of non-detects
#' 
#' @param df data frame of groundwater monitoring data in long format
#' @param lt column of non-detects. Default is set to lt_measure which has 
#' the "<" symbol.
#' @export


percent_lt <- function(lt) {
  yes <- length(lt[lt == "<"])
  total <- length(lt)
  p <- (yes / total) * 100
  return(p)
}


#' function to remove duplicate samples
#' Example: If you have wells named MW-1 and another named MW-1 Duplicate
#' this function will remove the MW-1 Duplicate sample
#'
#'@param df data frame of groundwater data in long format with location_id as the column 
#' name for monitoring wells
#' @export

remove_dup <- function(df){
  df_nodup <- df[-grep("*Dup", df$location_id), ]
  return(df_nodup)
}

#' Calculate intrawell prediction limit
#'
#' @param df data frame of groundwater monitoring data
#' @param back_datas vector of background dates
#' @param comp_dates vector of compliance dates
#' @param num_wells number of wells
#' @param num_params number of parameters 
#' @param m type of 1-of-m retesting scheme (usually m= 1, 2, 3, or 4)
#' @param swfpr site-wide-false-positive-rate, default is 0.05
#' @param ne number of yearly evaluations (4 = quarterly, 2 = semi-anually, 1 = annually)
#' @param ord order of the mean to be predicted (for tests on observations, set ord = 1)
#' @export 

intrawell_prediction <- function(df, back_dates, comp_dates, num_wells, num_params, m, 
                                 swfpr = 0.05, ne = 2, ord = 1){
  back_data <- subset(df, sample_date >= back_dates[1] & sample_date <= back_dates[2])
  back_mean <- mean(df$analysis_result, na.rm = TRUE)
  back_sd <- sd(df$analysis_result, na.rm = TRUE)
  
  n_back <- nrow(back_data)
  
  kappa <- calc_kappa(n = n_back, w = num_wells, coc = num_params, m = m, 
                      swfpr = swfpr, ne = ne, ord = ord)$kappa
  
  upl <- back_mean + kappa * sqrt(1 + 1 / n_back) * back_sd
    
  return(upl)

}


#' Function to summarize the number of samples, mean, sd, and percentage of 
#' non-detects. This is useful for calculating the upper prediction limit.
#' 
#' @param df data frame of groundwater monitoring network data 
#' @export

groundwater_summary <- function(df){
  
  gw <- ddply(df, .(location_id, param_name, default_unit), summarise, 
              n = length(analysis_result),
              mean = round(mean(analysis_result), digits = 3), 
              sd = round(sd(analysis_result), digits = 3),
              percent_lt = round(percent_lt(lt_measure), digits = 3))
  
  return(gw)
  
}

#' function to convert geochemical plot data into meq/L

convert_mgL_to_meqL <- function(df, Mg, Ca, Na, K, Cl, SO4, CO3, HCO3){
  
  # formuala weights
  Ca_fwt <- 40.078
  Mg_fwt <- 24.305
  Na_fwt <- 22.990
  K_fwt <- 39.098
  S_fwt <- 32.06
  O_fwt <- 15.999
  H_fwt <- 1.008
  C_fwt <- 12.011
  Cl_fwt <- 35.45
  
  # absolute value of charge
  Ca_chrg <- 2
  Mg_chrg <- 2
  Na_chrg <- 1
  K_chrg <- 1
  SO4_chrg <- 2
  CO3_chrg <- 2
  HCO3_chrg <- 1
  Cl_chrg <- 1
  
  # conversion 
  df$Mg <- Mg / Mg_fwt * Mg_chrg
  df$Ca <- Ca / Ca_fwt * Ca_chrg
  df$Na <- Na / Na_fwt * Na_chrg
  df$K <- K / K_fwt * K_chrg
  df$Cl <- Cl / Cl_fwt * Cl_chrg
  df$SO4 <- SO4 / (S_fwt + 4 * O_fwt) * SO4_chrg
  df$CO3 <- CO3 / (C_fwt + 3 * O_fwt) * CO3_chrg
  df$HCO3 <- HCO3 / (H_fwt + C_fwt + 3 * O_fwt) * HCO3_chrg
  
  return(df)
}