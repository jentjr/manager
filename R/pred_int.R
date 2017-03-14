#' Function to calculate prediction interval 
#' @param df df data frame of groundwater data in tidy format
#' @param n.mean n.mean positive integer specifying the sample size associated 
#' with the future averages. 
#' The default value is n.mean=1 (i.e., individual observations). 
#' Note that all future averages must be based on the same sample size.
#' @param k k positive integer specifying the number of future observations or
#' averages the prediction interval should contain with confidence level 
#' conf.level. The default value is k=1.
#' @param m m if m = 1, then same results as predInt
#' @param r r
#' @param rule rule "k.of.m"
#' @param pi.type character string indicating what kind of prediction interval 
#' to compute. The possible values are pi.type="upper" (the default), 
#' and pi.type="lower".
#' @param conf.level a scalar between 0 and 1 indicating the confidence level 
#' of the prediction interval. The default value is conf.level=0.95
#' @export

pred_int_norm <- function(df,
                          analysis_result,
                          n.mean = 1, 
                          k = 1, 
                          m = 1,
                          r = 1, 
                          rule = "k.of.m",
                          pi.type = "upper", 
                          conf.level = 0.95) {

  pred_int_result <- df %>%
    do(pred_int = EnvStats::predIntNormSimultaneous(
        .$analysis_result, 
        n.mean = n.mean, 
        k = k, 
        m = m, 
        r = r,
        rule = rule,
        pi.type = pi.type, 
        conf.level = conf.level
        )
      )
  
  # df$assumed_dist <- pred_int_result$pred_int[[1]]$distribution
  df$upl <- pred_int_result$pred_int[[1]]$interval$limits[["UPL"]]
  df$lpl <- pred_int_result$pred_int[[1]]$interval$limits[["LPL"]]
  
  return(df)
  
}