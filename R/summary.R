#' Summary function
#' 
#' @param df dataframe of groundwater data in tidy format
#' @export

summary <- function(df, na.rm = TRUE) {
 
  df %>%
    group_by(LOCATION_ID, PARAM_NAME, DEFAULT_UNIT) %>%
    summarise(n = n(),
              percent_lt = sum(LT_MEASURE == "<", na.rm = na.rm)/n()*100,
              mean = mean(ANALYSIS_RESULT, na.rm = na.rm),
              median = median(ANALYSIS_RESULT, na.rm = na.rm),
              sd = sd(ANALYSIS_RESULT, na.rm = na.rm),
              var = var(ANALYSIS_RESULT, na.rm = na.rm),
              min = min(ANALYSIS_RESULT, na.rm = na.rm),
              max = max(ANALYSIS_RESULT, na.rm = na.rm),
              cv = cv(ANALYSIS_RESULT, na.rm = na.rm),
              IQR = IQR(ANALYSIS_RESULT, na.rm = na.rm),
              mad = mad(ANALYSIS_RESULT, na.rm = na.rm)
              )
} 
