#' Summary function
#' 
#' @param df dataframe of groundwater data in tidy format
#' @export

summary <- function(df, na.rm = TRUE) {
 
  df %>%
    group_by(location_id, param_name) %>%
    summarise(n = n(),
              percent_lt = sum(lt_measure == "<", na.rm = na.rm)/n()*100,
              mean = mean(analysis_result, na.rm = na.rm),
              median = median(analysis_result, na.rm = na.rm),
              sd = sd(analysis_result, na.rm = na.rm),
              min = min(analysis_result, na.rm = na.rm),
              max = max(analysis_result, na.rm = na.rm),
              cv = cv(analysis_result, na.rm = na.rm)
              )
} 
