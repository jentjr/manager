#' Summary function
#'
#' @param df dataframe of groundwater data in tidy format
#' @param location_id column of sample locations
#' @param param_name column of constituents
#' @param lt_measure column of less than symbols for non-detect data
#' @param analysis_result column for numerical result
#' @param default_unit column of units
#' @param na.rm TRUE/FALSE for missing values
#' @export

summary <- function(df,
                    location_id = "location_id",
                    param_name = "param_name",
                    lt_measure = "lt_measure",
                    analysis_result = "analysis_result",
                    default_unit = "default_unit",
                    na.rm = TRUE) {

  df %>%
    group_by_(~location_id, ~param_name, ~default_unit) %>%
    summarise(n = n(),
              percent_lt = sum(lt_measure == "<", na.rm = na.rm) / n() * 100,
              mean = mean(analysis_result, na.rm = na.rm),
              median = median(analysis_result, na.rm = na.rm),
              sd = sd(analysis_result, na.rm = na.rm),
              var = var(analysis_result, na.rm = na.rm),
              min = min(analysis_result, na.rm = na.rm),
              max = max(analysis_result, na.rm = na.rm),
              cv = cv(analysis_result, na.rm = na.rm),
              `25%` = quantile(analysis_result, na.rm = na.rm)[2][[1]],
              `75%` = quantile(analysis_result, na.rm = na.rm)[4][[1]],
              IQR = IQR(analysis_result, na.rm = na.rm),
              mad = mad(analysis_result, na.rm = na.rm)
              )
}