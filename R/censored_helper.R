#' Function to ensure non-detect data are set to either the PQL, or RL
#' 
#' @param df data frame of groundwater data in tidy format
#' @param data_qualifier column for data validation qualifier(i.e. U, or J Flag)
#' @param analysis_result column for analysis results
#' @param censor_column column of the censoring limit
#' @export

set_censored <- function(df, 
                         data_qualifer = "data_qualifier",
                         analysis_result = "analysis_result",
                         censor_column = "RL"){
  
  df %>%
    mutate(
      analysis_result = if_else(
        data_qualifier == "J", RL, analysis_result, missing = analysis_result)
      ) %>% 
    mutate(analysis_result = if_else(
      data_qualifier == "U", RL, analysis_result, missing = analysis_result)
      )

}
