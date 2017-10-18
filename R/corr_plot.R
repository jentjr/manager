
corr_plot <- function(df,                           
                      LOCATION_ID = "LOCATION_ID",
                      SAMPLE_DATE = "SAMPLE_DATE",
                      PARAM_NAME = "PARAM_NAME",
                      ANALYSIS_RESULT = "ANALYSIS_RESULT",
                      constituents = NULL,
                      sample_locations = NULL
                      ) {
  
  df %>%
    .get_corr_data(., constituents = constituents, 
                   sample_locations = sample_locations) %>%
    ggpairs(columns = 3:ncol(.), aes(colour = LOCATION_ID))
  
}

#' Function to create a correlation matrix plot

.get_corr_data <- function(df,
                           LOCATION_ID = "LOCATION_ID",
                           SAMPLE_DATE = "SAMPLE_DATE",
                           PARAM_NAME = "PARAM_NAME",
                           ANALYSIS_RESULT = "ANALYSIS_RESULT",
                           constituents = NULL,
                           sample_locations = NULL
                           ) {
  
  
  df <- df %>% 
    filter_(~PARAM_NAME %in% constituents, 
            ~LOCATION_ID %in% sample_locations) %>%
    spread_(PARAM_NAME, ANALYSIS_RESULT) %>%
    group_by_(~LOCATION_ID, ~SAMPLE_DATE) %>%
    summarise_at(vars(constituents), mean, na.rm = TRUE) %>%
    ungroup()
  
  return(df)
  
}
