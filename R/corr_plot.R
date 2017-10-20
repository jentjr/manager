#' Create a correlation plot
#'
#' @param df dataframe
#' @param location_id column of sample locations
#' @param sample_date column of sample dates
#' @param param_name column of constituents
#' @param analysis_result column of numerical results
#' @param constituents list of consituents to plot e.g. c("Iron, dissolved",
#' "Manganese, dissolved")
#' @param sample_locations list of sample locations to plot e.g.
#' c("MW-1", "MW-2")
#' @export

corr_plot <- function(df,
                      location_id = "location_id",
                      sample_date = "sample_date",
                      param_name = "param_name",
                      analysis_result = "analysis_result",
                      constituents = NULL,
                      sample_locations = NULL
                      ) {

  df %>%
    .get_corr_data(., constituents = constituents,
                   sample_locations = sample_locations) %>%
    ggpairs(columns = 3:ncol(.), aes(colour = location_id))

}

#' Function to create a correlation matrix plot

.get_corr_data <- function(df,
                           location_id = "location_id",
                           sample_date = "sample_date",
                           param_name = "param_name",
                           analysis_result = "analysis_result",
                           constituents = NULL,
                           sample_locations = NULL
                           ) {

  df <- df %>%
    filter_(~param_name %in% constituents,
            ~location_id %in% sample_locations) %>%
    spread_(param_name, analysis_result) %>%
    group_by_(~location_id, ~sample_date) %>%
    summarise_at(vars(constituents), mean, na.rm = TRUE) %>%
    ungroup()

  return(df)

}