#' Function to calculate simultaneous prediction interval
#'
#' @param df df data frame of groundwater data in tidy format
#' @param analysis_result the analysis result column
#' @param distribution the distribution column
#' @param n_mean n.mean positive integer specifying the sample size associated
#' with the future averages.
#' The default value is n.mean=1 (i.e., individual observations).
#' Note that all future averages must be based on the same sample size.
#' @param k k positive integer specifying the number of future observations or
#' averages the prediction interval should contain with confidence level
#' conf.level. The default value is k=1.
#' @param m m if m = 1, then same results as predInt
#' @param r r
#' @param rule rule "k.of.m"
#' @param pi_type character string indicating what kind of prediction interval
#' to compute. The possible values are pi_type="upper" (the default),
#' and pi_type="lower".
#' @param conf_level a scalar between 0 and 1 indicating the confidence level
#' of the prediction interval. The default value is conf.level=0.95
#'
#' @examples
#' data("gw_data")
#'
#' wells <- c("MW-1", "MW-2", "MW-3", "MW-4")
#'
#' params <- c("Sulfate, total",
#'             "Arsenic, dissolved",
#'             "Boron, dissolved")
#'
#' background <- lubridate::ymd(c("2007-12-20", "2012-01-01"), tz = "UTC")
#'
#' # first group data by location, param, and background
#' # estimate percent less than and distribution
#' background_data <- gw_data %>%
#'  filter(location_id %in% wells, param_name %in% params,
#'          sample_date >= background[1] & sample_date <= background[2]) %>%
#'   group_by(location_id, param_name, default_unit) %>%
#'   percent_lt() %>%
#'   est_dist(., keep_data_object = TRUE) %>%
#'   arrange(location_id, param_name)
#'
#' background_data %>%
#' pred_int_sim(., pi_type = "upper", conf_level = 0.99)
#'
#' @export

pred_int_sim <- function(
  df,
  n_mean = 1,
  k = 1,
  pi_type = "upper",
  conf_level = 0.95
) {
  .compute_pred_int(
    df = df,
    simultaneous = TRUE,
    n_mean = n_mean,
    k = k,
    pi_type = pi_type,
    conf_level = conf_level
  )
}
