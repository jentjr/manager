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

pred_int_sim <- function(df,
                         analysis_result = "analysis_result",
                         distribution = "distribution",
                         n_mean = 1,
                         k = 1,
                         m = 2,
                         r = 1,
                         rule = "k.of.m",
                         pi_type = "upper",
                         conf_level = 0.95) {

  pred_int <- df %>%
    mutate(pred_int = case_when(
      distribution == "Normal" ~ map(.x = data,
                                     ~EnvStats::predIntNormSimultaneous(
                                       x = .x$analysis_result,
                                       n.mean = n_mean,
                                       k = k,
                                       pi.type = pi_type,
                                       conf.level = conf_level
                                       )
      ),
      distribution == "Lognormal" ~ map(.x = data,
                                        ~EnvStats::predIntLnormSimultaneous(
                                          x = .x$analysis_result,
                                          n.geomean = n_mean,
                                          k = k,
                                          pi.type = pi_type,
                                          conf.level = conf_level
                                          )
      ),
      distribution == "Nonparametric" ~ map(.x = data,
                                            ~EnvStats::predIntNparSimultaneous(
                                              x = .x$analysis_result,
                                              pi.type = pi_type
                                              )
      )
    )
    )

  pred_int %>%
    mutate(distribution = distribution,
           sample_size = map(.x = pred_int, ~ .x$sample.size),
           lpl = map(.x = pred_int, ~ .x$interval$limits["LPL"]),
           upl = map(.x = pred_int, ~ .x$interval$limits["UPL"]),
           conf_level = map(.x = pred_int, ~ .x$interval$conf.level)) %>%
    select(-data, -pred_int) %>%
    unnest(c(sample_size, lpl, upl, conf_level))

}

#' Function to calculate prediction interval
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
#' @param method default is "Bonferroni"
#' @param pi_type character string indicating what kind of prediction interval
#' to compute. The possible values are pi_type="two-sided" (the default),
#' pi_type="upper", and pi_type="lower".
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
#' pred_int(., pi_type = "upper", conf_level = 0.99)
#' 
#' 
#' @export

pred_int <- function(df,
                     analysis_result = "analysis_result",
                     distribution = "distribution",
                     method = "Bonferroni",
                     n_mean = 1,
                     k = 1,
                     pi_type = "upper",
                     conf_level = 0.95) {

  pred_int <- df %>%
    mutate(pred_int = case_when(
      distribution == "Normal" ~ map(.x = data,
                                     ~EnvStats::predIntNorm(
                                       x = .x$analysis_result,
                                       method = method,
                                       n.mean = n_mean,
                                       k = k,
                                       pi.type = pi_type,
                                       conf.level = conf_level)
                                     ),
      distribution == "Lognormal" ~ map(.x = data,
                                        ~EnvStats::predIntLnorm(
                                          x = .x$analysis_result,
                                          method = method,
                                          n.geomean = n_mean,
                                          k = k,
                                          pi.type = pi_type,
                                          conf.level = conf_level)
                                        ),
      distribution == "Nonparametric" ~ map(.x = data,
                                            ~EnvStats::predIntNpar(
                                              x = .x$analysis_result,
                                              pi.type = pi_type)
                                            )
                      )
    )
  
  pred_int %>%
    mutate(distribution = distribution,
           sample_size = map(.x = pred_int, ~ .x$sample.size),
           lpl = map(.x = pred_int, ~ .x$interval$limits["LPL"]),
           upl = map(.x = pred_int, ~ .x$interval$limits["UPL"]),
           conf_level = map(.x = pred_int, ~ .x$interval$conf.level)) %>%
    select(-data, -pred_int) %>%
    unnest(c(sample_size, lpl, upl, conf_level))

}