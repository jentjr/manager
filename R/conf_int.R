#' Function to calculate confidence intervals
#'
#' @param df df data frame of groundwater data in tidy format
#' @param analysis_result the analysis result column
#' @param distribution the distribution column
#' @param ci_type character string indicating what kind of confidence interval
#' to compute. The possible values are ci_type="two-sided" (the default),
#' ci_type="lower", and ci_type = "upper
#' @param conf_level a scalar between 0 and 1 indicating the confidence level
#' of the prediction interval. The default value is conf_level = 0.95
#' @param method character string specifying the method of estimation.
#' Possible values are "mvue" (minimum variance unbiased; the default),
#' and "mle/mme" (maximum likelihood/method of moments).
#'
#' @examples
#' data("gw_data")

#' wells <- c("MW-1", "MW-2", "MW-3", "MW-4")

#' params <- c("Sulfate, total",
#'             "Arsenic, dissolved",
#'             "Boron, dissolved")
#'
#' background <- lubridate::ymd(c("2007-12-20", "2012-01-01"), tz = "UTC")

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
#' conf_int(., ci_type = "lower", conf_level = 0.99)
#'
#' @export

conf_int <- function(df,
                     analysis_result = "analysis_result",
                     distribution = "distribution",
                     method = "mvue",
                     ci_type = "two-sided",
                     conf_level = 0.95) {

  conf_int <- df %>%
    mutate(conf_int = case_when(
      distribution == "Normal" ~ map(.x = data,
                                     ~EnvStats::enorm(
                                       x = .x$analysis_result,
                                       method = method,
                                       ci = TRUE,
                                       ci.type = ci_type,
                                       conf.level = conf_level,
                                       ci.param = "mean")
                                     ),
      distribution == "Lognormal" ~ map(.x = data,
                                        ~EnvStats::elnormAlt(
                                          x = .x$analysis_result,
                                          ci = TRUE,
                                          ci.type = ci_type,
                                          ci.method = "land",
                                          conf.level = conf_level)
                                        ),
      distribution == "Nonparametric" ~ map(.x = data,
                                            ~EnvStats::eqnpar(
                                              x = .x$analysis_result,
                                              ci = TRUE,
                                              ci.type = ci_type,
                                              ci.method = "interpolate",
                                              approx.conf.level = conf_level)
                                            )
                       )
    )

  conf_int %>%
    mutate(distribution = distribution,
           sample_size = map(.x = conf_int, ~ .x$sample.size),
           lcl = map(.x = conf_int, ~ round(.x$interval$limits["LCL"], 3)),
           ucl = map(.x = conf_int, ~ .x$interval$limits["UCL"]),
           conf_level = map(.x = conf_int, ~ .x$interval$conf.level)) %>%
    select(-data, -conf_int) %>%
    unnest()

}