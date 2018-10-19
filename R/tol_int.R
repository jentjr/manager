#' Function to calculate tolernace intervals
#'
#' @param df df data frame of groundwater data in tidy format
#' @param analysis_result the analysis result column
#' @param distribution the distribution column
#' @param coverage a scalar between 0 and 1 indicating the desired coverage of 
#' the tolerance interval. The default value is coverage=0.95. 
#' If cov.type="expectation", this argument is ignored
#' @param cov_type character string specifying the coverage type for the 
#' tolerance interval. The possible values are "content" 
#' (β-content; the default), and "expectation" (β-expectation).
#' @param method for the case of a two-sided tolerance interval, a character 
#' string specifying the method for constructing the tolerance interval. 
#' This argument is ignored if ti.type="lower" or ti.type="upper". 
#' The possible values are "exact" (the default) and 
#' "wald.wolfowitz" (the Wald-Wolfowitz approximation).
#' @param ti_type character string indicating what kind of tolerance interval
#' to compute. The possible values are ti_type="two-sided",
#' ti_type="upper", and ti_type="lower" (the default).
#' @param conf_level a scalar between 0 and 1 indicating the confidence level
#' of the tolerance interval. The default value is conf.level=0.95
#' @param est_method for tolIntLnormAlt, a character string specifying the 
#' method of estimating the mean and coefficient of variation. 
#' This argument has no effect on the method of constructing the 
#' tolerance interval. Possible values are "mvue" (minimum variance unbiased; 
#' the default), "qmle" (quasi maximum likelihood), "mle" (maximum likelihood),
#'  "mme" (method of moments), and "mmue" (method of moments based on the 
#'  unbiased estimate of variance). 
#' 
#' @export

tol_int <- function(df,
                    analysis_result = "analysis_result",
                    distribution = "distribution",
                    coverage = 0.95,
                    cov_type = "content",
                    ti_type = "lower",
                    conf_level = 0.95,
                    method = "exact",
                    est_method = "mvue") {
  
  tol_int <- df %>%
    mutate(tol_int = case_when(
      distribution == "Normal" ~ map(.x = data,
                                     ~EnvStats::tolIntNorm(
                                       x = .x$analysis_result,
                                       coverage = coverage,
                                       cov.type = cov_type,
                                       ti.type = ti_type,
                                       conf.level = conf_level,
                                       method = method)
      ),
      distribution == "Lognormal" ~ map(.x = data,
                                        ~EnvStats::tolIntLnormAlt(
                                          x = .x$analysis_result,
                                          coverage = coverage,
                                          cov.type = cov_type,
                                          ti.type = ti_type,
                                          conf.level = conf_level,
                                          method = method,
                                          est.method = est_method)
      ),
      distribution == "Nonparametric" ~ map(.x = data,
                                            ~EnvStats::tolIntNpar(
                                              x = .x$analysis_result,
                                              coverage = coverage,
                                              conf.level = conf_level,
                                              cov.type = cov_type,
                                              ti.type = ti_type)
      )
    )
    )
  
  tol_int %>%
    mutate(distribution = distribution,
           sample_size = map(.x = tol_int, ~ .x$sample.size),
           lpl = map(.x = tol_int, ~ .x$interval$limits["LPL"]),
           upl = map(.x = tol_int, ~ .x$interval$limits["UPL"]),
           conf_level = map(.x = tol_int, ~ .x$interval$conf.level)) %>%
    select(-data, -tol_int) %>%
    unnest()
  
}