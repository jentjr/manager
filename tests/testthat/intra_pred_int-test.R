set.seed(14)

df_norm1 <- data.frame(
  location_id = rep(1, 20),
  sample_date = seq(lubridate::ymd("2010-01-01"), 
                    by = "quarter", length.out = 20),
  param_name = rep("test", 20),
  lt_measure = rep("", 20),
  analysis_result = rnorm(20, mean = 5, sd = 1),
  default_unit = rep("mg/L", 20)
)

df_lnorm1 <- data.frame(
  location_id = rep(2, 20),
  sample_date = seq(lubridate::ymd("2010-01-01"), 
                    by = "quarter", length.out = 20),
  param_name = rep("test", 20),
  lt_measure = rep("", 20),
  analysis_result = rlnorm(20, meanlog = 5, sdlog = 1),
  default_unit = rep("mg/L", 20)
)

df_nonpar1 <- data.frame(
  location_id = rep(3, 20),
  sample_date = seq(lubridate::ymd("2010-01-01"), 
                    by = "quarter", length.out = 20),
  param_name = rep("test", 20),
  lt_measure = rep("", 20),
  analysis_result = rhyper(20, m = 30, n = 1, k = 20),
  default_unit = rep("mg/L", 20)
)

df1 <- rbind(df_norm1, df_lnorm1, df_nonpar1)
df1$location_id <- as.character(df1$location_id)
df1$default_unit <- as.character(df1$default_unit)
df1$param_name <- as.character(df1$param_name)

wells <- c("1", "2", "3")
params <- "test"
bkgd_dates <- lubridate::ymd(c("2010-01-01", "2013-10-01"))
comp_dates <- lubridate::ymd(c("2014-06-01", "2014-10-01"))

limit_test <- intra_pred_int(df1, analysis_result, wells, params, 
                             bkgd_dates, comp_dates, 
                             SWFPR = 0.1)

multi_gw_ts_plot(limit_test, limit1 = "lower_limit", limit2 = "upper_limit")
