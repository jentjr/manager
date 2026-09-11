# Extracted from test-pred_int.R:84

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "manager", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
set.seed(14)
df_norm <- data.frame(
  location_id = rep(1, 20),
  sample_date = seq(lubridate::ymd("2010-01-01", tz = Sys.timezone()),
                    by = "quarter", length.out = 20),
  param_name = rep("test", 20),
  lt_measure = rep("", 20),
  analysis_result = rnorm(20, mean = 5, sd = 1),
  default_unit = rep("mg/L", 20),
  distribution = "Normal"
)
df_norm <-  df_norm %>%
  group_by(location_id, param_name, default_unit, distribution) %>%
  nest()
set.seed(14)
df_lnorm <- data.frame(
  location_id = rep(2, 20),
  sample_date = seq(lubridate::ymd("2010-01-01", tz = Sys.timezone()),
                    by = "quarter", length.out = 20),
  param_name = rep("test", 20),
  lt_measure = rep("", 20),
  analysis_result = rlnorm(20, meanlog = 5, sdlog = 1),
  default_unit = rep("mg/L", 20),
  distribution = "Lognormal"
)
df_lnorm <-  df_lnorm %>%
  group_by(location_id, param_name, default_unit, distribution) %>%
  nest()
df_nonpar <- data.frame(
  location_id = rep(3, 20),
  sample_date = seq(lubridate::ymd("2010-01-01", tz = Sys.timezone()),
                    by = "quarter", length.out = 20),
  param_name = rep("test", 20),
  lt_measure = rep("", 20),
  analysis_result = rhyper(20, m = 30, n = 1, k = 20),
  default_unit = rep("mg/L", 20),
  distribution = "Nonparametric"
)
df_nonpar <-  df_nonpar %>%
  group_by(location_id, param_name, default_unit, distribution) %>%
  nest()

# test -------------------------------------------------------------------------
expect_equal(pred_int_sim(df_nonpar, distribution)$upl,
               20)
