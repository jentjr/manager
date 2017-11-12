# Normal prediction intervals --------------------------------------------------
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

test_that("normal prediction limit", {
  expect_equal(pred_int(df_norm, distribution)$upl,
               7.1847, 
               tolerance = 0.001)
})

test_that("normal simultanous prediction limit", {
  expect_equal(pred_int_sim(df_norm, distribution)$upl,
               6.401955, 
               tolerance = 0.001)
})

# Lognormal --------------------------------------------------------------------
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

test_that("lognormal prediction limit", {
  expect_equal(pred_int(df_lnorm, distribution, pi_type = "upper")$upl,
               1319.104, 
               tolerance = 0.001)
})

test_that("lognormal simultaneous prediction limit", {
  expect_equal(pred_int_sim(df_lnorm, distribution, pi_type = "upper")$upl,
               603.0231, 
               tolerance = 0.001)
})

# Nonparametric ---------------------------------------------------------------
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

test_that("non-parametric prediction limit", {
  expect_equal(pred_int(df_nonpar, distribution)$upl,
               20)
})

test_that("non-parametric prediction limit", {
  expect_equal(pred_int_sim(df_nonpar, distribution)$upl,
               20)
})
