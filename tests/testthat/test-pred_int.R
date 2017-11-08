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

test_that("normal prediction limit", {
  expect_equal(pred_int(df_norm, distribution)$interval$limits[["UPL"]],
               7.1847, 
               tolerance = 0.001)
})

test_that("lognormal prediction limit", {
  expect_equal(pred_int(df_lnorm, distribution)$interval$limits[["UPL"]],
               616.7976, 
               tolerance = 0.001)
})

test_that("non-parametric prediction limit", {
  expect_equal(pred_int(df_nonpar, distribution)$interval$limits[["UPL"]],
               20)
})
