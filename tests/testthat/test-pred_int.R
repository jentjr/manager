set.seed(14)

df_norm <- data.frame(
  LOCATION_ID = rep(1, 20),
  SAMPLE_DATE = seq(lubridate::ymd("2010-01-01", tz = Sys.timezone()), 
                    by = "quarter", length.out = 20),
  PARAM_NAME = rep("test", 20),
  LT_MEASURE = rep("", 20),
  ANALYSIS_RESULT = rnorm(20, mean = 5, sd = 1),
  DEFAULT_UNIT = rep("mg/L", 20),
  DISTRIBUTION = "norm"
)

df_lnorm <- data.frame(
  LOCATION_ID = rep(2, 20),
  SAMPLE_DATE = seq(lubridate::ymd("2010-01-01", tz = Sys.timezone()), 
                    by = "quarter", length.out = 20),
  PARAM_NAME = rep("test", 20),
  LT_MEASURE = rep("", 20),
  ANALYSIS_RESULT = rlnorm(20, meanlog = 5, sdlog = 1),
  DEFAULT_UNIT = rep("mg/L", 20),
  DISTRIBUTION = "lnorm"
)

df_nonpar <- data.frame(
  LOCATION_ID = rep(3, 20),
  SAMPLE_DATE = seq(lubridate::ymd("2010-01-01", tz = Sys.timezone()), 
                    by = "quarter", length.out = 20),
  PARAM_NAME = rep("test", 20),
  LT_MEASURE = rep("", 20),
  ANALYSIS_RESULT = rhyper(20, m = 30, n = 1, k = 20),
  DEFAULT_UNIT = rep("mg/L", 20),
  DISTRIBUTION = "none"
)

test_that("normal prediction limit", {
  expect_equal(pred_int(df_norm, DISTRIBUTION)$interval$limits[["UPL"]], 7.1847, 
               tolerance = 0.001)
})

test_that("lognormal prediction limit", {
  expect_equal(pred_int(df_lnorm, DISTRIBUTION)$interval$limits[["UPL"]], 616.7976, 
               tolerance = 0.001)
})

test_that("non-parametric prediction limit", {
  expect_equal(pred_int(df_nonpar, DISTRIBUTION)$interval$limits[["UPL"]], 20)
})

df <- bind_rows(df_norm, df_lnorm, df_nonpar)