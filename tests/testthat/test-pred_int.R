set.seed(14)

df_norm1 <- data.frame(
  LOCATION_ID = rep(1, 20),
  SAMPLE_DATE = seq(lubridate::ymd("2010-01-01", tz = Sys.timezone()), 
                    by = "quarter", length.out = 20),
  PARAM_NAME = rep("test", 20),
  LT_MEASURE = rep("", 20),
  ANALYSIS_RESULT = rnorm(20, mean = 5, sd = 1),
  DEFAULT_UNIT = rep("mg/L", 20)
)

df_lnorm1 <- data.frame(
  LOCATION_ID = rep(2, 20),
  SAMPLE_DATE = seq(lubridate::ymd("2010-01-01", tz = Sys.timezone()), 
                    by = "quarter", length.out = 20),
  PARAM_NAME = rep("test", 20),
  LT_MEASURE = rep("", 20),
  ANALYSIS_RESULT = rlnorm(20, meanlog = 5, sdlog = 1),
  DEFAULT_UNIT = rep("mg/L", 20)
)

df_nonpar1 <- data.frame(
  LOCATION_ID = rep(3, 20),
  SAMPLE_DATE = seq(lubridate::ymd("2010-01-01", tz = Sys.timezone()), 
                    by = "quarter", length.out = 20),
  PARAM_NAME = rep("test", 20),
  LT_MEASURE = rep("", 20),
  ANALYSIS_RESULT = rhyper(20, m = 30, n = 1, k = 20),
  DEFAULT_UNIT = rep("mg/L", 20)
)

# test_that("normal prediction limit", {
#   expect_equal(pred_int_norm(df_norm1)$upl[1], 7.18)
# })

# test_that("lognormal prediction limit", {
#   expect_that(pred_int(df_lnorm1$analysis_result)$interval$limits[["UPL"]],
#               equals(526.84205))
# })

# test_that("non-parametric prediction limit", {
#   expect_that(pred_int(df_nonpar1$analysis_result)$interval$limits[["UPL"]],
#               equals(20))
# })