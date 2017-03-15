set.seed(14)

df_norm1 <- data.frame(
  location_id = rep(1, 20),
  sample_date = seq(lubridate::ymd("2010-01-01", tz = Sys.timezone()), 
                    by = "quarter", length.out = 20),
  param_name = rep("test", 20),
  lt_measure = rep("", 20),
  analysis_result = rnorm(20, mean = 5, sd = 1),
  default_unit = rep("mg/L", 20)
)

df_lnorm1 <- data.frame(
  location_id = rep(2, 20),
  sample_date = seq(lubridate::ymd("2010-01-01", tz = Sys.timezone()), 
                    by = "quarter", length.out = 20),
  param_name = rep("test", 20),
  lt_measure = rep("", 20),
  analysis_result = rlnorm(20, meanlog = 5, sdlog = 1),
  default_unit = rep("mg/L", 20)
)

df_nonpar1 <- data.frame(
  location_id = rep(3, 20),
  sample_date = seq(lubridate::ymd("2010-01-01", tz = Sys.timezone()), 
                    by = "quarter", length.out = 20),
  param_name = rep("test", 20),
  lt_measure = rep("", 20),
  analysis_result = rhyper(20, m = 30, n = 1, k = 20),
  default_unit = rep("mg/L", 20)
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