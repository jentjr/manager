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

test_that("data are normal", { 
  expect_true(is_normal(df_norm1$analysis_result))
  expect_match(dist(df_norm1$analysis_result), "norm")
})

test_that("data are lognormal", {
  expect_true(is_lognormal(df_lnorm1$analysis_result))
  expect_match(dist(df_lnorm1$analysis_result), "lnorm")
})

test_that("data are neither normal, nor lognormal", {
  expect_false(is_normal(df_nonpar1$analysis_result))
  expect_false(is_lognormal(df_nonpar1$analysis_result))
  expect_match(dist(df_nonpar1$analysis_result), "none")
})

df1 <- rbind(df_norm1, df_lnorm1, df_nonpar1)

test_that("normality check works on gw data.frame",{
  expect_match(est_dist(df1)$distribution[1], "norm")
  expect_match(est_dist(df1)$distribution[2], "lnorm")
  expect_match(est_dist(df1)$distribution[3], "none")
})
