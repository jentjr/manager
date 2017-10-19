set.seed(14)

df_norm1 <- data.frame(
  LOCATION_ID = rep(1, 20),
  SAMPLE_DATE = seq(lubridate::ymd("2010-01-01"), 
                    by = "quarter", length.out = 20),
  PARAM_NAME = rep("test", 20),
  LT_MEASURE = rep("", 20),
  ANALYSIS_RESULT = rnorm(20, mean = 5, sd = 1),
  DEFAULT_UNIT = rep("mg/L", 20)
)

df_lnorm1 <- data.frame(
  LOCATION_ID = rep(2, 20),
  SAMPLE_DATE = seq(lubridate::ymd("2010-01-01"), 
                    by = "quarter", length.out = 20),
  PARAM_NAME = rep("test", 20),
  LT_MEASURE = rep("", 20),
  ANALYSIS_RESULT = rlnorm(20, meanlog = 5, sdlog = 1),
  DEFAULT_UNIT = rep("mg/L", 20)
)

df_nonpar1 <- data.frame(
  LOCATION_ID = rep(3, 20),
  SAMPLE_DATE = seq(lubridate::ymd("2010-01-01"), 
                    by = "quarter", length.out = 20),
  PARAM_NAME = rep("test", 20),
  LT_MEASURE = rep("", 20),
  ANALYSIS_RESULT = rhyper(20, m = 30, n = 1, k = 20),
  DEFAULT_UNIT = rep("mg/L", 20)
  )

test_that("data are normal", { 
  expect_true(is_normal(df_norm1$ANALYSIS_RESULT))
  expect_match(dist(df_norm1$ANALYSIS_RESULT), "norm")
})

test_that("data are lognormal", {
  expect_true(is_lognormal(df_lnorm1$ANALYSIS_RESULT))
  expect_match(dist(df_lnorm1$ANALYSIS_RESULT), "lnorm")
})

test_that("data are neither normal, nor lognormal", {
  expect_false(is_normal(df_nonpar1$ANALYSIS_RESULT))
  expect_false(is_lognormal(df_nonpar1$ANALYSIS_RESULT))
  expect_match(dist(df_nonpar1$ANALYSIS_RESULT), "none")
})

df1 <- rbind(df_norm1, df_lnorm1, df_nonpar1)

df1_est_dist <- est_dist(df1)

norm_result <- df1_est_dist %>%
  filter(LOCATION_ID == 1) %>%
  select(distribution)

lnorm_result <- df1_est_dist %>%
  filter(LOCATION_ID == 2) %>%
  select(distribution)

npar_result <- df1_est_dist %>%
  filter(LOCATION_ID == 3) %>%
  select(distribution)

test_that("normality check works on gw data.frame",{
  expect_match(norm_result$distribution, "norm")
  expect_match(lnorm_result$distribution, "lnorm")
  expect_match(npar_result$distribution, "none")
})
