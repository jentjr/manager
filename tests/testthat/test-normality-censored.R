# Censored Normal Data ---------------------------------------------------------
set.seed(14)
df_norm1_cen <- data.frame(
  location_id = rep(1, 20),
  sample_date = seq(lubridate::ymd("2010-01-01"), 
                    by = "quarter", length.out = 20),
  param_name = rep("test", 20),
  analysis_result = rnorm(20, mean = 7, sd = 2.5),
  default_unit = rep("mg/L", 20)
)

df_norm1_cen <- df_norm1_cen %>%
  mutate(lt_measure = if_else(analysis_result < 8, "<", ""))

df_norm1_cen <- df_norm1_cen %>%
  mutate(analysis_result = if_else(
    lt_measure == "<", 8, analysis_result, missing = analysis_result
    )
  ) %>%
  to_censored() %>%
  percent_lt()

test_that("censored data are normal", { 
  expect_match(est_dist_censored(df_norm1_cen, left_censored)$distribution,
               "Normal")
})

# Censored Lognormal Data ------------------------------------------------------
set.seed(1444)
df_lnorm1_cen <- data.frame(
  location_id = rep(2, 20),
  sample_date = seq(lubridate::ymd("2010-01-01"), 
                    by = "quarter", length.out = 20),
  param_name = rep("test", 20),
  lt_measure = rep("", 20),
  analysis_result = rlnormAlt(20),
  default_unit = rep("mg/L", 20)
)

df_lnorm1_cen <- df_lnorm1_cen %>%
  mutate(lt_measure = if_else(analysis_result < 1, "<", ""))

df_lnorm1_cen <- df_lnorm1_cen %>%
  mutate(analysis_result = if_else(
    lt_measure == "<", 1, analysis_result, missing = analysis_result
  )
  ) %>%
  to_censored() %>%
  percent_lt()

test_that("censored data are lognormal", {
  expect_match(est_dist_censored(df_lnorm1_cen, left_censored)$distribution,
               "Lognormal")
})

# Censored Non-parametric Data -------------------------------------------------
set.seed(155)
df_nonpar1_cen <- data.frame(
  location_id = rep(3, 20),
  sample_date = seq(lubridate::ymd("2010-01-01"), 
                    by = "quarter", length.out = 20),
  param_name = rep("test", 20),
  lt_measure = rep("", 20),
  analysis_result = rhyper(20, m = 30, n = 15, k = 20),
  default_unit = rep("mg/L", 20)
)

df_nonpar1_cen <- df_nonpar1_cen %>%
  mutate(lt_measure = if_else(analysis_result < 13, "<", ""))

df_nonpar1_cen <- df_nonpar1_cen %>%
  to_censored() %>%
  percent_lt()

test_that("censored data are neither normal, nor lognormal", {
  expect_match(est_dist_censored(df_nonpar1_cen, left_cesored)$distribution,
               "Nonparametric")
})

# Test Data Frame Case ---------------------------------------------------------
df1 <- rbind(df_norm1_cen, df_lnorm1_cen, df_nonpar1_cen)

df1 <- est_dist_censored(df1, left_censored)

norm_result <- df1 %>%
  filter(location_id == 1) %>%
  select(distribution)

lnorm_result <- df1 %>%
  filter(location_id == 2) %>%
  select(distribution)

npar_result <- df1 %>%
  filter(location_id == 3) %>%
  select(distribution)

test_that("normality check works on data.frame",{
  expect_match(norm_result$distribution, "Normal")
  expect_match(lnorm_result$distribution, "Lognormal")
  expect_match(npar_result$distribution, "Nonparametric")
})

