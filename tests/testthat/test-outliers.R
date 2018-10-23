data(gw_data)

high_low <- gw_data %>%
  filter(location_id == "MW-1", param_name == "Chloride, total") %>%
  tukey_outlier(., type = 7)

test_that("Tukey outlier test for high and low", {
  expect_true(high_low[which(high_low$analysis_result == 30), ]$outlier)
  expect_true(high_low[which(high_low$analysis_result == 13), ]$outlier)
  expect_false(any(filter(high_low, !(analysis_result %in% c(30, 13)))$outlier))
})
