data("gw_data")

# Antimony in data frame is in units of mg/L, the standards_df table is in ug/L
# Check to make sure the units are converted properly
default_unit <- gw_data %>%
  dplyr::filter(location_id == "MW-1", param_name == "Antimony, dissolved",
         sample_date == lubridate::ymd("2008-10-28")) %>%
  dplyr::mutate(analysis_result = 0.005, default_unit = "mg/L")

gwps_unit <- gw_data %>%
  dplyr::filter(location_id == "MW-1", param_name == "Antimony, dissolved",
         sample_date == lubridate::ymd("2008-10-28")) %>%
  assign_limits()

test_that("units convert to default", {
  expect_equal(default_unit$default_unit, "mg/L")
  expect_equal(default_unit$analysis_result, 0.005)
  expect_equal(gwps_unit$analysis_result, 5)
  expect_equal(gwps_unit$default_unit, "ug/L")
  expect_equal(gwps_unit$gwps, 6)
})

gwps_params <- c("Arsenic, dissolved",
                 "Antimony, dissolved",
                 "Lithium, dissolved",
                 "Molybdenum, dissolved")

gw_data %>%
  dplyr::filter(location_id == "MW-1", param_name %in% gwps_params)
