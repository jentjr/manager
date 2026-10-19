test_that("est_dist retains nested data", {
  result <- gw_data %>%
    dplyr::group_by(
      location_id,
      param_name,
      default_unit
    ) %>%
    percent_lt() %>%
    est_dist()

  expect_true(
    "data" %in% names(result)
  )

  expect_true(
    inherits(result$data, "list")
  )
})

test_that("nested data contains analysis_result", {
  result <- gw_data %>%
    dplyr::group_by(
      location_id,
      param_name,
      default_unit
    ) %>%
    percent_lt() %>%
    est_dist()

  expect_true(
    all(
      purrr::map_lgl(
        result$data,
        ~ "analysis_result" %in% names(.x)
      )
    )
  )
})

test_that("est_dist output passes validation", {
  result <- gw_data %>%
    dplyr::group_by(
      location_id,
      param_name,
      default_unit
    ) %>%
    percent_lt() %>%
    est_dist()

  expect_no_error(
    .validate_est_dist_output(result)
  )
})
