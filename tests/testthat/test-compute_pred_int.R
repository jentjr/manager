# test-compute_pred_int.R

background_data <- tibble::tibble(
  location_id = rep("MW-1", 20),
  sample_date = seq.Date(
    as.Date("2020-01-01"),
    by = "quarter",
    length.out = 20
  ),
  param_name = rep("Chloride", 20),
  default_unit = rep("mg/L", 20),
  analysis_result = c(
    4.1,
    4.2,
    4.5,
    4.7,
    4.9,
    5.0,
    5.1,
    5.2,
    5.4,
    5.5,
    5.7,
    5.8,
    5.9,
    6.0,
    6.1,
    6.2,
    6.4,
    6.5,
    6.8,
    7.0
  ),
  censored = FALSE
) |>
  env_schema(
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit
  ) |>
  dplyr::group_by(
    location_id,
    param_name,
    default_unit
  ) |>
  estimate_distribution()


test_that("errors for invalid confidence level", {
  expect_error(
    .compute_pred_int(
      df = background_data,
      simultaneous = FALSE,
      conf_level = 0
    ),
    "conf"
  )

  expect_error(
    .compute_pred_int(
      df = background_data,
      simultaneous = FALSE,
      conf_level = 1.1
    ),
    "conf"
  )
})

test_that("errors for invalid prediction interval type", {
  expect_error(
    .compute_pred_int(
      df = background_data,
      simultaneous = FALSE,
      pi_type = "invalid"
    )
  )
})

test_that("errors for invalid n_mean", {
  expect_error(
    .compute_pred_int(
      df = background_data,
      simultaneous = FALSE,
      n_mean = 0
    )
  )

  expect_error(
    .compute_pred_int(
      df = background_data,
      simultaneous = FALSE,
      n_mean = -1
    )
  )
})

test_that("errors for invalid k", {
  expect_error(
    .compute_pred_int(
      df = background_data,
      simultaneous = FALSE,
      k = 0
    )
  )

  expect_error(
    .compute_pred_int(
      df = background_data,
      simultaneous = FALSE,
      k = -1
    )
  )
})

test_that("returns a tibble", {
  result <- .compute_pred_int(
    df = background_data,
    simultaneous = FALSE
  )

  expect_s3_class(
    result,
    "tbl_df"
  )
})

test_that("returns expected output columns", {
  result <- .compute_pred_int(
    df = background_data,
    simultaneous = FALSE
  )

  expect_true(
    "distribution" %in% names(result)
  )

  expect_true(
    "sample_size" %in% names(result)
  )

  expect_true(
    "conf_level" %in% names(result)
  )

  expect_true(
    "lpl" %in% names(result)
  )

  expect_true(
    "upl" %in% names(result)
  )
})

test_that("upper prediction intervals are ordered correctly", {
  result <- .compute_pred_int(
    df = background_data,
    simultaneous = FALSE,
    pi_type = "upper"
  )

  expect_true(
    all(
      result$upl >= result$lpl,
      na.rm = TRUE
    )
  )
})

test_that("lower prediction intervals are ordered correctly", {
  result <- .compute_pred_int(
    df = background_data,
    simultaneous = FALSE,
    pi_type = "lower"
  )

  expect_true(
    all(
      result$upl >= result$lpl,
      na.rm = TRUE
    )
  )
})

test_that("two-sided intervals return lower and upper limits", {
  result <- .compute_pred_int(
    df = background_data,
    simultaneous = FALSE,
    pi_type = "two-sided"
  )

  expect_true(
    "lpl" %in% names(result)
  )

  expect_true(
    "upl" %in% names(result)
  )

  expect_true(
    all(
      result$upl >= result$lpl,
      na.rm = TRUE
    )
  )
})

test_that("simultaneous prediction intervals run", {
  result <- .compute_pred_int(
    df = background_data,
    simultaneous = TRUE
  )

  expect_s3_class(
    result,
    "tbl_df"
  )

  expect_gt(
    nrow(result),
    0
  )
})

test_that("estimate_distribution includes decision metadata", {
  expect_true(
    "distribution_method" %in% names(background_data)
  )

  expect_true(
    "percent_censored" %in% names(background_data)
  )

  expect_true(
    "distribution_config" %in% names(background_data)
  )

  expect_true(
    "interval_config" %in% names(background_data)
  )
})
