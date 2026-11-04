# test-pred_int.R

background_data <- tibble::tibble(
  location_id = rep("MW-1", 20),
  param_name = rep("Benzene", 20),
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
  )
) |>
  dplyr::group_by(
    location_id,
    param_name,
    default_unit
  ) |>
  est_dist(
    return_result = TRUE
  )

test_that("pred_int returns a tibble", {
  result <- pred_int(
    background_data
  )

  expect_s3_class(
    result,
    "tbl_df"
  )
})

test_that("pred_int returns expected columns", {
  result <- pred_int(
    background_data
  )

  expect_true(
    all(
      c(
        "distribution",
        "sample_size",
        "conf_level",
        "lpl",
        "upl"
      ) %in%
        names(result)
    )
  )
})

test_that("pred_int upper intervals are ordered correctly", {
  result <- pred_int(
    background_data,
    pi_type = "upper"
  )

  expect_true(
    all(
      result$upl >= result$lpl,
      na.rm = TRUE
    )
  )
})

test_that("pred_int lower intervals are ordered correctly", {
  result <- pred_int(
    background_data,
    pi_type = "lower"
  )

  expect_true(
    all(
      result$upl >= result$lpl,
      na.rm = TRUE
    )
  )
})

test_that("pred_int two-sided returns interval limits", {
  result <- pred_int(
    background_data,
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

test_that("pred_int matches internal implementation", {
  expected <- .compute_pred_int(
    df = background_data,
    simultaneous = FALSE
  )

  actual <- pred_int(
    background_data
  )

  expect_equal(
    actual,
    expected
  )
})

test_that("pred_int propagates confidence level validation", {
  expect_error(
    pred_int(
      background_data,
      conf_level = 0
    ),
    "conf"
  )

  expect_error(
    pred_int(
      background_data,
      conf_level = 1.1
    ),
    "conf"
  )
})

test_that("pred_int propagates n_mean validation", {
  expect_error(
    pred_int(
      background_data,
      n_mean = 0
    )
  )

  expect_error(
    pred_int(
      background_data,
      n_mean = -1
    )
  )
})

test_that("pred_int propagates k validation", {
  expect_error(
    pred_int(
      background_data,
      k = 0
    )
  )

  expect_error(
    pred_int(
      background_data,
      k = -1
    )
  )
})

test_that("pred_int propagates pi_type validation", {
  expect_error(
    pred_int(
      background_data,
      pi_type = "invalid"
    )
  )
})
