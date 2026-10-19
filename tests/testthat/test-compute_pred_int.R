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
      k = -5
    )
  )
})

test_that("returns a tibble", {
  result <- .compute_pred_int(
    df = background_data,
    simultaneous = FALSE
  )

  expect_s3_class(result, "tbl_df")
})

test_that("returns required prediction interval columns", {
  result <- .compute_pred_int(
    df = background_data,
    simultaneous = FALSE
  )

  expect_true("prediction_interval" %in% names(result))
  expect_true("analysis_result" %in% names(result))
})

test_that("upper prediction intervals exceed estimated center", {
  result <- .compute_pred_int(
    df = background_data,
    simultaneous = FALSE,
    pi_type = "upper"
  )

  expect_true(
    all(result$prediction_interval > result$mean, na.rm = TRUE)
  )
})

test_that("lower prediction intervals are below estimated center", {
  result <- .compute_pred_int(
    df = background_data,
    simultaneous = FALSE,
    pi_type = "lower"
  )

  expect_true(
    all(result$prediction_interval < result$mean, na.rm = TRUE)
  )
})

test_that("two-sided intervals return lower and upper limits", {
  result <- .compute_pred_int(
    df = background_data,
    simultaneous = FALSE,
    pi_type = "two-sided"
  )

  expect_true("lower_limit" %in% names(result))
  expect_true("upper_limit" %in% names(result))

  expect_true(
    all(result$lower_limit < result$upper_limit)
  )
})
