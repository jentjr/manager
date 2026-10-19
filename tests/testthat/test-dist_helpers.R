test_that("validate_required_cols succeeds when columns exist", {
  df <- tibble::tibble(
    x = 1,
    y = 2
  )

  expect_invisible(
    .validate_required_cols(
      data = df,
      cols = c("x", "y")
    )
  )
})

test_that("validate_required_cols errors for missing columns", {
  df <- tibble::tibble(
    x = 1
  )

  expect_error(
    .validate_required_cols(
      data = df,
      cols = c("x", "y")
    ),
    "Missing required column"
  )
})

test_that("validate_est_dist_args accepts valid arguments", {
  expect_invisible(
    .validate_est_dist_args(
      alpha = 0.05,
      method = "sw",
      choices = c("norm", "lnorm"),
      return_result = FALSE
    )
  )
})

test_that("validate_est_dist_args rejects invalid alpha", {
  expect_error(
    .validate_est_dist_args(
      alpha = 0,
      method = "sw",
      choices = c("norm"),
      return_result = FALSE
    ),
    "alpha"
  )

  expect_error(
    .validate_est_dist_args(
      alpha = 1,
      method = "sw",
      choices = c("norm"),
      return_result = FALSE
    ),
    "alpha"
  )
})

test_that("validate_est_dist_args rejects invalid method", {
  expect_error(
    .validate_est_dist_args(
      alpha = 0.05,
      method = c("sw", "sf"),
      choices = c("norm"),
      return_result = FALSE
    ),
    "method"
  )
})

test_that("validate_est_dist_args rejects empty choices", {
  expect_error(
    .validate_est_dist_args(
      alpha = 0.05,
      method = "sw",
      choices = character(),
      return_result = FALSE
    ),
    "choices"
  )
})

test_that("validate_est_dist_args rejects invalid return_result", {
  expect_error(
    .validate_est_dist_args(
      alpha = 0.05,
      method = "sw",
      choices = c("norm"),
      return_result = "TRUE"
    ),
    "return_result"
  )
})

test_that("make_dist_result returns expected tibble", {
  result <- .make_dist_result(
    distribution = "norm",
    n = 10
  )

  expect_s3_class(
    result,
    "tbl_df"
  )

  expect_equal(
    names(result),
    c("distribution", "n")
  )

  expect_equal(
    result$distribution,
    "norm"
  )

  expect_equal(
    result$n,
    10
  )
})

test_that("make_dist_result stores result object", {
  fit <- list(
    decision = "norm"
  )

  result <- .make_dist_result(
    distribution = "norm",
    n = 10,
    result = fit,
    return_result = TRUE
  )

  expect_named(
    result,
    c(
      "distribution",
      "n",
      "result"
    )
  )

  expect_identical(
    result$result[[1]],
    fit
  )
})

test_that("handle_dist_result returns estimation_failed", {
  result <- .handle_dist_result(
    result = NULL,
    n = 10,
    return_result = FALSE
  )

  expect_equal(
    result$distribution,
    "estimation_failed"
  )
})

test_that("handle_dist_result extracts decision", {
  fit <- list(
    decision = "lnorm"
  )

  result <- .handle_dist_result(
    result = fit,
    n = 10,
    return_result = FALSE
  )

  expect_equal(
    result$distribution,
    "lnorm"
  )

  expect_equal(
    result$n,
    10
  )
})

test_that("handle_dist_result collapses multiple decisions", {
  fit <- list(
    decision = c(
      "norm",
      "lnorm"
    )
  )

  result <- .handle_dist_result(
    result = fit,
    n = 10,
    return_result = FALSE
  )

  expect_equal(
    result$distribution,
    "norm, lnorm"
  )
})
