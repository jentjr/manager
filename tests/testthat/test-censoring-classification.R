test_that("censoring classification uses guidance boundaries", {
  profile <- list(
    low_threshold = 15,
    moderate_threshold = 50,
    high_threshold = 90
  )

  expect_identical(
    .classify_censoring(
      c(0, 14.9, 15, 49.9, 50, 89.9, 90, 100, NA_real_),
      profile
    ),
    c(
      "low_censoring",
      "low_censoring",
      "moderate_censoring",
      "moderate_censoring",
      "high_censoring",
      "high_censoring",
      "extreme_censoring",
      "extreme_censoring",
      NA_character_
    )
  )
})


test_that("statistical_decision and select_statistical_method share classification", {
  profile <- list(
    low_threshold = 15,
    moderate_threshold = 50,
    high_threshold = 90
  )

  percentages <- c(10, 25, 75, 95)

  decision <- statistical_decision(
    percent_censored = percentages,
    n_used = rep(10, 4),
    n_detect = c(9, 8, 3, 1),
    profile = profile
  )

  expect_identical(
    decision$censoring_class,
    .classify_censoring(percentages, profile)
  )
})


test_that("invalid censoring percentages fail clearly", {
  expect_error(
    .classify_censoring(-1),
    "percentages from 0 to 100"
  )

  expect_error(
    .classify_censoring(101),
    "percentages from 0 to 100"
  )
})

test_that("statistical_decision does not use free threshold variables", {
  decision <- statistical_decision(
    percent_censored = c(5, 25, 80, 95),
    n_used = c(10, 10, 10, 10),
    n_detect = c(9, 7, 2, 1),
    profile = guidance_profile("epa_unified")
  )

  testthat::expect_identical(
    decision$interval_method,
    c(
      "parametric",
      "ros_censored",
      "nonparametric",
      "descriptive_only"
    )
  )

  testthat::expect_true(
    all(
      vapply(
        decision$interval_config,
        is.list,
        logical(1)
      )
    )
  )
})

test_that("derive_decision resolves default analysis columns", {
  data <- data.frame(
    analysis_result = c(1, 2, 3, 4),
    censored = c(FALSE, FALSE, TRUE, FALSE)
  )

  result <- derive_decision(data)

  expect_s3_class(result, "statistical_decision")
  expect_equal(result$n_used, 4L)
  expect_equal(result$n_detect, 3L)
  expect_equal(result$percent_censored, 25)
  expect_equal(result$censoring_class, "moderate_censoring")
})
