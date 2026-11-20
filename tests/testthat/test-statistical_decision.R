# tests/testthat/test-statistical_decision.R

# -------------------------------------------------------------------------
# EPA unified profile defaults
# -------------------------------------------------------------------------
test_that("statistical_decision respects guidance_options", {
  old <- getOption("manager.guidance")

  on.exit(
    options(manager.guidance = old),
    add = TRUE
  )

  options(
    manager.guidance = list(
      low_threshold = 10,
      moderate_threshold = 25,
      high_threshold = 75
    )
  )

  out <- statistical_decision(
    percent_censored = 15,
    n_used = 10,
    n_detect = 8
  )

  expect_equal(
    out$censoring_class,
    "moderate_censoring"
  )
})

test_that("guidance_profile returns expected EPA thresholds", {
  profile <- guidance_profile(
    "epa_unified"
  )

  expect_equal(
    profile$low_threshold,
    15
  )

  expect_equal(
    profile$moderate_threshold,
    50
  )

  expect_equal(
    profile$high_threshold,
    90
  )
})

test_that("guidance_profile returns expected conservative thresholds", {
  profile <- guidance_profile(
    "conservative"
  )

  expect_equal(
    profile$low_threshold,
    10
  )

  expect_equal(
    profile$moderate_threshold,
    25
  )

  expect_equal(
    profile$high_threshold,
    75
  )
})

# -------------------------------------------------------------------------
# censoring classification
# -------------------------------------------------------------------------

test_that("low censoring classification is assigned correctly", {
  out <- statistical_decision(
    percent_censored = 5,
    n_used = 10,
    n_detect = 9
  )

  expect_equal(
    out$censoring_class,
    "low_censoring"
  )

  expect_equal(
    out$distribution_method,
    "parametric"
  )

  expect_equal(
    out$comparison_method,
    "anova"
  )

  expect_equal(
    out$censored_method,
    "standard"
  )
})

test_that("moderate censoring classification is assigned correctly", {
  out <- statistical_decision(
    percent_censored = 25,
    n_used = 10,
    n_detect = 7
  )

  expect_equal(
    out$censoring_class,
    "moderate_censoring"
  )

  expect_equal(
    out$distribution_method,
    "ros_censored"
  )

  expect_equal(
    out$comparison_method,
    "censored_nonparametric"
  )

  expect_equal(
    out$censored_method,
    "ros"
  )
})

test_that("high censoring classification is assigned correctly", {
  out <- statistical_decision(
    percent_censored = 80,
    n_used = 10,
    n_detect = 4
  )

  expect_equal(
    out$censoring_class,
    "high_censoring"
  )

  expect_equal(
    out$distribution_method,
    "nonparametric"
  )

  expect_equal(
    out$comparison_method,
    "censored_nonparametric"
  )

  expect_equal(
    out$censored_method,
    "nonparametric"
  )
})

test_that("extreme censoring classification is assigned correctly", {
  out <- statistical_decision(
    percent_censored = 95,
    n_used = 20,
    n_detect = 1
  )

  expect_equal(
    out$censoring_class,
    "extreme_censoring"
  )

  expect_equal(
    out$distribution_method,
    "descriptive_only"
  )

  expect_equal(
    out$comparison_method,
    "insufficient_data"
  )

  expect_equal(
    out$censored_method,
    "descriptive_only"
  )
})

# -------------------------------------------------------------------------
# insufficient data overrides
# -------------------------------------------------------------------------

test_that("insufficient_data overrides distribution recommendation", {
  out <- statistical_decision(
    percent_censored = 5,
    n_used = 3,
    n_detect = 3
  )

  expect_equal(
    out$distribution_method,
    "insufficient_data"
  )

  expect_equal(
    out$comparison_method,
    "insufficient_data"
  )

  expect_equal(
    out$decision_basis,
    "fewer_than_4_observations"
  )
})

# -------------------------------------------------------------------------
# trend guidance
# -------------------------------------------------------------------------

test_that("formal trend testing is recommended for low censoring", {
  out <- statistical_decision(
    percent_censored = 10,
    n_used = 20,
    n_detect = 18
  )

  expect_equal(
    out$trend_guidance,
    "formal_trend_test"
  )
})

test_that("insufficient detects overrides trend guidance", {
  out <- statistical_decision(
    percent_censored = 10,
    n_used = 20,
    n_detect = 3
  )

  expect_equal(
    out$trend_guidance,
    "formal_trend_test"
  )
})

test_that("screening only recommendation is used for extreme censoring", {
  out <- statistical_decision(
    percent_censored = 95,
    n_used = 20,
    n_detect = 5
  )

  expect_equal(
    out$trend_guidance,
    "screening_only"
  )
})

# -------------------------------------------------------------------------
# vectorization
# -------------------------------------------------------------------------

test_that("statistical_decision is vectorized", {
  out <- statistical_decision(
    percent_censored = c(
      5,
      25,
      80,
      95
    ),
    n_used = c(
      10,
      10,
      10,
      10
    ),
    n_detect = c(
      9,
      7,
      2,
      1
    )
  )

  expect_equal(
    nrow(out),
    4
  )

  expect_equal(
    out$censoring_class,
    c(
      "low_censoring",
      "moderate_censoring",
      "high_censoring",
      "extreme_censoring"
    )
  )

  expect_equal(
    out$distribution_method,
    c(
      "parametric",
      "ros_censored",
      "nonparametric",
      "descriptive_only"
    )
  )
})

# -------------------------------------------------------------------------
# profile behavior
# -------------------------------------------------------------------------

test_that("custom profile affects recommendations", {
  out <- statistical_decision(
    percent_censored = 15,
    n_used = 10,
    n_detect = 8,
    profile = guidance_profile(
      "conservative"
    )
  )

  expect_equal(
    out$censoring_class,
    "moderate_censoring"
  )

  expect_equal(
    out$distribution_method,
    "ros_censored"
  )
})

# -------------------------------------------------------------------------
# output structure
# -------------------------------------------------------------------------

test_that("returns statistical_decision object", {
  out <- statistical_decision(
    percent_censored = 5,
    n_used = 10,
    n_detect = 9
  )

  expect_s3_class(
    out,
    "statistical_decision"
  )

  expect_s3_class(
    out,
    "tbl_df"
  )

  expect_named(
    out,
    c(
      "n_used",
      "n_detect",
      "percent_censored",
      "censoring_class",
      "distribution_method",
      "trend_method",
      "trend_guidance",
      "censored_method",
      "distribution_config",
      "interval_method",
      "interval_config",
      "decision_basis",
      "comparison_method"
    ),
    ignore.order = TRUE
  )
})

test_that("statistical_decision handles NA methods", {
  result <- statistical_decision(
    percent_censored = NA_real_,
    n_used = 0L,
    n_detect = 0L
  )

  expect_type(result$interval_config[[1]], "list")
  expect_length(result$interval_config[[1]], 0L)
})
