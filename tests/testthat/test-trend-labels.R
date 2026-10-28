test_that("compact label reports increasing trend", {
  lbl <- manager:::.format_trend_label(
    slope = 1,
    p_value = 0.0123,
    conf_low = NA_real_,
    conf_high = NA_real_,
    trend_label = "compact",
    trend = "mann_kendall"
  )

  expect_match(
    lbl,
    "Increasing"
  )

  expect_match(
    lbl,
    "p="
  )
})

test_that("compact label reports decreasing trend", {
  lbl <- manager:::.format_trend_label(
    slope = -1,
    p_value = 0.0123,
    conf_low = NA_real_,
    conf_high = NA_real_,
    trend_label = "compact",
    trend = "mann_kendall"
  )

  expect_match(
    lbl,
    "Decreasing"
  )
})

test_that("compact label reports stable trend", {
  lbl <- manager:::.format_trend_label(
    slope = 0,
    p_value = 0.50,
    conf_low = NA_real_,
    conf_high = NA_real_,
    trend_label = "compact",
    trend = "mann_kendall"
  )

  expect_match(
    lbl,
    "Stable"
  )
})

test_that("none returns empty string", {
  lbl <- manager:::.format_trend_label(
    slope = 1,
    p_value = 0.01,
    conf_low = 0,
    conf_high = 2,
    trend_label = "none",
    trend = "lm"
  )

  expect_equal(lbl, "")
})

test_that("NA p value returns empty string", {
  lbl <- manager:::.format_trend_label(
    slope = 1,
    p_value = NA_real_,
    conf_low = 0,
    conf_high = 2,
    trend_label = "compact",
    trend = "lm"
  )

  expect_identical(
    lbl,
    ""
  )
})

test_that("mann kendall label omits confidence interval when unavailable", {
  lbl <- manager:::.format_trend_label(
    slope = 1.25,
    p_value = 0.01,
    conf_low = NA_real_,
    conf_high = NA_real_,
    trend_label = "full",
    trend = "mann_kendall"
  )

  expect_match(
    lbl,
    "Mann-Kendall"
  )

  expect_match(
    lbl,
    "Sen="
  )

  expect_no_match(
    lbl,
    "CI="
  )
})

test_that("mann kendall label includes confidence interval when available", {
  lbl <- manager:::.format_trend_label(
    slope = 1.25,
    p_value = 0.01,
    conf_low = 0.75,
    conf_high = 1.75,
    trend_label = "full",
    trend = "mann_kendall"
  )

  expect_match(
    lbl,
    "Mann-Kendall"
  )

  expect_match(
    lbl,
    "Sen="
  )

  expect_match(
    lbl,
    "CI="
  )
})

test_that("lm label omits confidence interval when unavailable", {
  lbl <- manager:::.format_trend_label(
    slope = 0.5,
    p_value = 0.02,
    conf_low = NA_real_,
    conf_high = NA_real_,
    trend_label = "full",
    trend = "lm"
  )

  expect_match(
    lbl,
    "Slope="
  )

  expect_match(
    lbl,
    "p="
  )

  expect_no_match(
    lbl,
    "CI="
  )
})

test_that("lm label includes confidence interval when available", {
  lbl <- manager:::.format_trend_label(
    slope = 0.5,
    p_value = 0.02,
    conf_low = 0.1,
    conf_high = 0.9,
    trend_label = "full",
    trend = "lm"
  )

  expect_match(
    lbl,
    "Slope="
  )

  expect_match(
    lbl,
    "p="
  )

  expect_match(
    lbl,
    "CI="
  )
})

test_that("returned label is always length one character", {
  lbl <- manager:::.format_trend_label(
    slope = 1,
    p_value = 0.01,
    conf_low = 0,
    conf_high = 2,
    trend_label = "full",
    trend = "mann_kendall"
  )

  expect_type(
    lbl,
    "character"
  )

  expect_length(
    lbl,
    1
  )
})
