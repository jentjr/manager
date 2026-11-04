test_that("compact label reports increasing trend", {
  lbl <- manager:::.format_trend_label(
    slope = 1,
    p_value = 0.01,
    conf_low = NA_real_,
    conf_high = NA_real_,
    trend_label = "compact",
    trend = "mann_kendall"
  )

  expect_match(lbl, "Increasing")
  expect_match(lbl, "p=")
})

test_that("compact label reports decreasing trend", {
  lbl <- manager:::.format_trend_label(
    slope = -1,
    p_value = 0.01,
    conf_low = NA_real_,
    conf_high = NA_real_,
    trend_label = "compact",
    trend = "mann_kendall"
  )

  expect_match(lbl, "Decreasing")
})

test_that("compact label reports stable trend", {
  lbl <- manager:::.format_trend_label(
    slope = 0,
    p_value = 0.5,
    conf_low = NA_real_,
    conf_high = NA_real_,
    trend_label = "compact",
    trend = "mann_kendall"
  )

  expect_match(lbl, "Stable")
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

  expect_identical(lbl, "")
})

test_that("NA p value returns empty string", {
  lbl <- manager:::.format_trend_label(
    slope = 1,
    p_value = NA_real_,
    conf_low = 0,
    conf_high = 2,
    trend_label = "full",
    trend = "lm"
  )

  expect_identical(lbl, "")
})

test_that("mann kendall full label supports missing confidence interval", {
  lbl <- manager:::.format_trend_label(
    slope = 1,
    p_value = 0.01,
    conf_low = NA_real_,
    conf_high = NA_real_,
    trend_label = "full",
    trend = "mann_kendall"
  )

  expect_match(lbl, "Mann-Kendall")
  expect_match(lbl, "Sen=")
  expect_no_match(lbl, "CI=")
})

test_that("mann kendall full label includes confidence interval", {
  lbl <- manager:::.format_trend_label(
    slope = 1,
    p_value = 0.01,
    conf_low = 0.5,
    conf_high = 1.5,
    trend_label = "full",
    trend = "mann_kendall"
  )

  expect_match(lbl, "CI=")
})

test_that("lm full label includes confidence interval", {
  lbl <- manager:::.format_trend_label(
    slope = 1,
    p_value = 0.01,
    conf_low = 0.5,
    conf_high = 1.5,
    trend_label = "full",
    trend = "lm"
  )

  expect_match(lbl, "Slope=")
  expect_match(lbl, "CI=")
})

test_that("formatted label is scalar character", {
  lbl <- manager:::.format_trend_label(
    slope = 1,
    p_value = 0.01,
    conf_low = 0.5,
    conf_high = 1.5,
    trend_label = "full",
    trend = "lm"
  )

  expect_type(lbl, "character")
  expect_length(lbl, 1)
})
