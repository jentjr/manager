test_that("trend_summary returns expected columns", {
  out <- trend_summary(gw)

  expect_s3_class(out, "tbl_df")

  expect_true(all(
    c(
      "n",
      "first_date",
      "last_date",
      "date_range_days",
      "date_range_years",
      "sampling_frequency",
      "minimum_concentration",
      "median_concentration",
      "mean_concentration",
      "maximum_concentration",
      "standard_deviation",
      "coefficient_of_variation",
      "trend_method",
      "censored_method",
      "slope",
      "p_value",
      "conf_low",
      "conf_high",
      "significant",
      "trend_direction"
    ) %in%
      names(out)
  ))
})

test_that("trend_summary groups by location and analyte by default", {
  out <- trend_summary(gw)

  expected_n <- gw |>
    dplyr::distinct(
      !!rlang::sym(location_var(gw)),
      !!rlang::sym(analyte_var(gw))
    ) |>
    nrow()

  expect_equal(
    nrow(out),
    expected_n
  )
})

test_that("trend_summary respects existing grouping", {
  out <- gw |>
    dplyr::group_by(
      !!rlang::sym(location_var(gw))
    ) |>
    trend_summary()

  expected_n <- gw |>
    dplyr::distinct(
      !!rlang::sym(location_var(gw))
    ) |>
    nrow()

  expect_equal(
    nrow(out),
    expected_n
  )
})

test_that("trend_summary works with custom mappings", {
  x <- tibble::tibble(
    site = rep("A", 10),
    chem = rep("Nitrate", 10),
    samp_date = seq.Date(
      as.Date("2020-01-01"),
      by = "year",
      length.out = 10
    ),
    value = seq(1, 10)
  )

  x <- env_schema(
    x,
    location = site,
    date = samp_date,
    analyte = chem,
    result = value
  )

  out <- trend_summary(x)

  expect_equal(
    nrow(out),
    1
  )

  expect_false(
    is.na(out$slope)
  )
})

test_that("mann kendall detects increasing trend", {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Nitrate",
    sample_date = seq.Date(
      as.Date("2020-01-01"),
      by = "year",
      length.out = 10
    ),
    result = 1:10
  )

  x <- env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result
  )

  out <- trend_summary(
    x,
    method = "mann_kendall",
    censored_method = "as_reported"
  )

  expect_gt(
    out$slope,
    0
  )

  expect_equal(
    out$trend_direction,
    "Increasing"
  )
})

test_that("mann kendall detects decreasing trend", {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Nitrate",
    sample_date = seq.Date(
      as.Date("2020-01-01"),
      by = "year",
      length.out = 10
    ),
    result = 10:1
  )

  x <- env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result
  )

  out <- trend_summary(
    x,
    method = "mann_kendall",
    censored_method = "as_reported"
  )

  expect_lt(
    out$slope,
    0
  )

  expect_equal(
    out$trend_direction,
    "Decreasing"
  )
})

test_that("constant data returns NA trend stats", {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Nitrate",
    sample_date = seq.Date(
      as.Date("2020-01-01"),
      by = "year",
      length.out = 10
    ),
    result = rep(5, 10)
  )

  x <- env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result
  )

  out <- trend_summary(x)

  expect_true(
    is.na(out$slope)
  )

  expect_true(
    is.na(out$p_value)
  )
})

test_that("less than three observations returns NA trend stats", {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Nitrate",
    sample_date = as.Date(
      c(
        "2020-01-01",
        "2021-01-01"
      )
    ),
    result = c(1, 2)
  )

  x <- env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result
  )

  out <- trend_summary(x)

  expect_true(
    is.na(out$slope)
  )

  expect_true(
    is.na(out$p_value)
  )
})

test_that("cenken workflow returns trend statistics", {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Nitrate",
    sample_date = seq.Date(
      as.Date("2020-01-01"),
      by = "year",
      length.out = 6
    ),
    result = c(
      1,
      2,
      3,
      4,
      5,
      6
    ),
    qualifier = c(
      "",
      "U",
      "",
      "",
      "U",
      ""
    )
  )

  x <- env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result,
    reporting_flag = qualifier
  )

  out <- trend_summary(
    x,
    censored_method = "cenken"
  )

  expect_false(
    is.na(out$slope)
  )

  expect_false(
    is.na(out$p_value)
  )

  expect_true(
    is.na(out$conf_low)
  )

  expect_true(
    is.na(out$conf_high)
  )
})
