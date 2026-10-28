test_that("lm trend returns expected fields", {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Nitrate",
    sample_date = seq.Date(
      as.Date("2020-01-01"),
      by = "year",
      length.out = 10
    ),
    result = seq(1, 10)
  )

  x <- env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result
  )

  tr <- manager:::.compute_trend(
    df = x,
    date_col = date_var(x),
    method = "lm"
  )

  expect_false(is.null(tr))

  expect_equal(tr$method, "lm")

  expect_true(is.numeric(tr$slope))
  expect_true(is.numeric(tr$p_value))

  expect_false(is.na(tr$conf_low))
  expect_false(is.na(tr$conf_high))

  expect_gt(nrow(tr$trend_df), 0)
})

test_that("loess trend returns fitted values", {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Nitrate",
    sample_date = seq.Date(
      as.Date("2020-01-01"),
      by = "year",
      length.out = 20
    ),
    result = c(
      seq(1, 10),
      seq(10, 5, length.out = 10)
    )
  )

  x <- env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result
  )

  tr <- manager:::.compute_trend(
    df = x,
    date_col = date_var(x),
    method = "loess"
  )

  expect_false(is.null(tr))

  expect_equal(tr$method, "loess")

  expect_true("fit" %in% names(tr$trend_df))
  expect_true("lwr" %in% names(tr$trend_df))
  expect_true("upr" %in% names(tr$trend_df))
})

test_that("gam trend returns fitted values", {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Nitrate",
    sample_date = seq.Date(
      as.Date("2020-01-01"),
      by = "month",
      length.out = 24
    ),
    result = sin(seq(0, 2 * pi, length.out = 24))
  )

  x <- env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result
  )

  tr <- manager:::.compute_trend(
    df = x,
    date_col = date_var(x),
    method = "gam"
  )

  expect_false(is.null(tr))

  expect_equal(tr$method, "gam")

  expect_true("fit" %in% names(tr$trend_df))
  expect_true("lwr" %in% names(tr$trend_df))
  expect_true("upr" %in% names(tr$trend_df))
})

test_that("mann kendall as_reported returns trend statistics", {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Nitrate",
    sample_date = seq.Date(
      as.Date("2020-01-01"),
      by = "year",
      length.out = 10
    ),
    result = seq(1, 10)
  )

  x <- env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result
  )

  tr <- manager:::.compute_trend(
    df = x,
    date_col = date_var(x),
    method = "mann_kendall",
    censored_method = "as_reported"
  )

  expect_false(is.null(tr))

  expect_equal(
    tr$method,
    "mann_kendall"
  )

  expect_gt(tr$slope, 0)

  expect_false(is.na(tr$p_value))
  expect_named(
    tr,
    c(
      "method",
      "slope",
      "p_value",
      "conf_low",
      "conf_high",
      "trend_df"
    )
  )

  expect_length(tr$conf_low, 1)
  expect_length(tr$conf_high, 1)
})

test_that("mann kendall cenken returns trend statistics", {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Nitrate",
    sample_date = seq.Date(
      as.Date("2020-01-01"),
      by = "year",
      length.out = 10
    ),
    result = c(
      1,
      1.5,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9
    ),
    flag = c(
      "U",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      ""
    )
  )

  x <- env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result,
    reporting_flag = flag
  )

  tr <- manager:::.compute_trend(
    df = x,
    date_col = date_var(x),
    method = "mann_kendall",
    censored_method = "cenken"
  )

  expect_false(is.null(tr))

  expect_equal(
    tr$method,
    "mann_kendall"
  )

  expect_false(is.na(tr$slope))
  expect_false(is.na(tr$p_value))

  expect_true(is.na(tr$conf_low))
  expect_true(is.na(tr$conf_high))
})

test_that("none method returns NULL", {
  x <- gw

  tr <- manager:::.compute_trend(
    df = x,
    date_col = date_var(x),
    method = "none"
  )

  expect_null(tr)
})

test_that("constant data returns NULL", {
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

  tr <- manager:::.compute_trend(
    df = x,
    date_col = date_var(x),
    method = "lm"
  )

  expect_null(tr)
})

test_that("fewer than three observations returns NULL", {
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

  tr <- manager:::.compute_trend(
    df = x,
    date_col = date_var(x),
    method = "lm"
  )

  expect_null(tr)
})
