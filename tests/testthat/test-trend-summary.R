# ------------------------------------------------------------------
# Deterministic synthetic trend fixtures
# ------------------------------------------------------------------

make_increasing_trend <- function() {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Nitrate",
    sample_date = seq.Date(
      as.Date("2000-01-01"),
      by = "year",
      length.out = 20
    ),
    result = seq_len(20)
  )

  env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result
  )
}

make_decreasing_trend <- function() {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Nitrate",
    sample_date = seq.Date(
      as.Date("2000-01-01"),
      by = "year",
      length.out = 20
    ),
    result = seq(20, 1)
  )

  env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result
  )
}

make_stable_trend <- function() {
  set.seed(42)

  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Nitrate",
    sample_date = seq.Date(
      as.Date("2000-01-01"),
      by = "year",
      length.out = 30
    ),
    result = rnorm(
      30,
      mean = 10,
      sd = 0.25
    )
  )

  env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result
  )
}

make_censored_trend <- function() {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Arsenic",
    sample_date = seq.Date(
      as.Date("2000-01-01"),
      by = "year",
      length.out = 10
    ),
    result = seq_len(10),
    qualifier = c(
      "U",
      "U",
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

  env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result,
    reporting_flag = qualifier
  )
}

# ------------------------------------------------------------------
# trend_summary structure
# ------------------------------------------------------------------

test_that("trend_summary returns expected columns", {
  out <- trend_summary(gw)

  expect_s3_class(out, "tbl_df")

  expect_true(
    all(
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
    )
  )
})

test_that("trend_summary respects grouping", {
  out <- gw |>
    dplyr::group_by(
      !!rlang::sym(location_var(gw))
    ) |>
    trend_summary()

  expect_equal(
    nrow(out),
    gw |>
      dplyr::distinct(
        !!rlang::sym(location_var(gw))
      ) |>
      nrow()
  )
})

# ------------------------------------------------------------------
# Mann-Kendall
# ------------------------------------------------------------------

test_that("kendall_trend detects increasing trend", {
  tr <- kendall_trend(
    make_increasing_trend()
  )

  expect_lt(
    tr$p.value,
    0.05
  )

  expect_gt(
    tr$estimate[["slope"]],
    0
  )
})

test_that("kendall_trend detects decreasing trend", {
  tr <- kendall_trend(
    make_decreasing_trend()
  )

  expect_lt(
    tr$p.value,
    0.05
  )

  expect_lt(
    tr$estimate[["slope"]],
    0
  )
})

test_that("trend_summary reports increasing trend", {
  out <- trend_summary(
    make_increasing_trend(),
    method = "mann_kendall"
  )

  expect_equal(
    out$trend_direction,
    "Increasing"
  )

  expect_true(
    out$significant
  )
})

test_that("trend_summary reports decreasing trend", {
  out <- trend_summary(
    make_decreasing_trend(),
    method = "mann_kendall"
  )

  expect_equal(
    out$trend_direction,
    "Decreasing"
  )

  expect_true(
    out$significant
  )
})

test_that("stable trend is not significant", {
  out <- trend_summary(
    make_stable_trend(),
    method = "mann_kendall"
  )

  expect_gt(
    out$p_value,
    0.05
  )
})

# ------------------------------------------------------------------
# Edge cases
# ------------------------------------------------------------------

test_that("constant data returns NA trend statistics", {
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

  expect_true(is.na(out$slope))
  expect_true(is.na(out$p_value))
})

test_that("fewer than three observations returns NA trend statistics", {
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

  expect_true(is.na(out$slope))
  expect_true(is.na(out$p_value))
})

# ------------------------------------------------------------------
# Censored trends
# ------------------------------------------------------------------

test_that("cenken detects increasing censored trend", {
  out <- trend_summary(
    make_censored_trend(),
    method = "mann_kendall",
    censored_method = "cenken"
  )

  expect_gt(
    out$slope,
    0
  )
})

test_that("cenken workflow returns expected values", {
  out <- trend_summary(
    make_censored_trend(),
    method = "mann_kendall",
    censored_method = "cenken"
  )

  expect_false(is.na(out$slope))
  expect_false(is.na(out$p_value))

  expect_true(is.na(out$conf_low))
  expect_true(is.na(out$conf_high))
})

test_that("cenken identifies increasing censored trend", {
  out <- trend_summary(
    make_censored_trend(),
    method = "mann_kendall",
    censored_method = "cenken"
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

test_that("cenken returns stable when trend is not significant", {
  set.seed(999)

  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Arsenic",
    sample_date = seq.Date(
      as.Date("2000-01-01"),
      by = "year",
      length.out = 20
    ),
    result = rnorm(
      20,
      mean = 5,
      sd = 0.2
    ),
    qualifier = c(
      rep("U", 8),
      rep("", 12)
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
    method = "mann_kendall",
    censored_method = "cenken"
  )

  if (!is.na(out$p_value)) {
    expect_false(
      out$significant
    )

    expect_equal(
      out$trend_direction,
      "Stable"
    )
  }
})
# ------------------------------------------------------------------
# Multiple trend groups
# ------------------------------------------------------------------

test_that("trend_summary calculates separate trends by location", {
  x <- tibble::tibble(
    location = rep(
      c("MW-1", "MW-2"),
      each = 20
    ),
    analyte = "Nitrate",
    sample_date = rep(
      seq.Date(
        as.Date("2000-01-01"),
        by = "year",
        length.out = 20
      ),
      2
    ),
    result = c(
      1:20,
      20:1
    )
  )

  x <- env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result
  )

  out <- trend_summary(x)

  expect_equal(
    nrow(out),
    2
  )
})

test_that("trend_summary classifies stable trend correctly", {
  out <- trend_summary(
    make_stable_trend(),
    method = "mann_kendall"
  )

  expect_gt(
    out$p_value,
    0.05
  )

  expect_false(
    out$significant
  )

  expect_equal(
    out$trend_direction,
    "Stable"
  )
})

test_that("non-significant positive slope is classified as stable", {
  set.seed(123)

  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Nitrate",
    sample_date = seq.Date(
      as.Date("2000-01-01"),
      by = "year",
      length.out = 30
    ),
    result = 10 + seq(0, 1, length.out = 30) + rnorm(30, sd = 5)
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
    method = "mann_kendall"
  )

  expect_false(out$significant)

  expect_equal(
    out$trend_direction,
    "Stable"
  )
})

test_that("non-significant negative slope is classified as stable", {
  set.seed(456)

  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Nitrate",
    sample_date = seq.Date(
      as.Date("2000-01-01"),
      by = "year",
      length.out = 30
    ),
    result = 10 - seq(0, 1, length.out = 30) + rnorm(30, sd = 5)
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
    method = "mann_kendall"
  )

  expect_false(out$significant)

  expect_equal(
    out$trend_direction,
    "Stable"
  )
})

test_that("significant increasing trend is classified correctly", {
  out <- trend_summary(
    make_increasing_trend(),
    method = "mann_kendall"
  )

  expect_true(
    out$significant
  )

  expect_equal(
    out$trend_direction,
    "Increasing"
  )
})

test_that("significant decreasing trend is classified correctly", {
  out <- trend_summary(
    make_decreasing_trend(),
    method = "mann_kendall"
  )

  expect_true(
    out$significant
  )

  expect_equal(
    out$trend_direction,
    "Decreasing"
  )
})

test_that("significant trend direction matches slope sign", {
  out <- trend_summary(
    make_increasing_trend(),
    method = "mann_kendall"
  )

  expect_true(out$slope > 0)

  expect_equal(
    out$trend_direction,
    "Increasing"
  )
})

test_that("stable trends are not significant", {
  out <- trend_summary(
    make_stable_trend(),
    method = "mann_kendall"
  )

  expect_false(
    out$significant
  )

  expect_equal(
    out$trend_direction,
    "Stable"
  )
})
