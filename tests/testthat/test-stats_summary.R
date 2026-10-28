# -------------------------------------------------------------------------
# Test fixture
# -------------------------------------------------------------------------

make_env_schema <- function() {
  env_schema(
    tibble::tibble(
      location_id = c(
        "MW-1",
        "MW-1",
        "MW-2",
        "MW-2"
      ),
      sample_date = as.Date(
        c(
          "2020-01-01",
          "2020-02-01",
          "2020-01-01",
          "2020-02-01"
        )
      ),
      param_name = rep(
        "Chloride, total",
        4
      ),
      analysis_result = c(
        1,
        2,
        3,
        4
      ),
      default_unit = "mg/L",
      lt_measure = c(
        NA,
        "<",
        NA,
        "<"
      )
    ),
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    reporting_flag = lt_measure
  )
}

# -------------------------------------------------------------------------
# Default schema behavior
# -------------------------------------------------------------------------

test_that("stats_summary.env_schema uses schema mappings by default", {
  x <- make_env_schema()

  out <- stats_summary(x)

  expect_s3_class(
    out,
    "tbl_df"
  )

  expect_true(
    "location_id" %in% names(out)
  )

  expect_true(
    "param_name" %in% names(out)
  )

  expect_true(
    ".analysis_unit" %in% names(out)
  )
})

# -------------------------------------------------------------------------
# Analysis result behavior
# -------------------------------------------------------------------------

test_that("stats_summary uses .analysis_result", {
  x <- make_env_schema()

  x$.analysis_result <- c(
    100,
    200,
    300,
    400
  )

  out <- stats_summary(x)

  expect_equal(
    sort(out$mean),
    c(
      150,
      350
    )
  )
})

test_that("stats_summary ignores source result values", {
  x <- make_env_schema()

  x$analysis_result <- rep(
    999,
    4
  )

  x$.analysis_result <- c(
    1,
    2,
    3,
    4
  )

  out <- stats_summary(x)

  expect_equal(
    sort(out$mean),
    c(
      1.5,
      3.5
    )
  )
})

# -------------------------------------------------------------------------
# Censoring behavior
# -------------------------------------------------------------------------

test_that("stats_summary uses .censored", {
  x <- make_env_schema()

  x$.censored <- c(
    TRUE,
    TRUE,
    FALSE,
    FALSE
  )

  out <- stats_summary(x)

  expect_equal(
    sort(out$percent_censored),
    c(
      0,
      100
    )
  )
})

test_that("stats_summary ignores reporting flags once .censored exists", {
  x <- make_env_schema()

  x$lt_measure <- rep(
    "<",
    4
  )

  x$.censored <- c(
    FALSE,
    FALSE,
    FALSE,
    FALSE
  )

  out <- stats_summary(x)

  expect_equal(
    sort(out$percent_censored),
    c(
      0,
      0
    )
  )
})

# -------------------------------------------------------------------------
# Grouping behavior
# -------------------------------------------------------------------------

test_that("stats_summary respects grouped workflows", {
  x <- make_env_schema()

  out <- x %>%
    dplyr::group_by(
      location_id
    ) %>%
    stats_summary()

  expect_equal(
    nrow(out),
    2
  )

  expect_setequal(
    out$location_id,
    c(
      "MW-1",
      "MW-2"
    )
  )
})

test_that("stats_summary uses .analysis_unit for default grouping", {
  x <- make_env_schema()

  x$default_unit <- c(
    "ug/L",
    "mg/L",
    "ug/L",
    "mg/L"
  )

  x$.analysis_unit <- rep(
    "mg/L",
    4
  )

  out <- stats_summary(x)

  expect_equal(
    nrow(out),
    2
  )
})

# -------------------------------------------------------------------------
# Summary statistic calculations
# -------------------------------------------------------------------------

test_that("stats_summary returns expected summary statistics", {
  x <- make_env_schema()

  out <- stats_summary(
    dplyr::filter(
      x,
      location_id == "MW-1"
    )
  )

  expect_equal(
    out$n,
    2
  )

  expect_equal(
    out$percent_censored,
    50
  )

  expect_equal(
    out$mean,
    1.5
  )

  expect_equal(
    out$median,
    1.5
  )

  expect_equal(
    out$min,
    1
  )

  expect_equal(
    out$max,
    2
  )

  expect_equal(
    out$`25%`,
    1.25
  )

  expect_equal(
    out$`75%`,
    1.75
  )

  expect_equal(
    out$IQR,
    0.5
  )
})

# -------------------------------------------------------------------------
# Error handling
# -------------------------------------------------------------------------

test_that("stats_summary errors when .analysis_result is missing", {
  x <- make_env_schema()

  x$.analysis_result <- NULL

  expect_error(
    stats_summary(x),
    "Missing required columns"
  )
})

test_that("stats_summary errors when .censored is missing", {
  x <- make_env_schema()

  x$.censored <- NULL

  expect_error(
    stats_summary(x),
    "Missing required columns"
  )
})

# -------------------------------------------------------------------------
# Missing values
# -------------------------------------------------------------------------
test_that("stats_summary ignores missing analysis results", {
  x <- make_env_schema()

  x$.analysis_result <- c(
    1,
    NA,
    3,
    4
  )

  out <- stats_summary(
    dplyr::filter(
      x,
      location_id == "MW-1"
    )
  )

  expect_equal(out$n, 2)
  expect_equal(out$mean, 1)
})

test_that("stats_summary handles empty input", {
  x <- make_env_schema()[0, ]

  expect_no_warning({
    out <- stats_summary(x)
  })

  expect_equal(
    nrow(out),
    0
  )
})

# -------------------------------------------------------------------------
# Censored data
# -------------------------------------------------------------------------
test_that("stats_summary handles fully censored groups", {
  x <- make_env_schema()

  x$.censored <- TRUE

  out <- stats_summary(x)

  expect_true(
    all(out$percent_censored == 100)
  )
})

test_that("stats_summary handles uncensored groups", {
  x <- make_env_schema()

  x$.censored <- FALSE

  out <- stats_summary(x)

  expect_true(
    all(out$percent_censored == 0)
  )
})

test_that("stats_summary handles constant results", {
  x <- make_env_schema()

  x$.analysis_result <- rep(
    5,
    4
  )

  out <- stats_summary(x)

  expect_true(
    all(out$mean == 5)
  )

  expect_true(
    all(out$median == 5)
  )

  expect_true(
    all(out$min == 5)
  )

  expect_true(
    all(out$max == 5)
  )
})

test_that("stats_summary produces one row per analyte", {
  x <- make_env_schema()

  x$param_name <- c(
    "Chloride",
    "Chloride",
    "Sulfate",
    "Sulfate"
  )

  out <- stats_summary(x)

  expect_equal(
    nrow(out),
    2
  )
})

test_that("stats_summary handles empty input", {
  x <- make_env_schema()

  x <- x[0, ]

  out <- stats_summary(x)

  expect_s3_class(
    out,
    "tbl_df"
  )

  expect_equal(
    nrow(out),
    0
  )
})

test_that("stats_summary works with custom column mappings", {
  x <- tibble::tibble(
    site = c(
      "MW-1",
      "MW-1",
      "MW-2",
      "MW-2"
    ),
    collected = as.Date(
      c(
        "2020-01-01",
        "2020-02-01",
        "2020-01-01",
        "2020-02-01"
      )
    ),
    chemical = rep(
      "Chloride",
      4
    ),
    value = c(
      1,
      2,
      3,
      4
    )
  )

  x <- env_schema(
    x,
    location = site,
    date = collected,
    analyte = chemical,
    result = value
  )

  out <- stats_summary(x)

  expect_true(
    "site" %in% names(out)
  )
})

test_that("stats_summary respects analyte grouping", {
  x <- make_env_schema()

  out <- x |>
    dplyr::group_by(
      param_name
    ) |>
    stats_summary()

  expect_equal(
    nrow(out),
    1
  )
})

test_that("stats_summary handles a single observation", {
  x <- make_env_schema()

  out <- stats_summary(
    dplyr::slice(
      x,
      1
    )
  )

  expect_equal(
    out$n,
    1
  )

  expect_equal(
    out$mean,
    1
  )

  expect_equal(
    out$median,
    1
  )
})
