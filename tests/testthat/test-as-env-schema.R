test_that("as_env_schema returns env_schema unchanged", {
  x <- env_schema(
    tibble::tibble(
      location = "MW-1",
      date = as.Date("2024-01-01"),
      analyte = "Arsenic",
      result = 1
    ),
    location = location,
    date = date,
    analyte = analyte,
    result = result
  )

  expect_identical(
    as_env_schema(x),
    x
  )
})

test_that("as_env_schema converts schema-shaped tibble", {
  x <- tibble::tibble(
    .location = "MW-1",
    .date = as.Date("2024-01-01"),
    .analyte = "Arsenic",
    .analyte_id = "arsenic",
    .original_lab_result = 1,
    .analysis_result = 1,
    .original_lab_unit = "ug/L",
    .analysis_unit = "ug/L"
  )

  attr(x, "threshold_metadata") <- character()
  attr(x, "env_schema_version") <- "0.4.0"

  out <- as_env_schema(x)

  expect_s3_class(
    out,
    "env_schema"
  )
})

test_that("as_env_schema coerces raw data frame", {
  y <- suppressWarnings(
    as_env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  expect_s3_class(y, "env_schema")
})

test_that("as_env_schema creates required schema columns", {
  y <- suppressWarnings(
    as_env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  expect_true(
    all(
      c(
        ".location",
        ".date",
        ".analyte",
        ".analysis_result"
      ) %in%
        names(y)
    )
  )
})

test_that("coerced data validates", {
  y <- suppressWarnings(
    as_env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  expect_no_error(
    validate_env_schema(y)
  )
})
