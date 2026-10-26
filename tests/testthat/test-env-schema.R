test_that("env_schema creates valid env_schema object", {
  df <- tibble::tibble(
    location = "MW-1",
    date = as.Date("2024-01-01"),
    analyte = "Arsenic",
    result = 5,
    unit = "ug/L"
  )

  out <- env_schema(
    df,
    location = location,
    date = date,
    analyte = analyte,
    result = result,
    unit = unit
  )

  expect_s3_class(out, "env_schema")

  expect_true(
    validate_env_schema(out)
  )
})

test_that("env_schema creates analysis columns", {
  df <- tibble::tibble(
    location = "MW-1",
    date = as.Date("2024-01-01"),
    analyte = "Arsenic",
    result = 5,
    unit = "ug/L"
  )

  out <- env_schema(
    df,
    location = location,
    date = date,
    analyte = analyte,
    result = result,
    unit = unit
  )

  expect_true(
    all(
      c(
        ".original_lab_result",
        ".analysis_result",
        ".original_lab_unit",
        ".analysis_unit"
      ) %in%
        names(out)
    )
  )
})

test_that("env_schema records threshold metadata", {
  df <- tibble::tibble(
    location = "MW-1",
    date = as.Date("2024-01-01"),
    analyte = "Arsenic",
    result = 5,
    rl = 1
  )

  out <- env_schema(
    df,
    location = location,
    date = date,
    analyte = analyte,
    result = result,
    reporting_limit = rl
  )

  expect_identical(
    attr(out, "threshold_metadata"),
    ".reporting_limit"
  )
})
