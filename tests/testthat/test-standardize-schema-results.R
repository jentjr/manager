# tests/testthat/test-standardize-schema-results.R

test_that(".standardize_schema_results returns input unchanged when .std_unit is absent", {
  df <- tibble::tibble(
    .analysis_result = 1,
    .analysis_unit = "mg/L"
  )

  out <- .standardize_schema_results(df)

  expect_identical(
    out,
    df
  )
})

test_that(".standardize_schema_results validates required columns", {
  df <- tibble::tibble(
    .analysis_result = 1,
    .std_unit = "ug/L"
  )

  expect_error(
    .standardize_schema_results(df),
    "Missing required column\\(s\\)"
  )
})

test_that(".standardize_schema_results normalizes units before comparison", {
  df <- tibble::tibble(
    .analysis_result = 1000,
    .analysis_unit = " UG/L ",
    .std_unit = "ug/l"
  )

  out <- .standardize_schema_results(df)

  expect_equal(
    out$.analysis_result,
    1000
  )

  expect_equal(
    out$.analysis_unit,
    "ug/l"
  )
})

test_that(".standardize_schema_results converts values for matching conversion groups", {
  df <- tibble::tibble(
    .analysis_result = c(
      1,
      2
    ),
    .analysis_unit = c(
      "mg/L",
      "mg/L"
    ),
    .std_unit = c(
      "ug/L",
      "ug/L"
    )
  )

  out <- .standardize_schema_results(df)

  expect_equal(
    out$.analysis_result,
    c(
      1000,
      2000
    )
  )

  expect_equal(
    unique(out$.analysis_unit),
    "ug/l"
  )
})

test_that(".standardize_schema_results leaves rows with missing standard units unchanged", {
  df <- tibble::tibble(
    .analysis_result = c(
      1,
      2
    ),
    .analysis_unit = c(
      "mg/L",
      "mg/L"
    ),
    .std_unit = c(
      "ug/L",
      NA_character_
    )
  )

  out <- .standardize_schema_results(df)

  expect_equal(
    out$.analysis_result,
    c(
      1000,
      2
    )
  )

  expect_equal(
    out$.analysis_unit,
    c(
      "ug/l",
      "mg/l"
    )
  )
})

test_that(".standardize_schema_results standardizes equivalent concentrations identically", {
  df <- tibble::tibble(
    .analysis_result = c(
      1,
      1000
    ),
    .analysis_unit = c(
      "mg/L",
      "ug/L"
    ),
    .std_unit = c(
      "ug/L",
      "ug/L"
    )
  )

  out <- .standardize_schema_results(df)

  expect_equal(
    out$.analysis_result[1],
    out$.analysis_result[2]
  )

  expect_equal(
    out$.analysis_unit[1],
    out$.analysis_unit[2]
  )
})

test_that(".standardize_schema_results uses the standard unit when source unit is missing", {
  df <- tibble::tibble(
    .analysis_result = 1,
    .analysis_unit = NA_character_,
    .std_unit = "mg/L"
  )

  expect_message(
    out <- .standardize_schema_results(df),
    "standard unit"
  )

  expect_equal(
    out$.analysis_result,
    1
  )

  expect_equal(
    out$.analysis_unit,
    "mg/l"
  )
})

test_that("STD source units use the analyte standard unit", {
  df <- tibble::tibble(
    .analysis_result = 7,
    .analysis_unit = "STD",
    .std_unit = "pH"
  )

  expect_message(
    out <- .standardize_schema_results(df),
    "standard unit"
  )

  expect_equal(out$.analysis_result, 7)
  expect_equal(out$.analysis_unit, "ph")
})

test_that("STD normalizes to a missing unit", {
  expect_true(is.na(.normalize_unit("STD")))
})
