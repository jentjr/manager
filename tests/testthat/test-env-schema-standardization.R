# tests/testthat/test-env-schema-standardization.R

make_test_data <- function(
  analyte = "Iron, dissolved",
  result = 1,
  unit = "mg/L"
) {
  tibble::tibble(
    location_id = "MW-01",
    sample_date = as.Date("2024-01-01"),
    param_name = analyte,
    analysis_result = result,
    default_unit = unit
  )
}

make_test_schema <- function(
  analyte = "Iron, dissolved",
  result = 1,
  unit = "mg/L"
) {
  env_schema(
    make_test_data(
      analyte = analyte,
      result = result,
      unit = unit
    ),
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit
  )
}

get_std_unit <- function(analyte) {
  resolved <- .resolve_analyte(analyte)

  analyte_tbl %>%
    dplyr::filter(
      analyte_id == resolved$analyte_id[[1]]
    ) %>%
    dplyr::pull(
      std_unit
    ) %>%
    unique() %>%
    .normalize_unit()
}

test_that("original laboratory values are preserved", {
  out <- make_test_schema(
    analyte = "Iron, dissolved",
    result = 1,
    unit = "mg/L"
  )

  expect_equal(
    out$.original_lab_result,
    1
  )

  expect_equal(
    out$.original_lab_unit,
    "mg/L"
  )
})

test_that("analysis unit matches metadata standard unit", {
  x <- tibble::tibble(
    location_id = c(
      "MW-01",
      "MW-02"
    ),

    sample_date = as.Date(
      c(
        "2024-01-01",
        "2024-01-02"
      )
    ),

    param_name = c(
      "Iron, dissolved",
      "Calcium, dissolved"
    ),

    analysis_result = c(
      1,
      1
    ),

    default_unit = c(
      "mg/L",
      "mg/L"
    )
  )

  out <- env_schema(
    x,
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit
  )

  expect_equal(
    out$.analysis_unit,
    out$.std_unit
  )
})

test_that("unit normalization applies analyte standard unit", {
  out <- make_test_schema(
    analyte = "Calcium, dissolved",
    result = 100,
    unit = "mg/L"
  )

  expect_equal(
    out$.analysis_unit,
    out$.std_unit
  )
})

test_that("analytical results are converted to metadata standard units", {
  expected_unit <- get_std_unit(
    "Iron, dissolved"
  )

  expected_result <- .safe_convert_units(
    value = 1,
    from_unit = "mg/L",
    to_unit = expected_unit
  )

  out <- make_test_schema(
    analyte = "Iron, dissolved",
    result = 1,
    unit = "mg/L"
  )

  expect_equal(
    out$.analysis_unit,
    expected_unit
  )

  expect_equal(
    out$.analysis_result,
    expected_result
  )
})

test_that("boron converts according to analyte metadata", {
  expected_unit <- get_std_unit(
    "Boron, dissolved"
  )

  expected_result <- .safe_convert_units(
    value = 2.34,
    from_unit = "mg/L",
    to_unit = expected_unit
  )

  out <- make_test_schema(
    analyte = "Boron, dissolved",
    result = 2.34,
    unit = "mg/L"
  )

  expect_equal(
    out$.analysis_unit,
    expected_unit
  )

  expect_equal(
    out$.analysis_result,
    expected_result
  )
})

test_that("values already reported in standard units are unchanged", {
  expected_unit <- get_std_unit(
    "Iron, dissolved"
  )

  out <- make_test_schema(
    analyte = "Iron, dissolved",
    result = 1000,
    unit = expected_unit
  )

  expect_equal(
    out$.analysis_result,
    1000
  )

  expect_equal(
    out$.analysis_unit,
    expected_unit
  )
})

test_that("unsupported analytes retain original values and units", {
  expect_warning(
    out <- make_test_schema(
      analyte = "Made Up Parameter",
      result = 10,
      unit = "mg/L"
    ),

    "not found in analyte metadata"
  )

  expect_true(
    is.na(
      out$.std_unit
    )
  )

  expect_equal(
    out$.analysis_result,
    10
  )

  expect_equal(
    out$.analysis_unit,
    .normalize_unit("mg/L")
  )
})

test_that("zero values convert correctly", {
  expected_unit <- get_std_unit(
    "Iron, dissolved"
  )

  out <- make_test_schema(
    analyte = "Iron, dissolved",
    result = 0,
    unit = "mg/L"
  )

  expect_equal(
    out$.analysis_result,
    0
  )

  expect_equal(
    out$.analysis_unit,
    expected_unit
  )
})

test_that("missing analytical results are preserved", {
  out <- make_test_schema(
    analyte = "Iron, dissolved",
    result = NA_real_,
    unit = "mg/L"
  )

  expect_true(
    is.na(
      out$.analysis_result
    )
  )
})

test_that("missing units fall back to analyte standard units", {
  out <- make_test_schema(
    analyte = "Iron, dissolved",
    result = 1,
    unit = NA_character_
  )

  expect_equal(
    out$.analysis_unit,
    get_std_unit("Iron, dissolved")
  )

  expect_equal(
    out$.analysis_result,
    1
  )
})
test_that("mixed reporting units convert correctly", {
  std_unit <- get_std_unit(
    "Iron, dissolved"
  )

  expected <- c(
    .safe_convert_units(
      1,
      "mg/L",
      std_unit
    ),
    .safe_convert_units(
      1000,
      "ug/l",
      std_unit
    ),
    .safe_convert_units(
      0.5,
      "mg/L",
      std_unit
    )
  )

  x <- tibble::tibble(
    location_id = c(
      "MW-01",
      "MW-01",
      "MW-01"
    ),

    sample_date = as.Date(
      c(
        "2024-01-01",
        "2024-01-02",
        "2024-01-03"
      )
    ),

    param_name = rep(
      "Iron, dissolved",
      3
    ),

    analysis_result = c(
      1,
      1000,
      0.5
    ),

    default_unit = c(
      "mg/L",
      "ug/l",
      "mg/L"
    )
  )

  out <- env_schema(
    x,
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit
  )

  expect_equal(
    out$.analysis_result,
    expected
  )

  expect_equal(
    unique(out$.analysis_unit),
    std_unit
  )
})

test_that("equivalent concentrations standardize identically", {
  x <- tibble::tibble(
    location_id = c(
      "MW-01",
      "MW-01"
    ),

    sample_date = as.Date(
      c(
        "2024-01-01",
        "2024-01-02"
      )
    ),

    param_name = c(
      "Iron, dissolved",
      "Iron, dissolved"
    ),

    analysis_result = c(
      1,
      1000
    ),

    default_unit = c(
      "mg/L",
      "ug/l"
    )
  )

  out <- env_schema(
    x,
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit
  )

  expect_equal(
    out$.analysis_result[1],
    out$.analysis_result[2]
  )

  expect_equal(
    out$.analysis_unit[1],
    out$.analysis_unit[2]
  )
})
