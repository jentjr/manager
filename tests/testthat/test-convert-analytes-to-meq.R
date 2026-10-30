# tests/testthat/test-convert-analytes-to-meq.R

make_meq_data <- function(
  analyte = "Calcium, dissolved",
  result = 100,
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

make_meq_schema <- function(
  analyte = "Calcium, dissolved",
  result = 100,
  unit = "mg/L"
) {
  env_schema(
    make_meq_data(
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

get_meq_factor <- function(analyte) {
  resolved <- .resolve_analyte(analyte)

  .get_meq_conversion_factors() %>%
    dplyr::filter(
      analyte_id == resolved$analyte_id[[1]]
    ) %>%
    dplyr::pull(
      meq_factor
    ) %>%
    unique()
}

test_that("meq output columns are created", {
  out <- make_meq_schema() %>%
    convert_analytes_to_meq()

  expect_true(
    "analysis_result_meq" %in%
      names(out)
  )

  expect_true(
    "default_unit_meq" %in%
      names(out)
  )
})

test_that("meq unit is assigned", {
  out <- make_meq_schema() %>%
    convert_analytes_to_meq()

  expect_equal(
    out$default_unit_meq,
    "meq/L"
  )
})

test_that("meq conversion uses analyte metadata", {
  x <- make_meq_schema(
    analyte = "Calcium, dissolved",
    result = 100
  )

  factor <- get_meq_factor(
    "Calcium, dissolved"
  )

  out <- convert_analytes_to_meq(x)

  expect_equal(
    out$analysis_result_meq,
    x$.analysis_result * factor
  )
})

test_that("known calcium meq value is correct", {
  x <- make_meq_schema(
    analyte = "Calcium, dissolved",
    result = 100
  )

  out <- convert_analytes_to_meq(x)

  expected <- 100 *
    get_meq_factor(
      "Calcium, dissolved"
    )

  expect_equal(
    out$analysis_result_meq,
    expected
  )
})

test_that("known magnesium meq value is correct", {
  x <- make_meq_schema(
    analyte = "Magnesium, dissolved",
    result = 24.305
  )

  out <- convert_analytes_to_meq(x)

  expected <- 24.305 *
    get_meq_factor(
      "Magnesium, dissolved"
    )

  expect_equal(
    out$analysis_result_meq,
    expected
  )
})

test_that("zero concentrations produce zero meq", {
  out <- make_meq_schema(
    analyte = "Calcium, dissolved",
    result = 0
  ) %>%
    convert_analytes_to_meq()

  expect_equal(
    out$analysis_result_meq,
    0
  )
})

test_that("missing concentrations remain missing", {
  out <- make_meq_schema(
    analyte = "Calcium, dissolved",
    result = NA_real_
  ) %>%
    convert_analytes_to_meq()

  expect_true(
    is.na(
      out$analysis_result_meq
    )
  )

  expect_true(
    is.na(
      out$default_unit_meq
    )
  )
})

test_that("unsupported analytes return missing meq values", {
  expect_warning(
    x <- make_meq_schema(
      analyte = "Imaginary Analyte",
      result = 10
    ),

    "not found in analyte metadata"
  )

  out <- convert_analytes_to_meq(x)

  expect_true(
    is.na(
      out$analysis_result_meq
    )
  )

  expect_true(
    is.na(
      out$default_unit_meq
    )
  )
})

test_that("equivalent concentrations produce identical meq values", {
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
      "ug/L"
    )
  )

  x <- env_schema(
    x,
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit
  )

  out <- convert_analytes_to_meq(x)

  expect_equal(
    out$analysis_result_meq[1],
    out$analysis_result_meq[2]
  )
})

test_that("major ions have valid meq factors", {
  major <- analyte_tbl %>%
    dplyr::filter(
      is_major_ion
    )

  expect_true(
    all(
      !is.na(
        major$meq_factor
      )
    )
  )

  expect_true(
    all(
      major$meq_factor > 0
    )
  )
})

test_that("major ions have charge definitions", {
  major <- analyte_tbl %>%
    dplyr::filter(
      is_major_ion
    )

  expect_false(
    any(
      is.na(
        major$charge
      )
    )
  )
})

test_that("major ions have molecular weights", {
  major <- analyte_tbl %>%
    dplyr::filter(
      is_major_ion
    )

  expect_false(
    any(
      is.na(
        major$molecular_weight
      )
    )
  )
})

test_that("meq factors are internally consistent", {
  major <- analyte_tbl %>%
    dplyr::filter(
      is_major_ion
    )

  expected <-
    abs(major$charge) /
    major$molecular_weight

  expect_equal(
    major$meq_factor,
    expected,
    tolerance = 1e-10
  )
})
