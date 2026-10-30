# tests/testthat/test-prepare-major-ion-chemistry.R

make_major_ion_data <- function() {
  tibble::tibble(
    location_id = rep(
      "MW-01",
      6
    ),

    sample_date = rep(
      as.Date("2024-01-01"),
      6
    ),

    param_name = c(
      "Calcium, dissolved",
      "Magnesium, dissolved",
      "Sodium, dissolved",
      "Potassium, dissolved",
      "Chloride, total",
      "Sulfate, total"
    ),

    analysis_result = c(
      100,
      25,
      50,
      5,
      75,
      100
    ),

    default_unit = rep(
      "mg/L",
      6
    )
  )
}

make_major_ion_schema <- function() {
  env_schema(
    make_major_ion_data(),
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit
  )
}

test_that("returns env_schema", {
  out <- .prepare_major_ion_chemistry(
    make_major_ion_schema()
  )

  expect_s3_class(
    out,
    "env_schema"
  )

  expect_no_error(
    validate_env_schema(out)
  )
})

test_that("creates MEQ output columns", {
  out <- .prepare_major_ion_chemistry(
    make_major_ion_schema()
  )

  expect_true(
    ".analysis_result_meq" %in%
      names(out)
  )

  expect_true(
    ".analysis_unit_meq" %in%
      names(out)
  )
})

test_that("MEQ units are assigned", {
  out <- .prepare_major_ion_chemistry(
    make_major_ion_schema()
  )

  expect_true(
    all(
      out$.analysis_unit_meq == "meq/L"
    )
  )
})

test_that("MEQ values are produced", {
  out <- .prepare_major_ion_chemistry(
    make_major_ion_schema()
  )

  expect_true(
    all(
      !is.na(
        out$.analysis_result_meq
      )
    )
  )
})

test_that("calcium MEQ conversion is correct", {
  out <- .prepare_major_ion_chemistry(
    make_major_ion_schema()
  )

  calcium <- dplyr::filter(
    out,
    .analyte_id == "calcium"
  )

  expected <-
    calcium$.analysis_result *
    analyte_tbl$meq_factor[
      analyte_tbl$analyte_id == "calcium"
    ]

  expect_equal(
    calcium$.analysis_result_meq,
    expected
  )
})

test_that("duplicate analytes are averaged before MEQ conversion", {
  x <- tibble::tibble(
    location_id = c(
      "MW-01",
      "MW-01"
    ),

    sample_date = as.Date(
      c(
        "2024-01-01",
        "2024-01-01"
      )
    ),

    param_name = c(
      "Calcium, dissolved",
      "Calcium, dissolved"
    ),

    analysis_result = c(
      100,
      120
    ),

    default_unit = c(
      "mg/L",
      "mg/L"
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

  out <- .prepare_major_ion_chemistry(
    x
  )

  expect_equal(
    out$.analysis_result,
    110
  )
})

test_that("returns one row per location date analyte", {
  out <- .prepare_major_ion_chemistry(
    make_major_ion_schema()
  )

  expect_equal(
    nrow(out),
    6
  )
})

test_that("exact matching preserves analyte labels", {
  out <- .prepare_major_ion_chemistry(
    make_major_ion_schema(),
    analyte_matching = "exact"
  )

  expect_setequal(
    unique(out$analyte),
    c(
      "Calcium, dissolved",
      "Magnesium, dissolved",
      "Sodium, dissolved",
      "Potassium, dissolved",
      "Chloride, total",
      "Sulfate, total"
    )
  )
})

test_that("strict matching returns strict analyte keys", {
  out <- .prepare_major_ion_chemistry(
    make_major_ion_schema(),
    analyte_matching = "strict"
  )

  expect_true(
    all(
      grepl(
        "::",
        unique(out$analyte),
        fixed = TRUE
      )
    )
  )
})

test_that("normalized matching returns normalized analyte names", {
  out <- .prepare_major_ion_chemistry(
    make_major_ion_schema(),
    analyte_matching = "normalized"
  )

  expect_setequal(
    unique(out$analyte),
    c(
      "calcium",
      "magnesium",
      "sodium",
      "potassium",
      "chloride",
      "sulfate"
    )
  )
})

test_that(".analyte_id is invariant across matching modes", {
  exact <- .prepare_major_ion_chemistry(
    make_major_ion_schema(),
    analyte_matching = "exact"
  )

  strict <- .prepare_major_ion_chemistry(
    make_major_ion_schema(),
    analyte_matching = "strict"
  )

  normalized <- .prepare_major_ion_chemistry(
    make_major_ion_schema(),
    analyte_matching = "normalized"
  )

  expect_identical(
    sort(unique(exact$.analyte_id)),
    sort(unique(strict$.analyte_id))
  )

  expect_identical(
    sort(unique(exact$.analyte_id)),
    sort(unique(normalized$.analyte_id))
  )
})

test_that("MEQ values are invariant across matching modes", {
  exact <- .prepare_major_ion_chemistry(
    make_major_ion_schema(),
    analyte_matching = "exact"
  )

  strict <- .prepare_major_ion_chemistry(
    make_major_ion_schema(),
    analyte_matching = "strict"
  )

  normalized <- .prepare_major_ion_chemistry(
    make_major_ion_schema(),
    analyte_matching = "normalized"
  )

  expect_equal(
    exact$.analysis_result_meq,
    strict$.analysis_result_meq
  )

  expect_equal(
    exact$.analysis_result_meq,
    normalized$.analysis_result_meq
  )
})

test_that("MEQ metadata columns are registered", {
  out <- .prepare_major_ion_chemistry(
    make_major_ion_schema()
  )

  expect_true(
    ".analysis_result_meq" %in%
      attr(
        out,
        "analyte_metadata"
      )
  )

  expect_true(
    ".analysis_unit_meq" %in%
      attr(
        out,
        "analyte_metadata"
      )
  )
})
