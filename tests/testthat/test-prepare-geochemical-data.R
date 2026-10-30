# tests/testthat/test-prepare-geochemical-data.R

make_geo_data <- function() {
  tibble::tibble(
    location_id = c(
      "MW-01",
      "MW-01",
      "MW-01",
      "MW-01",
      "MW-01"
    ),

    sample_date = as.Date(
      c(
        "2024-01-01",
        "2024-01-01",
        "2024-01-01",
        "2024-01-01",
        "2024-01-01"
      )
    ),

    param_name = c(
      "Calcium, dissolved",
      "Calcium, dissolved",
      "Magnesium, dissolved",
      "Sodium, dissolved",
      "Iron, dissolved"
    ),

    analysis_result = c(
      100,
      120,
      24,
      10,
      200
    ),

    default_unit = c(
      "mg/L",
      "mg/L",
      "mg/L",
      "mg/L",
      "ug/L"
    )
  )
}

make_geo_schema <- function() {
  env_schema(
    make_geo_data(),
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit
  )
}

test_that("requires env_schema input", {
  expect_error(
    .prepare_geochemical_data(
      tibble::tibble(),
      analytes = "Calcium, dissolved"
    ),
    "env_schema"
  )
})

test_that("filters requested analytes", {
  out <- .prepare_geochemical_data(
    make_geo_schema(),
    analytes = c(
      "Calcium, dissolved",
      "Magnesium, dissolved"
    )
  )

  expected <- .resolve_geochemical_analytes(
    c(
      "Calcium, dissolved",
      "Magnesium, dissolved"
    )
  )

  expect_true(
    all(
      out$analyte %in% expected
    )
  )

  expect_equal(
    sort(unique(out$analyte)),
    sort(expected)
  )
})

test_that("additional analytes are included", {
  out <- .prepare_geochemical_data(
    make_geo_schema(),
    analytes = "Calcium, dissolved",
    additional_analytes = "Magnesium, dissolved"
  )

  expect_equal(
    length(
      unique(out$analyte)
    ),
    2
  )
})

test_that("duplicate analytes are averaged", {
  out <- .prepare_geochemical_data(
    make_geo_schema(),
    analytes = "Calcium, dissolved"
  )

  expect_equal(
    nrow(out),
    1
  )

  expect_equal(
    out$.analysis_result,
    110
  )
})

test_that("aggregation occurs within location and date", {
  x <- dplyr::bind_rows(
    make_geo_data(),

    tibble::tibble(
      location_id = "MW-02",
      sample_date = as.Date(
        "2024-01-01"
      ),
      param_name = "Calcium, dissolved",
      analysis_result = 200,
      default_unit = "mg/L"
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

  out <- .prepare_geochemical_data(
    x,
    analytes = "Calcium, dissolved"
  )

  expect_equal(
    nrow(out),
    2
  )
})

test_that("returns analysis result column", {
  out <- .prepare_geochemical_data(
    make_geo_schema(),
    analytes = "Calcium, dissolved"
  )

  expect_true(
    ".analysis_result" %in%
      names(out)
  )
})

test_that("returns analysis unit column", {
  out <- .prepare_geochemical_data(
    make_geo_schema(),
    analytes = "Calcium, dissolved"
  )

  expect_true(
    ".analysis_unit" %in%
      names(out)
  )
})

test_that("returns analyte column", {
  out <- .prepare_geochemical_data(
    make_geo_schema(),
    analytes = "Calcium, dissolved"
  )

  expect_true(
    "analyte" %in%
      names(out)
  )
})

test_that("analyte column contains resolved analytes", {
  out <- .prepare_geochemical_data(
    make_geo_schema(),
    analytes = "Calcium, dissolved"
  )

  expect_equal(
    unique(out$analyte),
    .resolve_geochemical_analytes(
      "Calcium, dissolved"
    )
  )
})

test_that("analysis units remain standardized", {
  out <- .prepare_geochemical_data(
    make_geo_schema(),
    analytes = "Calcium, dissolved"
  )

  expect_equal(
    length(
      unique(out$.analysis_unit)
    ),
    1
  )
})

test_that("supports exact matching", {
  expect_no_error(
    .prepare_geochemical_data(
      make_geo_schema(),
      analytes = "Calcium, dissolved",
      analyte_matching = "exact"
    )
  )
})

test_that("supports strict matching", {
  expect_no_error(
    .prepare_geochemical_data(
      make_geo_schema(),
      analytes = "Calcium, dissolved",
      analyte_matching = "strict"
    )
  )
})

test_that("supports normalized matching", {
  expect_no_error(
    .prepare_geochemical_data(
      make_geo_schema(),
      analytes = "Calcium, dissolved",
      analyte_matching = "normalized"
    )
  )
})

test_that("unknown analytes return zero rows", {
  out <- .prepare_geochemical_data(
    make_geo_schema(),
    analytes = "Imaginary Analyte"
  )

  expect_equal(
    nrow(out),
    0
  )
})

test_that("multiple requested analytes are retained", {
  out <- .prepare_geochemical_data(
    make_geo_schema(),
    analytes = c(
      "Calcium, dissolved",
      "Magnesium, dissolved",
      "Sodium, dissolved"
    )
  )

  expect_equal(
    length(
      unique(out$analyte)
    ),
    3
  )
})
