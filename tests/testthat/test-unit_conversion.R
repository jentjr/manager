test_that(".standardize_ion_name strips qualifiers", {
  expect_equal(
    .standardize_ion_name("Calcium, dissolved"),
    "Calcium"
  )

  expect_equal(
    .standardize_ion_name("Calcium, total"),
    "Calcium"
  )

  expect_equal(
    .standardize_ion_name("Calcium as CaCO3"),
    "Calcium"
  )
})

test_that(".standardize_ion_name is case insensitive", {
  expect_equal(
    .standardize_ion_name("CALCIUM"),
    "Calcium"
  )

  expect_equal(
    .standardize_ion_name("calcium, dissolved"),
    "Calcium"
  )

  expect_equal(
    .standardize_ion_name("CALCIUM AS CACO3"),
    "Calcium"
  )
})

test_that(".convert_ions_to_meq handles analyte aliases", {
  df <- tibble::tibble(
    param_name = c(
      "Calcium",
      "Calcium, dissolved",
      "Ca",
      "Ca+2",
      "Calcium as CaCO3"
    ),
    default_unit = "mg/L",
    analysis_result = 40
  )

  result <- .convert_ions_to_meq(df)

  expect_equal(
    length(unique(result$analysis_result)),
    1
  )
})

test_that(".convert_ions_to_meq converts mg/L to meq/L", {
  df <- tibble::tibble(
    param_name = "Calcium",
    default_unit = "mg/L",
    analysis_result = 40
  )

  result <- .convert_ions_to_meq(df)

  expected <- 40 * 2 / 40.078

  expect_equal(
    result$analysis_result,
    expected,
    tolerance = 1e-6
  )
})

test_that(".convert_ions_to_meq leaves meq/L values unchanged", {
  df <- tibble::tibble(
    param_name = "Calcium",
    default_unit = "meq/L",
    analysis_result = 5
  )

  result <- .convert_ions_to_meq(df)

  expect_equal(
    result$analysis_result,
    5
  )
})

test_that(".convert_ions_to_meq handles multiple analytes", {
  df <- tibble::tibble(
    param_name = c(
      "Calcium",
      "Magnesium"
    ),
    default_unit = c(
      "mg/L",
      "mg/L"
    ),
    analysis_result = c(
      40,
      24
    )
  )

  result <- .convert_ions_to_meq(df)

  expect_equal(
    result$analysis_result[1],
    40 * 2 / 40.078,
    tolerance = 1e-6
  )

  expect_equal(
    result$analysis_result[2],
    24 * 2 / 24.305,
    tolerance = 1e-6
  )
})

test_that(".convert_ions_to_meq removes helper columns after conversion", {
  df <- tibble::tibble(
    param_name = "Calcium",
    default_unit = "mg/L",
    analysis_result = 40
  )

  result <- .convert_ions_to_meq(df)

  expect_false("charge" %in% names(result))
  expect_false("molecular_weight" %in% names(result))
})

test_that(".convert_ions_to_meq preserves row count", {
  df <- tibble::tibble(
    param_name = c(
      "Calcium",
      "Magnesium"
    ),
    default_unit = c(
      "mg/L",
      "mg/L"
    ),
    analysis_result = c(
      40,
      24
    )
  )

  result <- .convert_ions_to_meq(df)

  expect_equal(
    nrow(result),
    nrow(df)
  )
})

test_that(".convert_ions_to_meq calls validator", {
  df <- tibble::tibble(
    param_name = "Calcium",
    default_unit = "mg/L",
    analysis_result = 40
  )

  called <- FALSE

  mockery::stub(
    .convert_ions_to_meq,
    ".validate_ion_units",
    function(...) {
      called <<- TRUE
      invisible(NULL)
    }
  )

  .convert_ions_to_meq(df)

  expect_true(called)
})

test_that(".convert_ions_to_meq returns expected output structure", {
  df <- tibble::tibble(
    location_id = "MW-1",
    param_name = "Calcium",
    default_unit = "mg/L",
    analysis_result = 40
  )

  result <- .convert_ions_to_meq(df)

  expect_named(
    result,
    c(
      "location_id",
      "param_name",
      "default_unit",
      "analysis_result"
    )
  )

  expect_s3_class(result, "tbl_df")
})
