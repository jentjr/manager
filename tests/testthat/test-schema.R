test_that("env_schema constructs a valid schema", {
  x <- env_schema(
    gw_data,
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    reporting_flag = lt_measure
  )

  expect_s3_class(
    x,
    "env_schema"
  )

  expect_true(
    is_env_schema(x)
  )

  expect_true(
    all(
      c(
        ".location",
        ".date",
        ".analyte",
        ".original_lab_result",
        ".analysis_result"
      ) %in%
        names(x)
    )
  )
})

test_that("validate_env_schema succeeds for valid schema", {
  x <- env_schema(
    gw_data,
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result
  )

  expect_invisible(
    validate_env_schema(x)
  )
})

test_that("validate_env_schema rejects non-schema objects", {
  expect_error(
    validate_env_schema(gw_data),
    "env_schema"
  )
})

test_that("original result and analysis result are created", {
  x <- env_schema(
    gw_data,
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result
  )

  expect_equal(
    x$.original_lab_result,
    gw_data$analysis_result
  )

  expect_equal(
    x$.analysis_result,
    gw_data$analysis_result
  )
})

test_that("units are created when supplied", {
  x <- env_schema(
    gw_data,
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit
  )

  expect_true(
    ".original_lab_unit" %in%
      names(x)
  )

  expect_true(
    ".analysis_unit" %in%
      names(x)
  )
})

test_that("unit columns are optional", {
  x <- env_schema(
    gw_data,
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result
  )

  expect_false(
    ".original_lab_unit" %in%
      names(x)
  )

  expect_false(
    ".analysis_unit" %in%
      names(x)
  )
})

test_that("qualifier classification identifies detects", {
  x <- tibble::tibble(
    location = "MW-1",
    date = Sys.Date(),
    analyte = "Chloride",
    result = 10,
    qualifier = NA_character_
  )

  x <- env_schema(
    x,
    location = location,
    date = date,
    analyte = analyte,
    result = result,
    reporting_flag = qualifier
  )

  expect_equal(
    x$.result_type,
    "detect"
  )

  expect_true(
    x$.detected
  )
})

test_that("qualifier classification identifies non-detects", {
  x <- tibble::tibble(
    location = "MW-1",
    date = Sys.Date(),
    analyte = "Chloride",
    result = 1,
    qualifier = "<"
  )

  x <- env_schema(
    x,
    location = location,
    date = date,
    analyte = analyte,
    result = result,
    reporting_flag = qualifier
  )

  expect_equal(
    x$.result_type,
    "nondetect"
  )

  expect_true(
    x$.censored
  )
})

test_that("qualifier classification identifies estimated values", {
  x <- tibble::tibble(
    location = "MW-1",
    date = Sys.Date(),
    analyte = "Chloride",
    result = 1,
    qualifier = "J"
  )

  x <- env_schema(
    x,
    location = location,
    date = date,
    analyte = analyte,
    result = result,
    reporting_flag = qualifier
  )

  expect_equal(
    x$.result_type,
    "estimated"
  )

  expect_true(
    x$.estimated
  )
})

test_that("qualifier classification identifies rejected values", {
  x <- tibble::tibble(
    location = "MW-1",
    date = Sys.Date(),
    analyte = "Chloride",
    result = 1,
    qualifier = "R"
  )

  x <- env_schema(
    x,
    location = location,
    date = date,
    analyte = analyte,
    result = result,
    reporting_flag = qualifier
  )

  expect_equal(
    x$.result_type,
    "rejected"
  )

  expect_true(
    x$.rejected
  )
})

test_that("unknown qualifiers are identified", {
  x <- tibble::tibble(
    location = "MW-1",
    date = Sys.Date(),
    analyte = "Chloride",
    result = 1,
    qualifier = "XYZ"
  )

  x <- env_schema(
    x,
    location = location,
    date = date,
    analyte = analyte,
    result = result,
    reporting_flag = qualifier
  )

  expect_equal(
    x$.result_type,
    "unknown"
  )
})

test_that("relative uncertainty is calculated", {
  x <- tibble::tibble(
    location = "MW-1",
    date = Sys.Date(),
    analyte = "Ra-226",
    result = 10,
    uncertainty = 2
  )

  x <- env_schema(
    x,
    location = location,
    date = date,
    analyte = analyte,
    result = result,
    measurement_uncertainty = uncertainty
  )

  expect_equal(
    x$.relative_uncertainty,
    0.2
  )
})

test_that("relative uncertainty handles zero result", {
  x <- tibble::tibble(
    location = "MW-1",
    date = Sys.Date(),
    analyte = "Ra-226",
    result = 0,
    uncertainty = 2
  )

  x <- env_schema(
    x,
    location = location,
    date = date,
    analyte = analyte,
    result = result,
    measurement_uncertainty = uncertainty
  )

  expect_true(
    is.na(
      x$.relative_uncertainty
    )
  )
})

test_that("schema metadata attribute is stored", {
  x <- env_schema(
    gw_data,
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result,
    metadata = lt_measure
  )

  expect_equal(
    schema_metadata(x),
    "lt_measure"
  )
})

test_that("schema mapping attribute is stored", {
  x <- env_schema(
    gw_data,
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result
  )

  mapping <- schema_mapping(x)

  expect_equal(
    mapping$.location,
    "location_id"
  )

  expect_equal(
    mapping$.date,
    "sample_date"
  )

  expect_equal(
    mapping$.analyte,
    "param_name"
  )
})

test_that("schema version is stored", {
  x <- env_schema(
    gw_data,
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result
  )

  expect_equal(
    schema_version(x),
    "0.2.0"
  )
})

test_that("schema created timestamp exists", {
  x <- env_schema(
    gw_data,
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result
  )

  expect_s3_class(
    schema_created(x),
    "POSIXct"
  )
})

test_that("missing required column throws error", {
  bad <- dplyr::select(
    gw_data,
    -analysis_result
  )

  expect_error(
    env_schema(
      bad,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    ),
    "Missing columns"
  )
})

test_that("custom qualifier table overrides defaults", {
  quals <- qualifier_table() %>%
    dplyr::add_row(
      reporting_flag = "B",
      result_type = "detect",
      detected = TRUE,
      censored = FALSE,
      estimated = FALSE,
      rejected = FALSE
    )

  x <- tibble::tibble(
    location = "MW-1",
    date = Sys.Date(),
    analyte = "Chloride",
    result = 1,
    qualifier = "B"
  )

  x <- env_schema(
    x,
    location = location,
    date = date,
    analyte = analyte,
    result = result,
    reporting_flag = qualifier,
    qualifier_table = quals
  )

  expect_equal(
    x$.result_type,
    "detect"
  )
})

test_that("schema creation preserves row count", {
  x <- env_schema(
    gw_data,
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result
  )

  expect_equal(
    nrow(x),
    nrow(gw_data)
  )
})
