test_that("unknown analytes generate warning", {
  expect_warning(
    expect_warning(
      env_schema(
        gw_data,
        location = location_id,
        date = sample_date,
        analyte = param_name,
        result = analysis_result
      ),
      "unsupported analytes"
    ),
    "Analyte metadata missing"
  )
})


test_that("env_schema constructor creates valid object", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result,
      unit = default_unit,
      reporting_flag = lt_measure
    )
  )

  expect_s3_class(x, "env_schema")
  expect_true(is_env_schema(x))
  expect_true(is_valid_env_schema(x))

  expect_no_error(
    validate_env_schema(x)
  )
})


test_that("schema mappings are stored correctly", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result,
      unit = default_unit,
      reporting_flag = lt_measure
    )
  )

  mapping <- schema_mapping(x)

  expect_equal(mapping$location, "location_id")
  expect_equal(mapping$date, "sample_date")
  expect_equal(mapping$analyte, "param_name")
  expect_equal(mapping$result, "analysis_result")
  expect_equal(mapping$unit, "default_unit")
  expect_equal(mapping$reporting_flag, "lt_measure")
})


test_that("required accessors return source columns", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  expect_identical(
    location(x),
    x$location_id
  )

  expect_identical(
    date(x),
    x$sample_date
  )

  expect_identical(
    analyte(x),
    x$param_name
  )

  expect_identical(
    result(x),
    x$analysis_result
  )
})


test_that("optional accessors return source columns", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result,
      unit = default_unit,
      reporting_flag = lt_measure
    )
  )

  expect_identical(
    unit(x),
    x$default_unit
  )

  expect_identical(
    reporting_flag(x),
    x$lt_measure
  )
})


test_that("missing optional mappings return NULL", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  expect_null(unit(x))
  expect_null(reporting_flag(x))
  expect_null(sample_id(x))
  expect_null(method(x))
})


test_that("standardized accessors return manager columns", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result,
      unit = default_unit
    )
  )

  expect_identical(
    analysis_result(x),
    x$.analysis_result
  )

  expect_identical(
    analysis_unit(x),
    x$.analysis_unit
  )

  expect_identical(
    detected(x),
    x$.detected
  )

  expect_identical(
    censored(x),
    x$.censored
  )

  expect_identical(
    estimated(x),
    x$.estimated
  )

  expect_identical(
    rejected(x),
    x$.rejected
  )

  expect_identical(
    usable(x),
    x$.usable
  )
})


test_that("schema metadata accessors work", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  expect_type(
    schema_version(x),
    "character"
  )

  expect_true(
    inherits(
      schema_created(x),
      c("POSIXct", "POSIXt")
    )
  )

  expect_true(
    is.list(schema_mapping(x))
  )

  expect_true(
    is.character(schema_metadata(x))
  )
})


test_that("analyte metadata columns are attached", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result,
      unit = default_unit
    )
  )

  expect_true(
    all(
      c(
        ".analyte_id",
        ".analyte",
        ".short_name",
        ".analyte_group",
        ".normalized_analyte"
      ) %in%
        names(x)
    )
  )
})


test_that("analyte labels are created", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result,
      unit = default_unit
    )
  )

  expect_true(
    ".analyte_label" %in% names(x)
  )

  expect_true(
    ".short_analyte_label" %in% names(x)
  )

  expect_true(
    all(
      nzchar(
        stats::na.omit(
          x$.analyte_label
        )
      )
    )
  )
})


test_that("filter preserves env_schema", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result,
      unit = default_unit
    )
  )

  y <- dplyr::filter(
    x,
    !is.na(.analysis_result)
  )

  expect_s3_class(y, "env_schema")

  expect_identical(
    schema_mapping(y),
    schema_mapping(x)
  )

  expect_no_error(
    validate_env_schema(y)
  )
})


test_that("mutate preserves env_schema", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  y <- dplyr::mutate(
    x,
    test_col = 1
  )

  expect_s3_class(y, "env_schema")

  expect_identical(
    schema_mapping(y),
    schema_mapping(x)
  )

  expect_true(
    "test_col" %in% names(y)
  )
})


test_that("select preserves env_schema", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result,
      unit = default_unit
    )
  )

  y <- dplyr::select(
    x,
    -default_unit
  )

  expect_s3_class(y, "env_schema")

  expect_identical(
    schema_mapping(y),
    schema_mapping(x)
  )

  expect_null(
    unit(y)
  )

  expect_no_error(
    validate_env_schema(y)
  )
})


test_that("required mapped columns must remain present", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  y <- dplyr::select(
    x,
    -location_id
  )

  expect_error(
    validate_env_schema(y),
    "Missing mapped columns"
  )

  y <- dplyr::select(
    x,
    -sample_date
  )

  expect_error(
    validate_env_schema(y),
    "Missing mapped columns"
  )

  y <- dplyr::select(
    x,
    -param_name
  )

  expect_error(
    validate_env_schema(y),
    "Missing mapped columns"
  )

  y <- dplyr::select(
    x,
    -analysis_result
  )

  expect_error(
    validate_env_schema(y),
    "Missing mapped columns"
  )
})


test_that("default methods fail for unsupported classes", {
  expect_error(
    location(mtcars)
  )

  expect_error(
    analyte(mtcars)
  )

  expect_error(
    result(mtcars)
  )

  expect_error(
    analysis_result(mtcars)
  )
})


test_that("has_env_schema_structure detects schema structure", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  expect_true(
    has_env_schema_structure(x)
  )

  y <- tibble::as_tibble(x)

  attr(y, "env_mapping") <- NULL

  expect_false(
    has_env_schema_structure(y)
  )
})

test_that("optional mappings may be removed", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result,
      unit = default_unit
    )
  )

  y <- dplyr::select(
    x,
    -default_unit
  )

  expect_null(unit(y))

  expect_no_error(
    validate_env_schema(y)
  )
})

test_that("slice preserves env_schema", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  y <- dplyr::slice_head(
    x,
    n = 10
  )

  expect_s3_class(y, "env_schema")

  expect_identical(
    schema_mapping(y),
    schema_mapping(x)
  )
})

test_that("as_env_schema preserves metadata", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  y <- as_env_schema(x)

  expect_s3_class(y, "env_schema")

  expect_identical(
    schema_mapping(y),
    schema_mapping(x)
  )
})
