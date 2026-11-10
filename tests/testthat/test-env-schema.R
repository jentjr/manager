test_that("unknown analytes generate warning", {
  expect_warning(
    env_schema(
      gw_data,
      location = location_id,
      sample_date = sample_date,
      analyte = param_name,
      result = analysis_result
    ),
    "not found in analyte metadata"
  )
})

test_that("env_schema constructor creates valid object", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      sample_date = sample_date,
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
      sample_date = sample_date,
      analyte = param_name,
      result = analysis_result,
      unit = default_unit,
      reporting_flag = lt_measure
    )
  )

  mapping <- schema_mapping(x)

  expect_equal(mapping$location, "location_id")
  expect_equal(mapping$sample_date, "sample_date")
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
      sample_date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  expect_identical(
    location(x),
    x$location_id
  )

  expect_identical(
    sample_date(x),
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
      sample_date = sample_date,
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
      sample_date = sample_date,
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
      sample_date = sample_date,
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
      sample_date = sample_date,
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
      sample_date = sample_date,
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
      sample_date = sample_date,
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
      sample_date = sample_date,
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
      sample_date = sample_date,
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
      sample_date = sample_date,
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

  expect_null(unit(y))

  expect_null(
    schema_mapping(y)$unit
  )

  expect_equal(
    schema_mapping(y)$location,
    schema_mapping(x)$location
  )

  expect_equal(
    schema_mapping(y)$sample_date,
    schema_mapping(x)$sample_date
  )

  expect_equal(
    schema_mapping(y)$analyte,
    schema_mapping(x)$analyte
  )

  expect_equal(
    schema_mapping(y)$result,
    schema_mapping(x)$result
  )
})


test_that("required mapped columns must remain present", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      sample_date = sample_date,
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
      sample_date = sample_date,
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
      sample_date = sample_date,
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
      sample_date = sample_date,
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
      sample_date = sample_date,
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


# -------------------------------------------------------------------------
# Display metadata
# -------------------------------------------------------------------------

test_that("display metadata columns are created", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      sample_date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  expect_true(
    all(
      c(
        ".display_result",
        ".display_threshold",
        ".display_threshold_type"
      ) %in%
        names(x)
    )
  )
})

test_that("default display options are attached", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      sample_date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  opts <- display_options(x)

  expect_true(is.list(opts))

  expect_equal(opts$digits, 6)
  expect_false(opts$scientific)
  expect_true(opts$trim_trailing_zeros)
  expect_equal(opts$censor_prefix, "<")
  expect_equal(opts$report_style, "standard")
})

test_that("display_options accessor returns stored options", {
  opts <- list(
    digits = 3,
    scientific = TRUE,
    trim_trailing_zeros = FALSE,
    censor_prefix = "ND",
    report_style = "verbose"
  )

  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      sample_date = sample_date,
      analyte = param_name,
      result = analysis_result,
      display_options = opts
    )
  )

  expect_identical(
    display_options(x),
    opts
  )
})

test_that("invalid display options fail validation", {
  expect_error(
    env_schema(
      gw_data,
      location = location_id,
      sample_date = sample_date,
      analyte = param_name,
      result = analysis_result,
      display_options = list()
    )
  )

  expect_error(
    env_schema(
      gw_data,
      location = location_id,
      sample_date = sample_date,
      analyte = param_name,
      result = analysis_result,
      display_options = modifyList(
        default_display_options(),
        list(digits = 0)
      )
    ),
    "positive integer"
  )

  expect_error(
    env_schema(
      gw_data,
      location = location_id,
      sample_date = sample_date,
      analyte = param_name,
      result = analysis_result,
      display_options = modifyList(
        default_display_options(),
        list(report_style = "invalid")
      )
    ),
    "standard.*verbose"
  )
})

test_that("set_display_options updates schema display options", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      sample_date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  y <- set_display_options(
    x,
    digits = 3,
    report_style = "verbose"
  )

  expect_equal(
    display_options(y)$digits,
    3
  )

  expect_equal(
    display_options(y)$report_style,
    "verbose"
  )

  expect_s3_class(
    y,
    "env_schema"
  )

  expect_no_error(
    validate_env_schema(y)
  )
})

test_that("display options survive dplyr operations", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      sample_date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  x <- set_display_options(
    x,
    digits = 3,
    report_style = "verbose"
  )

  y <- dplyr::filter(
    x,
    !is.na(.analysis_result)
  )

  expect_identical(
    display_options(y),
    display_options(x)
  )
})

test_that("display threshold type values are valid", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      sample_date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  valid <- c(
    "reporting_limit",
    "quantitation_limit",
    "detection_limit",
    "analysis_result"
  )

  expect_true(
    all(
      stats::na.omit(
        x$.display_threshold_type
      ) %in%
        valid
    )
  )
})

test_that("corrupt display options fail schema validation", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      sample_date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  attr(
    x,
    "display_options"
  ) <- list()

  expect_error(
    validate_env_schema(x),
    "Missing display options"
  )
})

test_that("corrupt display threshold type fails validation", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      sample_date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  x$.display_threshold_type <- "bad_type"

  expect_error(
    validate_env_schema(x),
    "Invalid display threshold type"
  )
})

test_that("refresh_display_results regenerates display output", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      sample_date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  x$.display_result <- NA_character_

  expect_true(
    all(is.na(x$.display_result))
  )

  x <- refresh_display_results(x)

  expect_false(
    all(is.na(x$.display_result))
  )
})

test_that("reporting limit is preferred for display threshold", {
  dat <- tibble::tibble(
    location_id = "MW-1",
    sample_date = as.Date("2024-01-01"),
    param_name = "Arsenic",
    analysis_result = 0.001,
    reporting_limit = 0.005,
    lt_measure = "U"
  )

  x <- env_schema(
    dat,
    location = location_id,
    sample_date = sample_date,
    analyte = param_name,
    result = analysis_result,
    reporting_limit = reporting_limit,
    reporting_flag = lt_measure
  )

  expect_equal(
    x$.display_threshold,
    0.005
  )

  expect_equal(
    x$.display_threshold_type,
    "reporting_limit"
  )

  expect_equal(
    x$.display_result,
    "<0.005"
  )
})

test_that("to_wide returns analysis results by default", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      sample_date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  expect_no_error(
    to_wide(x)
  )
})

test_that("to_wide supports display results", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      sample_date = sample_date,
      analyte = param_name,
      result = analysis_result,
      reporting_flag = lt_measure
    )
  )

  y <- to_wide(
    x,
    result = "display"
  )

  expect_s3_class(
    y,
    "tbl_df"
  )
})

test_that("source metadata survives reconstruction", {
  gw <- env_schema(
    tibble::tibble(
      location_id = "MW-1",
      sample_date = Sys.Date(),
      analyte = "Benzene",
      result = 1,
      .site_name = "Site A",
      .water_class = "GW"
    ),
    location = location_id,
    sample_date = sample_date,
    analyte = analyte,
    result = result
  )

  attr(gw, "source_metadata") <- c(
    ".site_name",
    ".water_class"
  )

  gw2 <- gw |>
    dplyr::mutate(x = 1)

  expect_equal(
    source_metadata(gw2),
    c(".site_name", ".water_class")
  )
})
