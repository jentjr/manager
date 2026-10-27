test_that("as_env_schema returns env_schema unchanged", {
  x <- suppressWarnings(
    env_schema(
      tibble::tibble(
        location = "MW-1",
        date = as.Date("2024-01-01"),
        analyte = "Arsenic",
        result = 1
      ),
      location = location,
      date = date,
      analyte = analyte,
      result = result
    )
  )

  y <- as_env_schema(x)

  expect_identical(y, x)
})


test_that("as_env_schema coerces raw data frame", {
  y <- suppressWarnings(
    as_env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  expect_s3_class(
    y,
    "env_schema"
  )

  expect_true(
    is_env_schema(y)
  )
})


test_that("as_env_schema stores mappings correctly", {
  y <- suppressWarnings(
    as_env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  mapping <- schema_mapping(y)

  expect_equal(
    mapping$location,
    "location_id"
  )

  expect_equal(
    mapping$date,
    "sample_date"
  )

  expect_equal(
    mapping$analyte,
    "param_name"
  )

  expect_equal(
    mapping$result,
    "analysis_result"
  )
})


test_that("as_env_schema creates required manager columns", {
  y <- suppressWarnings(
    as_env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  expect_true(
    all(
      c(
        ".analyte_id",
        ".analyte",
        ".analysis_result",
        ".result_type",
        ".detected",
        ".censored",
        ".usable"
      ) %in%
        names(y)
    )
  )
})


test_that("as_env_schema preserves source columns", {
  y <- suppressWarnings(
    as_env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  expect_true(
    all(
      c(
        "location_id",
        "sample_date",
        "param_name",
        "analysis_result"
      ) %in%
        names(y)
    )
  )
})


test_that("accessors work after coercion", {
  y <- suppressWarnings(
    as_env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  expect_identical(
    location(y),
    y$location_id
  )

  expect_identical(
    date(y),
    y$sample_date
  )

  expect_identical(
    analyte(y),
    y$param_name
  )

  expect_identical(
    result(y),
    y$analysis_result
  )
})


test_that("coerced data validates", {
  y <- suppressWarnings(
    as_env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  expect_no_error(
    validate_env_schema(y)
  )

  expect_true(
    is_valid_env_schema(y)
  )
})


test_that("as_env_schema attaches analyte metadata", {
  y <- suppressWarnings(
    as_env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  expect_true(
    all(
      c(
        ".short_name",
        ".analyte_group",
        ".normalized_analyte"
      ) %in%
        names(y)
    )
  )
})


test_that("as_env_schema creates analyte labels", {
  y <- suppressWarnings(
    as_env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  expect_true(
    ".analyte_label" %in% names(y)
  )

  expect_true(
    ".short_analyte_label" %in% names(y)
  )
})

test_that("validation fails when location mapping points to missing column", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  attr(x, "env_mapping")$location <- "does_not_exist"

  expect_error(
    validate_env_schema(x),
    "Missing mapped columns"
  )
})


test_that("validation fails when date mapping points to missing column", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  attr(x, "env_mapping")$date <- "does_not_exist"

  expect_error(
    validate_env_schema(x),
    "Missing mapped columns"
  )
})


test_that("validation fails when analyte mapping points to missing column", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  attr(x, "env_mapping")$analyte <- "does_not_exist"

  expect_error(
    validate_env_schema(x),
    "Missing mapped columns"
  )
})


test_that("validation fails when location mapping points to missing column", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  attr(x, "env_mapping")$location <- "does_not_exist"

  expect_error(
    validate_env_schema(x),
    "Missing mapped columns"
  )
})


test_that("validation fails when date mapping points to missing column", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  attr(x, "env_mapping")$date <- "does_not_exist"

  expect_error(
    validate_env_schema(x),
    "Missing mapped columns"
  )
})


test_that("validation fails when analyte mapping points to missing column", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  attr(x, "env_mapping")$analyte <- "does_not_exist"

  expect_error(
    validate_env_schema(x),
    "Missing mapped columns"
  )
})


test_that("validation fails when result mapping points to missing column", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  attr(x, "env_mapping")$result <- "does_not_exist"

  expect_error(
    validate_env_schema(x),
    "Missing mapped columns"
  )
})

test_that("corrupted location mapping fails validation", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  attr(x, "env_mapping")$location <- "does_not_exist"

  expect_null(
    location(x)
  )

  expect_error(
    validate_env_schema(x),
    "Missing mapped columns"
  )
})

test_that("optional accessors return NULL when mapped column is missing", {
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

  attr(x, "env_mapping")$unit <- "does_not_exist"

  expect_null(
    unit(x)
  )
})

test_that("validation fails when env_mapping attribute is removed", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  attr(x, "env_mapping") <- NULL

  expect_error(
    validate_env_schema_structure(x),
    "Missing env_mapping attribute"
  )
})

test_that("location column must be character", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  x$location_id <- seq_len(nrow(x))

  expect_error(
    validate_env_schema(x),
    "Location column must be character"
  )
})

test_that("analyte column must be character", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  x$param_name <- seq_len(nrow(x))

  expect_error(
    validate_env_schema(x),
    "'Analyte' must be character"
  )
})

test_that("date column must be Date or POSIXct", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  x$sample_date <- as.character(x$sample_date)

  expect_error(
    validate_env_schema(x),
    "'date' must be a Date or POSIXct column"
  )
})

test_that("result column must be numeric", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  x$analysis_result <- as.character(x$analysis_result)

  expect_error(
    validate_env_schema(x),
    "Result column must be numeric"
  )
})

test_that(".analyte_id must be character", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  x$.analyte_id <- seq_len(nrow(x))

  expect_error(
    validate_env_schema(x),
    "'.analyte_id' must be character"
  )
})

test_that(".original_lab_result must be numeric", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  x$.original_lab_result <- as.character(
    x$.original_lab_result
  )

  expect_error(
    validate_env_schema(x),
    "'.original_lab_result' must be numeric"
  )
})

test_that("classification columns must be logical", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  x$.detected <- as.character(x$.detected)

  expect_error(
    validate_env_schema(x),
    "'.detected' must be logical"
  )
})

test_that(".censored must be logical", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  x$.censored <- as.character(x$.censored)

  expect_error(
    validate_env_schema(x),
    "'.censored' must be logical"
  )
})

test_that(".estimated must be logical", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  x$.estimated <- as.character(x$.estimated)

  expect_error(
    validate_env_schema(x),
    "'.estimated' must be logical"
  )
})

test_that(".rejected must be logical", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  x$.rejected <- as.character(x$.rejected)

  expect_error(
    validate_env_schema(x),
    "'.rejected' must be logical"
  )
})

test_that(".usable must be logical", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  x$.usable <- as.character(x$.usable)

  expect_error(
    validate_env_schema(x),
    "'.usable' must be logical"
  )
})

test_that("optional numeric columns must be numeric", {
  x <- suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result
    )
  )

  if (".charge" %in% names(x)) {
    x$.charge <- as.character(x$.charge)

    expect_error(
      validate_env_schema(x),
      "'.charge' must be numeric"
    )
  }
})
