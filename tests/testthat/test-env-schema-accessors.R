make_schema <- function() {
  env_schema(
    tibble::tibble(
      location = "MW-1",
      date = as.Date("2024-01-01"),
      analyte = "Arsenic",
      result = 5
    ),
    location = location,
    date = date,
    analyte = analyte,
    result = result
  )
}

test_that("schema_version returns version", {
  x <- make_schema()

  expect_identical(
    schema_version(x),
    "0.4.0"
  )
})

test_that("schema_created returns timestamp", {
  x <- make_schema()

  expect_s3_class(
    schema_created(x),
    c("POSIXct", "POSIXt")
  )
})

test_that("schema_mapping returns mapping metadata", {
  x <- make_schema()

  mapping <- schema_mapping(x)

  expect_type(
    mapping,
    "list"
  )

  expect_true(
    "source" %in% names(mapping)
  )

  expect_true(
    ".location" %in% names(mapping$source)
  )

  expect_identical(
    mapping$source$.location,
    "location"
  )
})

test_that("schema_metadata returns metadata columns", {
  x <- make_schema()

  meta <- schema_metadata(x)

  expect_type(
    meta,
    "character"
  )
})

test_that("schema_result_types returns result counts", {
  x <- make_schema()

  out <- schema_result_types(x)

  expect_s3_class(
    out,
    "tbl_df"
  )

  expect_true(
    all(
      c(".result_type", "n") %in%
        names(out)
    )
  )
})

test_that("schema_version works on invalid env_schema subclasses", {
  x <- make_schema()

  y <- x %>%
    dplyr::select(
      .location,
      .analysis_result
    )

  expect_s3_class(
    y,
    "env_schema"
  )

  expect_false(
    has_env_schema_structure(y)
  )

  expect_identical(
    schema_version(y),
    "0.4.0"
  )
})

test_that("schema_mapping survives select", {
  x <- make_schema()

  y <- x %>%
    dplyr::select(
      .location,
      .analysis_result
    )

  expect_no_error(
    mapping <- schema_mapping(y)
  )

  expect_type(
    mapping,
    "list"
  )
})

test_that("schema_created survives select", {
  x <- make_schema()

  y <- x %>%
    dplyr::select(
      .location,
      .analysis_result
    )

  expect_no_error(
    schema_created(y)
  )
})

test_that("accessors reject non env_schema objects", {
  x <- tibble::tibble(
    a = 1
  )

  expect_error(
    schema_version(x)
  )

  expect_error(
    schema_mapping(x)
  )

  expect_error(
    schema_created(x)
  )

  expect_error(
    schema_metadata(x)
  )
})

test_that("validate_env_schema remains strict", {
  x <- make_schema()

  y <- x %>%
    dplyr::select(
      .location,
      .analysis_result
    )

  expect_error(
    validate_env_schema(y),
    "Missing required schema columns"
  )
})
