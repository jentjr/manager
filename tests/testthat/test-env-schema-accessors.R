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

  expect_type(
    schema_version(x),
    "character"
  )

  expect_false(
    is.na(schema_version(x))
  )
})


test_that("schema_created returns timestamp", {
  x <- make_schema()

  expect_s3_class(
    schema_created(x),
    c("POSIXct", "POSIXt")
  )
})


test_that("schema_mapping returns stored mappings", {
  x <- make_schema()

  mapping <- schema_mapping(x)

  expect_type(
    mapping,
    "list"
  )

  expect_identical(
    mapping$location,
    "location"
  )

  expect_identical(
    mapping$date,
    "date"
  )

  expect_identical(
    mapping$analyte,
    "analyte"
  )

  expect_identical(
    mapping$result,
    "result"
  )
})


test_that("schema_metadata returns metadata", {
  x <- make_schema()

  meta <- schema_metadata(x)

  expect_true(
    is.list(meta) || is.character(meta)
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


test_that("schema_version works on reconstructed schema objects", {
  x <- make_schema()

  y <- dplyr::select(
    x,
    location,
    date,
    analyte,
    result,
    .analysis_result
  )

  expect_s3_class(
    y,
    "env_schema"
  )

  expect_identical(
    schema_version(y),
    schema_version(x)
  )
})


test_that("schema_mapping survives select", {
  x <- make_schema()

  y <- dplyr::select(
    x,
    location,
    date,
    analyte,
    result
  )

  expect_s3_class(
    y,
    "env_schema"
  )

  expect_identical(
    schema_mapping(y),
    schema_mapping(x)
  )
})


test_that("schema_created survives select", {
  x <- make_schema()

  y <- dplyr::select(
    x,
    location,
    date,
    analyte,
    result
  )

  expect_identical(
    schema_created(y),
    schema_created(x)
  )
})


test_that("schema_metadata survives select", {
  x <- make_schema()

  y <- dplyr::select(
    x,
    location,
    date,
    analyte,
    result
  )

  expect_identical(
    schema_metadata(y),
    schema_metadata(x)
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


test_that("validate_env_schema remains strict for required mapped columns", {
  x <- make_schema()

  y <- dplyr::select(
    x,
    -location
  )

  expect_error(
    validate_env_schema(y),
    "Missing mapped columns"
  )

  y <- dplyr::select(
    x,
    -date
  )

  expect_error(
    validate_env_schema(y),
    "Missing mapped columns"
  )

  y <- dplyr::select(
    x,
    -analyte
  )

  expect_error(
    validate_env_schema(y),
    "Missing mapped columns"
  )

  y <- dplyr::select(
    x,
    -result
  )

  expect_error(
    validate_env_schema(y),
    "Missing mapped columns"
  )
})


test_that("schema_mapping survives mutate", {
  x <- make_schema()

  y <- dplyr::mutate(
    x,
    test_col = 1
  )

  expect_identical(
    schema_mapping(y),
    schema_mapping(x)
  )
})


test_that("schema_mapping survives filter", {
  x <- make_schema()

  y <- dplyr::filter(
    x,
    result > 0
  )

  expect_identical(
    schema_mapping(y),
    schema_mapping(x)
  )
})


test_that("metadata accessors work on valid schema", {
  x <- make_schema()

  expect_no_error(
    schema_version(x)
  )

  expect_no_error(
    schema_created(x)
  )

  expect_no_error(
    schema_mapping(x)
  )

  expect_no_error(
    schema_metadata(x)
  )
})
