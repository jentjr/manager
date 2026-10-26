test_that("mutate preserves env_schema", {
  x <- env_schema(
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

  y <- x %>%
    dplyr::mutate(
      log_result = log(.analysis_result)
    )

  expect_s3_class(
    y,
    "env_schema"
  )

  expect_true(
    has_env_schema_structure(y)
  )

  expect_no_error(
    validate_env_schema(y)
  )
})


test_that("filter preserves env_schema", {
  x <- env_schema(
    tibble::tibble(
      location = c("MW-1", "MW-2"),
      date = as.Date(c(
        "2024-01-01",
        "2024-01-01"
      )),
      analyte = c(
        "Arsenic",
        "Arsenic"
      ),
      result = c(1, 2)
    ),
    location = location,
    date = date,
    analyte = analyte,
    result = result
  )

  y <- x %>%
    dplyr::filter(
      .location == "MW-1"
    )

  expect_s3_class(
    y,
    "env_schema"
  )

  expect_true(
    has_env_schema_structure(y)
  )

  expect_no_error(
    validate_env_schema(y)
  )
})


test_that("select preserves env_schema subclass", {
  x <- env_schema(
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

  expect_error(
    validate_env_schema(y),
    "Missing required schema columns"
  )
})


test_that("rename preserves env_schema subclass", {
  x <- env_schema(
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

  y <- x %>%
    dplyr::rename(
      result_value = .analysis_result
    )

  expect_s3_class(
    y,
    "env_schema"
  )

  expect_false(
    has_env_schema_structure(y)
  )

  expect_error(
    validate_env_schema(y),
    "Missing required schema columns"
  )
})


test_that("arrange preserves env_schema", {
  x <- env_schema(
    tibble::tibble(
      location = c("MW-2", "MW-1"),
      date = as.Date(c(
        "2024-01-02",
        "2024-01-01"
      )),
      analyte = "Arsenic",
      result = c(2, 1)
    ),
    location = location,
    date = date,
    analyte = analyte,
    result = result
  )

  y <- x %>%
    dplyr::arrange(
      .analysis_result
    )

  expect_s3_class(
    y,
    "env_schema"
  )

  expect_true(
    has_env_schema_structure(y)
  )

  expect_no_error(
    validate_env_schema(y)
  )
})


test_that("class and validity are distinct concepts", {
  x <- env_schema(
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

  y <- x %>%
    dplyr::select(
      .location,
      .analysis_result
    )

  expect_true(
    inherits(y, "env_schema")
  )

  expect_false(
    has_env_schema_structure(y)
  )

  expect_error(
    validate_env_schema(y)
  )
})


test_that("env_schema metadata survives dplyr operations", {
  x <- env_schema(
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

  y <- x %>%
    dplyr::mutate(
      x = 1
    )

  expect_identical(
    attr(y, "env_schema_version"),
    attr(x, "env_schema_version")
  )

  expect_identical(
    attr(y, "env_mapping"),
    attr(x, "env_mapping")
  )
})
