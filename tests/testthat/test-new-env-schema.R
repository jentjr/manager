test_that("new_env_schema creates env_schema object", {
  x <- tibble::tibble(
    .location = "MW-1",
    .date = as.Date("2024-01-01"),
    .analyte = "Arsenic",
    .analyte_id = "arsenic",
    .analysis_result = 1
  )

  out <- new_env_schema(x)

  expect_s3_class(out, "env_schema")
  expect_s3_class(out, "tbl_df")
})

test_that("new_env_schema populates analyte_metadata automatically", {
  x <- tibble::tibble(
    .location = "MW-1",
    .date = as.Date("2024-01-01"),
    .analyte = "Arsenic",
    .analyte_id = "arsenic",
    .analysis_result = 1,
    .short_name = "As",
    .casrn = "7440-38-2"
  )

  out <- new_env_schema(x)

  expect_true(
    ".short_name" %in%
      attr(out, "analyte_metadata")
  )

  expect_true(
    ".casrn" %in%
      attr(out, "analyte_metadata")
  )
})

test_that("new_env_schema stores package lookup source", {
  x <- tibble::tibble(
    .location = "MW-1",
    .date = as.Date("2024-01-01"),
    .analyte = "Arsenic",
    .analyte_id = "arsenic",
    .analysis_result = 1
  )

  out <- new_env_schema(
    x,
    analyte_lookup = analyte_tbl
  )

  expect_identical(
    attr(out, "analyte_lookup_source"),
    "package"
  )
})

test_that("new_env_schema stores user lookup source", {
  lookup <- analyte_tbl

  lookup <- lookup %>%
    dplyr::slice(1:5)

  x <- tibble::tibble(
    .location = "MW-1",
    .date = as.Date("2024-01-01"),
    .analyte = "Arsenic",
    .analyte_id = "arsenic",
    .analysis_result = 1
  )

  out <- new_env_schema(
    x,
    analyte_lookup = lookup
  )

  expect_identical(
    attr(out, "analyte_lookup_source"),
    "user"
  )
})
