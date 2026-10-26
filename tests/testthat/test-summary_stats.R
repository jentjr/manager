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

test_that("summary_stats dispatches for env_schema", {
  x <- make_schema()

  out <- summary_stats(x)

  expect_s3_class(
    out,
    "tbl_df"
  )
})

test_that("summary_stats dispatches for data.frame", {
  x <- tibble::tibble(
    location_id = "MW-1",
    param_name = "Arsenic",
    lt_measure = NA_character_,
    analysis_result = 5,
    default_unit = "ug/L"
  )

  out <- summary_stats(x)

  expect_s3_class(
    out,
    "tbl_df"
  )
})

test_that("summary_stats works after filter and mutate", {
  out <- gw %>%
    dplyr::filter(
      .location == unique(.location)[1]
    ) %>%
    dplyr::mutate(
      result_x2 = .analysis_result * 2
    ) %>%
    summary_stats()

  expect_s3_class(
    out,
    "tbl_df"
  )
})

test_that("summary_stats rejects invalid env_schema", {
  x <- make_schema() %>%
    dplyr::select(
      .location,
      .analysis_result
    )

  expect_error(
    summary_stats(x)
  )
})
