test_that("trend_summary dispatches for data.frame", {
  out <- trend_summary(gw_data)

  expect_s3_class(out, "tbl_df")

  expect_gt(
    nrow(out),
    0
  )
})

test_that("trend_summary dispatches for env_schema", {
  out <- trend_summary(gw)

  expect_s3_class(out, "tbl_df")

  expect_true(
    all(
      c(
        ".location",
        ".analyte",
        "n",
        "trend_method",
        "slope",
        "p_value",
        "trend_direction"
      ) %in%
        names(out)
    )
  )
})

test_that("trend_summary returns one row per location and analyte", {
  expected_n <- gw |>
    dplyr::distinct(
      .location,
      .analyte
    ) |>
    nrow()

  out <- trend_summary(gw)

  expect_equal(
    nrow(out),
    expected_n
  )
})

test_that("trend_summary works with lm method", {
  out <- trend_summary(
    gw,
    method = "lm"
  )

  expect_true(
    all(out$trend_method == "lm")
  )
})

test_that("trend_summary works with loess method", {
  out <- trend_summary(
    gw,
    method = "loess"
  )

  expect_true(
    all(out$trend_method == "loess")
  )

  expect_true(
    all(is.na(out$slope))
  )
})

test_that("trend_summary accepts gam method", {
  expect_no_error(
    trend_summary(
      gw |>
        dplyr::filter(
          .location == "MW-1",
          .analyte == "Chloride"
        ),
      method = "gam"
    )
  )
})

test_that("trend_summary works with mann_kendall method", {
  out <- trend_summary(
    gw,
    method = "mann_kendall"
  )

  expect_true(
    all(out$trend_method == "mann_kendall")
  )
})

test_that("trend_summary rejects invalid method", {
  expect_error(
    trend_summary(
      gw,
      method = "banana"
    ),
    "must be one of"
  )
})

test_that("trend_summary works after dplyr verbs", {
  out <- gw |>
    dplyr::filter(
      !is.na(.analysis_result)
    ) |>
    trend_summary()

  expect_s3_class(
    out,
    "tbl_df"
  )
})

test_that("trend_summary handles constant series", {
  x <- tibble::tibble(
    location_id = rep("MW-1", 10),
    param_name = rep("Arsenic", 10),
    sample_date = seq.Date(
      as.Date("2020-01-01"),
      by = "year",
      length.out = 10
    ),
    analysis_result = rep(1, 10)
  )

  out <- trend_summary(x)

  expect_equal(
    nrow(out),
    1
  )

  expect_true(
    is.na(out$slope)
  )
})

test_that("kendall_trend dispatches for env_schema", {
  x <- gw |>
    dplyr::filter(
      .location == dplyr::first(.location),
      .analyte == dplyr::first(.analyte)
    )

  out <- kendall_trend(x)

  expect_false(
    is.null(out)
  )
})

test_that("kendall_trend dispatches for data.frame", {
  x <- gw_data |>
    dplyr::filter(
      location_id == dplyr::first(location_id),
      param_name == dplyr::first(param_name)
    )

  out <- kendall_trend(x)

  expect_false(
    is.null(out)
  )
})

test_that("kendall_trend validates schema", {
  expect_error(
    kendall_trend(
      tibble::tibble(x = 1:5)
    )
  )
})
