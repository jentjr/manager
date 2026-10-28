test_that("trend method is recorded", {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Arsenic",
    sample_date = seq.Date(
      as.Date("2020-01-01"),
      by = "month",
      length.out = 24
    ),
    result = sin(
      seq(0, 2 * pi, length.out = 24)
    )
  )

  x <- env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result
  )

  out <- manager:::.compute_group_trends(
    x,
    method = "gam"
  )

  expect_true(
    all(out$.trend_method == "gam")
  )
})

test_that("compute_group_trends returns one fitted series per group", {
  x <- tibble::tibble(
    location = rep(
      c("MW-1", "MW-2"),
      each = 10
    ),
    analyte = rep(
      c("Arsenic", "Arsenic"),
      each = 10
    ),
    sample_date = rep(
      seq.Date(
        as.Date("2020-01-01"),
        by = "year",
        length.out = 10
      ),
      2
    ),
    result = c(
      1:10,
      11:20
    )
  )

  x <- env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result
  )

  out <- manager:::.compute_group_trends(
    x,
    method = "lm"
  )

  expect_s3_class(out, "tbl_df")

  expect_true(
    "fit" %in% names(out)
  )

  expect_equal(
    dplyr::n_distinct(out$location),
    2
  )
})

test_that("compute_group_trends adds trend metadata columns", {
  out <- manager:::.compute_group_trends(
    gw,
    method = "lm"
  )

  expect_true(all(
    c(
      ".trend_method",
      ".trend_slope",
      ".trend_p_value",
      ".trend_conf_low",
      ".trend_conf_high"
    ) %in%
      names(out)
  ))
})

test_that("lm group trends return confidence limits", {
  out <- manager:::.compute_group_trends(
    gw,
    method = "lm"
  )

  expect_true(
    any(!is.na(out$.trend_conf_low))
  )

  expect_true(
    any(!is.na(out$.trend_conf_high))
  )
})

test_that("cenken group trends return NA confidence limits", {
  out <- manager:::.compute_group_trends(
    gw,
    method = "mann_kendall",
    censored_method = "cenken"
  )

  expect_true(
    all(is.na(out$.trend_conf_low))
  )

  expect_true(
    all(is.na(out$.trend_conf_high))
  )
})

test_that("trend method is recorded", {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Arsenic",
    sample_date = seq.Date(
      as.Date("2020-01-01"),
      by = "month",
      length.out = 24
    ),
    result = sin(
      seq(0, 2 * pi, length.out = 24)
    )
  )

  x <- env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result
  )

  out <- manager:::.compute_group_trends(
    x,
    method = "gam"
  )

  expect_true(
    all(out$.trend_method == "gam")
  )
})

test_that("groups with insufficient observations are dropped", {
  x <- tibble::tibble(
    location = c(
      "MW-1",
      "MW-1"
    ),
    analyte = c(
      "Arsenic",
      "Arsenic"
    ),
    sample_date = as.Date(
      c(
        "2020-01-01",
        "2021-01-01"
      )
    ),
    result = c(
      1,
      2
    )
  )

  x <- env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result
  )

  out <- manager:::.compute_group_trends(
    x,
    method = "lm"
  )

  expect_equal(
    nrow(out),
    0
  )
})

test_that("compute_group_trends uses active grouping variables", {
  x <- gw |>
    dplyr::group_by(location_id)

  out <- manager:::.compute_group_trends(
    x,
    method = "lm"
  )

  expect_equal(
    dplyr::n_distinct(out$location_id),
    dplyr::n_distinct(gw$location_id)
  )

  expect_false(
    "analyte" %in%
      dplyr::group_vars(x)
  )
})

test_that("compute_group_trends computes separate analyte trends", {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = rep(
      c("Arsenic", "Barium"),
      each = 10
    ),
    sample_date = rep(
      seq.Date(
        as.Date("2020-01-01"),
        by = "year",
        length.out = 10
      ),
      2
    ),
    result = c(
      1:10,
      10:1
    )
  )

  x <- env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result
  )

  out <- manager:::.compute_group_trends(
    x,
    method = "lm"
  )

  slopes <- out |>
    dplyr::distinct(
      analyte,
      .trend_slope
    )

  expect_equal(
    nrow(slopes),
    2
  )
})
