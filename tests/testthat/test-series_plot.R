test_that("series_plot accepts env_schema", {
  expect_no_error(
    series_plot(gw)
  )
})

test_that("series_plot accepts raw data", {
  expect_no_error(
    series_plot(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result,
      unit = default_unit,
      reporting_flag = lt_measure
    )
  )
})

test_that("series_plot validates env_schema", {
  bad <- gw

  bad$.analysis_result <- as.character(
    bad$.analysis_result
  )

  expect_error(
    validate_env_schema(bad)
  )
})

test_that("series_plot returns a named list of ggplots", {
  plots <- series_plot(gw)

  expect_type(plots, "list")
  expect_gt(length(plots), 0)

  expect_true(
    all(
      purrr::map_lgl(
        plots,
        inherits,
        "ggplot"
      )
    )
  )

  expect_false(
    is.null(names(plots))
  )
})


test_that("split mode returns list of plots", {
  plots <- series_plot(
    gw,
    plot_type = "split"
  )

  expect_type(plots, "list")

  expect_true(
    all(
      purrr::map_lgl(
        plots,
        inherits,
        "ggplot"
      )
    )
  )
})


test_that("combined mode returns a ggplot", {
  p <- series_plot(
    gw,
    plot_type = "combined"
  )

  expect_s3_class(
    p,
    "ggplot"
  )
})


test_that("invalid scale throws error", {
  expect_error(
    series_plot(
      gw,
      scale = "bad_scale"
    ),
    "`scale` must be one of"
  )
})


test_that("invalid scale_y_trans throws error", {
  expect_error(
    series_plot(
      gw,
      scale_y_trans = "foobar"
    ),
    "`scale_y_trans` must be one of"
  )
})


test_that("invalid trend throws error", {
  expect_error(
    series_plot(
      gw,
      trend = "bad_trend"
    ),
    "`trend` must be one of"
  )
})


test_that("invalid trend_label throws error", {
  expect_error(
    series_plot(
      gw,
      trend_label = "bad_label"
    )
  )
})


test_that("invalid plot_type throws error", {
  expect_error(
    series_plot(
      gw,
      plot_type = "bad"
    )
  )
})


test_that("background shading is supported", {
  expect_no_error(
    series_plot(
      gw,
      background = as.POSIXct(
        c(
          "2010-01-01",
          "2015-01-01"
        )
      )
    )
  )
})


test_that("lm trend is supported", {
  expect_no_error(
    series_plot(
      gw,
      trend = "lm"
    )
  )
})


test_that("facet wrapping is supported", {
  expect_no_error(
    series_plot(
      gw,
      facet_var = "location"
    )
  )
})

test_that("positive standards can be plotted", {
  expect_no_error(
    series_plot(
      gw,
      standards = TRUE
    )
  )
})

test_that("standards can be disabled", {
  expect_no_error(
    series_plot(
      gw,
      standards = FALSE
    )
  )
})

test_that("non-detect values are handled", {
  df <- env_schema(
    tibble::tibble(
      location = "MW-1",
      date = Sys.Date() + 1:4,
      analyte = "Chloride",
      result = c(1, 1, 1, 1),
      qualifier = c("<", "ND", "U", NA)
    ),
    location = location,
    date = date,
    analyte = analyte,
    result = result,
    reporting_flag = qualifier
  )

  expect_no_error(
    series_plot(
      df,
      plot_type = "combined",
      standards = FALSE
    )
  )
})


test_that("series_plot returns empty list for empty dataframe", {
  df <- gw[0, ]

  plots <- series_plot(df)

  expect_type(
    plots,
    "list"
  )

  expect_length(
    plots,
    0
  )
})

test_that("single observation dataset plots successfully", {
  df <- gw[1, , drop = FALSE]

  expect_no_error(
    series_plot(df)
  )
})

test_that("split mode returns one plot per constituent", {
  plots <- series_plot(gw)

  expect_length(
    plots,
    dplyr::n_distinct(gw$.analyte)
  )
})
