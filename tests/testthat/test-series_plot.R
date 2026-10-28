test_that("combined mode returns ggplot", {
  p <- series_plot(
    gw,
    plot_type = "combined"
  )

  expect_s3_class(
    p,
    "ggplot"
  )
})

test_that("split mode returns named list of ggplots", {
  plots <- series_plot(gw)

  expect_type(
    plots,
    "list"
  )

  expect_gt(
    length(plots),
    0
  )

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

test_that("series_plot accepts raw data", {
  expect_no_error(
    suppressWarnings(
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
  )
})

test_that("combined plot works with raw data", {
  p <- suppressWarnings(
    series_plot(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result,
      plot_type = "combined"
    )
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

test_that("standards can be plotted", {
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

test_that("empty data aborts", {
  expect_error(
    series_plot(
      gw[0, ]
    ),
    "No observations available to plot"
  )
})

test_that("single observation dataset plots successfully", {
  df <- gw[1, , drop = FALSE]

  expect_no_error(
    series_plot(df)
  )
})

test_that("split mode returns one plot per analyte", {
  plots <- series_plot(gw)

  expected_analytes <- unique(
    as.character(
      gw[[analyte_var(gw)]]
    )
  )

  expect_setequal(
    names(plots),
    expected_analytes
  )
})

test_that("split mode uses schema analyte mapping", {
  plots <- series_plot(gw)

  expect_length(
    plots,
    dplyr::n_distinct(
      gw[[analyte_var(gw)]]
    )
  )
})

test_that("series_plot splits using analyte schema mapping", {
  plots <- series_plot(gw)

  expected <- unique(
    as.character(
      gw[[analyte_var(gw)]]
    )
  )

  expect_setequal(
    names(plots),
    expected
  )
})

test_that("series_plot respects custom column mappings", {
  df <- tibble::tibble(
    monitoring_well = rep("MW-1", 4),
    collected_on = as.POSIXct(
      "2020-01-01"
    ) +
      c(0, 30, 60, 90) * 86400,
    constituent = c(
      "Benzene",
      "Benzene",
      "Toluene",
      "Toluene"
    ),
    concentration = c(
      1,
      2,
      3,
      4
    )
  )

  x <- suppressWarnings(
    env_schema(
      df,
      location = monitoring_well,
      date = collected_on,
      analyte = constituent,
      result = concentration
    )
  )

  plots <- series_plot(x)

  expect_setequal(
    names(plots),
    c(
      "Benzene",
      "Toluene"
    )
  )
})

test_that("series_plot splits using analyte schema mapping", {
  plots <- series_plot(gw)

  expected <- unique(
    as.character(
      gw[[analyte_var(gw)]]
    )
  )

  expect_setequal(
    names(plots),
    expected
  )
})

test_that("series_plot splits on mapped analyte column", {
  plots <- series_plot(gw)

  expect_setequal(
    names(plots),
    unique(
      as.character(
        gw[[analyte_var(gw)]]
      )
    )
  )
})
