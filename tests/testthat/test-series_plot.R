test_that("series_plot returns a named list of ggplots", {
  
  plots <- series_plot(test_data)
  
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
  
  expect_false(is.null(names(plots)))
})

test_that("invalid scale throws error", {
  
  expect_error(
    series_plot(
      test_data,
      scale = "bad_scale"
    ),
    "`scale` must be one of"
  )
  
})

test_that("invalid scale_y_trans throws error", {
  
  expect_error(
    series_plot(
      test_data,
      scale_y_trans = "foobar"
    ),
    "`scale_y_trans` must be one of"
  )
  
})

test_that("standards disabled when required columns missing", {
  
  bad_df <- test_data %>%
    dplyr::select(
      -dplyr::any_of(
        c(
          "standard_type",
          "standard_value"
        )
      )
    )
  
  expect_warning(
    plots <- series_plot(
      bad_df,
      standards = TRUE
    ),
    "Cannot add standards"
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
  
})

test_that("non-detect values are classified correctly", {
  
  df <- tibble::tibble(
    sample_date = as.POSIXct(
      c(
        "2020-01-01",
        "2020-02-01",
        "2020-03-01",
        "2020-04-01"
      )
    ),
    analysis_result = c(1, 2, 3, 4),
    param_name = "Arsenic",
    location_id = "MW-1",
    lt_measure = c(
      "<",
      "ND",
      "U",
      NA
    )
  )
  
  p <- .series_plot(
    df,
    standards = FALSE
  )
  
  point_layer <- p$layers[[2]]
  
  expect_true(
    "shape" %in%
      names(point_layer$mapping)
  )
  
})

test_that("non-detect values are case insensitive", {
  
  df <- tibble::tibble(
    sample_date = Sys.time() + 1:4,
    analysis_result = 1:4,
    param_name = "Arsenic",
    location_id = "MW-1",
    lt_measure = c(
      "nd",
      "ND",
      "u",
      "<"
    )
  )
  
  expect_no_error(
    .series_plot(
      df,
      standards = FALSE
    )
  )
  
})

test_that("background rectangle is added", {
  
  p <- .series_plot(
    test_data,
    background = as.POSIXct(
      c(
        "2010-01-01",
        "2015-01-01"
      )
    ),
    standards = FALSE
  )
  
  has_rect <- any(
    purrr::map_lgl(
      p$layers,
      ~ inherits(.x$geom, "GeomRect")
    )
  )
  
  expect_true(has_rect)
  
})

test_that("positive standards are plotted", {
  
  p <- .series_plot(
    test_data,
    standards = TRUE
  )
  
  has_hline <- any(
    purrr::map_lgl(
      p$layers,
      ~ inherits(.x$geom, "GeomHline")
    )
  )
  
  expect_true(has_hline)
  
})

test_that("non-positive standards are excluded", {
  
  df <- test_data
  
  df$standard_type <- "MCL"
  df$standard_value <- c(
    -1,
    0,
    10,
    20
  )[seq_len(nrow(df))]
  
  expect_warning(
    p <- .series_plot(
      df,
      standards = TRUE
    ),
    "excluded from the plot"
  )
  
  hline_layer <- purrr::keep(
    p$layers,
    ~ inherits(.x$geom, "GeomHline")
  )
  
  expect_length(
    hline_layer,
    1
  )
  
})

test_that("facet_wrap is added when facet_var supplied", {
  
  p <- .series_plot(
    test_data,
    facet_var = "location_id",
    standards = FALSE
  )
  
  expect_s3_class(
    p$facet,
    "FacetWrap"
  )
  
})

test_that("trend line is added", {
  
  p <- .series_plot(
    test_data,
    trend = "lm",
    standards = FALSE
  )
  
  has_smooth <- any(
    purrr::map_lgl(
      p$layers,
      ~ inherits(.x$geom, "GeomSmooth")
    )
  )
  
  expect_true(has_smooth)
  
})

test_that("plot structure remains stable", {
  
  p <- .series_plot(
    test_data,
    standards = FALSE
  )
  
  expect_snapshot(
    names(p)
  )
  
})

test_that("series_plot returns empty list for empty dataframe", {
  
  df <- test_data[0, ]
  
  plots <- series_plot(df)
  
  expect_type(plots, "list")
  expect_length(plots, 0)
  
})

test_that("series_plot errors for empty dataframe", {
  
  df <- test_data[0, ]
  
  expect_error(
    series_plot(df),
    "No rows found"
  )
  
})

test_that(".series_plot handles empty dataframe", {
  
  df <- test_data[0, ]
  
  expect_error(
    .series_plot(df),
    NA
  )
  
})

test_that("plot succeeds with all missing standards", {
  
  df <- test_data
  
  df$standard_type <- NA_character_
  df$standard_value <- NA_real_
  
  expect_no_error(
    .series_plot(
      df,
      standards = TRUE
    )
  )
  
})

test_that("all standards removed when non-positive", {
  
  df <- test_data
  
  df$standard_type <- "MCL"
  df$standard_value <- 0
  
  expect_warning(
    p <- .series_plot(
      df,
      standards = TRUE
    ),
    "excluded from the plot"
  )
  
  has_hline <- any(
    purrr::map_lgl(
      p$layers,
      ~ inherits(.x$geom, "GeomHline")
    )
  )
  
  expect_false(has_hline)
  
})

test_that("single observation dataset plots successfully", {
  
  df <- test_data[1, ]
  
  expect_no_error(
    .series_plot(
      df,
      standards = FALSE
    )
  )
  
})