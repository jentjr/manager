# tests/testthat/test-boxplot.R

library(testthat)
library(ggplot2)
library(dplyr)


validator_defaults <- list(
  fill = NULL,
  scale_y_trans = "identity",
  coef = 1.5,
  notch = FALSE,
  show_points = FALSE,
  short_name = FALSE,
  coord_flip = FALSE,
  plot_ci = FALSE,
  standards = FALSE,
  legend_title = NULL,
  add_explanation = FALSE
)

test_that("validate_boxplot_args succeeds with valid data", {
  expect_no_error(
    do.call(
      .validate_boxplot_args,
      c(
        list(df = gw),
        validator_defaults
      )
    )
  )
})

test_that("non-numeric result generates error", {
  bad <- gw
  bad[[result_var(bad)]] <- as.character(
    bad[[result_var(bad)]]
  )

  expect_error(
    do.call(
      .validate_boxplot_args,
      c(
        list(df = bad),
        validator_defaults
      )
    ),
    "must be numeric"
  )
})

test_that("invalid transformation generates error", {
  expect_error(
    do.call(
      .validate_boxplot_args,
      c(
        list(
          df = gw,
          scale_y_trans = "not_a_transform"
        ),
        validator_defaults[
          names(validator_defaults) != "scale_y_trans"
        ]
      )
    ),
    "Invalid scale_y_trans value"
  )
})

test_that("log transforms require positive values", {
  bad <- gw
  bad[[result_var(bad)]][1] <- 0

  expect_error(
    do.call(
      .validate_boxplot_args,
      c(
        list(
          df = bad,
          scale_y_trans = "log10"
        ),
        validator_defaults[
          names(validator_defaults) != "scale_y_trans"
        ]
      )
    ),
    "positive"
  )
})

test_that("sqrt transform rejects negative values", {
  bad <- gw
  bad[[result_var(bad)]][1] <- -1

  expect_error(
    do.call(
      .validate_boxplot_args,
      c(
        list(
          df = bad,
          scale_y_trans = "sqrt"
        ),
        validator_defaults[
          names(validator_defaults) != "scale_y_trans"
        ]
      )
    ),
    "non-negative"
  )
})

test_that("coef must be positive numeric scalar", {
  expect_error(
    do.call(
      .validate_boxplot_args,
      c(
        list(
          df = gw,
          coef = 0
        ),
        validator_defaults[
          names(validator_defaults) != "coef"
        ]
      )
    ),
    "positive number"
  )

  expect_error(
    do.call(
      .validate_boxplot_args,
      c(
        list(
          df = gw,
          coef = c(1, 2)
        ),
        validator_defaults[
          names(validator_defaults) != "coef"
        ]
      )
    )
  )
})

test_that("prepare_boxplot_data identifies non-detects", {
  out <- .prepare_boxplot_data(
    gw,
    reporting_flag_var(gw)
  )

  expected <- sum(
    gw[[reporting_flag_var(gw)]] %in%
      c("<", "ND", "U"),
    na.rm = TRUE
  )

  expect_equal(
    sum(out$non_detect == "Non-detect"),
    expected
  )
})

test_that("get_plot_title returns analyte title", {
  expect_match(
    .get_plot_title(
      gw,
      analyte = analyte_var(gw)
    ),
    "^Boxplot for"
  )
})

test_that("create_boxplot_base returns ggplot", {
  p <- .create_boxplot_base(
    df = gw,
    x = ".location",
    y = ".analysis_result",
    fill = NULL,
    scale_y_trans = "identity",
    unit_lbl = "mg/L",
    title = "Test",
    legend_title = NULL
  )

  expect_s3_class(
    p,
    "ggplot"
  )
})

test_that("non-identity transform adds scale label", {
  p <- .create_boxplot_base(
    df = gw,
    x = ".location",
    y = ".analysis_result",
    fill = NULL,
    scale_y_trans = "log10",
    unit_lbl = "mg/L",
    title = "Test",
    legend_title = NULL
  )

  expect_match(
    p$labels$y,
    "Scale: log10"
  )
})

test_that("add_boxplot_layers adds boxplot layer", {
  p <- ggplot(
    gw,
    aes(
      .location,
      .analysis_result
    )
  )

  out <- .add_boxplot_layers(
    p,
    coef = 1.5,
    notch = FALSE,
    plot_ci = FALSE,
    pnt = 2
  )

  expect_equal(
    length(out$layers),
    1
  )
})

test_that("plot_ci adds additional layers", {
  p <- ggplot(
    gw,
    aes(
      .location,
      .analysis_result
    )
  )

  out <- .add_boxplot_layers(
    p,
    coef = 1.5,
    notch = FALSE,
    plot_ci = TRUE,
    pnt = 2
  )

  expect_equal(
    length(out$layers),
    3
  )
})

test_that("boxplot returns one plot per analyte", {
  out <- boxplot(gw)

  expect_type(
    out,
    "list"
  )

  expect_equal(
    length(out),
    dplyr::n_distinct(
      gw[[analyte_var(gw)]]
    )
  )
})

test_that("boxplot names correspond to analytes", {
  out <- boxplot(gw)

  expect_setequal(
    names(out),
    unique(
      as.character(
        gw[[analyte_var(gw)]]
      )
    )
  )
})

test_that("boxplot works with fill variable", {
  expect_no_error(
    boxplot(
      gw,
      fill = location_var(gw)
    )
  )
})

test_that("boxplot works with coord_flip", {
  expect_no_error(
    boxplot(
      gw,
      coord_flip = TRUE
    )
  )
})

test_that("boxplot warns for small n when notch is TRUE", {
  small <- gw[1:4, ]

  expect_warning(
    boxplot(
      small,
      notch = TRUE
    ),
    "fewer than 5 observations"
  )
})

test_that("all non-detect values are handled", {
  df <- gw
  df[[reporting_flag_var(df)]] <- "ND"

  expect_no_error(
    boxplot(
      df,
      show_points = TRUE
    )
  )
})

test_that("all result values NA does not crash", {
  df <- gw
  df$.analysis_result <- NA_real_

  expect_no_error(
    boxplot(df)
  )
})

test_that("show_points validator works", {
  expect_error(
    boxplot(
      gw,
      show_points = "yes"
    ),
    "show_points must be TRUE or FALSE"
  )
})

test_that("coord_flip validator works", {
  expect_error(
    boxplot(
      gw,
      coord_flip = 1
    ),
    "coord_flip must be TRUE or FALSE"
  )
})

test_that("plot_ci validator works", {
  expect_error(
    boxplot(
      gw,
      plot_ci = "TRUE"
    ),
    "plot_ci must be TRUE or FALSE"
  )
})

test_that("add_explanation validator works", {
  expect_error(
    boxplot(
      gw,
      add_explanation = "TRUE"
    ),
    "add_explanation must be TRUE or FALSE"
  )
})

test_that("boxplot works with explanation", {
  expect_no_error(
    boxplot(
      gw %>%
        dplyr::filter(
          .data[[analyte_var(gw)]] == "Chloride, total"
        ),
      add_explanation = TRUE
    )
  )
})

test_that("boxplot works with explanation, standards and fill", {
  expect_no_error(
    boxplot(
      gw %>%
        dplyr::filter(
          .data[[analyte_var(gw)]] == "Chloride, total"
        ),
      fill = location_var(gw),
      standards = TRUE,
      add_explanation = TRUE
    )
  )
})

test_that("boxplot uses analyte schema mapping", {
  plots <- boxplot(gw)

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

test_that("empty env_schema aborts", {
  expect_error(
    boxplot(
      gw[0, ]
    ),
    "No observations available to plot"
  )
})

test_that("boxplot.data.frame dispatch works", {
  out <- boxplot(
    gw_data,
    location = location_id,
    analyte = param_name,
    date = sample_date,
    result = analysis_result
  )

  expect_type(out, "list")

  expect_gt(length(out), 0)
})

test_that("boxplot honors custom schema mappings", {
  df <- tibble::tibble(
    site = c("A", "A", "B", "B"),
    chem = c(
      "Chloride",
      "Chloride",
      "Sulfate",
      "Sulfate"
    ),
    date = as.Date(c("2020-10-10", "2021-10-10", "2020-10-10", "2021-10-10")),
    value = c(1, 2, 3, 4)
  )

  out <- boxplot(
    df,
    location = site,
    analyte = chem,
    date = date,
    result = value
  )

  expect_setequal(
    names(out),
    c(
      "Chloride",
      "Sulfate"
    )
  )
})

test_that("short_name uses short analyte labels", {
  plt <- boxplot(
    gw %>%
      filter(.data[[analyte_var(gw)]] == "Chloride, total"),
    short_name = TRUE
  )[[1]]

  expect_match(
    plt$labels$title,
    "Cl"
  )
  
  expect_false(
    grepl(
      "Chloride, total",
      plt$labels$title
    )
  )
  
})
