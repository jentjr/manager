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
  bad$.analysis_result <- as.character(
    bad$.analysis_result
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
  bad$.analysis_result[1] <- 0

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
  bad$.analysis_result[1] <- -1

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
    ".reporting_flag"
  )

  expect_true(
    "non_detect" %in% names(out)
  )

  expect_equal(
    sum(
      out$non_detect == "Non-detect"
    ),
    3
  )
})

test_that("get_plot_title returns analyte title", {
  expect_equal(
    .get_plot_title(
      gw,
      analyte = ".analyte"
    ),
    "Boxplot for Chloride, total (mg/L)"
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
      gw$.analyte
    )
  )
})

test_that("boxplot names correspond to analytes", {
  out <- boxplot(gw)

  expect_setequal(
    names(out),
    unique(gw$.analyte)
  )
})

test_that("boxplot works with fill variable", {
  expect_no_error(
    boxplot(
      gw,
      fill = "gradient"
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
  df$.reporting_flag <- "ND"

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
          .analyte == "Chloride, total"
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
          .analyte == "Chloride, total"
        ),
      fill = "gradient",
      standards = TRUE,
      add_explanation = TRUE
    )
  )
})
