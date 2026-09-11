# tests/testthat/test-boxplot.R

library(testthat)
library(ggplot2)

test_df <- data.frame(
  location_id = rep(c("MW-1", "MW-2"), each = 5),
  analysis_result = c(1:10),
  lt_measure = c(
    "",
    "<",
    "ND",
    "U",
    "",
    "",
    "",
    "",
    "",
    ""
  ),
  param_name = rep(
    c("Chloride, total", "Sulfate, total"),
    each = 5
  ),
  default_unit = "mg/L",
  limit_a = 8,
  limit_b = 12,
  gradient = rep(
    c("Upgradient", "Downgradient"),
    each = 5
  ),
  stringsAsFactors = FALSE
)

test_that("validate_boxplot_inputs succeeds with valid data", {
  expect_invisible(
    .validate_boxplot_inputs(
      df = test_df,
      x = "location_id",
      y = "analysis_result",
      lt_measure = "lt_measure",
      fill = NULL,
      scale_y_trans = "identity",
      limit1 = NULL,
      limit2 = NULL,
      group_var = "param_name",
      coef = 1.5,
      notch = FALSE
    )
  )
})

test_that("missing columns generate error", {
  expect_error(
    .validate_boxplot_inputs(
      df = test_df,
      x = "missing_column",
      y = "analysis_result",
      lt_measure = "lt_measure",
      fill = NULL,
      scale_y_trans = "identity",
      limit1 = NULL,
      limit2 = NULL,
      group_var = "param_name",
      coef = 1.5,
      notch = FALSE
    ),
    "Missing columns"
  )
})

test_that("non-numeric y generates error", {
  bad_df <- test_df
  bad_df$analysis_result <- as.character(
    bad_df$analysis_result
  )

  expect_error(
    .validate_boxplot_inputs(
      df = bad_df,
      x = "location_id",
      y = "analysis_result",
      lt_measure = "lt_measure",
      fill = NULL,
      scale_y_trans = "identity",
      limit1 = NULL,
      limit2 = NULL,
      group_var = "param_name",
      coef = 1.5,
      notch = FALSE
    ),
    "must be numeric"
  )
})

test_that("invalid transformation generates error", {
  expect_error(
    .validate_boxplot_inputs(
      df = test_df,
      x = "location_id",
      y = "analysis_result",
      lt_measure = "lt_measure",
      fill = NULL,
      scale_y_trans = "not_a_transform",
      limit1 = NULL,
      limit2 = NULL,
      group_var = "param_name",
      coef = 1.5,
      notch = FALSE
    ),
    "Invalid scale_y_trans value"
  )
})

test_that("log transforms require positive values", {
  bad_df <- test_df
  bad_df$analysis_result[1] <- 0

  expect_error(
    .validate_boxplot_inputs(
      df = bad_df,
      x = "location_id",
      y = "analysis_result",
      lt_measure = "lt_measure",
      fill = NULL,
      scale_y_trans = "log10",
      limit1 = NULL,
      limit2 = NULL,
      group_var = "param_name",
      coef = 1.5,
      notch = FALSE
    ),
    "require all values to be positive"
  )
})

test_that("sqrt transform rejects negative values", {
  bad_df <- test_df
  bad_df$analysis_result[1] <- -1

  expect_error(
    .validate_boxplot_inputs(
      df = bad_df,
      x = "location_id",
      y = "analysis_result",
      lt_measure = "lt_measure",
      fill = NULL,
      scale_y_trans = "sqrt",
      limit1 = NULL,
      limit2 = NULL,
      group_var = "param_name",
      coef = 1.5,
      notch = FALSE
    ),
    "non-negative values"
  )
})

test_that("coef must be positive numeric scalar", {
  expect_error(
    .validate_boxplot_inputs(
      df = test_df,
      x = "location_id",
      y = "analysis_result",
      lt_measure = "lt_measure",
      fill = NULL,
      scale_y_trans = "identity",
      limit1 = NULL,
      limit2 = NULL,
      group_var = "param_name",
      coef = 0,
      notch = FALSE
    ),
    "positive number"
  )

  expect_error(
    .validate_boxplot_inputs(
      test_df,
      "location_id",
      "analysis_result",
      "lt_measure",
      NULL,
      "identity",
      NULL,
      NULL,
      "param_name",
      -1,
      FALSE
    )
  )

  expect_error(
    .validate_boxplot_inputs(
      test_df,
      "location_id",
      "analysis_result",
      "lt_measure",
      NULL,
      "identity",
      NULL,
      NULL,
      "param_name",
      c(1, 2),
      FALSE
    )
  )
})

test_that("prepare_boxplot_data identifies non-detects", {
  out <- .prepare_boxplot_data(
    test_df,
    "lt_measure"
  )

  expect_true("non_detect" %in% names(out))

  expect_equal(
    levels(out$non_detect),
    c("Detected", "Non-detect")
  )

  expect_equal(
    sum(out$non_detect == "Non-detect"),
    3
  )
})

test_that("NA lt_measure values are treated as detected", {
  df <- test_df
  df$lt_measure[1] <- NA

  out <- .prepare_boxplot_data(
    df,
    "lt_measure"
  )

  expect_equal(
    as.character(out$non_detect[1]),
    "Detected"
  )
})

test_that("get_plot_title returns title from param_name", {
  title <- .get_plot_title(test_df)

  expect_equal(
    title,
    "Boxplot for Chloride, total"
  )
})

test_that("get_plot_title returns generic title when param_name missing", {
  df <- subset(
    test_df,
    select = -param_name
  )

  expect_equal(
    .get_plot_title(df),
    "Boxplot"
  )
})

test_that("get_unit_label returns default unit", {
  expect_equal(
    .get_unit_label(test_df),
    "mg/L"
  )
})

test_that("get_unit_label returns blank when unit missing", {
  df <- subset(
    test_df,
    select = -default_unit
  )

  expect_equal(
    .get_unit_label(df),
    ""
  )
})

test_that("create_boxplot_base returns ggplot object", {
  p <- .create_boxplot_base(
    df = test_df,
    x = "location_id",
    y = "analysis_result",
    fill = NULL,
    scale_y_trans = "identity",
    unit_lbl = "mg/L",
    title = "Test",
    legend_title = NULL
  )

  expect_s3_class(p, "ggplot")
})

test_that("non-identity transform adds scale label", {
  p <- .create_boxplot_base(
    df = test_df,
    x = "location_id",
    y = "analysis_result",
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

test_that("add_limits adds one limit line", {
  p <- ggplot(
    test_df,
    aes(location_id, analysis_result)
  )

  out <- .add_limits(
    p,
    test_df,
    limit1 = "limit_a",
    limit2 = NULL
  )

  expect_gt(
    length(out$layers),
    length(p$layers)
  )
})

test_that("add_boxplot_layers adds boxplot layer", {
  p <- ggplot(
    test_df,
    aes(location_id, analysis_result)
  )

  out <- .add_boxplot_layers(
    p,
    test_df,
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

test_that("plot_ci adds additional summary layers", {
  p <- ggplot(
    test_df,
    aes(location_id, analysis_result)
  )

  out <- .add_boxplot_layers(
    p,
    test_df,
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

test_that("boxplot returns one plot per constituent", {
  out <- boxplot(test_df)

  expect_type(out, "list")

  expect_equal(
    length(out),
    length(unique(test_df$param_name))
  )
})

test_that("boxplot names correspond to group_var levels", {
  out <- boxplot(test_df)

  expect_setequal(
    names(out),
    unique(test_df$param_name)
  )
})

test_that("boxplot works with fill variable", {
  expect_no_error(
    boxplot(
      test_df,
      fill = "gradient"
    )
  )
})

test_that("boxplot works with coordinate flip", {
  expect_no_error(
    boxplot(
      test_df,
      coord_flip = TRUE
    )
  )
})

test_that("boxplot warns for small n when notch is TRUE", {
  small_df <- test_df[1:4, ]

  expect_warning(
    boxplot(
      small_df,
      notch = TRUE
    ),
    "fewer than 5 observations"
  )
})

test_that("empty data frame returns empty list", {
  df <- test_df[0, ]
  expect_equal(length(boxplot(df)), 0)
})

test_that("single constituent returns length-one list", {
  df <- subset(
    test_df,
    param_name == "Chloride, total"
  )

  out <- boxplot(df)

  expect_length(out, 1)
})

test_that("all values non-detect are handled", {
  df <- test_df
  df$lt_measure <- "ND"

  expect_no_error(
    boxplot(
      df,
      show_points = TRUE
    )
  )
})

test_that("all analysis_result values NA does not crash validation", {
  df <- test_df
  df$analysis_result <- NA_real_

  expect_no_error(
    boxplot(df)
  )
})
