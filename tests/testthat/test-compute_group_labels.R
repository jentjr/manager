test_that("compute_group_labels returns one label per group", {
  trend_df <- tibble::tibble(
    location = rep("MW-1", 5),
    analyte = rep("Arsenic", 5),
    date = seq.Date(
      as.Date("2020-01-01"),
      by = "year",
      length.out = 5
    ),
    fit = 1:5,
    .trend_slope = 1,
    .trend_p_value = 0.01,
    .trend_conf_low = 0.5,
    .trend_conf_high = 1.5
  )

  out <- manager:::.compute_group_labels(
    trend_df = trend_df,
    trend = "lm",
    trend_label = "full",
    date_col = "date",
    location_col = "location",
    analyte_col = "analyte"
  )

  expect_s3_class(out, "tbl_df")

  expect_equal(
    nrow(out),
    1
  )
})

test_that("compute_group_labels returns one label for each location analyte group", {
  trend_df <- tibble::tibble(
    location = c(
      rep("MW-1", 5),
      rep("MW-2", 5)
    ),
    analyte = "Arsenic",
    date = rep(
      seq.Date(
        as.Date("2020-01-01"),
        by = "year",
        length.out = 5
      ),
      2
    ),
    fit = 1:10,
    .trend_slope = 1,
    .trend_p_value = 0.01,
    .trend_conf_low = 0.5,
    .trend_conf_high = 1.5
  )

  out <- manager:::.compute_group_labels(
    trend_df = trend_df,
    trend = "lm",
    trend_label = "full",
    date_col = "date",
    location_col = "location",
    analyte_col = "analyte"
  )

  expect_equal(
    nrow(out),
    2
  )
})

test_that("label_x is latest date in each group", {
  trend_df <- tibble::tibble(
    location = "MW-1",
    analyte = "Arsenic",
    date = seq.Date(
      as.Date("2020-01-01"),
      by = "year",
      length.out = 5
    ),
    fit = 1:5,
    .trend_slope = 1,
    .trend_p_value = 0.01,
    .trend_conf_low = 0,
    .trend_conf_high = 2
  )

  out <- manager:::.compute_group_labels(
    trend_df = trend_df,
    trend = "lm",
    trend_label = "full",
    date_col = "date",
    location_col = "location",
    analyte_col = "analyte"
  )

  expect_equal(
    out$label_x,
    max(trend_df$date)
  )
})

test_that("label_y equals final fitted value", {
  trend_df <- tibble::tibble(
    location = "MW-1",
    analyte = "Arsenic",
    date = seq.Date(
      as.Date("2020-01-01"),
      by = "year",
      length.out = 5
    ),
    fit = c(
      10,
      20,
      30,
      40,
      50
    ),
    .trend_slope = 1,
    .trend_p_value = 0.01,
    .trend_conf_low = 0,
    .trend_conf_high = 2
  )

  out <- manager:::.compute_group_labels(
    trend_df = trend_df,
    trend = "lm",
    trend_label = "full",
    date_col = "date",
    location_col = "location",
    analyte_col = "analyte"
  )

  expect_equal(
    out$label_y,
    50
  )
})

test_that("compact labels are generated", {
  trend_df <- tibble::tibble(
    location = "MW-1",
    analyte = "Arsenic",
    date = seq.Date(
      as.Date("2020-01-01"),
      by = "year",
      length.out = 5
    ),
    fit = 1:5,
    .trend_slope = 1,
    .trend_p_value = 0.01,
    .trend_conf_low = NA_real_,
    .trend_conf_high = NA_real_
  )

  out <- manager:::.compute_group_labels(
    trend_df = trend_df,
    trend = "mann_kendall",
    trend_label = "compact",
    date_col = "date",
    location_col = "location",
    analyte_col = "analyte"
  )

  expect_match(
    out$label,
    "Increasing"
  )
})

test_that("mann kendall labels are generated", {
  trend_df <- tibble::tibble(
    location = "MW-1",
    analyte = "Arsenic",
    date = seq.Date(
      as.Date("2020-01-01"),
      by = "year",
      length.out = 5
    ),
    fit = 1:5,
    .trend_slope = 1,
    .trend_p_value = 0.01,
    .trend_conf_low = NA_real_,
    .trend_conf_high = NA_real_
  )

  out <- manager:::.compute_group_labels(
    trend_df = trend_df,
    trend = "mann_kendall",
    trend_label = "full",
    date_col = "date",
    location_col = "location",
    analyte_col = "analyte"
  )

  expect_match(
    out$label,
    "Mann-Kendall"
  )

  expect_match(
    out$label,
    "Sen="
  )
})

test_that("multiple analytes generate separate labels", {
  trend_df <- tibble::tibble(
    location = "MW-1",
    analyte = rep(
      c(
        "Arsenic",
        "Barium"
      ),
      each = 5
    ),
    date = rep(
      seq.Date(
        as.Date("2020-01-01"),
        by = "year",
        length.out = 5
      ),
      2
    ),
    fit = 1:10,
    .trend_slope = c(
      rep(1, 5),
      rep(-1, 5)
    ),
    .trend_p_value = 0.01,
    .trend_conf_low = 0,
    .trend_conf_high = 2
  )

  out <- manager:::.compute_group_labels(
    trend_df = trend_df,
    trend = "lm",
    trend_label = "full",
    date_col = "date",
    location_col = "location",
    analyte_col = "analyte"
  )

  expect_equal(
    nrow(out),
    2
  )
})

test_that("returned labels contain required columns", {
  trend_df <- tibble::tibble(
    location = "MW-1",
    analyte = "Arsenic",
    date = seq.Date(
      as.Date("2020-01-01"),
      by = "year",
      length.out = 5
    ),
    fit = 1:5,
    .trend_slope = 1,
    .trend_p_value = 0.05,
    .trend_conf_low = 0,
    .trend_conf_high = 2
  )

  out <- manager:::.compute_group_labels(
    trend_df = trend_df,
    trend = "lm",
    trend_label = "full",
    date_col = "date",
    location_col = "location",
    analyte_col = "analyte"
  )

  expect_true(all(
    c(
      "label_x",
      "label_y",
      "label"
    ) %in%
      names(out)
  ))
})

test_that("empty trend data returns empty result", {
  trend_df <- tibble::tibble(
    location = character(),
    analyte = character(),
    date = as.Date(character()),
    fit = numeric(),
    .trend_slope = numeric(),
    .trend_p_value = numeric(),
    .trend_conf_low = numeric(),
    .trend_conf_high = numeric()
  )

  out <- manager:::.compute_group_labels(
    trend_df = trend_df,
    trend = "lm",
    trend_label = "full",
    date_col = "date",
    location_col = "location",
    analyte_col = "analyte"
  )

  expect_equal(
    nrow(out),
    0
  )
})
