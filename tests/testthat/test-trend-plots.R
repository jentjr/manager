make_plot_trend_data <- function() {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Arsenic",
    sample_date = seq.Date(
      as.Date("2020-01-01"),
      by = "month",
      length.out = 24
    ),
    result = seq_len(24)
  )

  env_schema(
    x,
    location = location,
    sample_date = sample_date,
    analyte = analyte,
    result = result
  )
}

make_censored_trend <- function() {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Arsenic",
    sample_date = seq.Date(
      as.Date("2020-01-01"),
      by = "month",
      length.out = 24
    ),
    result = c(
      rep(1, 8),
      seq(2, 17)
    ),
    reporting_limit = 2,
    censored = c(
      rep(TRUE, 8),
      rep(FALSE, 16)
    )
  )

  env_schema(
    x,
    location = location,
    sample_date = sample_date,
    analyte = analyte,
    result = result,
    reporting_limit = reporting_limit,
  )
}

test_that("add_trend returns unchanged plot when trend is none", {
  x <- make_plot_trend_data()

  p <- ggplot2::ggplot()

  p2 <- manager:::.add_trend(
    p = p,
    df = x,
    trend = "none"
  )

  expect_equal(
    length(p$layers),
    length(p2$layers)
  )
})

test_that("lm adds trend layer", {
  p <- manager:::.add_trend(
    p = ggplot2::ggplot(),
    df = make_plot_trend_data(),
    trend = "lm"
  )

  expect_s3_class(
    p,
    "ggplot"
  )
})

test_that("loess adds trend layer", {
  p <- manager:::.add_trend(
    p = ggplot2::ggplot(),
    df = make_plot_trend_data(),
    trend = "loess"
  )

  expect_gt(
    length(p$layers),
    0
  )
})

test_that("gam adds trend layer", {
  p <- manager:::.add_trend(
    p = ggplot2::ggplot(),
    df = make_plot_trend_data(),
    trend = "gam"
  )

  expect_gt(
    length(p$layers),
    0
  )
})

test_that("mann kendall adds trend layer", {
  p <- manager:::.add_trend(
    p = ggplot2::ggplot(),
    df = make_plot_trend_data(),
    trend = "mann_kendall",
    censored_method = "as_reported"
  )

  expect_gt(
    length(p$layers),
    0
  )
})

test_that("confidence interval adds ribbon layer", {
  x <- make_plot_trend_data()

  p1 <- manager:::.add_trend(
    p = ggplot2::ggplot(),
    df = x,
    trend = "lm",
    trend_ci = FALSE
  )

  p2 <- manager:::.add_trend(
    p = ggplot2::ggplot(),
    df = x,
    trend = "lm",
    trend_ci = TRUE
  )

  expect_gt(
    length(p2$layers),
    length(p1$layers)
  )
})

test_that("full labels add additional layer", {
  x <- make_plot_trend_data()

  p1 <- manager:::.add_trend(
    p = ggplot2::ggplot(),
    df = x,
    trend = "lm",
    trend_label = "none"
  )

  p2 <- manager:::.add_trend(
    p = ggplot2::ggplot(),
    df = x,
    trend = "lm",
    trend_label = "full",
    trend_label_sig_only = FALSE
  )

  expect_gt(
    length(p2$layers),
    length(p1$layers)
  )
})

test_that("cenken plotting succeeds", {
  x <- make_censored_trend()

  p <- manager:::.add_trend(
    p = ggplot2::ggplot(),
    df = x,
    trend = "mann_kendall",
    censored_method = "cenken"
  )

  expect_s3_class(
    p,
    "ggplot"
  )

  expect_gt(
    length(p$layers),
    0
  )
})

test_that("grouped trend plotting succeeds", {
  x <- tibble::tibble(
    location = rep(
      c("MW-1", "MW-2"),
      each = 24
    ),
    analyte = "Arsenic",
    sample_date = rep(
      seq.Date(
        as.Date("2020-01-01"),
        by = "month",
        length.out = 24
      ),
      2
    ),
    result = c(
      seq_len(24),
      seq_len(24) * 2
    )
  )

  x <- env_schema(
    x,
    location = location,
    sample_date = sample_date,
    analyte = analyte,
    result = result
  )

  expect_silent(
    manager:::.add_trend(
      p = ggplot2::ggplot(),
      df = x,
      trend = "lm",
      plot_type = "combined"
    )
  )
})
