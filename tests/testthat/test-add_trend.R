make_trend_data <- function() {
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
    date = sample_date,
    analyte = analyte,
    result = result
  )
}

test_that("add_trend returns unchanged plot when trend is none", {
  x <- make_trend_data()

  p <- ggplot2::ggplot(
    x,
    ggplot2::aes(
      x = date,
      y = result
    )
  )

  p2 <- manager:::.add_trend(
    p = p,
    df = x,
    trend = "none"
  )

  expect_equal(
    length(p2$layers),
    length(p$layers)
  )
})

test_that("lm trend adds line layer", {
  x <- make_trend_data()

  p <- ggplot2::ggplot()

  p2 <- manager:::.add_trend(
    p = p,
    df = x,
    trend = "lm"
  )

  expect_s3_class(
    p2,
    "ggplot"
  )

  expect_gt(
    length(p2$layers),
    length(p$layers)
  )
})

test_that("loess trend adds layer", {
  x <- make_trend_data()

  p <- ggplot2::ggplot()

  p2 <- manager:::.add_trend(
    p = p,
    df = x,
    trend = "loess"
  )

  expect_gt(
    length(p2$layers),
    length(p$layers)
  )
})

test_that("gam trend adds layer", {
  x <- make_trend_data()

  p <- ggplot2::ggplot()

  p2 <- manager:::.add_trend(
    p = p,
    df = x,
    trend = "gam"
  )

  expect_gt(
    length(p2$layers),
    length(p$layers)
  )
})

test_that("mann kendall adds layer", {
  x <- make_trend_data()

  p <- ggplot2::ggplot()

  p2 <- manager:::.add_trend(
    p = p,
    df = x,
    trend = "mann_kendall",
    censored_method = "as_reported"
  )

  expect_gt(
    length(p2$layers),
    length(p$layers)
  )
})

test_that("confidence interval adds additional layer", {
  x <- make_trend_data()

  p <- ggplot2::ggplot()

  p1 <- manager:::.add_trend(
    p = p,
    df = x,
    trend = "lm",
    trend_ci = FALSE
  )

  p2 <- manager:::.add_trend(
    p = p,
    df = x,
    trend = "lm",
    trend_ci = TRUE
  )

  expect_gt(
    length(p2$layers),
    length(p1$layers)
  )
})

test_that("trend labels add text layer", {
  x <- make_trend_data()

  p <- ggplot2::ggplot()

  p1 <- manager:::.add_trend(
    p = p,
    df = x,
    trend = "lm",
    trend_label = "none"
  )

  p2 <- manager:::.add_trend(
    p = p,
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

test_that("compact labels work", {
  x <- make_trend_data()

  p <- ggplot2::ggplot()

  expect_silent(
    manager:::.add_trend(
      p = p,
      df = x,
      trend = "mann_kendall",
      trend_label = "compact",
      trend_label_sig_only = FALSE,
      censored_method = "as_reported"
    )
  )
})

test_that("insufficient data returns original plot", {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Arsenic",
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

  p <- ggplot2::ggplot()

  p2 <- manager:::.add_trend(
    p = p,
    df = x,
    trend = "lm"
  )

  expect_equal(
    length(p2$layers),
    length(p$layers)
  )
})

test_that("grouped trend mode succeeds", {
  x <- tibble::tibble(
    location = rep(
      c("MW-1", "MW-2"),
      each = 24
    ),
    analyte = rep(
      c("Arsenic", "Arsenic"),
      each = 24
    ),
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
    date = sample_date,
    analyte = analyte,
    result = result
  )

  p <- ggplot2::ggplot()

  expect_silent(
    manager:::.add_trend(
      p = p,
      df = x,
      trend = "lm",
      plot_type = "combined"
    )
  )
})
