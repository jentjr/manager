# test-pred_int.R

x <- c(
  4.1,
  4.2,
  4.5,
  4.7,
  4.9,
  5.0,
  5.1,
  5.2,
  5.4,
  5.5,
  5.7,
  5.8,
  5.9,
  6.0,
  6.1,
  6.2,
  6.4,
  6.5,
  6.8,
  7.0
)

make_normal_dist <- function(x) {
  tibble::tibble(
    distribution = "Normal",
    n_used = length(x),
    data = list(
      tibble::tibble(
        .analysis_result = x
      )
    )
  )
}

make_lognormal_dist <- function(x) {
  tibble::tibble(
    distribution = "Lognormal",
    n_used = length(x),
    data = list(
      tibble::tibble(
        .analysis_result = x
      )
    )
  )
}

background_data <- tibble::tibble(
  location_id = rep("MW-1", length(x)),
  sample_date = seq.Date(
    as.Date("2020-01-01"),
    by = "quarter",
    length.out = length(x)
  ),
  param_name = rep("Chloride", length(x)),
  default_unit = rep("mg/L", length(x)),
  analysis_result = x
) |>
  env_schema(
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit
  ) |>
  dplyr::group_by(
    location_id,
    param_name,
    default_unit
  ) |>
  estimate_distribution()


test_that("pred_int returns a tibble", {
  result <- pred_int(
    background_data
  )

  expect_s3_class(
    result,
    "tbl_df"
  )
})

test_that("pred_int returns expected columns", {
  result <- pred_int(
    background_data
  )

  expect_true(
    all(
      c(
        "distribution",
        "sample_size",
        "conf_level",
        "lpl",
        "upl"
      ) %in%
        names(result)
    )
  )
})

test_that("pred_int upper intervals are ordered correctly", {
  result <- pred_int(
    background_data,
    pi_type = "upper"
  )

  expect_true(
    all(
      result$upl >= result$lpl,
      na.rm = TRUE
    )
  )
})

test_that("pred_int lower intervals are ordered correctly", {
  result <- pred_int(
    background_data,
    pi_type = "lower"
  )

  expect_true(
    all(
      result$upl >= result$lpl,
      na.rm = TRUE
    )
  )
})

test_that("pred_int two-sided returns interval limits", {
  result <- pred_int(
    background_data,
    pi_type = "two-sided"
  )

  expect_true(
    "lpl" %in% names(result)
  )

  expect_true(
    "upl" %in% names(result)
  )

  expect_true(
    all(
      result$upl >= result$lpl,
      na.rm = TRUE
    )
  )
})

test_that("pred_int matches internal implementation", {
  expected <- .compute_pred_int(
    df = background_data,
    simultaneous = FALSE
  )

  actual <- pred_int(
    background_data
  )

  expect_equal(
    actual,
    expected
  )
})

test_that("pred_int propagates confidence level validation", {
  expect_error(
    pred_int(
      background_data,
      conf_level = 0
    ),
    "conf"
  )

  expect_error(
    pred_int(
      background_data,
      conf_level = 1.1
    ),
    "conf"
  )
})

test_that("pred_int propagates n_mean validation", {
  expect_error(
    pred_int(
      background_data,
      n_mean = 0
    )
  )

  expect_error(
    pred_int(
      background_data,
      n_mean = -1
    )
  )
})

test_that("pred_int propagates k validation", {
  expect_error(
    pred_int(
      background_data,
      k = 0
    )
  )

  expect_error(
    pred_int(
      background_data,
      k = -1
    )
  )
})

test_that("pred_int propagates pi_type validation", {
  expect_error(
    pred_int(
      background_data,
      pi_type = "invalid"
    )
  )
})

test_that("pred_int matches EnvStats::predIntNorm", {
  x <- c(
    4.1,
    4.2,
    4.5,
    4.7,
    4.9,
    5.0,
    5.1,
    5.2,
    5.4,
    5.5,
    5.7,
    5.8,
    5.9,
    6.0,
    6.1,
    6.2,
    6.4,
    6.5,
    6.8,
    7.0
  )

  expected <- EnvStats::predIntNorm(
    x = x,
    method = "Bonferroni",
    n.mean = 1,
    k = 1,
    pi.type = "upper",
    conf.level = 0.95
  )

  dist <- make_normal_dist(x)

  result <- pred_int(
    dist,
    method = "Bonferroni",
    n_mean = 1,
    k = 1,
    pi_type = "upper",
    conf_level = 0.95
  )

  expect_equal(
    result$upl,
    unname(expected$interval$limits["UPL"]),
    tolerance = 1e-8
  )
})

test_that("pred_int matches EnvStats::predIntLnorm", {
  x <- exp(
    c(
      1.0,
      1.2,
      1.3,
      1.4,
      1.5,
      1.6,
      1.7,
      1.8,
      1.9,
      2.0
    )
  )

  dist <- make_lognormal_dist(x)

  expected <- EnvStats::predIntLnorm(
    x = x,
    method = "Bonferroni",
    n.geomean = 1,
    k = 1,
    pi.type = "upper",
    conf.level = 0.95
  )

  result <- pred_int(
    dist,
    method = "Bonferroni",
    n_mean = 1,
    k = 1,
    pi_type = "upper",
    conf_level = 0.95
  )

  expect_equal(
    result$upl,
    unname(expected$interval$limits["UPL"]),
    tolerance = 1e-8
  )
})


test_that("99 percent upper prediction interval exceeds sample maximum", {
  x <- c(
    4.1,
    4.2,
    4.5,
    4.7,
    4.9,
    5.0,
    5.1,
    5.2,
    5.4,
    5.5,
    5.7,
    5.8,
    5.9,
    6.0,
    6.1,
    6.2,
    6.4,
    6.5,
    6.8,
    7.0
  )

  dist <- make_normal_dist(x)

  result <- pred_int(
    dist,
    conf_level = 0.99,
    pi_type = "upper"
  )

  expect_gt(
    result$upl,
    max(x)
  )
})

test_that("pred_int matches EnvStats two-sided limits", {
  x <- c(
    4.1,
    4.2,
    4.5,
    4.7,
    4.9,
    5.0,
    5.1,
    5.2,
    5.4,
    5.5,
    5.7,
    5.8,
    5.9,
    6.0,
    6.1,
    6.2,
    6.4,
    6.5,
    6.8,
    7.0
  )

  expected <- EnvStats::predIntNorm(
    x = x,
    pi.type = "two-sided",
    conf.level = 0.95
  )

  dist <- make_normal_dist(x)

  result <- pred_int(
    dist,
    pi_type = "two-sided"
  )

  expect_equal(
    result$lpl,
    unname(expected$interval$limits["LPL"]),
    tolerance = 1e-8
  )

  expect_equal(
    result$upl,
    unname(expected$interval$limits["UPL"]),
    tolerance = 1e-8
  )
})

test_that("pred_int supports nonparametric distributions", {
  x <- c(
    1,
    2,
    3,
    3,
    4,
    5,
    7,
    10,
    12,
    15,
    20
  )

  dist <- tibble::tibble(
    distribution = "Nonparametric",
    n_used = length(x),
    data = list(
      tibble::tibble(
        .analysis_result = x
      )
    )
  )

  result <- pred_int(
    dist,
    pi_type = "upper"
  )

  expect_s3_class(
    result,
    "tbl_df"
  )

  expect_false(
    is.na(result$upl)
  )
})

test_that("pred_int rejects insufficient_data", {
  dist <- tibble::tibble(
    distribution = "insufficient_data",
    n_used = 3,
    data = list(
      tibble::tibble(
        .analysis_result = c(1, 2, 3)
      )
    )
  )

  expect_error(
    pred_int(dist),
    "Intervals are only available"
  )
})

test_that("pred_int preserves decision metadata", {
  dist <- tibble::tibble(
    distribution = "Normal",
    n_used = 20,
    distribution_method = "parametric",
    decision_basis = "low_censoring",
    data = list(
      tibble::tibble(
        .analysis_result = rnorm(20)
      )
    )
  )

  result <- pred_int(dist)

  expect_equal(
    result$distribution_method,
    "parametric"
  )

  expect_equal(
    result$decision_basis,
    "low_censoring"
  )
})

test_that("prediction limits increase with confidence level", {
  x <- c(
    4.1,
    4.2,
    4.5,
    4.7,
    4.9,
    5.0,
    5.1,
    5.2,
    5.4,
    5.5,
    5.7,
    5.8,
    5.9,
    6.0,
    6.1,
    6.2,
    6.4,
    6.5,
    6.8,
    7.0
  )

  dist <- make_normal_dist(x)

  low <- pred_int(
    dist,
    conf_level = 0.90
  )

  high <- pred_int(
    dist,
    conf_level = 0.99
  )

  expect_gt(
    high$upl,
    low$upl
  )
})

test_that("prediction limits increase with k", {
  dist <- make_normal_dist(x)

  one_future <- pred_int(
    dist,
    k = 1
  )

  ten_future <- pred_int(
    dist,
    k = 10
  )

  expect_gt(
    ten_future$upl,
    one_future$upl
  )
})

test_that("small samples still produce finite intervals", {
  x <- c(
    1.2,
    1.4,
    1.8,
    2.0,
    2.1,
    2.3,
    2.5,
    2.7
  )

  dist <- make_normal_dist(x)

  result <- pred_int(dist)

  expect_true(
    is.finite(result$upl)
  )
})

test_that("prediction intervals preserve sample size", {
  result <- pred_int(
    make_normal_dist(x)
  )

  expect_equal(
    result$sample_size,
    length(x)
  )
})
