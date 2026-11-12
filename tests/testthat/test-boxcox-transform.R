# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

build_boxcox_schema <- function(x) {
  build_schema(
    tibble::tibble(
      location_id = rep("MW-1", length(x)),
      sample_date = seq.Date(
        as.Date("2020-01-01"),
        by = "year",
        length.out = length(x)
      ),
      param_name = "Chloride",
      analysis_result = x,
      default_unit = "mg/L"
    )
  )
}

# ------------------------------------------------------------------------------
# Tests
# ------------------------------------------------------------------------------

test_that("boxcox_transform adds Box-Cox columns", {
  gw <- build_boxcox_schema(
    c(1, 2, 3, 5, 10)
  )

  out <- boxcox_transform(gw)

  expect_true(
    ".boxcox_transform" %in% names(out)
  )

  expect_true(
    ".boxcox_lambda" %in% names(out)
  )

  expect_equal(
    nrow(out),
    nrow(gw)
  )

  expect_type(
    out$.boxcox_transform,
    "double"
  )

  expect_type(
    out$.boxcox_lambda,
    "double"
  )
})

test_that("boxcox_transform does not overwrite results", {
  gw <- build_boxcox_schema(
    c(1, 2, 3, 5, 10)
  )

  out <- boxcox_transform(gw)

  expect_equal(
    out$.analysis_result,
    gw$.analysis_result
  )
})

test_that("all rows within a group share the same lambda", {
  gw <- build_schema(
    tibble::tibble(
      location_id = rep(
        c("MW-1", "MW-2"),
        each = 5
      ),
      sample_date = seq.Date(
        as.Date("2020-01-01"),
        by = "month",
        length.out = 10
      ),
      param_name = "Chloride",
      analysis_result = c(
        1:5,
        10:14
      ),
      default_unit = "mg/L"
    )
  ) |>
    dplyr::group_by(location_id)

  out <- boxcox_transform(gw)

  expect_equal(
    dplyr::n_distinct(
      out$.boxcox_lambda[
        out$location_id == "MW-1"
      ]
    ),
    1
  )

  expect_equal(
    dplyr::n_distinct(
      out$.boxcox_lambda[
        out$location_id == "MW-2"
      ]
    ),
    1
  )
})

test_that("grouping affects Box-Cox estimation", {
  gw <- build_schema(
    tibble::tibble(
      location_id = c(
        rep("MW-1", 8),
        rep("MW-2", 8)
      ),
      sample_date = seq.Date(
        as.Date("2020-01-01"),
        by = "month",
        length.out = 16
      ),
      param_name = "Chloride",
      analysis_result = c(
        1:8,
        exp(seq(1, 2, length.out = 8))
      ),
      default_unit = "mg/L"
    )
  ) |>
    dplyr::group_by(location_id)

  out <- boxcox_transform(gw)

  lambdas <- out |>
    dplyr::distinct(
      location_id,
      .boxcox_lambda
    )

  expect_equal(
    nrow(lambdas),
    2
  )

  expect_false(
    isTRUE(
      all.equal(
        lambdas$.boxcox_lambda[1],
        lambdas$.boxcox_lambda[2]
      )
    )
  )
})

test_that("keep_data_object retains boxcox results", {
  gw <- build_boxcox_schema(
    c(1, 2, 3, 5, 10)
  )

  out <- boxcox_transform(
    gw,
    keep_data_object = TRUE
  )

  expect_true(
    "boxcox" %in% names(out)
  )

  expect_s3_class(
    out$boxcox[[1]],
    "boxcox"
  )
})

test_that("boxcox_transform matches EnvStats::boxcoxTransform", {
  x <- c(
    1,
    2,
    4,
    8,
    16,
    32
  )

  gw <- build_boxcox_schema(x)

  out <- boxcox_transform(
    gw,
    keep_data_object = TRUE
  )

  lambda <- out$.boxcox_lambda[[1]]

  expected <- EnvStats::boxcoxTransform(
    x,
    lambda = lambda
  )

  expect_equal(
    out$.boxcox_transform,
    expected,
    tolerance = 1e-10
  )
})

test_that("boxcox_transform preserves env_schema class", {
  gw <- build_boxcox_schema(
    c(1, 2, 3, 5, 10)
  )

  out <- boxcox_transform(gw)

  expect_s3_class(
    out,
    "env_schema"
  )
})

test_that("ungrouped env_schema computes separate lambdas by default groups", {
  gw <- build_schema(
    tibble::tibble(
      location_id = c(
        rep("MW-1", 6),
        rep("MW-2", 6)
      ),
      sample_date = seq.Date(
        as.Date("2020-01-01"),
        by = "month",
        length.out = 12
      ),
      param_name = "Chloride",
      analysis_result = c(
        1:6,
        exp(seq(1, 2, length.out = 6))
      ),
      default_unit = "mg/L"
    )
  )

  out <- boxcox_transform(gw)

  expect_gt(
    dplyr::n_distinct(
      out$.boxcox_lambda
    ),
    1
  )
})

test_that("boxcox_transform preserves ungrouped state", {
  gw <- build_boxcox_schema(
    c(1, 2, 3, 5, 10)
  )

  out <- boxcox_transform(gw)

  expect_false(
    dplyr::is_grouped_df(out)
  )
})
