test_that("boxcox_transform adds Box-Cox columns", {
  df <- tibble::tibble(
    analysis_result = c(
      1,
      2,
      3,
      5,
      10
    )
  )

  out <- boxcox_transform(df)

  expect_true(
    ".boxcox_transform" %in% names(out)
  )

  expect_true(
    ".boxcox_lambda" %in% names(out)
  )

  expect_equal(
    nrow(out),
    nrow(df)
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
  df <- tibble::tibble(
    analysis_result = c(
      1,
      2,
      3,
      5,
      10
    )
  )

  out <- boxcox_transform(df)

  expect_equal(
    out$analysis_result,
    df$analysis_result
  )
})

test_that("all rows within a group share the same lambda", {
  df <- tibble::tibble(
    group = rep(c("A", "B"), each = 5),
    analysis_result = c(
      1:5,
      10:14
    )
  ) |>
    dplyr::group_by(group)

  out <- boxcox_transform(df)

  expect_equal(
    dplyr::n_distinct(
      out$.boxcox_lambda[
        out$group == "A"
      ]
    ),
    1
  )

  expect_equal(
    dplyr::n_distinct(
      out$.boxcox_lambda[
        out$group == "B"
      ]
    ),
    1
  )
})

test_that("grouping affects Box-Cox estimation", {
  df <- tibble::tibble(
    group = c(
      rep("A", 8),
      rep("B", 8)
    ),
    analysis_result = c(
      1:8,
      exp(seq(1, 2, length.out = 8))
    )
  ) |>
    dplyr::group_by(group)

  out <- boxcox_transform(df)

  lambdas <- out |>
    dplyr::distinct(
      group,
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
  df <- tibble::tibble(
    analysis_result = c(
      1,
      2,
      3,
      5,
      10
    )
  )

  out <- boxcox_transform(
    df,
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

  df <- tibble::tibble(
    analysis_result = x
  )

  out <- boxcox_transform(
    df,
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
  gw <- env_schema(
    tibble::tibble(
      location_id = rep("MW-1", 5),
      sample_date = seq.Date(
        as.Date("2020-01-01"),
        by = "year",
        length.out = 5
      ),
      analyte = "Benzene",
      result = c(
        1,
        2,
        3,
        5,
        10
      )
    ),
    location = location_id,
    sample_date = sample_date,
    analyte = analyte,
    result = result
  )

  out <- boxcox_transform(gw)

  expect_s3_class(
    out,
    "env_schema"
  )
})

test_that("ungrouped env_schema computes separate lambdas by default groups", {
  gw <- env_schema(
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
      analyte = "Benzene",
      result = c(
        1:6,
        exp(seq(1, 2, length.out = 6))
      ),
      unit = "mg/L"
    ),
    location = location_id,
    sample_date = sample_date,
    analyte = analyte,
    result = result,
    unit = unit
  )

  out <- boxcox_transform(gw)

  expect_gt(
    dplyr::n_distinct(
      out$.boxcox_lambda
    ),
    1
  )
})
