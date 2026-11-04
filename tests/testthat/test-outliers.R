make_outlier_schema <- function() {
  suppressWarnings(
    env_schema(
      gw_data,
      location = location_id,
      date = sample_date,
      analyte = param_name,
      result = analysis_result,
      unit = default_unit
    )
  )
}

make_dixon_schema <- function() {
  env_schema(
    tibble::tibble(
      location_id = rep("MW-1", 7),
      sample_date = seq.Date(
        as.Date("2000-01-01"),
        by = "year",
        length.out = 7
      ),
      param_name = "Arsenic, dissolved",
      analysis_result = c(
        9,
        10,
        10,
        11,
        10,
        9,
        100
      ),
      default_unit = "ug/L"
    ),
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit
  )
}

schema <- make_outlier_schema()

test_that("rosner_test identifies a high outlier", {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Arsenic",
    sample_date = seq.Date(
      as.Date("2000-01-01"),
      by = "year",
      length.out = 11
    ),
    result = c(
      10,
      11,
      9,
      10,
      11,
      10,
      12,
      9,
      10,
      11,
      100
    )
  )

  x <- env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result
  )

  out <- rosner_test(
    x,
    k = 1
  )

  expect_true(
    any(out$Outlier)
  )
})

test_that("rosner_test finds no outliers in stable data", {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Arsenic",
    sample_date = seq.Date(
      as.Date("2000-01-01"),
      by = "year",
      length.out = 10
    ),
    result = c(
      10,
      11,
      10,
      9,
      10,
      11,
      10,
      9,
      10,
      11
    )
  )

  x <- env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result
  )

  out <- rosner_test(
    x,
    k = 1
  )

  expect_false(
    any(out$Outlier)
  )
})

test_that("grubbs_test identifies high outlier", {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Arsenic",
    sample_date = seq.Date(
      as.Date("2000-01-01"),
      by = "year",
      length.out = 8
    ),
    result = c(
      10,
      9,
      10,
      11,
      10,
      9,
      10,
      100
    )
  )

  x <- env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result
  )

  out <- grubbs_test(
    x,
    keep_data_object = TRUE
  )

  expect_true(
    out$p_value[[1]] < 0.05
  )
})

test_that("dixon_test identifies high outlier", {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Arsenic",
    sample_date = seq.Date(
      as.Date("2000-01-01"),
      by = "year",
      length.out = 7
    ),
    result = c(
      9,
      10,
      10,
      11,
      10,
      9,
      100
    )
  )

  x <- env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result
  )

  out <- dixon_test(
    x,
    keep_data_object = TRUE
  )

  expect_true(
    out$p_value[[1]] < 0.05
  )
})

test_that("dixon_test groups by location analyte and unit by default", {
  out <- dixon_test(make_dixon_schema())

  expect_true(
    all(
      c(
        location_var(schema),
        analyte_var(schema),
        ".analysis_unit"
      ) %in%
        names(out)
    )
  )
})

test_that("dixon_test respects existing grouping", {
  schema <- make_dixon_schema()

  out <- schema |>
    dplyr::group_by(
      !!rlang::sym(
        location_var(schema)
      )
    ) |>
    dixon_test()

  expect_true(
    location_var(schema) %in%
      names(out)
  )
})

test_that("dixon_test data.frame dispatch works", {
  df <- tibble::tibble(
    location_id = rep("MW-1", 7),
    sample_date = seq.Date(
      as.Date("2000-01-01"),
      by = "year",
      length.out = 7
    ),
    param_name = "Arsenic, dissolved",
    analysis_result = c(
      9,
      10,
      10,
      11,
      10,
      9,
      100
    ),
    default_unit = "ug/L"
  )

  expect_no_error(
    dixon_test(
      df,
      location_id = "location_id",
      param_name = "param_name",
      sample_date = "sample_date",
      analysis_result = "analysis_result",
      default_unit = "default_unit"
    )
  )
})


test_that("tukey_outlier identifies high outlier", {
  x <- tibble::tibble(
    location = "MW-1",
    analyte = "Arsenic",
    sample_date = seq.Date(
      as.Date("2000-01-01"),
      by = "year",
      length.out = 11
    ),
    result = c(
      10,
      11,
      9,
      10,
      11,
      10,
      12,
      9,
      10,
      11,
      100
    )
  )

  x <- env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result
  )

  out <- tukey_outlier(x)

  expect_true(
    any(out$outlier)
  )
})

test_that("rosner_test respects existing grouping", {
  x <- tibble::tibble(
    location = rep(
      c("MW-1", "MW-2"),
      each = 11
    ),
    analyte = "Arsenic",
    sample_date = rep(
      seq.Date(
        as.Date("2000-01-01"),
        by = "year",
        length.out = 11
      ),
      2
    ),
    result = c(
      10,
      11,
      9,
      10,
      11,
      10,
      12,
      9,
      10,
      11,
      100,
      10,
      11,
      10,
      9,
      10,
      11,
      10,
      9,
      10,
      11,
      10
    )
  )

  x <- env_schema(
    x,
    location = location,
    date = sample_date,
    analyte = analyte,
    result = result
  )

  out <- x |>
    dplyr::group_by(
      !!rlang::sym(location_var(x))
    ) |>
    rosner_test()

  expect_equal(
    dplyr::n_distinct(
      out[[location_var(x)]]
    ),
    2
  )
})

test_that("rosner_test data.frame dispatch works", {
  df <- tibble::tibble(
    location_id = rep("MW-1", 11),
    sample_date = seq.Date(
      as.Date("2000-01-01"),
      by = "year",
      length.out = 11
    ),
    param_name = "Arsenic, dissolved",
    analysis_result = c(
      10,
      11,
      9,
      10,
      11,
      10,
      12,
      9,
      10,
      11,
      100
    ),
    default_unit = "ug/L"
  )

  out <- rosner_test(
    df,
    location_id = "location_id",
    param_name = "param_name",
    sample_date = "sample_date",
    analysis_result = "analysis_result",
    default_unit = "default_unit"
  )

  expect_s3_class(
    out,
    "data.frame"
  )
})
