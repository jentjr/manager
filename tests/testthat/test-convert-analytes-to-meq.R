test_that(".convert_analytes_to_meq converts supported analytes", {
  df <- tibble::tibble(
    analyte = "calcium",
    analysis_result_std = 40,
    default_unit_std = "mg/l"
  )

  result <- .convert_analytes_to_meq(
    df
  )

  expect_true(
    "analysis_result_meq" %in%
      names(result)
  )

  expect_equal(
    result$analysis_result_meq,
    40 *
      analyte_tbl$meq_factor[
        match(
          "calcium",
          analyte_tbl$analyte
        )
      ]
  )

  expect_equal(
    result$default_unit_meq,
    "meq/L"
  )
})

test_that(".convert_analytes_to_meq returns NA for unsupported analytes", {
  df <- tibble::tibble(
    analyte = "benzene",
    analysis_result_std = 1,
    default_unit_std = "ug/l"
  )

  result <- .convert_analytes_to_meq(
    df
  )

  expect_true(
    is.na(
      result$analysis_result_meq
    )
  )

  expect_true(
    is.na(
      result$default_unit_meq
    )
  )
})

test_that(".convert_analytes_to_meq preserves missing values", {
  df <- tibble::tibble(
    analyte = "calcium",
    analysis_result_std = NA_real_,
    default_unit_std = "mg/l"
  )

  result <- .convert_analytes_to_meq(
    df
  )

  expect_true(
    is.na(
      result$analysis_result_meq
    )
  )
})

test_that(".convert_analytes_to_meq validates units", {
  df <- tibble::tibble(
    analyte = "calcium",
    analysis_result_std = 40,
    default_unit_std = "ug/l"
  )

  expect_error(
    .convert_analytes_to_meq(
      df
    ),
    "Non-standard units found"
  )
})

test_that(".convert_analytes_to_meq supports custom column names", {
  df <- tibble::tibble(
    analyte = "calcium",
    concentration = 40,
    unit = "mg/l"
  )

  result <- .convert_analytes_to_meq(
    df,
    value_col = "concentration",
    unit_col = "unit",
    value_out_col = "meq",
    unit_out_col = "meq_unit"
  )

  expect_true(
    all(
      c(
        "meq",
        "meq_unit"
      ) %in%
        names(result)
    )
  )
})

test_that("calcium meq conversion matches handbook value", {
  expect_equal(
    .convert_to_meq(
      value = 40,
      meq_factor = 0.0499
    ),
    1.996,
    tolerance = 1e-3
  )
})
