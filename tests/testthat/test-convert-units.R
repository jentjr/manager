test_that(".convert_units converts mg/L to ug/L", {
  result <- .convert_units(
    value = 1,
    from_unit = "mg/L",
    to_unit = "ug/L"
  )

  expect_equal(
    result,
    1000
  )
})

test_that(".convert_units is vectorized", {
  result <- .convert_units(
    value = c(1, 2),
    from_unit = "mg/L",
    to_unit = "ug/L"
  )

  expect_equal(
    result,
    c(1000, 2000)
  )
})

test_that(".convert_units errors on incompatible units", {
  expect_error(
    .convert_units(
      value = 1,
      from_unit = "mg/L",
      to_unit = "ft"
    )
  )
})

test_that(
  "analytes without standard units retain values and units",
  {
    
    x <- gw_data %>%
      dplyr::filter(
        param_name == "Total Dissolved Solids"
      ) %>%
      standardize_units()
    
    expect_true(
      all(
        x$analysis_result_std ==
          x$analysis_result,
        na.rm = TRUE
      )
    )
    
    expect_equal(
      x$default_unit_std,
      .normalize_unit(
        x$default_unit
      )
    )
    
  }
)

test_that(
  "analytes without standard units do not create missing standardized values",
  {
    
    x <- gw_data %>%
      dplyr::filter(
        param_name == "Total Dissolved Solids"
      ) %>%
      standardize_units()
    
    expect_false(
      any(
        is.na(x$analysis_result_std)
      )
    )
    
  }
)