test_that(".validate_analyte_meq_units accepts standard units", {
  df <- tibble::tibble(
    analyte = "calcium",
    default_unit_std = "mg/l"
  )

  expect_invisible(
    .validate_analyte_meq_units(
      df
    )
  )
})

test_that(".validate_analyte_meq_units errors on non-standard units", {
  df <- tibble::tibble(
    analyte = "calcium",
    default_unit_std = "ug/l"
  )

  expect_error(
    .validate_analyte_meq_units(
      df
    ),
    "Non-standard units found"
  )
})

test_that(".validate_analyte_meq_units ignores unsupported analytes", {
  df <- tibble::tibble(
    analyte = "benzene",
    default_unit_std = "ug/l"
  )

  expect_invisible(
    .validate_analyte_meq_units(
      df
    )
  )
})

test_that(".validate_analyte_meq_units requires columns", {
  expect_error(
    .validate_analyte_meq_units(
      tibble::tibble()
    )
  )
})
