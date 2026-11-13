test_that(".get_meq_conversion_factors returns expected columns", {
  result <- .get_meq_conversion_factors()

  expect_true(
    all(
      c(
        "analyte_id",
        "std_unit",
        "meq_factor"
      ) %in%
        names(result)
    )
  )
})

test_that(".get_meq_conversion_factors normalizes units", {
  lookup <- tibble::tibble(
    analyte_id = "test ion",
    analyte = "Test ION",
    normalized_analyte = "test ion",
    short_name = "TI",
    analyte_group = "Major Ion",
    aliases = list(character()),
    std_unit = " MG/L ",
    ion_class = "cation",
    charge = 1,
    molecular_weight = 1,
    meq_factor = 1,
    fractionable = FALSE
  )

  result <- .get_meq_conversion_factors(
    lookup
  )

  expect_equal(
    result$std_unit,
    "mg/l"
  )
})

test_that(".get_meq_conversion_factors normalizes std_unit", {
  lookup <- tibble::tibble(
    analyte_id = "test_ion",
    analyte = "Test Ion",
    normalized_analyte = "test ion",
    short_name = "TI",
    analyte_group = "Major Ion",
    std_unit = " MG/L ",
    meq_factor = 1,
    fractionable = FALSE
  )

  result <- .get_meq_conversion_factors(
    lookup = lookup
  )

  expect_equal(
    result$std_unit,
    .normalize_unit(" MG/L ")
  )
})
