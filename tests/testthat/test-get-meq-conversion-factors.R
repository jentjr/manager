test_that(".get_meq_conversion_factors returns expected columns", {
  result <- .get_meq_conversion_factors()

  expect_true(
    all(
      c(
        "analyte",
        "std_unit",
        "meq_factor"
      ) %in%
        names(result)
    )
  )
})

test_that(".get_meq_conversion_factors normalizes units", {
  lookup <- tibble::tibble(
    analyte = "test ion",
    aliases = list(character()),
    std_unit = " MG/L ",
    ion_class = "cation",
    charge = 1,
    molecular_weight = 1,
    meq_factor = 1
  )

  result <- .get_meq_conversion_factors(
    lookup
  )

  expect_equal(
    result$std_unit,
    "mg/l"
  )
})
