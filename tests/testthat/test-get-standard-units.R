test_that(".get_standard_units returns expected columns", {
  result <- .get_standard_units()

  expect_s3_class(
    result,
    "data.frame"
  )

  expect_true(
    all(
      c(
        "analyte",
        "std_unit"
      ) %in%
        names(result)
    )
  )
})

test_that(".get_standard_units validates lookup table", {
  bad_lookup <- tibble::tibble(
    analyte = "calcium"
  )

  expect_error(
    .get_standard_units(
      bad_lookup
    )
  )
})
