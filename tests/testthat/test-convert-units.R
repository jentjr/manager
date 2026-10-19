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
