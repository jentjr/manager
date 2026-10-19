test_that(".convert_to_meq applies conversion factor", {
  expect_equal(
    .convert_to_meq(
      value = 100,
      meq_factor = 0.0499
    ),
    4.99
  )
})

test_that(".convert_to_meq is vectorized", {
  expect_equal(
    .convert_to_meq(
      value = c(10, 20),
      meq_factor = c(1, 0.5)
    ),
    c(10, 10)
  )
})

test_that(".convert_to_meq propagates NA", {
  expect_true(
    is.na(
      .convert_to_meq(
        NA_real_,
        0.5
      )
    )
  )
})
