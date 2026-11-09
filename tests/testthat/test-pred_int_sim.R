# test-pred_int_sim.R

test_that("pred_int_sim matches EnvStats::predIntNormSimultaneous", {
  x <- c(
    4.1,
    4.2,
    4.5,
    4.7,
    4.9,
    5.0,
    5.1,
    5.2,
    5.4,
    5.5,
    5.7,
    5.8,
    5.9,
    6.0,
    6.1,
    6.2,
    6.4,
    6.5,
    6.8,
    7.0
  )

  expected <- EnvStats::predIntNormSimultaneous(
    x = x,
    n.mean = 1,
    k = 1,
    pi.type = "upper",
    conf.level = 0.95
  )

  dist <- tibble::tibble(
    distribution = "Normal",
    n_used = length(x),
    data = list(
      tibble::tibble(
        .analysis_result = x
      )
    )
  )

  result <- pred_int_sim(
    dist,
    n_mean = 1,
    k = 1,
    pi_type = "upper",
    conf_level = 0.95
  )

  expect_equal(
    result$upl,
    unname(expected$interval$limits["UPL"]),
    tolerance = 1e-8
  )
  expect_false(any(is.na(result$lpl)))
  expect_false(any(is.na(result$upl)))
  expect_true(all(result$upl >= result$lpl))
})
