test_that("pred_int forwards default arguments to .compute_pred_int", {
  expected <- tibble::tibble(
    prediction_limit = 10
  )

  mockery::stub(
    pred_int,
    ".compute_pred_int",
    function(
      df,
      simultaneous,
      method,
      n_mean,
      k,
      pi_type,
      conf_level
    ) {
      expect_identical(df, dummy_df)
      expect_false(simultaneous)
      expect_identical(method, "Bonferroni")
      expect_identical(n_mean, 1)
      expect_identical(k, 1)
      expect_identical(pi_type, "upper")
      expect_identical(conf_level, 0.95)

      expected
    }
  )

  dummy_df <- tibble::tibble(x = 1:5)

  result <- pred_int(dummy_df)

  expect_identical(result, expected)
})

test_that("pred_int forwards user supplied arguments", {
  expected <- tibble::tibble(
    prediction_limit = 25
  )

  mockery::stub(
    pred_int,
    ".compute_pred_int",
    function(
      df,
      simultaneous,
      method,
      n_mean,
      k,
      pi_type,
      conf_level
    ) {
      expect_identical(df, dummy_df)
      expect_false(simultaneous)
      expect_identical(method, "KM")
      expect_identical(n_mean, 5)
      expect_identical(k, 3)
      expect_identical(pi_type, "two-sided")
      expect_identical(conf_level, 0.99)

      expected
    }
  )

  dummy_df <- tibble::tibble(x = 1:5)

  result <- pred_int(
    df = dummy_df,
    method = "KM",
    n_mean = 5,
    k = 3,
    pi_type = "two-sided",
    conf_level = 0.99
  )

  expect_identical(result, expected)
})

test_that("pred_int sets simultaneous = FALSE", {
  mockery::stub(
    pred_int,
    ".compute_pred_int",
    function(simultaneous, ...) {
      expect_false(simultaneous)
      tibble::tibble()
    }
  )

  pred_int(tibble::tibble(x = 1))
})
