test_that("est_dist_censored errors when required columns are missing", {
  df <- tibble::tibble(
    analysis_result = 1:10
  )

  expect_error(
    est_dist_censored(df),
    "Missing required column"
  )
})

test_that("est_dist_censored returns insufficient data", {
  df <- tibble::tibble(
    group = "A",
    analysis_result = c(
      1,
      2,
      3
    ),
    left_censored = c(
      FALSE,
      FALSE,
      TRUE
    )
  ) %>%
    dplyr::group_by(group)

  result <- est_dist_censored(
    df,
    min_n = 4
  )

  expect_equal(
    result$distribution,
    "insufficient_data"
  )
})

test_that("est_dist_censored returns one row per group", {
  df <- tibble::tibble(
    group = rep(
      c("A", "B"),
      each = 20
    ),
    analysis_result = rgamma(
      40,
      shape = 2
    ),
    left_censored = c(
      rep(TRUE, 2),
      rep(FALSE, 18),
      rep(TRUE, 2),
      rep(FALSE, 18)
    )
  ) %>%
    dplyr::group_by(group)

  result <- est_dist_censored(df)

  expect_equal(
    nrow(result),
    2
  )
})

test_that("est_dist_censored can return result object", {
  df <- tibble::tibble(
    group = "A",
    analysis_result = rgamma(
      20,
      shape = 2
    ),
    left_censored = c(
      rep(TRUE, 2),
      rep(FALSE, 18)
    )
  ) %>%
    dplyr::group_by(group)

  result <- est_dist_censored(
    df,
    return_result = TRUE
  )

  expect_true(
    "result" %in%
      names(result)
  )
})

test_that("est_dist_censored excludes missing analysis results", {
  df <- tibble::tibble(
    group = "A",
    analysis_result = c(
      rgamma(10, 2),
      NA,
      NA
    ),
    left_censored = c(
      rep(TRUE, 2),
      rep(FALSE, 8),
      FALSE,
      FALSE
    )
  ) %>%
    dplyr::group_by(group)

  result <- est_dist_censored(
    df,
    min_n = 4
  )

  expect_equal(
    result$n,
    10
  )
})
