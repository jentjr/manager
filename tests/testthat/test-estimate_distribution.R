test_that("estimate_distribution returns decision metadata", {
  result <- build_schema() |>
    dplyr::group_by(
      location_id,
      param_name
    ) |>
    estimate_distribution()

  expect_true(
    all(
      c(
        "distribution_method",
        "distribution_config",
        "interval_config",
        "decision_basis",
        "n_detect",
        "percent_censored"
      ) %in%
        names(result)
    )
  )
})

test_that("validate_est_dist_output accepts new estimate_distribution output", {
  result <- build_schema() |>
    dplyr::group_by(
      location_id,
      param_name
    ) |>
    estimate_distribution()

  expect_invisible(
    .validate_est_dist_output(result)
  )
})

test_that("estimate_distribution returns decision metadata", {
  dist <- build_schema() |>
    dplyr::group_by(
      location_id,
      param_name
    ) |>
    estimate_distribution()

  expect_true(
    all(
      c(
        "distribution_method",
        "censored_method",
        "distribution_config",
        "interval_config",
        "decision_basis"
      ) %in%
        names(dist)
    )
  )
})

test_that("pred_int runs on supported distributions", {
  dist <- build_schema() |>
    dplyr::group_by(
      location_id,
      param_name
    ) |>
    estimate_distribution() |>
    dplyr::filter(
      distribution %in%
        c(
          "Normal",
          "Lognormal",
          "Nonparametric"
        )
    )

  skip_if(
    nrow(dist) == 0,
    "No interval-compatible distributions."
  )

  result <- pred_int(dist)

  expect_s3_class(
    result,
    "tbl_df"
  )
})
