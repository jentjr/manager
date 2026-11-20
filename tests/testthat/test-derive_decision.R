# tests/testthat/test-derive_decision.R

# -------------------------------------------------------------------------
# Fixtures
# -------------------------------------------------------------------------

build_decision_fixture <- function() {
  tibble::tibble(
    location_id = rep("MW-1", 10),

    sample_date = seq.Date(
      as.Date("2020-01-01"),
      by = "month",
      length.out = 10
    ),

    param_name = "Sulfate",

    default_unit = "mg/L",

    analysis_result = c(
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10
    ),

    censored = c(
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE
    )
  )
}

# -------------------------------------------------------------------------
# derive_decision()
# -------------------------------------------------------------------------

test_that("derive_decision calculates expected metrics", {
  decision <- derive_decision(
    build_decision_fixture()
  )

  expect_equal(
    decision$n_used,
    10
  )

  expect_equal(
    decision$n_detect,
    8
  )

  expect_equal(
    decision$percent_censored,
    20
  )

  expect_equal(
    decision$censoring_class,
    "moderate_censoring"
  )

  expect_equal(
    decision$distribution_method,
    "ros_censored"
  )

  expect_equal(
    decision$comparison_method,
    "censored_nonparametric"
  )
})

# -------------------------------------------------------------------------
# missing results excluded from n_used
# -------------------------------------------------------------------------

test_that("derive_decision excludes missing analytical results", {
  gw <- build_decision_fixture()

  gw$analysis_result[c(1, 2)] <- NA_real_

  decision <- derive_decision(
    gw
  )

  expect_equal(
    decision$n_used,
    8
  )

  expect_equal(
    decision$n_detect,
    8
  )
})

# -------------------------------------------------------------------------
# detects calculated correctly
# -------------------------------------------------------------------------

test_that("derive_decision computes n_detect correctly", {
  gw <- build_decision_fixture()

  gw$censored <- c(
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE
  )

  decision <- derive_decision(
    gw
  )

  expect_equal(
    decision$n_used,
    10
  )

  expect_equal(
    decision$n_detect,
    6
  )

  expect_equal(
    decision$percent_censored,
    40
  )
})

# -------------------------------------------------------------------------
# low censoring pathway
# -------------------------------------------------------------------------

test_that("derive_decision selects low censoring recommendations", {
  gw <- build_decision_fixture()

  gw$censored <- c(
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    TRUE
  )

  decision <- derive_decision(
    gw
  )

  expect_equal(
    decision$percent_censored,
    10
  )

  expect_equal(
    decision$censoring_class,
    "low_censoring"
  )

  expect_equal(
    decision$distribution_method,
    "parametric"
  )

  expect_equal(
    decision$comparison_method,
    "anova"
  )
})

# -------------------------------------------------------------------------
# high censoring pathway
# -------------------------------------------------------------------------

test_that("derive_decision selects high censoring recommendations", {
  gw <- build_decision_fixture()

  gw$censored <- c(
    rep(TRUE, 8),
    FALSE,
    FALSE
  )

  decision <- derive_decision(
    gw
  )

  expect_equal(
    decision$percent_censored,
    80
  )

  expect_equal(
    decision$censoring_class,
    "high_censoring"
  )

  expect_equal(
    decision$distribution_method,
    "nonparametric"
  )

  expect_equal(
    decision$comparison_method,
    "insufficient_data"
  )
})

# -------------------------------------------------------------------------
# extreme censoring pathway
# -------------------------------------------------------------------------

test_that("derive_decision selects extreme censoring recommendations", {
  gw <- build_decision_fixture()

  gw$censored <- c(
    rep(TRUE, 9),
    FALSE
  )

  decision <- derive_decision(
    gw
  )

  expect_equal(
    decision$percent_censored,
    90
  )

  expect_equal(
    decision$censoring_class,
    "extreme_censoring"
  )

  expect_equal(
    decision$distribution_method,
    "descriptive_only"
  )

  expect_equal(
    decision$comparison_method,
    "insufficient_data"
  )
})

# -------------------------------------------------------------------------
# insufficient data pathway
# -------------------------------------------------------------------------

test_that("derive_decision selects insufficient data recommendations", {
  gw <- build_decision_fixture() |>
    dplyr::slice(1:3)

  decision <- derive_decision(
    gw
  )

  expect_equal(
    decision$n_used,
    3
  )

  expect_equal(
    decision$distribution_method,
    "insufficient_data"
  )

  expect_equal(
    decision$comparison_method,
    "insufficient_data"
  )

  expect_equal(
    decision$decision_basis,
    "fewer_than_4_observations"
  )
})

test_that("all missing censoring indicators return NA censoring metrics", {
  gw <- build_decision_fixture()

  gw$censored <- NA

  decision <- derive_decision(
    gw
  )

  expect_true(
    is.na(decision$percent_censored)
  )

  expect_true(
    is.na(decision$censoring_class)
  )
})
# -------------------------------------------------------------------------
# custom column names
# -------------------------------------------------------------------------

test_that("derive_decision respects result_col and censored_col", {
  df <- tibble::tibble(
    value = c(
      1,
      2,
      3,
      4,
      5
    ),
    flag = c(
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE
    )
  )

  decision <- derive_decision(
    df,
    result_col = "value",
    censored_col = "flag"
  )

  expect_equal(
    decision$n_used,
    5
  )

  expect_equal(
    decision$n_detect,
    4
  )

  expect_equal(
    decision$percent_censored,
    20
  )
})

# -------------------------------------------------------------------------
# returned object
# -------------------------------------------------------------------------

test_that("derive_decision returns statistical_decision object", {
  decision <- derive_decision(
    build_decision_fixture()
  )

  expect_s3_class(
    decision,
    "statistical_decision"
  )

  expect_s3_class(
    decision,
    "tbl_df"
  )
})

# -------------------------------------------------------------------------
# grouped_df object
# -------------------------------------------------------------------------

test_that("grouped data returns one decision per group", {
  gw <- tibble::tibble(
    location_id = c(
      rep("MW-1", 5),
      rep("MW-2", 5)
    ),
    analysis_result = c(
      1,
      2,
      3,
      4,
      5,
      1,
      2,
      3,
      4,
      5
    ),
    censored = c(
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE
    )
  ) |>
    dplyr::group_by(location_id)

  out <- derive_decision(gw)

  expect_equal(
    nrow(out),
    2
  )

  expect_equal(
    out$percent_censored,
    c(0, 100)
  )
})

test_that("grouped data.frame computes decisions per group", {
  df <- tibble::tibble(
    grp = c("A", "A", "A", "B", "B", "B"),
    analysis_result = c(1, 2, 3, 1, 2, 3),
    censored = c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE)
  ) |>
    dplyr::group_by(grp)

  out <- derive_decision(df)

  expect_equal(
    nrow(out),
    2
  )

  expect_equal(
    out$n_used,
    c(3, 3)
  )
})

test_that("derive_decision respects grouping", {
  df <- tibble::tibble(
    grp = c(
      rep("A", 10),
      rep("B", 10)
    ),
    analysis_result = 1:20,
    censored = c(
      rep(FALSE, 10),
      rep(TRUE, 10)
    )
  ) |>
    dplyr::group_by(grp)

  out <- derive_decision(df)

  expect_equal(
    out$percent_censored,
    c(0, 100)
  )

  expect_equal(
    out$censoring_class,
    c(
      "low_censoring",
      "extreme_censoring"
    )
  )
})

test_that("grouped env_schema computes one decision per group", {
  gw <- env_schema(
    tibble::tibble(
      location_id = c(
        rep("MW-1", 5),
        rep("MW-2", 5)
      ),
      sample_date = seq.Date(
        as.Date("2020-01-01"),
        by = "month",
        length.out = 10
      ),
      param_name = "Sulfate",
      analysis_result = c(1:10),
      reporting_flag = c(
        rep("", 5),
        rep("<", 5)
      )
    ),
    location = location_id,
    sample_date = sample_date,
    analyte = param_name,
    result = analysis_result,
    reporting_flag = reporting_flag
  ) |>
    dplyr::group_by(location_id)

  out <- derive_decision(gw)

  expect_equal(
    nrow(out),
    2
  )

  expect_equal(
    out$percent_censored,
    c(0, 100)
  )
})
