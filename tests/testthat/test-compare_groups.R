# tests/testthat/test-compare_groups.R

# -------------------------------------------------------------------------
# Fixtures
# -------------------------------------------------------------------------

build_comparison_fixture <- function() {
  tibble::tibble(
    location_id = rep(
      c("MW-1", "MW-2", "MW-3"),
      each = 5
    ),

    sample_date = seq.Date(
      as.Date("2020-01-01"),
      by = "month",
      length.out = 15
    ),

    param_name = "Sulfate",
    default_unit = "mg/L",

    analysis_result = c(
      10,
      11,
      12,
      13,
      14,
      20,
      21,
      22,
      23,
      24,
      30,
      31,
      32,
      33,
      34
    ),

    censored = FALSE
  )
}

build_null_comparison_fixture <- function() {
  tibble::tibble(
    location_id = rep(
      c("MW-1", "MW-2", "MW-3"),
      each = 5
    ),

    sample_date = seq.Date(
      as.Date("2020-01-01"),
      by = "month",
      length.out = 15
    ),

    param_name = "Sulfate",
    default_unit = "mg/L",

    analysis_result = rep(
      c(10, 11, 12, 13, 14),
      3
    ),

    censored = FALSE
  )
}

build_censored_comparison_fixture <- function() {
  gw <- build_comparison_fixture()

  gw$censored <- c(
    rep(TRUE, 9),
    rep(FALSE, 6)
  )

  gw
}

expected_comparison_results <- list(
  anova = list(
    statistic = 200,
    p_value = 6.112564e-10
  ),

  welch = list(
    statistic = 184.615385,
    p_value = 2.022907e-07
  ),

  kruskal = list(
    statistic = 12.5,
    p_value = 0.001930454
  )
)

# -------------------------------------------------------------------------
# Internal statistic extraction
# -------------------------------------------------------------------------

test_that(".extract_comparison_statistic handles aov and htest objects", {
  gw <- build_comparison_fixture()

  anova_fit <- stats::aov(
    analysis_result ~ location_id,
    data = gw
  )

  welch_fit <- stats::oneway.test(
    analysis_result ~ location_id,
    data = gw,
    var.equal = FALSE
  )

  kruskal_fit <- stats::kruskal.test(
    analysis_result ~ location_id,
    data = gw
  )

  expect_equal(
    .extract_test_results(anova_fit)$statistic,
    expected_comparison_results$anova$statistic,
    tolerance = 1e-8
  )

  expect_equal(
    .extract_test_results(anova_fit)$p_value,
    expected_comparison_results$anova$p_value,
    tolerance = 1e-12
  )

  expect_equal(
    .extract_test_results(welch_fit)$statistic,
    expected_comparison_results$welch$statistic,
    tolerance = 1e-6
  )

  expect_equal(
    .extract_test_results(kruskal_fit)$statistic,
    expected_comparison_results$kruskal$statistic,
    tolerance = 1e-8
  )
})

# -------------------------------------------------------------------------
# anova_test()
# -------------------------------------------------------------------------

test_that("anova_test returns expected statistic and p-value", {
  gw <- build_comparison_fixture()

  result <- anova_test(
    gw,
    compare = "location_id"
  )

  expect_s3_class(
    result,
    "tbl_df"
  )

  expect_equal(
    result$comparison_method,
    "anova"
  )

  expect_equal(
    result$statistic,
    expected_comparison_results$anova$statistic,
    tolerance = 1e-8
  )

  expect_equal(
    result$p_value,
    expected_comparison_results$anova$p_value,
    tolerance = 1e-12
  )
})

test_that("anova_test does not detect differences when groups are identical", {
  gw <- build_null_comparison_fixture()

  result <- anova_test(
    gw,
    compare = "location_id"
  )

  expect_gt(
    result$p_value,
    0.99
  )
})

# -------------------------------------------------------------------------
# welch_test()
# -------------------------------------------------------------------------

test_that("welch_test returns expected statistic and p-value", {
  gw <- build_comparison_fixture()

  result <- welch_test(
    gw,
    compare = "location_id"
  )

  expect_s3_class(
    result,
    "tbl_df"
  )

  expect_equal(
    result$comparison_method,
    "welch"
  )

  expect_equal(
    result$statistic,
    expected_comparison_results$welch$statistic,
    tolerance = 1e-6
  )

  expect_equal(
    result$p_value,
    expected_comparison_results$welch$p_value,
    tolerance = 1e-10
  )
})

# -------------------------------------------------------------------------
# kruskal_test()
# -------------------------------------------------------------------------

test_that("kruskal_test returns expected statistic and p-value", {
  gw <- build_comparison_fixture()

  result <- kruskal_test(
    gw,
    compare = "location_id"
  )

  expect_s3_class(
    result,
    "tbl_df"
  )

  expect_equal(
    result$comparison_method,
    "kruskal"
  )

  expect_equal(
    result$statistic,
    expected_comparison_results$kruskal$statistic,
    tolerance = 1e-8
  )

  expect_equal(
    result$p_value,
    expected_comparison_results$kruskal$p_value,
    tolerance = 1e-8
  )
})

# -------------------------------------------------------------------------
# compare_groups() automatic selection
# -------------------------------------------------------------------------

test_that("compare_groups selects anova under low censoring", {
  gw <- build_comparison_fixture()

  result <- compare_groups(
    gw,
    compare = "location_id"
  )

  expect_equal(
    unique(result$comparison_method),
    "anova"
  )

  expect_equal(
    result$statistic,
    expected_comparison_results$anova$statistic,
    tolerance = 1e-8
  )

  expect_equal(
    result$p_value,
    expected_comparison_results$anova$p_value,
    tolerance = 1e-12
  )
})

test_that("statistical_decision selects censored nonparametric under moderate censoring", {
  decision <- statistical_decision(
    percent_censored = 60,
    n_used = 15,
    n_detect = 6
  )

  expect_equal(
    decision$comparison_method,
    "censored_nonparametric"
  )
})

# -------------------------------------------------------------------------
# insufficient data
# -------------------------------------------------------------------------

test_that("compare_groups returns insufficient_data recommendation", {
  gw <- build_comparison_fixture() |>
    dplyr::slice(1:3)

  result <- compare_groups(
    gw,
    compare = "location_id"
  )

  expect_equal(
    result$comparison_method,
    "insufficient_data"
  )

  expect_true(
    all(is.na(result$statistic))
  )

  expect_true(
    all(is.na(result$p_value))
  )
})

# -------------------------------------------------------------------------
# single comparison level
# -------------------------------------------------------------------------

test_that("single comparison level returns NA statistics", {
  gw <- build_comparison_fixture()

  gw$location_id <- "MW-1"

  result <- compare_groups(
    gw,
    compare = "location_id"
  )

  expect_true(
    all(is.na(result$statistic))
  )

  expect_true(
    all(is.na(result$p_value))
  )
})

# -------------------------------------------------------------------------
# grouped data behavior
# -------------------------------------------------------------------------

test_that("compare_groups respects existing grouping", {
  gw <- build_comparison_fixture()

  chloride <- dplyr::mutate(
    gw,
    param_name = "Chloride"
  )

  combined <- dplyr::bind_rows(
    gw,
    chloride
  )

  result <- combined |>
    dplyr::group_by(param_name) |>
    compare_groups(
      compare = "location_id"
    )

  expect_equal(
    nrow(result),
    2
  )

  expect_setequal(
    result$param_name,
    c(
      "Sulfate",
      "Chloride"
    )
  )

  expect_true(
    all(result$statistic > 0)
  )

  expect_true(
    all(result$p_value < 0.05)
  )
})

# -------------------------------------------------------------------------
# validation behavior
# -------------------------------------------------------------------------

test_that("compare_groups errors when compare variable is also a grouping variable", {
  gw <- env_schema(
    build_comparison_fixture(),
    location = location_id,
    analyte = param_name,
    sample_date = sample_date,
    result = analysis_result,
    unit = default_unit
  ) |>
    dplyr::group_by(location_id)

  expect_error(
    compare_groups(
      gw,
      compare = "location_id"
    ),
    "cannot also be a grouping variable"
  )
})

# -------------------------------------------------------------------------
# wrapper methods preserve requested method
# -------------------------------------------------------------------------

test_that("anova_test preserves anova method", {
  gw <- build_comparison_fixture()

  result <- anova_test(
    gw,
    compare = "location_id"
  )

  expect_true(
    all(result$comparison_method == "anova")
  )
})

test_that("welch_test preserves welch method", {
  gw <- build_comparison_fixture()

  result <- welch_test(
    gw,
    compare = "location_id"
  )

  expect_true(
    all(result$comparison_method == "welch")
  )
})

test_that("kruskal_test preserves kruskal method", {
  gw <- build_comparison_fixture()

  result <- kruskal_test(
    gw,
    compare = "location_id"
  )

  expect_true(
    all(result$comparison_method == "kruskal")
  )
})

test_that("two censored groups use cenperm2", {
  dat <- tibble::tibble(
    result = c(1, 2, 3, 4, 5, 6, 7, 8),
    censored = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE),
    group = rep(c("A", "B"), each = 4)
  )

  result <- .run_censored_comparison_test(
    df = dat,
    method = "censored_nonparametric",
    compare = "group",
    result_col = "result",
    censored_col = "censored"
  )

  expect_true(is.list(result))
})


test_that("three censored groups use cenanova", {
  set.seed(42)
  n <- 24L

  censored_data <- tibble::tibble(
    location_id = rep(c("A", "B", "C"), each = 8),
    param_name = "Nitrate",
    sample_date = as.Date("2024-01-01"),
    analysis_result = c(
      # Group A: DL, detected, DL, detected, ...
      1.0,
      1.3,
      1.0,
      1.5,
      1.0,
      1.4,
      1.0,
      1.6,

      # Group B
      1.1,
      1.5,
      1.1,
      1.7,
      1.1,
      1.6,
      1.1,
      1.8,

      # Group C
      1.2,
      1.4,
      1.2,
      1.6,
      1.2,
      1.5,
      1.2,
      1.9
    ),
    default_unit = "mg/L",
    censored = rep(c(TRUE, FALSE), 12)
  )

  df <- manager::env_schema(
    censored_data,
    location = location_id,
    analyte = param_name,
    sample_date = sample_date,
    result = analysis_result,
    unit = default_unit
  )

  result <- manager:::.run_censored_comparison_test(
    df = df,
    method = "censored_nonparametric",
    compare = "location_id",
    result_col = "analysis_result",
    censored_col = "censored"
  )

  expect_false(is.null(result))
})

test_that("cenanova returns NULL for unsupported sample sizes", {
  df <- tibble::tibble(
    group = rep(c("A", "B", "C"), each = 4L),
    value = c(
      1.1,
      1.2,
      1.3,
      1.4,
      2.1,
      2.2,
      2.3,
      2.4,
      3.1,
      3.2,
      3.3,
      3.4
    ),
    censored = rep(FALSE, 12L)
  )

  result <- manager:::.run_censored_comparison_test(
    df = df,
    method = "cenanova",
    compare = "group",
    result_col = "value",
    censored_col = "censored"
  )

  expect_null(result)
})

test_that("moderate censoring selects censored nonparametric comparison", {
  decision <- manager:::statistical_decision(
    percent_censored = 60,
    n_used = 15,
    n_detect = 6
  )

  expect_equal(
    decision$comparison_method,
    "censored_nonparametric"
  )
})

test_that("cenanova works when NADA2 printstat is false", {
  data("PbHeron", package = "NADA2")

  result <- .run_cenanova(
    values = PbHeron$Liver,
    censored = PbHeron$LiverCen,
    groups = PbHeron$DosageGroup,
    log_transform = TRUE,
    printstat = FALSE
  )

  expect_false(is.null(result))
})
