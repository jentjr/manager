testthat::test_that("summary_stats dispatches for env_schema", {
  x <- env_schema(
    tibble::tibble(
      location = c("MW-1", "MW-1", "MW-1"),
      date = as.Date(c(
        "2024-01-01",
        "2024-01-02",
        "2024-01-03"
      )),
      analyte = "Arsenic",
      result = c(1, 2, 3)
    ),
    location = location,
    date = date,
    analyte = analyte,
    result = result
  )

  out <- summary_stats(x)

  expect_s3_class(out, "tbl_df")
})

testthat::test_that("summary_stats dispatches for data.frame", {
  x <- tibble::tibble(
    location_id = c("MW-1", "MW-1", "MW-1"),
    param_name = "Arsenic",
    lt_measure = NA_character_,
    analysis_result = c(1, 2, 3),
    default_unit = "ug/L"
  )

  out <- summary_stats(x)

  expect_s3_class(out, "tbl_df")
})

testthat::test_that("summary_stats computes expected values for env_schema", {
  x <- env_schema(
    tibble::tibble(
      location = c("MW-1", "MW-1", "MW-1"),
      date = as.Date(c(
        "2024-01-01",
        "2024-01-02",
        "2024-01-03"
      )),
      analyte = "Arsenic",
      result = c(1, 2, 3)
    ),
    location = location,
    date = date,
    analyte = analyte,
    result = result
  )

  out <- summary_stats(x)

  expect_equal(nrow(out), 1)

  expect_equal(out$n, 3)

  expect_equal(out$mean, 2)

  expect_equal(out$median, 2)

  expect_equal(out$min, 1)

  expect_equal(out$max, 3)

  expect_equal(out$`25%`, 1.5)

  expect_equal(out$`75%`, 2.5)

  expect_equal(out$percent_lt, 0)
})

testthat::test_that("summary_stats computes expected values for data.frame", {
  x <- tibble::tibble(
    location_id = c("MW-1", "MW-1", "MW-1"),
    param_name = "Arsenic",
    lt_measure = c(
      NA_character_,
      "<",
      "<"
    ),
    analysis_result = c(1, 2, 3),
    default_unit = "ug/L"
  )

  out <- summary_stats(x)

  expect_equal(out$n, 3)

  expect_equal(out$mean, 2)

  expect_equal(out$percent_lt, 2 / 3 * 100)
})

testthat::test_that("summary_stats supports custom source-column mappings", {
  x <- env_schema(
    tibble::tibble(
      well_id = c("MW-1", "MW-1", "MW-1"),
      sample_dt = as.Date(c(
        "2024-01-01",
        "2024-01-02",
        "2024-01-03"
      )),
      chemical = "Arsenic",
      concentration = c(1, 2, 3)
    ),
    location = well_id,
    date = sample_dt,
    analyte = chemical,
    result = concentration
  )

  out <- summary_stats(x)

  expect_equal(nrow(out), 1)

  expect_equal(out$n, 3)

  expect_equal(out$mean, 2)

  expect_equal(out$min, 1)

  expect_equal(out$max, 3)
})

testthat::test_that("summary_stats respects dplyr grouping", {
  x <- env_schema(
    tibble::tibble(
      location = c(
        "MW-1",
        "MW-1",
        "MW-2",
        "MW-2"
      ),
      date = as.Date(c(
        "2024-01-01",
        "2024-01-02",
        "2024-01-01",
        "2024-01-02"
      )),
      analyte = c(
        "Arsenic",
        "Arsenic",
        "Arsenic",
        "Arsenic"
      ),
      result = c(1, 2, 10, 20)
    ),
    location = location,
    date = date,
    analyte = analyte,
    result = result
  )

  out <- x %>%
    dplyr::group_by(location) %>%
    summary_stats()

  expect_equal(nrow(out), 2)

  expect_equal(
    out$mean[out$location == "MW-1"],
    1.5
  )

  expect_equal(
    out$mean[out$location == "MW-2"],
    15
  )
})

testthat::test_that("summary_stats works after filter and mutate", {
  x <- env_schema(
    tibble::tibble(
      location = c(
        "MW-1",
        "MW-1",
        "MW-2"
      ),
      date = as.Date(c(
        "2024-01-01",
        "2024-01-02",
        "2024-01-03"
      )),
      analyte = c(
        "Arsenic",
        "Arsenic",
        "Arsenic"
      ),
      result = c(1, 2, 3)
    ),
    location = location,
    date = date,
    analyte = analyte,
    result = result
  )

  out <- x %>%
    dplyr::filter(
      location == "MW-1"
    ) %>%
    dplyr::mutate(
      result_x2 = result * 2
    ) %>%
    summary_stats()

  expect_equal(nrow(out), 1)

  expect_equal(out$n, 2)

  expect_equal(out$mean, 1.5)
})

testthat::test_that("summary_stats errors for invalid env_schema", {
  x <- env_schema(
    tibble::tibble(
      location = "MW-1",
      date = as.Date("2024-01-01"),
      analyte = "Arsenic",
      result = 1
    ),
    location = location,
    date = date,
    analyte = analyte,
    result = result
  )

  class(x) <- setdiff(
    class(x),
    "env_schema"
  )

  expect_error(
    summary_stats.env_schema(x),
    "env_schema"
  )
})

testthat::test_that("summary_stats computes percent_lt correctly for censored env_schema values", {
  x <- env_schema(
    tibble::tibble(
      location = rep("MW-1", 4),
      date = as.Date(
        c(
          "2024-01-01",
          "2024-01-02",
          "2024-01-03",
          "2024-01-04"
        )
      ),
      analyte = "Arsenic",
      result = c(1, 2, 3, 4),
      reporting_flag = c(
        "<",
        "<",
        NA,
        NA
      )
    ),
    location = location,
    date = date,
    analyte = analyte,
    result = result,
    reporting_flag = reporting_flag
  )

  out <- summary_stats(x)

  expect_equal(
    out$percent_lt,
    50
  )
})

testthat::test_that("summary_stats returns 0 percent_lt when no values are censored", {
  x <- env_schema(
    tibble::tibble(
      location = rep("MW-1", 4),
      date = as.Date(
        c(
          "2024-01-01",
          "2024-01-02",
          "2024-01-03",
          "2024-01-04"
        )
      ),
      analyte = "Arsenic",
      result = c(1, 2, 3, 4)
    ),
    location = location,
    date = date,
    analyte = analyte,
    result = result
  )

  out <- summary_stats(x)

  expect_equal(
    out$percent_lt,
    0
  )
})

testthat::test_that("summary_stats returns 100 percent_lt when all values are censored", {
  x <- env_schema(
    tibble::tibble(
      location = rep("MW-1", 4),
      date = as.Date(
        c(
          "2024-01-01",
          "2024-01-02",
          "2024-01-03",
          "2024-01-04"
        )
      ),
      analyte = "Arsenic",
      result = c(1, 2, 3, 4),
      reporting_flag = rep("<", 4)
    ),
    location = location,
    date = date,
    analyte = analyte,
    result = result,
    reporting_flag = reporting_flag
  )

  out <- summary_stats(x)

  expect_equal(
    out$percent_lt,
    100
  )
})

testthat::test_that("summary_stats computes percent_lt correctly for data.frame inputs", {
  x <- tibble::tibble(
    location_id = rep("MW-1", 4),
    param_name = "Arsenic",
    analysis_result = c(1, 2, 3, 4),
    default_unit = "ug/L",
    lt_measure = c(
      "<",
      "<",
      NA,
      NA
    )
  )

  out <- summary_stats(x)

  expect_equal(
    out$percent_lt,
    50
  )
})

testthat::test_that("summary_stats computes percent_lt within groups", {
  x <- env_schema(
    tibble::tibble(
      location = c(
        "MW-1",
        "MW-1",
        "MW-2",
        "MW-2"
      ),
      date = as.Date(
        c(
          "2024-01-01",
          "2024-01-02",
          "2024-01-01",
          "2024-01-02"
        )
      ),
      analyte = "Arsenic",
      result = c(1, 2, 3, 4),
      reporting_flag = c(
        "<",
        NA,
        "<",
        "<"
      )
    ),
    location = location,
    date = date,
    analyte = analyte,
    result = result,
    reporting_flag = reporting_flag
  )

  out <- x %>%
    dplyr::group_by(location) %>%
    summary_stats()

  expect_equal(
    out$percent_lt[out$location == "MW-1"],
    50
  )

  expect_equal(
    out$percent_lt[out$location == "MW-2"],
    100
  )
})

test_that("summary_stats works on grouped env_schema", {
  out <- gw %>%
    dplyr::group_by(location_id) %>%
    summary_stats()

  expect_s3_class(
    out,
    "tbl_df"
  )

  expect_gt(
    nrow(out),
    0
  )
})

test_that("summary_stats respects grouping", {
  out <- make_schema() %>%
    dplyr::group_by(location) %>%
    summary_stats()

  expect_equal(
    nrow(out),
    2
  )

  expect_setequal(
    out$location,
    c("MW-1", "MW-2")
  )
})
