test_that("charge_balance returns near zero for balanced chemistry", {
  df <- tibble::tibble(
    site = c("MW-1", "MW-1"),
    dt = as.Date(c("2024-01-01", "2024-01-01")),
    chem = c(
      "Calcium, dissolved",
      "Chloride, total"
    ),
    value = c(
      40.08,
      70.90
    ),
    unit = c(
      "mg/L",
      "mg/L"
    )
  )

  out <- charge_balance(
    df,
    location = site,
    date = dt,
    analyte = chem,
    result = value,
    unit = unit
  )

  expect_lt(
    abs(out$charge_balance_pct),
    0.01
  )
})

test_that("charge_balance detects cation excess", {
  df <- tibble::tibble(
    site = c("MW-1", "MW-1"),
    dt = as.Date(c("2024-01-01", "2024-01-01")),
    chem = c(
      "Calcium, dissolved",
      "Chloride, total"
    ),
    value = c(
      80.16,
      70.90
    ),
    unit = "mg/L"
  )

  out <- charge_balance(
    df,
    location = site,
    date = dt,
    analyte = chem,
    result = value,
    unit = unit
  )

  expect_gt(
    out$charge_balance_pct,
    0
  )
})

test_that("charge_balance detects anion excess", {
  df <- tibble::tibble(
    site = c("MW-1", "MW-1"),
    dt = as.Date(c("2024-01-01", "2024-01-01")),
    chem = c(
      "Calcium, dissolved",
      "Chloride, total"
    ),
    value = c(
      40.08,
      141.8
    ),
    unit = "mg/L"
  )

  out <- charge_balance(
    df,
    location = site,
    date = dt,
    analyte = chem,
    result = value,
    unit = unit
  )

  expect_lt(
    out$charge_balance_pct,
    0
  )
})

test_that("charge_balance preserves env_schema dispatch", {
  x <- env_schema(
    gw_data,
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit
  )

  out <- charge_balance(x)

  expect_s3_class(
    out,
    "data.frame"
  )

  expect_true(
    "charge_balance_pct" %in% names(out)
  )
})

test_that("charge_balance.data.frame dispatch works", {
  out <- charge_balance(
    gw_data,
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit
  )

  expect_true(
    "charge_balance_pct" %in% names(out)
  )
})

test_that("charge_balance reproduces known answer", {
  df <- tibble::tibble(
    site = rep("MW-1", 4),
    dt = as.Date(rep("2024-01-01", 4)),
    chem = c(
      "Calcium, dissolved",
      "Magnesium, dissolved",
      "Chloride, total",
      "Sulfate, total"
    ),
    value = c(
      40.08, # 2 meq/L
      12.15, # 1 meq/L
      70.90, # 2 meq/L
      48.03 # 1 meq/L
    ),
    unit = "mg/L"
  )

  out <- charge_balance(
    df,
    location = site,
    date = dt,
    analyte = chem,
    result = value,
    unit = unit
  )

  expect_equal(
    out$charge_balance_pct,
    0,
    tolerance = 0.01
  )
})

test_that("charge_balance reproduces exact 10 percent imbalance", {
  df <- tibble::tibble(
    site = c(
      "MW-1",
      "MW-1"
    ),
    dt = as.Date(c(
      "2024-01-01",
      "2024-01-01"
    )),
    chem = c(
      "Calcium, dissolved",
      "Chloride, total"
    ),
    value = c(
      220.44, # 11 meq/L Ca
      319.05 # 9 meq/L Cl
    ),
    unit = c(
      "mg/L",
      "mg/L"
    )
  )

  out <- charge_balance(
    df,
    location = site,
    date = dt,
    analyte = chem,
    result = value,
    unit = unit
  )

  expect_equal(
    out$charge_balance_pct,
    10,
    tolerance = 0.01
  )
})

test_that("charge_balance reproduces exact negative 10 percent imbalance", {
  df <- tibble::tibble(
    site = c(
      "MW-1",
      "MW-1"
    ),
    dt = as.Date(c(
      "2024-01-01",
      "2024-01-01"
    )),
    chem = c(
      "Calcium, dissolved",
      "Chloride, total"
    ),
    value = c(
      180.36, # 9 meq/L Ca
      389.95 # 11 meq/L Cl
    ),
    unit = c(
      "mg/L",
      "mg/L"
    )
  )

  out <- charge_balance(
    df,
    location = site,
    date = dt,
    analyte = chem,
    result = value,
    unit = unit
  )

  expect_equal(
    out$charge_balance_pct,
    -10,
    tolerance = 0.01
  )
})
