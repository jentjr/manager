# ------------------------------------------------------------------------------
# Test fixture
# ------------------------------------------------------------------------------

major_ion_data <- tibble::tribble(
  ~location_id , ~sample_date          , ~param_name               , ~analysis_result , ~default_unit ,

  # MW-1 Sample 1
  "MW-1"       , as.Date("2020-01-01") , "Calcium, dissolved"      ,  40              , "mg/L"        ,
  "MW-1"       , as.Date("2020-01-01") , "Magnesium, dissolved"    ,  12              , "mg/L"        ,
  "MW-1"       , as.Date("2020-01-01") , "Sodium, dissolved"       ,  25              , "mg/L"        ,
  "MW-1"       , as.Date("2020-01-01") , "Potassium, dissolved"    ,   3              , "mg/L"        ,
  "MW-1"       , as.Date("2020-01-01") , "Chloride, total"         ,  18              , "mg/L"        ,
  "MW-1"       , as.Date("2020-01-01") , "Sulfate, total"          ,  30              , "mg/L"        ,
  "MW-1"       , as.Date("2020-01-01") , "Alkalinity, total (lab)" , 110              , "mg/L"        ,

  # MW-1 Sample 2
  "MW-1"       , as.Date("2020-06-01") , "Calcium, dissolved"      ,  45              , "mg/L"        ,
  "MW-1"       , as.Date("2020-06-01") , "Magnesium, dissolved"    ,  11              , "mg/L"        ,
  "MW-1"       , as.Date("2020-06-01") , "Sodium, dissolved"       ,  28              , "mg/L"        ,
  "MW-1"       , as.Date("2020-06-01") , "Potassium, dissolved"    ,   4              , "mg/L"        ,
  "MW-1"       , as.Date("2020-06-01") , "Chloride, total"         ,  20              , "mg/L"        ,
  "MW-1"       , as.Date("2020-06-01") , "Sulfate, total"          ,  35              , "mg/L"        ,
  "MW-1"       , as.Date("2020-06-01") , "Alkalinity, total (lab)" , 125              , "mg/L"        ,

  # MW-2 Sample 1
  "MW-2"       , as.Date("2020-01-01") , "Calcium, dissolved"      ,  55              , "mg/L"        ,
  "MW-2"       , as.Date("2020-01-01") , "Magnesium, dissolved"    ,  14              , "mg/L"        ,
  "MW-2"       , as.Date("2020-01-01") , "Sodium, dissolved"       ,  20              , "mg/L"        ,
  "MW-2"       , as.Date("2020-01-01") , "Potassium, dissolved"    ,   2              , "mg/L"        ,
  "MW-2"       , as.Date("2020-01-01") , "Chloride, total"         ,  12              , "mg/L"        ,
  "MW-2"       , as.Date("2020-01-01") , "Sulfate, total"          ,  22              , "mg/L"        ,
  "MW-2"       , as.Date("2020-01-01") , "Alkalinity, total (lab)" , 135              , "mg/L"        ,

  # Optional non-major-ion analytes
  "MW-1"       , as.Date("2020-01-01") , "Boron"                   ,   0.5            , "ug/L"        ,
  "MW-2"       , as.Date("2020-01-01") , "Boron"                   ,   0.3            , "ug/L"
)

build_major_ion_schema <- function(df = major_ion_data) {
  env_schema(
    df,
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    sample_date = sample_date
  )
}

build_simple_major_ion_schema <- function(
  analyte,
  result,
  unit = "mg/L"
) {
  env_schema(
    tibble::tibble(
      location_id = "MW-1",
      sample_date = as.Date("2020-01-01"),
      param_name = analyte,
      analysis_result = result,
      default_unit = unit
    ),
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    sample_date = sample_date
  )
}

# ------------------------------------------------------------------------------
# Ion name standardization
# ------------------------------------------------------------------------------

test_that(".standardize_ion_name returns canonical analyte ids", {
  expect_equal(
    .standardize_ion_name(
      "Calcium, dissolved"
    ),
    "calcium"
  )

  expect_equal(
    .standardize_ion_name(
      "Calcium, total"
    ),
    "calcium"
  )

  expect_equal(
    .standardize_ion_name(
      "Calcium as CaCO3"
    ),
    "calcium"
  )
})

test_that(".standardize_ion_name is case insensitive", {
  expect_equal(
    .standardize_ion_name(
      "CALCIUM"
    ),
    "calcium"
  )

  expect_equal(
    .standardize_ion_name(
      "calcium, dissolved"
    ),
    "calcium"
  )

  expect_equal(
    .standardize_ion_name(
      "CALCIUM AS CACO3"
    ),
    "calcium"
  )
})

# ------------------------------------------------------------------------------
# major_ion_chemistry()
# ------------------------------------------------------------------------------

test_that("major_ion_chemistry returns env_schema", {
  schema <- build_major_ion_schema()

  result <- major_ion_chemistry(
    schema
  )

  expect_s3_class(
    result,
    "env_schema"
  )
})

test_that("major_ion_chemistry preserves observations", {
  schema <- build_major_ion_schema()

  result <- major_ion_chemistry(
    schema
  )

  expect_gt(
    nrow(result),
    0
  )
})

test_that("major_ion_chemistry creates meq field", {
  schema <- build_major_ion_schema()

  result <- major_ion_chemistry(
    schema
  )

  expect_true(
    analysis_result_meq_var() %in%
      names(result)
  )
})

test_that("major_ion_chemistry resolves analytes", {
  schema <- build_major_ion_schema()

  expect_no_error(
    major_ion_chemistry(
      schema,
      analyte_matching = "normalized"
    )
  )
})

test_that("major_ion_chemistry converts calcium to meq/L", {
  chem <- major_ion_chemistry(
    build_simple_major_ion_schema(
      "Calcium, dissolved",
      40
    )
  )

  expected <- 40 * 2 / 40.078

  expect_equal(
    chem[[analysis_result_meq_var()]],
    expected,
    tolerance = 1e-6
  )
})

test_that("multiple ions are converted to meq/L", {
  schema <- build_major_ion_schema(
    tibble::tibble(
      location_id = c(
        "MW-1",
        "MW-1"
      ),
      sample_date = as.POSIXct("2020-01-01"),
      param_name = c(
        "Calcium, dissolved",
        "Magnesium, dissolved"
      ),
      analysis_result = c(
        40,
        24
      ),
      default_unit = "mg/L"
    )
  )

  chem <- major_ion_chemistry(
    schema
  )

  expect_true(
    all(
      !is.na(
        chem[[analysis_result_meq_var()]]
      )
    )
  )
})

# ------------------------------------------------------------------------------
# major_ions()
# ------------------------------------------------------------------------------

test_that("major_ions returns expected columns", {
  schema <- build_major_ion_schema()

  result <- major_ions(
    schema
  )

  expect_true(
    all(
      c(
        "calcium",
        "magnesium",
        "sodium",
        "potassium",
        "sodium_potassium",
        "chloride",
        "sulfate",
        "alkalinity"
      ) %in%
        names(result)
    )
  )
})

test_that("major_ions returns one row per sample", {
  schema <- build_major_ion_schema()

  result <- major_ions(
    schema
  )

  chemistry_df <- .prepare_major_ion_chemistry(
    schema
  )

  expected <- chemistry_df |>
    dplyr::distinct(
      .data[[location_var(chemistry_df)]],
      .data[[sample_date_var(chemistry_df)]]
    ) |>
    nrow()

  expect_equal(
    nrow(result),
    expected
  )
})

test_that("major_ions returns missing component when analyte absent", {
  schema <- build_major_ion_schema(
    dplyr::filter(
      major_ion_data,
      param_name != "Calcium, dissolved"
    )
  )

  ions <- major_ions(
    schema
  )

  expect_true(
    "calcium" %in%
      names(ions)
  )
})

# ------------------------------------------------------------------------------
# charge_balance()
# ------------------------------------------------------------------------------

test_that("charge_balance returns expected fields", {
  schema <- build_major_ion_schema()

  result <- charge_balance(
    schema
  )

  expect_true(
    all(
      c(
        "total_cations",
        "total_anions",
        "charge_balance_pct"
      ) %in%
        names(result)
    )
  )
})

test_that("charge_balance returns one row per sample", {
  schema <- build_major_ion_schema()

  result <- charge_balance(
    schema
  )

  chemistry_df <- .prepare_major_ion_chemistry(
    schema
  )

  expected <- chemistry_df |>
    dplyr::distinct(
      .data[[location_var(chemistry_df)]],
      .data[[sample_date_var(chemistry_df)]]
    ) |>
    nrow()

  expect_equal(
    nrow(result),
    expected
  )
})

test_that("charge_balance percent is numeric", {
  schema <- build_major_ion_schema()

  result <- charge_balance(
    schema
  )

  expect_type(
    result$charge_balance_pct,
    "double"
  )
})

test_that("charge_balance handles missing ions", {
  schema <- build_major_ion_schema(
    dplyr::filter(
      major_ion_data,
      param_name != "Calcium, dissolved"
    )
  )

  expect_no_error(
    charge_balance(schema)
  )
})

# ------------------------------------------------------------------------------
# data.frame dispatch
# ------------------------------------------------------------------------------

test_that("major_ion_chemistry data.frame dispatch works", {
  result <- major_ion_chemistry(
    major_ion_data,
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    sample_date = sample_date
  )

  expect_s3_class(
    result,
    "env_schema"
  )
})

test_that("major_ions data.frame dispatch works", {
  result <- major_ions(
    major_ion_data,
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    sample_date = sample_date
  )

  expect_true(
    "calcium" %in% names(result)
  )
})

test_that("charge_balance data.frame dispatch works", {
  result <- charge_balance(
    major_ion_data,
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    sample_date = sample_date
  )

  expect_true(
    "charge_balance_pct" %in%
      names(result)
  )
})

# ------------------------------------------------------------------------------
# Piper / Durov / Stiff integrations
# ------------------------------------------------------------------------------

test_that("piper_data handles incomplete chemistry", {
  schema <- build_major_ion_schema(
    dplyr::filter(
      major_ion_data,
      param_name != "Chloride, total"
    )
  )

  expect_no_error(
    piper_data(schema)
  )
})

test_that("durov_data warns when chemistry is incomplete", {
  schema <- build_major_ion_schema(
    dplyr::filter(
      major_ion_data,
      param_name != "Sulfate, total"
    )
  )

  expect_warning(
    durov_data(schema)
  )
})

test_that("stiff_data handles incomplete chemistry", {
  schema <- build_major_ion_schema(
    dplyr::filter(
      major_ion_data,
      param_name != "Magnesium, dissolved"
    )
  )

  expect_no_error(
    stiff_data(schema)
  )
})
