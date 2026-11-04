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

test_that("major_ion_chemistry returns env_schema", {
  schema <- env_schema(
    gw_data,
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    date = sample_date
  )

  result <- major_ion_chemistry(
    schema
  )

  expect_s3_class(
    result,
    "env_schema"
  )
})

test_that("major_ion_chemistry preserves observations", {
  schema <- env_schema(
    gw_data,
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    date = sample_date
  )

  result <- major_ion_chemistry(
    schema
  )

  expect_gt(
    nrow(result),
    0
  )
})

test_that("major_ion_chemistry creates meq field", {
  schema <- env_schema(
    gw_data,
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    date = sample_date
  )

  result <- major_ion_chemistry(
    schema
  )

  expect_true(
    analysis_result_meq_var(
      result
    ) %in%
      names(result)
  )
})

test_that("major_ion_chemistry resolves analytes", {
  schema <- env_schema(
    gw_data,
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    date = sample_date
  )

  expect_no_error(
    major_ion_chemistry(
      schema,
      analyte_matching = "normalized"
    )
  )
})

test_that("major_ions returns expected columns", {
  schema <- env_schema(
    gw_data,
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    date = sample_date
  )

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
  schema <- env_schema(
    gw_data,
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    date = sample_date
  )

  result <- major_ions(
    schema
  )

  chemistry_df <- .prepare_major_ion_chemistry(schema)

  expected <- chemistry_df |>
    dplyr::distinct(
      .data[[location_var(chemistry_df)]],
      .data[[date_var(chemistry_df)]]
    ) |>
    nrow()

  expect_equal(
    nrow(result),
    expected
  )
})

test_that("charge_balance returns expected fields", {
  schema <- env_schema(
    gw_data,
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    date = sample_date
  )

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
  schema <- env_schema(
    gw_data,
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    date = sample_date
  )

  result <- charge_balance(
    schema
  )

  chemistry_df <- .prepare_major_ion_chemistry(schema)

  expected <- chemistry_df |>
    dplyr::distinct(
      .data[[location_var(chemistry_df)]],
      .data[[date_var(chemistry_df)]]
    ) |>
    nrow()

  expect_equal(
    nrow(result),
    expected
  )
})

test_that("charge_balance percent is numeric", {
  schema <- env_schema(
    gw_data,
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    date = sample_date
  )

  result <- charge_balance(
    schema
  )

  expect_type(
    result$charge_balance_pct,
    "double"
  )
})

test_that("major_ion_chemistry data.frame dispatch works", {
  result <- major_ion_chemistry(
    gw_data,
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    date = sample_date
  )

  expect_s3_class(
    result,
    "env_schema"
  )
})

test_that("major_ions data.frame dispatch works", {
  result <- major_ions(
    gw_data,
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    date = sample_date
  )

  expect_true(
    "calcium" %in% names(result)
  )
})

test_that("charge_balance data.frame dispatch works", {
  result <- charge_balance(
    gw_data,
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    date = sample_date
  )

  expect_true(
    "charge_balance_pct" %in%
      names(result)
  )
})

test_that("major_ion_chemistry converts calcium to meq/L", {
  df <- tibble::tibble(
    location_id = "MW-1",
    sample_date = as.POSIXct("2020-01-01"),
    param_name = "Calcium, dissolved",
    analysis_result = 40,
    default_unit = "mg/L"
  )

  schema <- env_schema(
    df,
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    date = sample_date
  )

  chem <- major_ion_chemistry(schema)

  meq_col <- analysis_result_meq_var(chem)

  expected <- 40 * 2 / 40.078

  expect_equal(
    chem[[meq_col]],
    expected,
    tolerance = 1e-6
  )
})

test_that("multiple ions are converted to meq/L", {
  df <- tibble::tibble(
    location_id = c("MW-1", "MW-1"),
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

  schema <- env_schema(
    df,
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    date = sample_date
  )

  chem <- major_ion_chemistry(schema)

  meq_col <- analysis_result_meq_var(chem)

  expect_true(
    all(
      !is.na(chem[[meq_col]])
    )
  )
})

test_that("charge_balance handles missing ions", {
  df <- dplyr::filter(
    gw_data,
    param_name != "Calcium, dissolved"
  )

  schema <- env_schema(
    df,
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    date = sample_date
  )

  expect_no_error(
    charge_balance(schema)
  )
})

test_that("major_ions returns missing component when analyte absent", {
  df <- dplyr::filter(
    gw_data,
    param_name != "Calcium, dissolved"
  )

  schema <- env_schema(
    df,
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    date = sample_date
  )

  ions <- major_ions(schema)

  expect_true(
    "calcium" %in%
      names(ions)
  )
})

test_that("piper_data handles incomplete chemistry", {
  df <- dplyr::filter(
    gw_data,
    param_name != "Chloride, total"
  )

  schema <- env_schema(
    df,
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    date = sample_date
  )

  expect_no_error(
    piper_data(schema)
  )
})

test_that("durov_data warns when chemistry is incomplete", {
  df <- dplyr::filter(
    gw_data,
    param_name != "Sulfate, total"
  )

  schema <- env_schema(
    df,
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    date = sample_date
  )

  expect_warning(
    durov_data(schema)
  )
})

test_that("stiff_data handles incomplete chemistry", {
  df <- dplyr::filter(
    gw_data,
    param_name != "Magnesium, dissolved"
  )

  schema <- env_schema(
    df,
    location = location_id,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit,
    date = sample_date
  )

  expect_no_error(
    stiff_data(schema)
  )
})
