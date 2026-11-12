# test-schoeller_plot.R

make_chemistry_df <- function(df) {
  schema <- env_schema(
    df,
    location = location_id,
    sample_date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit
  )

  .prepare_major_ion_chemistry(schema)
}

test_that(".transform_schoeller_data() returns expected Schoeller ions", {
  df <- tibble::tribble(
    ~location_id , ~sample_date          , ~param_name               , ~analysis_result , ~default_unit ,
    "MW-1"       , as.Date("2024-01-01") , "Calcium, dissolved"      ,               40 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Magnesium, dissolved"    ,               12 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Sodium, dissolved"       ,               20 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Potassium, dissolved"    ,                2 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Chloride, total"         ,               15 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Sulfate, total"          ,               30 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Alkalinity, total (lab)" ,              100 , "mg/L"
  )

  chemistry_df <- make_chemistry_df(df)

  result <- .transform_schoeller_data(
    chemistry_df
  )

  expect_setequal(
    levels(result$schoeller_axis),
    c(
      "Ca",
      "Mg",
      "Na + K",
      "Cl",
      "SO4",
      "Alk"
    )
  )

  expect_equal(
    nrow(result),
    6
  )
})

test_that("sodium and potassium are combined into Na + K", {
  df <- tibble::tribble(
    ~location_id , ~sample_date          , ~param_name            , ~analysis_result , ~default_unit ,
    "MW-1"       , as.Date("2024-01-01") , "Sodium, dissolved"    ,               20 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Potassium, dissolved" ,                5 , "mg/L"
  )

  chemistry_df <- make_chemistry_df(df)

  result <- .transform_schoeller_data(
    chemistry_df
  )

  expect_true(
    "Na + K" %in%
      unique(
        as.character(
          result$schoeller_axis
        )
      )
  )
})

test_that("missing potassium is handled gracefully", {
  df <- tibble::tribble(
    ~location_id , ~sample_date          , ~param_name            , ~analysis_result , ~default_unit ,
    "MW-1"       , as.Date("2024-01-01") , "Sodium, dissolved"    ,               20 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Calcium, dissolved"   ,               40 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Magnesium, dissolved" ,               10 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Chloride, total"      ,               20 , "mg/L"
  )

  chemistry_df <- make_chemistry_df(df)

  expect_no_error(
    .transform_schoeller_data(
      chemistry_df
    )
  )
})

test_that("Na + K becomes absent when both sodium and potassium are missing", {
  df <- tibble::tribble(
    ~location_id , ~sample_date          , ~param_name            , ~analysis_result , ~default_unit ,
    "MW-1"       , as.Date("2024-01-01") , "Calcium, dissolved"   ,               40 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Magnesium, dissolved" ,               10 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Chloride, total"      ,               20 , "mg/L"
  )

  chemistry_df <- make_chemistry_df(df)

  result <- .transform_schoeller_data(
    chemistry_df
  )

  expect_false(
    "Na + K" %in%
      as.character(
        stats::na.omit(
          result$schoeller_axis
        )
      )
  )
})

test_that("empty dataframe returns empty result", {
  df <- tibble::tibble(
    location_id = character(),
    sample_date = as.Date(character()),
    param_name = character(),
    analysis_result = numeric(),
    default_unit = character()
  )

  schema <- env_schema(
    df,
    location = location_id,
    sample_date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit
  )

  chemistry_df <- .prepare_major_ion_chemistry(
    schema
  )

  expect_no_error(
    result <- .transform_schoeller_data(
      chemistry_df
    )
  )

  expect_equal(
    nrow(result),
    0
  )
})

test_that("schoeller_plot returns a ggplot object", {
  df <- tibble::tribble(
    ~location_id , ~sample_date          , ~param_name               , ~analysis_result , ~default_unit ,
    "MW-1"       , as.Date("2024-01-01") , "Calcium, dissolved"      ,               40 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Magnesium, dissolved"    ,               10 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Sodium, dissolved"       ,               15 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Potassium, dissolved"    ,                2 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Chloride, total"         ,               20 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Sulfate, total"          ,               30 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Alkalinity, total (lab)" ,              100 , "mg/L"
  )

  p <- schoeller_plot(
    df,
    location = location_id,
    sample_date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit
  )

  expect_s3_class(
    p,
    "ggplot"
  )
})

test_that("schoeller_plot env_schema dispatch works", {
  df <- tibble::tribble(
    ~location_id , ~sample_date          , ~param_name               , ~analysis_result , ~default_unit ,
    "MW-1"       , as.Date("2024-01-01") , "Calcium, dissolved"      ,               40 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Magnesium, dissolved"    ,               10 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Sodium, dissolved"       ,               15 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Potassium, dissolved"    ,                2 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Chloride, total"         ,               20 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Sulfate, total"          ,               30 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Alkalinity, total (lab)" ,              100 , "mg/L"
  )

  schema <- env_schema(
    df,
    location = location_id,
    sample_date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit
  )

  p <- schoeller_plot(
    schema
  )

  expect_s3_class(
    p,
    "ggplot"
  )
})

test_that("both faceting options work", {
  df <- tibble::tribble(
    ~location_id , ~sample_date          , ~param_name               , ~analysis_result , ~default_unit ,
    "MW-1"       , as.Date("2024-01-01") , "Calcium, dissolved"      ,               40 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Magnesium, dissolved"    ,               10 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Sodium, dissolved"       ,               15 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Potassium, dissolved"    ,                2 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Chloride, total"         ,               20 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Sulfate, total"          ,               30 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Alkalinity, total (lab)" ,              100 , "mg/L"
  )

  schema <- env_schema(
    df,
    location = location_id,
    sample_date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit
  )

  expect_no_error(
    schoeller_plot(
      schema,
      facet_var = "location_id"
    )
  )

  expect_no_error(
    schoeller_plot(
      schema,
      facet_var = "sample_date"
    )
  )
})
