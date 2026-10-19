test_that(".get_schoeller_ions() returns standardized output", {
  df <- tibble::tibble(
    location_id = "MW-1",
    sample_date = as.Date("2024-01-01"),
    param_name = c(
      "Calcium, dissolved",
      "Magnesium, dissolved",
      "Chloride, total"
    ),
    analysis_result = c(40, 12, 20),
    default_unit = "mg/L"
  )

  result <- .get_schoeller_ions(df)

  expect_named(
    result,
    c(
      "location_id",
      "sample_date",
      "default_unit",
      "param_name",
      "value"
    )
  )

  expect_equal(nrow(result), 3)
})

test_that(".transform_schoeller_data() returns expected Schoeller ions", {
  df <- tibble::tribble(
    ~location_id , ~sample_date          , ~param_name               , ~analysis_result , ~default_unit   ,
    "MW-1"       , as.Date("2024-01-01") , "Calcium, dissolved"      ,               40 , "mg/L"          ,
    "MW-1"       , as.Date("2024-01-01") , "Magnesium, dissolved"    ,               12 , "mg/L"          ,
    "MW-1"       , as.Date("2024-01-01") , "Sodium, dissolved"       ,               20 , "mg/L"          ,
    "MW-1"       , as.Date("2024-01-01") , "Potassium, dissolved"    ,                2 , "mg/L"          ,
    "MW-1"       , as.Date("2024-01-01") , "Chloride, total"         ,               15 , "mg/L"          ,
    "MW-1"       , as.Date("2024-01-01") , "Sulfate, total"          ,               30 , "mg/L"          ,
    "MW-1"       , as.Date("2024-01-01") , "Alkalinity, total (lab)" ,              100 , "mg/L as CaCO3"
  )

  result <- .transform_schoeller_data(df)

  expect_setequal(
    levels(result$param_name),
    c(
      "Ca",
      "Mg",
      "Na + K",
      "Cl",
      "SO4",
      "Alk"
    )
  )

  expect_equal(nrow(result), 6)
})

test_that("sodium and potassium are combined into Na + K", {
  df <- tibble::tribble(
    ~location_id , ~sample_date          , ~param_name            , ~analysis_result , ~default_unit ,
    "MW-1"       , as.Date("2024-01-01") , "Sodium, dissolved"    ,               20 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Potassium, dissolved" ,                5 , "mg/L"
  )

  result <- .transform_schoeller_data(df)

  expect_true(
    "Na + K" %in% unique(as.character(result$param_name))
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

  expect_no_error(
    .transform_schoeller_data(df)
  )
})

test_that("Na + K becomes NA when both sodium and potassium are missing", {
  df <- tibble::tribble(
    ~location_id , ~sample_date          , ~param_name            , ~analysis_result , ~default_unit ,
    "MW-1"       , as.Date("2024-01-01") , "Calcium, dissolved"   ,               40 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Magnesium, dissolved" ,               10 , "mg/L"        ,
    "MW-1"       , as.Date("2024-01-01") , "Chloride, total"      ,               20 , "mg/L"
  )

  result <- .transform_schoeller_data(df)

  expect_false(
    "Na + K" %in%
      (result %>%
        dplyr::filter(!is.na(analysis_result)) %>%
        dplyr::pull(param_name) %>%
        as.character())
  )
})

test_that("unsupported units throw an error", {
  df <- tibble::tribble(
    ~location_id , ~sample_date          , ~param_name          , ~analysis_result , ~default_unit ,
    "MW-1"       , as.Date("2024-01-01") , "Calcium, dissolved" ,               40 , "ug/L"
  )

  expect_error(
    .transform_schoeller_data(df),
    "Unsupported analyte/unit"
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

  expect_no_error(
    result <- .transform_schoeller_data(df)
  )

  expect_equal(nrow(result), 0)
})

test_that("schoeller_plot returns a ggplot object", {
  df <- tibble::tribble(
    ~location_id , ~sample_date          , ~param_name               , ~analysis_result , ~default_unit   ,
    "MW-1"       , as.Date("2024-01-01") , "Calcium, dissolved"      ,               40 , "mg/L"          ,
    "MW-1"       , as.Date("2024-01-01") , "Magnesium, dissolved"    ,               10 , "mg/L"          ,
    "MW-1"       , as.Date("2024-01-01") , "Sodium, dissolved"       ,               15 , "mg/L"          ,
    "MW-1"       , as.Date("2024-01-01") , "Potassium, dissolved"    ,                2 , "mg/L"          ,
    "MW-1"       , as.Date("2024-01-01") , "Chloride, total"         ,               20 , "mg/L"          ,
    "MW-1"       , as.Date("2024-01-01") , "Sulfate, total"          ,               30 , "mg/L"          ,
    "MW-1"       , as.Date("2024-01-01") , "Alkalinity, total (lab)" ,              100 , "mg/L as CaCO3"
  )

  p <- schoeller_plot(df)

  expect_s3_class(
    p,
    "ggplot"
  )
})

test_that("both faceting options work", {
  df <- tibble::tribble(
    ~location_id , ~sample_date          , ~param_name               , ~analysis_result , ~default_unit   ,
    "MW-1"       , as.Date("2024-01-01") , "Calcium, dissolved"      ,               40 , "mg/L"          ,
    "MW-1"       , as.Date("2024-01-01") , "Magnesium, dissolved"    ,               10 , "mg/L"          ,
    "MW-1"       , as.Date("2024-01-01") , "Sodium, dissolved"       ,               15 , "mg/L"          ,
    "MW-1"       , as.Date("2024-01-01") , "Potassium, dissolved"    ,                2 , "mg/L"          ,
    "MW-1"       , as.Date("2024-01-01") , "Chloride, total"         ,               20 , "mg/L"          ,
    "MW-1"       , as.Date("2024-01-01") , "Sulfate, total"          ,               30 , "mg/L"          ,
    "MW-1"       , as.Date("2024-01-01") , "Alkalinity, total (lab)" ,              100 , "mg/L as CaCO3"
  )

  expect_no_error(
    schoeller_plot(
      df,
      facet_var = "location_id"
    )
  )

  expect_no_error(
    schoeller_plot(
      df,
      facet_var = "sample_date"
    )
  )
})
