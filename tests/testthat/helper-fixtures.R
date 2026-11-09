# ------------------------------------------------------------------------------
# Test fixture
# ------------------------------------------------------------------------------

gw_piper <- tibble::tribble(
  ~location_id , ~sample_date          , ~param_name               , ~analysis_result , ~default_unit ,

  "MW-1"       , as.Date("2020-01-01") , "Calcium, dissolved"      ,               40 , "mg/L"        ,
  "MW-1"       , as.Date("2020-01-01") , "Magnesium, dissolved"    ,               12 , "mg/L"        ,
  "MW-1"       , as.Date("2020-01-01") , "Sodium, dissolved"       ,               25 , "mg/L"        ,
  "MW-1"       , as.Date("2020-01-01") , "Potassium, dissolved"    ,                3 , "mg/L"        ,
  "MW-1"       , as.Date("2020-01-01") , "Chloride, total"         ,               18 , "mg/L"        ,
  "MW-1"       , as.Date("2020-01-01") , "Sulfate, total"          ,               30 , "mg/L"        ,
  "MW-1"       , as.Date("2020-01-01") , "Alkalinity, total (lab)" ,              110 , "mg/L"        ,

  "MW-2"       , as.Date("2020-01-01") , "Calcium, dissolved"      ,               55 , "mg/L"        ,
  "MW-2"       , as.Date("2020-01-01") , "Magnesium, dissolved"    ,               14 , "mg/L"        ,
  "MW-2"       , as.Date("2020-01-01") , "Sodium, dissolved"       ,               20 , "mg/L"        ,
  "MW-2"       , as.Date("2020-01-01") , "Potassium, dissolved"    ,                2 , "mg/L"        ,
  "MW-2"       , as.Date("2020-01-01") , "Chloride, total"         ,               12 , "mg/L"        ,
  "MW-2"       , as.Date("2020-01-01") , "Sulfate, total"          ,               22 , "mg/L"        ,
  "MW-2"       , as.Date("2020-01-01") , "Alkalinity, total (lab)" ,              135 , "mg/L"
)


build_schema <- function(df = gw_piper) {
  env_schema(
    df,
    location = location_id,
    date = sample_date,
    analyte = param_name,
    result = analysis_result,
    unit = default_unit
  )
}
