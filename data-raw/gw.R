gw <- env_schema(
  gw_data,
  location = location_id,
  date = sample_date,
  analyte = param_name,
  result = analysis_result,
  unit = default_unit,
  reporting_flag = lt_measure,
  analyte_lookup = analyte_tbl
)
