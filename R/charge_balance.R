#' Charge balance calculation
#'
#' @export
charge_balance <- function(
  df,
  location_id = "location_id",
  sample_date = "sample_date",
  param_name = "param_name",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  analyte_matching = c(
    "normalized",
    "strict",
    "exact"
  )
) {
  analyte_matching <- rlang::arg_match(
    analyte_matching
  )

  chemistry_df <- .prepare_geochemical_data(
    df = df,
    analytes = c(
      "Calcium, dissolved",
      "Magnesium, dissolved",
      "Sodium, dissolved",
      "Potassium, dissolved",
      "Chloride, total",
      "Sulfate, total",
      "Alkalinity, total (lab)"
    ),
    location_id = location_id,
    sample_date = sample_date,
    param_name = param_name,
    analysis_result = analysis_result,
    default_unit = default_unit,
    analyte_matching = analyte_matching
  )

  .calculate_charge_balance(
    chemistry_df
  )
}
