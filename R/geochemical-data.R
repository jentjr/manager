#' Prepare geochemical data
#'
#' Resolve analytes, validate units, standardize analytical results,
#' average duplicate analytes, and return a standardized long-format
#' chemistry data set.
#'
#' @noRd
.prepare_geochemical_data <- function(
  df,
  analytes,
  additional_analytes = NULL,
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

  analytes <- unique(
    c(
      analytes,
      additional_analytes
    )
  )

  resolved_analytes <- .resolve_geochemical_analytes(
    analytes,
    analyte_matching
  )

  df %>%
    standardize_units(
      analyte_col = param_name,
      value_col = analysis_result,
      unit_col = default_unit
    ) %>%
    .add_canonical_analyte(
      analyte_col = param_name,
      analyte_matching = analyte_matching
    ) %>%
    dplyr::filter(
      analyte %in% resolved_analytes
    ) %>%
    dplyr::group_by(
      .data[[location_id]],
      .data[[sample_date]],
      analyte
    ) %>%
    dplyr::summarise(
      analysis_result_std = mean(
        analysis_result_std,
        na.rm = TRUE
      ),
      default_unit_std = dplyr::first(
        default_unit_std
      ),
      .groups = "drop"
    ) %>%
    dplyr::transmute(
      location_id = .data[[location_id]],
      sample_date = .data[[sample_date]],
      analyte,
      analysis_result_std,
      default_unit_std
    )
}

#' Prepare major ion chemistry
#'
#' @noRd
.prepare_major_ion_chemistry <- function(
  df,
  analytes = c(
    "Calcium, dissolved",
    "Magnesium, dissolved",
    "Sodium, dissolved",
    "Potassium, dissolved",
    "Chloride, total",
    "Sulfate, total",
    "Alkalinity, total (lab)"
  ),
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

  .prepare_geochemical_data(
    df = df,
    analytes = analytes,
    location_id = location_id,
    sample_date = sample_date,
    param_name = param_name,
    analysis_result = analysis_result,
    default_unit = default_unit,
    analyte_matching = analyte_matching
  ) %>%
    convert_analytes_to_meq()
}

#' Resolve requested analytes
#'
#' @noRd
.resolve_geochemical_analytes <- function(
  analytes,
  analyte_matching = c(
    "normalized",
    "strict",
    "exact"
  )
) {
  analyte_matching <- rlang::arg_match(
    analyte_matching
  )

  .resolve_analyte_name(
    analytes,
    analyte_matching = analyte_matching
  ) %>%
    stats::na.omit() %>%
    as.character() %>%
    unique()
}
