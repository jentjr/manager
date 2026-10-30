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
  analyte_matching = c(
    "exact",
    "strict",
    "normalized"
  )
) {
  analyte_matching <- rlang::arg_match(
    analyte_matching
  )

  if (!inherits(df, "env_schema")) {
    rlang::abort(
      "df must inherit from 'env_schema'."
    )
  }

  validate_env_schema(df)

  location_col <- location_var(df)
  date_col <- date_var(df)
  analyte_id_col <- analyte_id_var(
    df
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

  display_analyte <- analysis_analyte(
    df,
    analyte_matching = analyte_matching
  )

  canonical_id <- analyte_id(df)

  out <- df |>
    dplyr::mutate(
      analyte = display_analyte,
      !!analyte_id_col := canonical_id
    ) |>
    dplyr::filter(
      analyte %in% resolved_analytes
    ) |>
    dplyr::group_by(
      .data[[location_col]],
      .data[[date_col]],
      analyte,
      .data[[analyte_id_col]]
    ) |>
    dplyr::summarise(
      .original_lab_result = mean(
        .original_lab_result,
        na.rm = TRUE
      ),

      .original_lab_unit = dplyr::first(
        .original_lab_unit
      ),

      .analysis_result = mean(
        .analysis_result,
        na.rm = TRUE
      ),

      .analysis_unit = dplyr::first(
        .analysis_unit
      ),

      .groups = "drop"
    )

  .restore_geochemical_schema(
    out,
    df
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
  analyte_matching = c(
    "exact",
    "strict",
    "normalized"
  )
) {
  analyte_matching <- rlang::arg_match(
    analyte_matching
  )

  out <- .prepare_geochemical_data(
    df = df,
    analytes = analytes,
    analyte_matching = analyte_matching
  ) |>
    convert_analytes_to_meq()

  .restore_geochemical_schema(
    out,
    df
  )
}

#' Resolve requested analytes
#'
#' @noRd
.resolve_geochemical_analytes <- function(
  analytes,
  analyte_matching = c(
    "exact",
    "strict",
    "normalized"
  )
) {
  .resolve_analyte_name(
    analytes,
    analyte_matching = analyte_matching
  ) |>
    unique()
}
