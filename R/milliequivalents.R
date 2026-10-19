#' Convert standardized analytes to milliequivalents per liter
#'
#' Converts standardized analytical concentrations to
#' milliequivalents per liter (meq/L) using analyte-specific
#' conversion factors stored in [analyte_tbl].
#'
#' If standardized concentration columns are not present,
#' the function automatically standardizes concentrations using
#' [standardize_units()] prior to conversion.
#'
#' The function validates that analytes eligible for
#' milliequivalent conversion are reported in their expected
#' standard unit.
#'
#' @param df Data frame containing analytical results.
#'
#' @param analyte_col Character string identifying the canonical
#'   analyte column. Default is `"analyte"`.
#'
#' @param value_col Character string identifying the standardized
#'   concentration column. Default is `"analysis_result_std"`.
#'
#' @param unit_col Character string identifying the standardized
#'   reporting unit column. Default is `"default_unit_std"`.
#'
#' @param value_out_col Character string identifying the output meq
#'   concentration column. Default is
#'   `"analysis_result_meq"`.
#'
#' @param unit_out_col Character string identifying the output meq
#'   reporting unit column. Default is
#'   `"default_unit_meq"`.
#'
#' @return
#' A data frame containing all original columns plus:
#'
#' \describe{
#'   \item{analysis_result_meq}{
#'     Concentration expressed as meq/L.
#'   }
#'   \item{default_unit_meq}{
#'     Reporting unit, always `"meq/L"` for supported analytes.
#'   }
#' }
#'
#' @details
#' Conversion factors are obtained from
#' [`.get_meq_conversion_factors()`].
#'
#' The function validates that all convertible analytes are reported
#' in their expected standard unit before conversion.
#'
#' Only analytes with a defined `meq_factor` in [analyte_tbl] are converted.
#'
#' Unsupported analytes receive `NA_real_` in the output
#' concentration column.
#'
#' @section Workflow:
#'
#' The following workflows are equivalent:
#'
#' \preformatted{
#' df |>
#'   standardize_units() |>
#'   convert_analytes_to_meq()
#'
#' df |>
#'   convert_analytes_to_meq()
#'
#' }
#'
#' @section Validation:
#'
#' The function validates:
#'
#' \itemize{
#'   \item required columns exist;
#'   \item analytes eligible for meq conversion are reported in the
#'   expected standard unit;
#'   \item analyte metadata are internally consistent.
#' }
#'
#' @section Notes:
#'
#' Milliequivalent concentrations are commonly used in
#' hydrogeochemical calculations and visualizations,
#' including:
#'
#' \itemize{
#'   \item Piper diagrams;
#'   \item Durov diagrams;
#'   \item Stiff diagrams;
#'   \item Schoeller diagrams;
#'   \item charge-balance calculations; and
#'   \item sodium adsorption ratio (SAR) calculations.
#' }
#'
#' @seealso
#' [standardize_units()],
#' [piper_plot()],
#' [durov_plot()],
#' [stiff_plot()],
#' [schoeller_plot()],
#' [charge_balance()]
#'
#' @examples
#' gw_data |>
#'   standardize_units() |>
#'   convert_analytes_to_meq()
#'
#' gw_data |>
#'   convert_analytes_to_meq(
#'     analyte_col = "analyte",
#'     value_col = "result_std",
#'     unit_col = "unit_std"
#'   )
#'
#' @export
convert_analytes_to_meq <- function(
  df,
  analyte_col = "analyte",
  value_col = "analysis_result_std",
  unit_col = "default_unit_std",
  value_out_col = "analysis_result_meq",
  unit_out_col = "default_unit_meq"
) {
  df <- .prep_concentrations(
    df = df,
    value_col = sub(
      "_std$",
      "",
      value_col
    )
  )

  .validate_required_cols(
    df,
    c(
      analyte_col,
      value_col,
      unit_col
    )
  )

  .validate_analyte_meq_units(
    df,
    analyte_col = analyte_col,
    unit_col = unit_col
  )

  meq_lookup <- .get_meq_conversion_factors() %>%
    dplyr::select(
      analyte,
      meq_factor
    )

  df %>%
    dplyr::left_join(
      meq_lookup,
      by = stats::setNames(
        "analyte",
        analyte_col
      ),
      relationship = "many-to-one"
    ) %>%
    dplyr::mutate(
      !!value_out_col := dplyr::if_else(
        !is.na(meq_factor) &
          !is.na(.data[[value_col]]),
        .convert_to_meq(
          value = .data[[value_col]],
          meq_factor = meq_factor
        ),
        NA_real_
      ),
      !!unit_out_col := dplyr::if_else(
        !is.na(.data[[value_out_col]]),
        "meq/L",
        NA_character_
      )
    ) %>%
    dplyr::select(
      -meq_factor
    )
}

#' Convert concentrations to milliequivalents per liter
#'
#' Performs analyte-specific conversion from a standardized
#' concentration to milliequivalents per liter (meq/L).
#'
#' @param value Numeric concentration vector.
#'
#' @param meq_factor Numeric vector of analyte-specific meq conversion
#'   factors.
#'
#' @return
#' Numeric vector expressed as meq/L.
#'
#' @details
#' Conversion is performed using:
#'
#' \deqn{
#' concentration_{meq/L} =
#' concentration_{standard} \times meq\_factor
#' }
#'
#' Conversion factors are derived from [analyte_tbl].
#'
#' @noRd
.convert_to_meq <- function(
  value,
  meq_factor
) {
  value * meq_factor
}

#' Extract meq conversion factors from analyte metadata
#'
#' Returns the canonical analyte-to-meq conversion lookup table
#' derived from [analyte_tbl].
#'
#' The lookup table is validated using
#' [`.validate_analyte_tbl()`] before extraction.
#'
#' @param lookup An analyte metadata table. Defaults to
#'   [analyte_tbl].
#'
#' @return
#' A tibble containing:
#'
#' \describe{
#'   \item{analyte}{Canonical analyte name.}
#'   \item{std_unit}{Expected standardized reporting unit.}
#'   \item{meq_factor}{Factor used to convert the standardized
#'   concentration to meq/L.}
#' }
#'
#' @details
#' The returned lookup table serves as the authoritative source for
#' meq conversion throughout the package.
#'
#' Standard units are normalized using
#' [`.normalize_unit()`].
#'
#' @noRd
.get_meq_conversion_factors <- function(
  lookup = analyte_tbl
) {
  .validate_analyte_tbl(
    lookup
  )

  lookup %>%
    dplyr::select(
      analyte,
      std_unit,
      meq_factor
    ) %>%
    dplyr::mutate(
      std_unit = .normalize_unit(
        std_unit
      )
    )
}
