#' Normalize analytical units
#'
#' Normalizes analytical units to canonical UDUNITS-2-compatible strings.
#'
#' @param x Character vector of analytical units.
#'
#' @return A normalized character vector of units.
#'
#' @noRd
.normalize_unit <- function(x) {
  x <- as.character(x)

  # Normalize whitespace and Unicode micro symbols.
  x <- x |>
    stringr::str_trim() |>
    stringr::str_replace_all("[\u00B5\u03BC]", "u") |>
    stringr::str_replace_all("\\s*/\\s*", "/") |>
    stringr::str_replace_all("\\s+", " ") |>
    stringr::str_to_lower()

  # Temperature.
  x <- stringr::str_replace_all(
    x,
    stringr::regex(
      "^(?:\u00B0\\s*c|deg\\s*c|degrees?\\s+celsius|celsius)$",
      ignore_case = TRUE
    ),
    "degC"
  )

  # Millisiemens per centimeter.
  x <- stringr::str_replace_all(
    x,
    stringr::regex(
      "^(?:ms/cm|millisiemens/cm|millisiemens per centimeter|ms cm-1)$",
      ignore_case = TRUE
    ),
    "mS/cm"
  )

  # Microsiemens or micromhos per centimeter.
  # 1 micromho = 1 microsiemens.
  x <- stringr::str_replace_all(
    x,
    stringr::regex(
      paste0(
        "^(?:",
        "us/cm|",
        "umho/cm|umhos/cm|",
        "microsiemens/cm|",
        "microsiemens per centimeter|",
        "micromho/cm|micromhos/cm|",
        "uS cm-1|umho cm-1",
        ")$"
      ),
      ignore_case = TRUE
    ),
    "uS/cm"
  )

  # Conductance-only units.
  x <- stringr::str_replace_all(
    x,
    stringr::regex(
      "^(?:us|umho|umhos|microsiemens|micromho|micromhos)$",
      ignore_case = TRUE
    ),
    "uS"
  )

  x
}

#' Standardize env_schema analytical results
#'
#' Converts .analysis_result values to analyte-specific
#' standard reporting units and updates .analysis_unit.
#'
#' Original laboratory values are preserved in
#' .original_lab_result and .original_lab_unit.
#'
#' @param df Data frame containing schema analyte metadata.
#'
#' @returns Data frame.
#'
#' @keywords internal
.standardize_schema_results <- function(df) {
  if (!".std_unit" %in% names(df)) {
    return(df)
  }

  required_cols <- c(
    ".analysis_result",
    ".analysis_unit",
    ".std_unit"
  )

  .validate_required_cols(
    df,
    required_cols
  )

  df$.analysis_unit <- .normalize_unit(
    df$.analysis_unit
  )

  df$.std_unit <- .normalize_unit(
    df$.std_unit
  )

  conversion_groups <- df |>
    dplyr::filter(
      !is.na(.std_unit)
    ) |>
    dplyr::distinct(
      .analysis_unit,
      .std_unit
    )

  for (i in seq_len(nrow(conversion_groups))) {
    from_unit <- conversion_groups$.analysis_unit[i]
    to_unit <- conversion_groups$.std_unit[i]

    if (identical(from_unit, to_unit)) {
      next
    }

    if (
      is.na(from_unit) ||
        is.na(to_unit)
    ) {
      next
    }

    idx <-
      !is.na(df$.analysis_unit) &
      !is.na(df$.std_unit) &
      df$.analysis_unit == from_unit &
      df$.std_unit == to_unit

    if (!any(idx)) {
      next
    }

    df$.analysis_result[idx] <-
      .safe_convert_units(
        value = df$.analysis_result[idx],
        from_unit = from_unit,
        to_unit = to_unit
      )
  }

  df$.analysis_unit <- dplyr::coalesce(
    df$.std_unit,
    df$.analysis_unit
  )

  df
}

#' Convert standards to reporting units
#'
#' Converts regulatory or screening standard values into the same
#' reporting units used by analytical results.
#'
#' @param df Data frame containing standard values and units.
#'
#' @param data_unit_col Character string identifying the reporting
#'   unit column associated with analytical results. Default is
#'   `"default_unit"`.
#'
#' @return
#' A data frame with standard values converted to match the
#' reporting units contained in `data_unit_col`.
#'
#' @details
#' Standard values are converted from `standard_unit` to the unit
#' specified in `data_unit_col`.
#'
#' Successful conversions update both:
#'
#' \describe{
#'   \item{standard_value}{
#'     Standard concentration converted to the reporting unit.
#'   }
#'   \item{standard_unit}{
#'     Updated to match the reporting unit.
#'   }
#' }
#'
#' Invalid unit definitions are identified using
#' [`.is_valid_unit()`] and reported through warnings.
#'
#' Unit conversion is performed row-wise because individual rows
#' may require different source and destination unit pairs.
#'
#' Conversion failures do not stop execution and instead return
#' `NA_real_` for affected standards.
#'
#' @section Validation:
#'
#' The function validates:
#'
#' \itemize{
#'   \item required columns exist;
#'   \item source and destination units are valid;
#'   \item standard values are non-missing before conversion.
#' }
#'
#' @noRd
.convert_standard_units <- function(
  df,
  data_unit_col = ".analysis_unit"
) {
  .validate_required_cols(
    df,
    c(
      ".standard_value",
      ".standard_unit",
      data_unit_col
    )
  )

  from_unit <- .normalize_unit(
    df$.standard_unit
  )

  to_unit <- .normalize_unit(
    df[[data_unit_col]]
  )

  valid_from <- .is_valid_unit(from_unit)

  valid_to <- .is_valid_unit(to_unit)

  invalid_idx <-
    (!valid_from | !valid_to) &
    !is.na(from_unit) &
    !is.na(to_unit)

  if (any(invalid_idx)) {
    invalid_pairs <- unique(
      paste(
        from_unit[invalid_idx],
        "->",
        to_unit[invalid_idx]
      )
    )

    cli::cli_warn(
      c(
        "Invalid unit conversions detected.",
        "i" = paste(
          invalid_pairs,
          collapse = ", "
        )
      )
    )
  }

  convert_idx <-
    !is.na(df$.standard_value) &
    valid_from &
    valid_to &
    from_unit != to_unit

  if (!any(convert_idx)) {
    return(df)
  }

  converted_values <- purrr::pmap_dbl(
    list(
      value = df$.standard_value[convert_idx],
      from_unit = from_unit[convert_idx],
      to_unit = to_unit[convert_idx]
    ),
    .safe_convert_unit
  )

  convert_rows <- which(convert_idx)

  successful_rows <- convert_rows[
    !is.na(converted_values)
  ]

  if (length(successful_rows) == 0) {
    return(df)
  }

  df$.standard_value[
    successful_rows
  ] <- converted_values[
    !is.na(converted_values)
  ]

  df$.standard_unit[
    successful_rows
  ] <- to_unit[
    successful_rows
  ]

  df
}

#' Convert analytical values between units
#'
#' Converts numeric values from one unit to another using the
#' \pkg{units} package.
#'
#' @param value Numeric vector of values.
#'
#' @param from_unit Character string specifying the source unit.
#'
#' @param to_unit Character string specifying the destination unit.
#'
#' @return
#' Numeric vector converted to the requested units.
#'
#' @details
#' Unit conversion is performed using
#' [units::set_units()] and [units::drop_units()].
#'
#' @noRd
.convert_units <- function(
  value,
  from_unit,
  to_unit
) {
  units::drop_units(
    units::set_units(
      units::set_units(
        value,
        from_unit,
        mode = "standard"
      ),
      to_unit,
      mode = "standard"
    )
  )
}

#' Safely convert a single value between units
#'
#' Performs a unit conversion while suppressing conversion errors.
#'
#' @param value Numeric value to convert.
#'
#' @param from_unit Character string specifying the source unit.
#'
#' @param to_unit Character string specifying the destination unit.
#'
#' @return
#' Converted numeric value.
#'
#' Returns `NA_real_` if conversion cannot be performed.
#'
#' @details
#' This helper wraps [`.convert_units()`] in a
#' [base::tryCatch()] block and converts conversion failures into
#' warnings.
#'
#' Typical causes of conversion failure include:
#'
#' \itemize{
#'   \item incompatible dimensions (for example `"mg/L"` to
#'   `"feet"`);
#'   \item invalid unit strings;
#'   \item unsupported unit definitions.
#' }
#'
#' This helper is intended for row-wise conversions where source
#' and destination units may vary between observations.
#'
#' @noRd
.safe_convert_unit <- function(
  value,
  from_unit,
  to_unit
) {
  if (
    is.na(from_unit) ||
      is.na(to_unit)
  ) {
    return(NA_real_)
  }

  tryCatch(
    .convert_units(
      value = value,
      from_unit = from_unit,
      to_unit = to_unit
    ),
    error = function(e) {
      cli::cli_warn(
        paste0(
          "Skipped incompatible unit conversion: ",
          from_unit,
          " -> ",
          to_unit
        )
      )

      NA_real_
    }
  )
}

#' Safely convert a vector of values between units
#'
#' Performs vectorized unit conversion while suppressing
#' conversion errors.
#'
#' @param value Numeric vector to convert.
#'
#' @param from_unit Character string specifying the source unit.
#'
#' @param to_unit Character string specifying the destination unit.
#'
#' @return
#' A numeric vector containing converted values.
#'
#' If conversion fails, a vector of `NA_real_` values having the
#' same length as `value` is returned.
#'
#' @details
#' This helper wraps [`.convert_units()`] in a
#' [base::tryCatch()] block and converts conversion failures into
#' warnings.
#'
#' Unlike [`.safe_convert_unit()`], this function is intended for
#' grouped conversions where all values share a common source and
#' destination unit.
#'
#' It is used by [`standardize_units()`] to efficiently convert
#' analytical results by unit-pair group rather than converting
#' observations one at a time.
#'
#' @noRd
.safe_convert_units <- function(
  value,
  from_unit,
  to_unit
) {
  if (
    is.na(from_unit) ||
      is.na(to_unit)
  ) {
    return(
      rep(
        NA_real_,
        length(value)
      )
    )
  }

  tryCatch(
    .convert_units(
      value = value,
      from_unit = from_unit,
      to_unit = to_unit
    ),
    error = function(e) {
      cli::cli_warn(
        paste0(
          "Skipped incompatible unit conversion: ",
          from_unit,
          " -> ",
          to_unit
        )
      )

      rep(
        NA_real_,
        length(value)
      )
    }
  )
}

#' Convert standardized analytes to milliequivalents per liter
#'
#' Converts standardized analytical concentrations to
#' milliequivalents per liter (meq/L) using analyte-specific
#' conversion factors stored in [analyte_tbl].
#'
#' For env_schema objects, analytical concentrations are
#' standardized during schema construction. This function
#' converts those standardized concentrations to meq/L.
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
#' @param value_col Character string identifying the analytical
#'   concentration column. Default is `".analysis_result"`.
#'
#' @param unit_col Character string identifying the reporting
#'   unit column. Default is `".analysis_unit"`.
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
#' [piper_plot()],
#' [durov_plot()],
#' [stiff_plot()],
#' [schoeller_plot()],
#' [charge_balance()]
#'
#' @examples
#' gw_data |>
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
  analyte_col = ".analyte_id",
  value_col = ".analysis_result",
  unit_col = ".analysis_unit",
  value_out_col = ".analysis_result_meq",
  unit_out_col = ".analysis_unit_meq"
) {
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

  meq_lookup <- .get_meq_conversion_factors() |>
    dplyr::select(
      analyte_id,
      meq_factor
    )

  df |>
    dplyr::left_join(
      meq_lookup,
      by = by <- stats::setNames(
        "analyte_id",
        analyte_col
      ),
      relationship = "many-to-one"
    ) |>
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
    ) |>
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
#' [`validate_analyte_tbl()`] before extraction.
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
  validate_analyte_tbl(
    lookup
  )

  lookup |>
    dplyr::mutate(
      std_unit = .normalize_unit(
        std_unit
      )
    )
}
