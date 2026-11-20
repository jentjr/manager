#' @keywords internal
.register_environmental_units <- function() {
  definitions <- list(
    mV = "1e-3 V",
    NTU = "unitless",
    pCi = "0.037 Bq"
  )

  for (symbol in names(definitions)) {
    is_installed <- tryCatch(
      {
        units::as_units(symbol)
        TRUE
      },
      error = function(e) FALSE
    )

    if (!is_installed) {
      units::install_unit(
        symbol = symbol,
        def = definitions[[symbol]]
      )
    }
  }

  invisible(TRUE)
}

#' @keywords internal
.convert_value_columns <- function(
  df,
  value_cols,
  from_unit_col,
  to_unit_col
) {
  .validate_required_cols(
    df,
    c(value_cols, from_unit_col, to_unit_col)
  )

  from_unit <- .normalize_unit(df[[from_unit_col]])
  to_unit <- .normalize_unit(df[[to_unit_col]])

  conversion_groups <- df |>
    dplyr::transmute(
      .from_unit = from_unit,
      .to_unit = to_unit
    ) |>
    dplyr::distinct() |>
    dplyr::filter(
      !is.na(.from_unit),
      !is.na(.to_unit),
      .from_unit != .to_unit
    )

  for (i in seq_len(nrow(conversion_groups))) {
    from <- conversion_groups$.from_unit[i]
    to <- conversion_groups$.to_unit[i]

    idx <- !is.na(from_unit) &
      !is.na(to_unit) &
      from_unit == from &
      to_unit == to

    for (col in value_cols) {
      df[[col]][idx] <- .safe_convert_units(
        value = df[[col]][idx],
        from_unit = from,
        to_unit = to
      )
    }
  }

  df
}

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
  if (!is.character(x)) {
    x <- as.character(x)
  }

  missing <- is.na(x)

  # Normalize whitespace and Unicode micro symbols.
  x <- x |>
    stringr::str_trim() |>
    stringr::str_replace_all("[\u00B5\u03BC]", "u") |>
    stringr::str_replace_all("\\s*/\\s*", "/") |>
    stringr::str_replace_all("\\s+", " ") |>
    stringr::str_to_lower()

  # Values used by some source systems to indicate that the value is
  # reported in the standard unit, rather than specifying a physical unit.
  standard_unit_sentinels <- c(
    "std",
    "standard",
    "standard unit",
    "standardized",
    "same as standard"
  )

  x[x %in% standard_unit_sentinels] <- NA_character_

  # Temperature aliases.
  x <- stringr::str_replace_all(
    x,
    stringr::regex(
      "^(?:°\\s*c|deg\\s*c|degrees?\\s+(?:c|celsius)|celsius)$",
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

  # Restore canonical case-sensitive symbols registered with {units}.
  # This must occur after str_to_lower().
  canonical_units <- c(
    "mv" = "mV",
    "ntu" = "NTU",
    "pci" = "pCi",
    "pci/l" = "pCi/L"
  )

  matched <- !is.na(x) & x %in% names(canonical_units)
  x[matched] <- unname(canonical_units[x[matched]])

  x[missing] <- NA_character_
  x[stringr::str_length(x) == 0L] <- NA_character_

  x
}

#' Standardize env_schema analytical results and limits
#'
#' Converts analytical results and mapped detection/reporting limits
#' to analyte-specific standard units.
#'
#' Missing source units are assumed to be the analyte lookup standard
#' unit and are reported with an informational message.
#'
#' @keywords internal
.standardize_schema_results <- function(
  df,
  env_mapping = NULL
) {
  if (!".std_unit" %in% names(df)) {
    return(df)
  }

  .validate_required_cols(
    df,
    c(
      ".analysis_result",
      ".analysis_unit",
      ".std_unit"
    )
  )

  if (!is.numeric(df$.analysis_result)) {
    rlang::abort("`.analysis_result` must be numeric.")
  }

  df$.analysis_unit <- .normalize_unit(df$.analysis_unit)
  df$.std_unit <- .normalize_unit(df$.std_unit)

  if (!".original_lab_result" %in% names(df)) {
    df$.original_lab_result <- df$.analysis_result
  }

  if (!".original_lab_unit" %in% names(df)) {
    df$.original_lab_unit <- df$.analysis_unit
  }

  # Standardize the analytical result first. This establishes the
  # final unit used by all limit columns below.
  result <- .standardize_schema_value(
    value = df$.analysis_result,
    source_unit = df$.analysis_unit,
    standard_unit = df$.std_unit,
    analyte = df$.analyte_id,
    value_label = "analytical result"
  )

  df$.analysis_result <- result$value
  df$.analysis_unit <- result$unit

  # Standardize mapped limits. These fields currently inherit the
  # analytical result source unit because the schema has no separate
  # limit-unit mappings.
  limit_specs <- c(
    reporting_limit = ".reporting_limit",
    detection_limit = ".detection_limit",
    quantitation_limit = ".quantitation_limit",
    achieved_mdc = ".achieved_mdc"
  )

  if (!is.null(env_mapping)) {
    for (mapping_name in names(limit_specs)) {
      source_col <- env_mapping[[mapping_name]]

      if (
        is.null(source_col) ||
          is.na(source_col) ||
          !nzchar(source_col) ||
          !source_col %in% names(df)
      ) {
        next
      }

      internal_col <- limit_specs[[mapping_name]]

      if (!is.numeric(df[[source_col]])) {
        rlang::abort(
          sprintf(
            "`%s` must be numeric.",
            source_col
          )
        )
      }

      original_col <- paste0(
        ".original_",
        mapping_name
      )

      if (!original_col %in% names(df)) {
        df[[original_col]] <- df[[source_col]]
      }

      standardized <- .standardize_schema_value(
        value = df[[source_col]],
        source_unit = df$.analysis_unit,
        standard_unit = df$.std_unit,
        analyte = df$.analyte_id,
        value_label = mapping_name
      )

      df[[internal_col]] <- standardized$value
    }
  }

  df
}

#' Standardize analyte-unit pair value vector
#'
#' @keywords internal
.standardize_schema_value <- function(
  value,
  source_unit,
  standard_unit,
  analyte = NULL,
  value_label = "value"
) {
  source_unit <- .normalize_unit(source_unit)
  standard_unit <- .normalize_unit(standard_unit)

  has_value <- !is.na(value)

  missing_source_unit <- has_value &
    is.na(source_unit) &
    !is.na(standard_unit)

  if (any(missing_source_unit)) {
    cli::cli_inform(
      c(
        "i" = paste0(
          sum(missing_source_unit),
          " ",
          value_label,
          " value(s) have no source unit."
        ),
        "i" = "The analyte lookup standard unit will be assumed."
      )
    )

    source_unit[missing_source_unit] <-
      standard_unit[missing_source_unit]
  }

  convert_idx <- has_value &
    !is.na(source_unit) &
    !is.na(standard_unit) &
    source_unit != standard_unit

  conversion_groups <- unique(
    data.frame(
      from = source_unit[convert_idx],
      to = standard_unit[convert_idx],
      stringsAsFactors = FALSE
    )
  )

  for (i in seq_len(nrow(conversion_groups))) {
    idx <- convert_idx &
      source_unit == conversion_groups$from[i] &
      standard_unit == conversion_groups$to[i]

    value[idx] <- .safe_convert_units(
      value = value[idx],
      from_unit = conversion_groups$from[i],
      to_unit = conversion_groups$to[i]
    )
  }

  list(
    value = value,
    unit = dplyr::coalesce(standard_unit, source_unit)
  )
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
    length(from_unit) != 1L ||
      length(to_unit) != 1L ||
      is.na(from_unit) ||
      is.na(to_unit)
  ) {
    return(rep(NA_real_, length(value)))
  }

  tryCatch(
    .convert_units(
      value = value,
      from_unit = from_unit,
      to_unit = to_unit
    ),
    error = function(e) {
      rlang::abort(
        c(
          "Unit conversion failed.",
          "x" = paste0(from_unit, " -> ", to_unit),
          "i" = conditionMessage(e)
        ),
        parent = e
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
  meq_l_unit <- "meq/L"

  .validate_required_cols(
    df,
    c(
      analyte_col,
      value_col,
      unit_col
    )
  )

  if (!is.numeric(df[[value_col]])) {
    rlang::abort(
      sprintf("`%s` must be numeric.", value_col)
    )
  }

  if (
    any(
      !is.na(df[[value_col]]) &
        !is.finite(df[[value_col]])
    )
  ) {
    rlang::abort(
      sprintf("`%s` cannot contain Inf or NaN.", value_col)
    )
  }

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

  if (!is.numeric(meq_lookup$meq_factor)) {
    rlang::abort("`meq_factor` must be numeric.")
  }

  if (
    any(
      !is.na(meq_lookup$meq_factor) &
        (!is.finite(meq_lookup$meq_factor) |
          meq_lookup$meq_factor <= 0)
    )
  ) {
    rlang::abort(
      "`meq_factor` must contain finite positive values."
    )
  }

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
        meq_l_unit,
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
