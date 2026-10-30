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

#' Validate unit strings
#'
#' Determines whether unit strings can be successfully interpreted
#' by the \pkg{units} package.
#'
#' Unit strings are first normalized using
#' [`.normalize_unit()`] before validation.
#'
#' @param x Character vector of unit strings.
#'
#' @return
#' A logical vector of the same length as `x` indicating whether
#' each unit string represents a valid unit recognized by
#' \pkg{units}.
#'
#' @details
#' Validation is performed using [units::as_units()].
#'
#' Missing values, empty strings, and unrecognized units are
#' considered invalid and return `FALSE`.
#'
#' This helper is primarily used internally by
#' [`standardize_units()`] and [`.convert_standard_units()`]
#' to identify invalid reporting or standard units before
#' attempting conversion.
#'
#' @noRd
.is_valid_unit <- function(x) {
  x <- .normalize_unit(x)

  vapply(
    x,
    function(unit) {
      if (
        is.na(unit) ||
          !nzchar(unit)
      ) {
        return(FALSE)
      }

      tryCatch(
        {
          units::as_units(unit)
          TRUE
        },
        error = function(e) FALSE
      )
    },
    logical(1)
  )
}

#' Normalize analytical units
#'
#' Normalizes unit strings to a consistent format for comparison and
#' unit conversion.
#'
#' The function:
#' \itemize{
#'   \item trims leading and trailing whitespace;
#'   \item converts microgram symbols (`µg`, `μg`) to `"ug"`;
#'   \item converts all text to lower case.
#' }
#'
#' Examples:
#'
#' \itemize{
#'   \item `" µg/L "` becomes `"ug/l"`
#'   \item `"MG/L"` becomes `"mg/l"`
#' }
#'
#' @param x Character vector of analytical units.
#'
#' @return
#' A normalized character vector of units.
#'
#' @noRd
.normalize_unit <- function(x) {
  x <- as.character(x)

  x %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("µg", "ug") %>%
    stringr::str_replace_all("μg", "ug") %>%
    stringr::str_to_lower()
}
