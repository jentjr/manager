#' Standardize analytical reporting units
#'
#' Resolves analytes to canonical names, looks up preferred reporting
#' units from [analyte_tbl], and converts analytical results to the
#' corresponding standard unit where possible.
#'
#' @param df A data frame containing analytical results.
#'
#' @param value_col Character string identifying the numeric result
#'   column. Default is `"analysis_result"`.
#'
#' @param unit_col Character string identifying the unit column.
#'   Default is `"default_unit"`.
#'
#' @param analyte_col Character string identifying the analyte name
#'   column. Default is `"param_name"`.
#'
#' @param analyte_matching Character string controlling analyte
#'   resolution. Passed to [`.resolve_analyte_column()`].
#'   Default is `"normalized"`.
#'
#' @return
#' A data frame containing all original columns plus:
#'
#' \describe{
#'   \item{analyte}{Canonical analyte name.}
#'   \item{<value_col>_std}{Analytical result converted to the standard
#'   reporting unit.}
#'   \item{<unit_col>_std}{Standard reporting unit for the analyte.}
#' }
#'
#' @details
#' Standard units are derived from [analyte_tbl].
#'
#' Unit strings are normalized before comparison using
#' [`.normalize_unit()`].
#'
#' Results already reported in the standard unit are copied directly
#' without conversion.
#'
#' Unit conversion is performed only when a supported standard unit is
#' available.
#'
#' @section Validation:
#'
#' The function performs several validation checks:
#'
#' \itemize{
#'   \item required columns must exist;
#'   \item the analyte metadata table must contain unique analyte names;
#'   \item analytes lacking a standard unit generate a warning;
#'   \item failed unit conversions generate a warning and return
#'   `NA_real_` for affected values.
#' }
#'
#' @section Analyte resolution:
#'
#' Analytes are resolved according to the specified
#' `analyte_matching` strategy before standard unit lookup.
#'
#' Under `"normalized"` matching, aliases such as `"Ca"` and
#' `"Calcium, dissolved"` may resolve to the same canonical analyte.
#'
#' @seealso
#' [convert_analytes_to_meq()]
#'
#' @examples
#' gw_data |>
#'   standardize_units()
#'
#' gw_data |>
#'   standardize_units(
#'     value_col = "result",
#'     unit_col = "unit",
#'     analyte_col = "parameter"
#'   )
#'
#' @export
standardize_units <- function(
  df,
  value_col = "analysis_result",
  unit_col = "default_unit",
  analyte_col = "param_name",
  analyte_matching = "normalized"
) {
  required_cols <- c(
    value_col,
    unit_col,
    analyte_col
  )

  .validate_required_cols(
    df,
    required_cols
  )

  std_lookup <- .get_standard_units()

  if (anyDuplicated(std_lookup$analyte)) {
    cli::cli_abort(
      "Duplicate analytes found in standard unit lookup."
    )
  }

  df2 <- df %>%
    .resolve_analyte_column(
      analyte_col = !!rlang::sym(analyte_col),
      output_col = "analyte",
      analyte_matching = analyte_matching
    ) %>%
    dplyr::left_join(
      std_lookup,
      by = "analyte",
      relationship = "many-to-one"
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(
          c(
            unit_col,
            "std_unit"
          )
        ),
        .normalize_unit
      )
    )

  missing_analytes <- df2 %>%
    dplyr::filter(
      is.na(std_unit)
    ) %>%
    dplyr::distinct(
      analyte
    ) %>%
    dplyr::pull(
      analyte
    )

  if (length(missing_analytes) > 0) {
    cli::cli_warn(
      c(
        "{length(missing_analytes)} analytes have no standard unit.",
        "i" = paste(
          utils::head(
            missing_analytes,
            10
          ),
          collapse = ", "
        )
      )
    )
  }

  out_col <- paste0(
    value_col,
    "_std"
  )

  unit_out_col <- paste0(
    unit_col,
    "_std"
  )

  df2[[out_col]] <- NA_real_

  same_unit_idx <-
    !is.na(df2[[unit_col]]) &
    !is.na(df2$std_unit) &
    df2[[unit_col]] == df2$std_unit

  df2[[out_col]][same_unit_idx] <-
    df2[[value_col]][same_unit_idx]

  conversion_groups <- df2 %>%
    dplyr::filter(
      !is.na(std_unit)
    ) %>%
    dplyr::distinct(
      .data[[unit_col]],
      std_unit
    )

  for (i in seq_len(nrow(conversion_groups))) {
    from_unit <- conversion_groups[[unit_col]][i]

    to_unit <- conversion_groups$std_unit[i]

    if (identical(from_unit, to_unit)) {
      next
    }

    if (
      !isTRUE(.is_valid_unit(from_unit)) ||
        !isTRUE(.is_valid_unit(to_unit))
    ) {
      cli::cli_warn(
        paste0(
          "Invalid conversion skipped: ",
          from_unit,
          " -> ",
          to_unit
        )
      )

      next
    }

    idx <-
      is.na(df2[[out_col]]) &
      !is.na(df2[[unit_col]]) &
      !is.na(df2$std_unit) &
      df2[[unit_col]] == from_unit &
      df2$std_unit == to_unit

    if (!any(idx)) {
      next
    }

    df2[[out_col]][idx] <-
      .safe_convert_units(
        value = df2[[value_col]][idx],
        from_unit = from_unit,
        to_unit = to_unit
      )
  }

  df2 %>%
    dplyr::mutate(
      !!unit_out_col := std_unit
    ) %>%
    dplyr::select(
      -std_unit
    )
}

#' Extract standard analytical units from analyte metadata
#'
#' Returns the canonical analyte-to-standard-unit lookup table derived
#' from `analyte_tbl`.
#'
#' The lookup table is validated using
#' [`.validate_analyte_tbl()`] before extraction.
#'
#' @param lookup An analyte metadata table. Defaults to
#'   [analyte_tbl].
#'
#' @return
#' A tibble containing:
#' \describe{
#'   \item{analyte}{Canonical analyte name.}
#'   \item{std_unit}{Preferred standard reporting unit.}
#' }
#'
#' @noRd
.get_standard_units <- function(
  lookup = analyte_tbl
) {
  .validate_analyte_tbl(
    lookup
  )

  lookup %>%
    dplyr::select(
      analyte,
      std_unit
    )
}

#' Prepare analytical concentrations
#'
#' Ensures that standardized concentration columns are available for
#' subsequent calculations.
#'
#' If a standardized result column is not present, analytical values are
#' standardized using [`standardize_units()`].
#'
#' @param df A data frame containing analytical results.
#'
#' @param value_col Character string identifying the analytical result
#'   column. Default is `"analysis_result"`.
#'
#' @param analyte_matching Character string controlling analyte
#'   resolution. Passed to [`standardize_units()`].
#'
#' @return
#' A data frame containing standardized concentration columns.
#'
#' @details
#' The function checks for the existence of
#' `paste0(value_col, "_std")`.
#'
#' If the standardized column already exists, the input data frame is
#' returned unchanged.
#'
#' @noRd
.prep_concentrations <- function(
  df,
  value_col = "analysis_result",
  analyte_matching = "normalized"
) {
  std_col <- paste0(
    value_col,
    "_std"
  )

  if (!std_col %in% names(df)) {
    df <- standardize_units(
      df = df,
      value_col = value_col,
      analyte_matching = analyte_matching
    )
  }

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
  data_unit_col = "default_unit"
) {
  .validate_required_cols(
    df,
    c(
      "standard_value",
      "standard_unit",
      data_unit_col
    )
  )

  from_unit <- .normalize_unit(
    df$standard_unit
  )

  to_unit <- .normalize_unit(
    df[[data_unit_col]]
  )

  valid_from <- .is_valid_unit(
    from_unit
  )

  valid_to <- .is_valid_unit(
    to_unit
  )

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
    !is.na(df$standard_value) &
    valid_from &
    valid_to &
    from_unit != to_unit

  if (!any(convert_idx)) {
    return(df)
  }

  converted_values <- purrr::pmap_dbl(
    list(
      value = df$standard_value[convert_idx],
      from_unit = from_unit[convert_idx],
      to_unit = to_unit[convert_idx]
    ),
    .safe_convert_unit
  )

  convert_rows <- which(
    convert_idx
  )

  successful_rows <- convert_rows[
    !is.na(converted_values)
  ]

  if (length(successful_rows) == 0) {
    return(df)
  }

  df$standard_value[
    successful_rows
  ] <- converted_values[
    !is.na(converted_values)
  ]

  df$standard_unit[
    successful_rows
  ] <- to_unit[
    successful_rows
  ]

  df
}
