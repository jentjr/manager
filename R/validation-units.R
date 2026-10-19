.validate_standard_units <- function(df) {
  bad_units <- df %>%
    dplyr::filter(is.na(result_unit_std)) %>%
    dplyr::distinct(param_name)

  if (nrow(bad_units) > 0) {
    warning(
      paste(
        "No standard unit mapping found for:",
        paste(
          bad_units$param_name,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  invisible(df)
}

#' Validate units for meq conversion
#'
#' Ensures analytes eligible for meq conversion are reported using
#' their expected standardized reporting units.
#'
#' @param df Data frame containing analytical results.
#'
#' @param analyte_col Character string identifying the canonical
#'   analyte column. Default is `"analyte"`.
#'
#' @param unit_col Character string identifying the reporting unit
#'   column. Default is `"default_unit_std"`.
#'
#' @return
#' Invisibly returns `TRUE`.
#'
#' @details
#' Validation is performed using the metadata returned by
#' [`.get_meq_conversion_factors()`].
#'
#' Only analytes having a non-missing `meq_factor` are validated.
#'
#' @section Validation:
#'
#' An error is raised when a convertible analyte is reported using a
#' unit that differs from the expected standard unit.
#'
#' This typically indicates that
#' [`.standardize_units()`] has not yet been executed.
#'
#' @noRd
.validate_analyte_meq_units <- function(
  df,
  analyte_col = "analyte",
  unit_col = "default_unit_std"
) {
  .validate_required_cols(
    df,
    c(
      analyte_col,
      unit_col
    )
  )

  meq_lookup <- .get_meq_conversion_factors() %>%
    dplyr::filter(
      !is.na(meq_factor)
    )

  invalid_units <- df %>%
    dplyr::mutate(
      !!unit_col := .normalize_unit(
        .data[[unit_col]]
      )
    ) %>%
    dplyr::left_join(
      meq_lookup,
      by = stats::setNames(
        "analyte",
        analyte_col
      ),
      relationship = "many-to-one"
    ) %>%
    dplyr::filter(
      !is.na(meq_factor),
      !is.na(.data[[unit_col]]),
      .data[[unit_col]] != std_unit
    ) %>%
    dplyr::distinct(
      .data[[analyte_col]],
      .data[[unit_col]],
      std_unit
    )

  if (nrow(invalid_units) > 0) {
    examples <- invalid_units %>%
      utils::head(10)

    cli::cli_abort(
      c(
        "Non-standard units found for meq conversion.",
        "x" = paste(
          paste0(
            examples[[analyte_col]],
            " (",
            examples[[unit_col]],
            " != ",
            examples$std_unit,
            ")"
          ),
          collapse = ", "
        ),
        "i" = "Run {.fun .standardize_units} before converting to meq."
      )
    )
  }

  invisible(TRUE)
}
