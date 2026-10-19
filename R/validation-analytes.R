#' Validate analyte metadata table
#'
#' Validates the structure and integrity of an analyte metadata table.
#'
#' The validation ensures that the metadata table contains all required
#' columns, stores aliases as a list-column, and contains unique
#' canonical analyte names.
#'
#' @param lookup An analyte metadata table. Defaults to
#'   [analyte_tbl].
#'
#' @return
#' Invisibly returns `TRUE` if validation succeeds.
#'
#' @details
#' Required columns are:
#'
#' \itemize{
#'   \item `analyte`
#'   \item `aliases`
#'   \item `std_unit`
#'   \item `ion_class`
#'   \item `charge`
#'   \item `molecular_weight`
#'   \item `meq_factor`
#' }
#'
#' Duplicate analyte names are not permitted.
#'
#' The `aliases` column must be stored as a list-column containing
#' character vectors of alternate analyte names.
#'
#' @section Validation checks:
#'
#' The function validates:
#'
#' \itemize{
#'   \item required metadata columns;
#'   \item alias list-column structure;
#'   \item duplicate canonical analyte names;
#'   \item suitability of the table for analyte resolution,
#'         unit standardization, and ion conversion workflows.
#' }
#'
#' @noRd
.validate_analyte_tbl <- function(
  lookup = analyte_tbl
) {
  required_cols <- c(
    "analyte",
    "aliases",
    "std_unit",
    "ion_class",
    "charge",
    "molecular_weight",
    "meq_factor"
  )

  missing_cols <- setdiff(
    required_cols,
    names(lookup)
  )

  if (length(missing_cols) > 0) {
    rlang::abort(
      paste0(
        "analyte_tbl missing columns: ",
        paste(
          missing_cols,
          collapse = ", "
        )
      )
    )
  }

  if (!is.list(lookup$aliases)) {
    rlang::abort(
      "'aliases' must be a list-column."
    )
  }

  dupes <- lookup$analyte[
    duplicated(lookup$analyte)
  ]

  if (length(dupes) > 0) {
    rlang::abort(
      paste0(
        "Duplicate analytes detected: ",
        paste(
          unique(dupes),
          collapse = ", "
        )
      )
    )
  }

  invisible(TRUE)
}

#' Resolve supported analytes
#'
#' Resolves analyte names according to the requested matching strategy.
#'
#' This is a lightweight wrapper around
#' [`.resolve_analyte_name()`].
#'
#' @param analytes Character vector of analyte names.
#'
#' @param analyte_matching Matching strategy. One of:
#'   `"normalized"`, `"strict"`, or `"exact"`.
#'
#' @param alias_lookup Alias lookup table.
#'
#' @return
#' Character vector of resolved analyte names.
#'
#' @details
#' Matching behavior:
#'
#' \describe{
#'   \item{exact}{
#'   Preserves analyte names exactly as supplied.
#'   }
#'
#'   \item{strict}{
#'   Normalizes case, punctuation, and whitespace while preserving
#'   reporting distinctions such as dissolved versus total.
#'   }
#'
#'   \item{normalized}{
#'   Resolves analyte aliases to canonical analyte names using the
#'   alias lookup table.
#'   }
#' }
#'
#' @noRd
.validate_supported_analytes <- function(
  analytes,
  analyte_matching = c(
    "normalized",
    "strict",
    "exact"
  ),
  alias_lookup = analyte_alias_lookup,
  lookup = analyte_tbl
) {
  .validate_analyte_tbl(
    lookup
  )

  .validate_alias_lookup(
    alias_lookup,
    lookup
  )

  analyte_matching <- rlang::arg_match(
    analyte_matching,
    c(
      "normalized",
      "strict",
      "exact"
    )
  )

  resolved <- .resolve_analyte_name(
    analytes,
    analyte_matching = analyte_matching,
    alias_lookup = alias_lookup
  )

  .warn_unsupported_analytes(
    resolved,
    lookup = lookup
  )

  resolved
}

#' Add analyte metadata
#'
#' Resolves analyte names and joins analyte metadata from
#' [analyte_tbl].
#'
#' @param df A data frame containing analytical results.
#'
#' @param analyte_col Character string identifying the analyte column.
#'
#' @param analyte_matching Matching strategy passed to
#'   [`.resolve_analyte_column()`].
#'
#' @return
#' A data frame with analyte metadata joined from [analyte_tbl].
#'
#' @details
#' The function:
#'
#' \enumerate{
#'   \item resolves analytes to canonical names;
#'   \item creates an `analyte` column;
#'   \item joins analyte metadata using the canonical analyte.
#' }
#'
#' Metadata fields include:
#'
#' \itemize{
#'   \item standard unit;
#'   \item ion classification;
#'   \item ionic charge;
#'   \item molecular weight;
#'   \item milliequivalent conversion factor.
#' }
#'
#' @noRd
.add_analyte_metadata <- function(
  df,
  analyte_col = "param_name",
  analyte_matching = "normalized",
  lookup = analyte_tbl
) {
  .validate_analyte_tbl(
    lookup
  )

  .validate_alias_lookup(
    analyte_alias_lookup,
    lookup
  )

  df %>%
    .add_canonical_analyte(
      analyte_col = analyte_col,
      analyte_matching = analyte_matching
    ) %>%
    dplyr::left_join(
      lookup,
      by = "analyte",
      relationship = "many-to-one"
    )
}

#' Validate analytes and units
#'
#' Validates required analyte and unit columns, resolves analytes
#' to canonical names, normalizes reporting units, and warns when
#' analytes are absent from the analyte metadata table.
#'
#' @param df A data frame.
#' @param param_name Character string identifying the analyte column.
#' @param default_unit Character string identifying the unit column.
#' @param analyte_matching Matching strategy used for analyte
#'   resolution.
#'
#' @return
#' A data frame containing:
#' \itemize{
#'   \item a canonical `analyte` column;
#'   \item normalized unit values.
#' }
#'
#' @noRd
.validate_analyte_units <- function(
  df,
  param_name = "param_name",
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

  required_cols <- c(
    param_name,
    default_unit
  )

  missing_cols <- setdiff(
    required_cols,
    names(df)
  )

  .validate_analyte_tbl(
    analyte_tbl
  )

  if (length(missing_cols) > 0) {
    rlang::abort(
      paste0(
        "Missing required columns: ",
        paste(
          missing_cols,
          collapse = ", "
        )
      )
    )
  }

  out <- df %>%
    .add_canonical_analyte(
      analyte_col = param_name,
      analyte_matching = analyte_matching
    )

  .warn_unsupported_analytes(
    out$analyte
  )

  out %>%
    dplyr::mutate(
      !!default_unit := .normalize_unit(
        .data[[default_unit]]
      )
    )
}

#' Add canonical analyte names
#'
#' Adds a canonical analyte column to a data frame.
#'
#' @param df A data frame.
#'
#' @param analyte_col Character string identifying the analyte column.
#'
#' @param analyte_matching Matching strategy used for analyte
#'   resolution.
#'
#' @return
#' A data frame containing an additional `analyte` column.
#'
#' @details
#' Canonical analyte names are generated using
#' [`.resolve_analyte_column()`].
#'
#' The resulting `analyte` column may be used for:
#'
#' \itemize{
#'   \item metadata joins;
#'   \item unit standardization;
#'   \item ion conversion;
#'   \item hydrochemical plotting workflows.
#' }
#'
#' @noRd
.add_canonical_analyte <- function(
  df,
  analyte_col = "param_name",
  analyte_matching = "normalized"
) {
  .validate_analyte_tbl(
    analyte_tbl
  )

  .validate_alias_lookup(
    analyte_alias_lookup
  )

  if (!analyte_col %in% names(df)) {
    rlang::abort(
      sprintf(
        "Column '%s' not found.",
        analyte_col
      )
    )
  }

  df %>%
    .resolve_analyte_column(
      analyte_col = !!rlang::sym(analyte_col),
      output_col = "analyte",
      analyte_matching = analyte_matching
    )
}

#' Validate alias lookup table
#'
#' Validates the structure of an analyte alias lookup table.
#'
#' @param alias_lookup Alias lookup table.
#'
#' @return
#' Invisibly returns the validated lookup table.
#'
#' @details
#' Required columns are:
#'
#' \itemize{
#'   \item `alias`
#'   \item `analyte`
#' }
#'
#' The alias lookup table maps normalized analyte aliases to a
#' canonical analyte name.
#'
#' It is used by [`.resolve_analyte_name()`] and
#' [`.resolve_analyte_column()`] when
#' `analyte_matching = "normalized"`.
#'
#' @noRd
.validate_alias_lookup <- function(
  alias_lookup = analyte_alias_lookup,
  lookup = analyte_tbl
) {
  required_cols <- c(
    "alias",
    "analyte"
  )

  .validate_analyte_tbl(
    lookup
  )

  missing_cols <- setdiff(
    required_cols,
    names(alias_lookup)
  )

  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "Alias lookup table is invalid.",
        "Missing columns:" = paste(
          missing_cols,
          collapse = ", "
        )
      )
    )
  }

  bad_targets <- setdiff(
    unique(alias_lookup$analyte),
    unique(lookup$analyte)
  )

  if (length(bad_targets) > 0) {
    cli::cli_abort(
      paste0(
        "Aliases reference unknown analytes: ",
        paste(
          bad_targets,
          collapse = ", "
        )
      )
    )
  }

  dupes <- alias_lookup$alias[
    duplicated(alias_lookup$alias)
  ]

  if (length(dupes) > 0) {
    cli::cli_abort(
      paste0(
        "Duplicate aliases detected: ",
        paste(
          unique(dupes),
          collapse = ", "
        )
      )
    )
  }

  invisible(alias_lookup)
}

#' Warn on analytes absent from the metadata table
#'
#' Compares analytes against the canonical analyte metadata table
#' and issues a warning for unsupported analyte names.
#'
#' @param analytes Character vector of analyte names.
#' @param lookup An analyte metadata table.
#'
#' @return Invisibly returns the supplied analytes.
#'
#' @noRd
.warn_unsupported_analytes <- function(
  analytes,
  lookup = analyte_tbl
) {
  .validate_analyte_tbl(lookup)

  unsupported <- setdiff(
    unique(stats::na.omit(analytes)),
    lookup$analyte
  )

  if (length(unsupported) > 0) {
    cli::cli_warn(
      c(
        "{length(unsupported)} analytes are not present in analyte_tbl.",
        "i" = paste(
          utils::head(
            unsupported,
            10
          ),
          collapse = ", "
        )
      )
    )
  }

  invisible(analytes)
}
