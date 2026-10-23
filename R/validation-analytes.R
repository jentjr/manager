#' Validate analyte metadata table
#'
#' Validates the structure and integrity of an analyte metadata table.
#'
#' An analyte metadata table provides the canonical analyte registry
#' used throughout the package for analyte resolution, metadata
#' assignment, standards matching, plotting, unit conversion, and
#' environmental data classification workflows.
#'
#' @param x An analyte metadata table, typically [analyte_tbl].
#'
#' @returns
#' Invisibly returns `TRUE` if validation succeeds.
#'
#' @details
#' The table must contain the following required columns:
#'
#' \itemize{
#'   \item `analyte_id`
#'   \item `analyte`
#'   \item `normalized_analyte`
#'   \item `short_name`
#'   \item `std_unit`
#'   \item `analyte_group`
#' }
#'
#' Validation includes:
#'
#' \itemize{
#'   \item presence of required columns;
#'   \item non-missing analyte identifiers;
#'   \item uniqueness of `analyte_id`;
#'   \item uniqueness of canonical analyte names;
#' }
#'
#' The `aliases` column must be stored as a list-column containing
#' one or more character aliases for each analyte.
#'
#' @seealso
#' [analyte_tbl],
#' [`.validate_alias_lookup`()]
#'
#' @keywords internal
validate_analyte_tbl <- function(x) {
  required <- c(
    "analyte_id",
    "analyte",
    "normalized_analyte",
    "short_name",
    "std_unit",
    "analyte_group"
  )

  missing <- setdiff(
    required,
    names(x)
  )

  if (length(missing) > 0) {
    rlang::abort(
      paste(
        "Missing analyte_tbl columns:",
        paste(
          missing,
          collapse = ", "
        )
      )
    )
  }

  if (!is.character(x$analyte_id)) {
    rlang::abort(
      "'analyte_id' must be character."
    )
  }

  if (anyNA(x$analyte_id)) {
    rlang::abort(
      "'analyte_id' cannot contain NA."
    )
  }

  if (!is.character(x$analyte)) {
    rlang::abort(
      "'analyte' must be character."
    )
  }

  if (anyNA(x$analyte)) {
    rlang::abort(
      "'analyte' cannot contain NA."
    )
  }

  if (
    anyDuplicated(
      x$analyte
    )
  ) {
    rlang::abort(
      "Duplicate analyte names detected."
    )
  }

  invisible(TRUE)
}

#' Validate supported analytes
#'
#' Resolves analyte names using the configured analyte registry and
#' validates that analytes can be matched to known analyte metadata.
#'
#' This function performs validation of both the analyte metadata
#' table and alias lookup table before attempting analyte resolution.
#'
#' Unsupported analytes are reported using
#' [`.warn_unsupported_analytes`()].
#'
#' @param analytes Character vector of analyte names.
#' @param alias_lookup Alias lookup table, typically
#'   [analyte_alias_lookup].
#' @param lookup An analyte metadata table, typically [analyte_tbl].
#'
#' @returns
#' Invisibly returns the resolved analyte metadata produced by
#' [`.resolve_analyte`()].
#'
#' @details
#' The returned object includes:
#'
#' \itemize{
#'   \item resolved analyte identifiers;
#'   \item canonical analyte names;
#'   \item normalized analyte names;
#'   \item fraction assignments;
#'   \item analyte metadata;
#'   \item analyte match status.
#' }
#'
#' Unknown analytes are not discarded. Instead, they are flagged and
#' reported through a warning.
#'
#' @seealso
#' [`.resolve_analyte`()],
#' [`.warn_unsupported_analytes`()]
#'
#' @keywords internal
.validate_supported_analytes <- function(
  analytes,
  alias_lookup = analyte_alias_lookup,
  lookup = analyte_tbl
) {
  validate_analyte_tbl(lookup)

  .validate_alias_lookup(
    alias_lookup,
    lookup
  )

  resolved <- .resolve_analyte(
    analytes,
    alias_lookup = alias_lookup,
    analyte_lookup = lookup
  )

  .warn_unsupported_analytes(
    resolved
  )

  invisible(resolved)
}

#' Validate analytes and units
#'
#' Validates analyte and unit fields within an analytical dataset.
#'
#' The function verifies required columns, resolves analytes using the
#' package analyte registry, warns on unsupported analytes, and
#' standardizes unit values.
#'
#' @param df A data frame containing analytical results.
#' @param param_name Character string identifying the analyte column.
#' @param default_unit Character string identifying the unit column.
#'
#' @returns
#' A data frame containing resolved analyte metadata and normalized
#' unit values.
#'
#' @details
#' Validation steps include:
#'
#' \enumerate{
#'   \item verification of required columns;
#'   \item validation of the analyte registry;
#'   \item validation of the alias lookup table;
#'   \item analyte resolution;
#'   \item unsupported analyte checks;
#'   \item unit normalization.
#' }
#'
#' This helper is primarily used during environmental chemistry data
#' import and schema creation workflows.
#'
#' @seealso
#' [`.resolve_analyte_column`()],
#' [`.normalize_unit`()]
#'
#' @keywords internal
.validate_analyte_units <- function(
  df,
  param_name = "param_name",
  default_unit = "default_unit"
) {
  required_cols <- c(
    param_name,
    default_unit
  )

  missing_cols <- setdiff(
    required_cols,
    names(df)
  )

  if (length(missing_cols) > 0) {
    rlang::abort(
      paste(
        "Missing required columns:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  validate_analyte_tbl(
    analyte_tbl
  )

  .validate_alias_lookup(
    analyte_alias_lookup,
    analyte_tbl
  )

  out <- df %>%
    .resolve_analyte_column(
      analyte_col = param_name
    )

  .warn_unsupported_analytes(
    out
  )

  out %>%
    dplyr::mutate(
      !!default_unit := .normalize_unit(
        .data[[default_unit]]
      )
    )
}

#' Validate alias lookup table
#'
#' Validates the structure and integrity of an analyte alias lookup
#' table.
#'
#' The alias lookup table is used during analyte resolution to map
#' normalized analyte aliases to canonical analyte identifiers and
#' metadata.
#'
#' @param alias_lookup Alias lookup table, typically
#'   [analyte_alias_lookup].
#' @param lookup An analyte metadata table, typically [analyte_tbl].
#'
#' @returns
#' Invisibly returns the validated alias lookup table.
#'
#' @details
#' Required columns are:
#'
#' \itemize{
#'   \item `alias`
#'   \item `analyte_id`
#'   \item `analyte`
#'   \item `normalized_analyte`
#'   \item `fraction`
#' }
#'
#' Validation includes:
#'
#' \itemize{
#'   \item required column checks;
#'   \item referential integrity against `analyte_tbl`;
#'   \item duplicate alias detection;
#'   \item character type validation;
#'   \item missing value checks;
#'   \item fraction validation;
#'   \item alias normalization checks.
#' }
#'
#' Valid fraction values are:
#'
#' \itemize{
#'   \item `"dissolved"`
#'   \item `"total"`
#'   \item `"filtered"`
#'   \item `"unfiltered"`
#' }
#'
#' All aliases are expected to have already been normalized using
#' [`.normalize_analyte_text`()].
#'
#' @seealso
#' [analyte_tbl],
#' [analyte_alias_lookup],
#' [`.normalize_analyte_text`()]
#'
#' @keywords internal
.validate_alias_lookup <- function(
  alias_lookup = analyte_alias_lookup,
  lookup = analyte_tbl
) {
  validate_analyte_tbl(lookup)

  required_cols <- c(
    "alias",
    "analyte_id",
    "analyte",
    "normalized_analyte",
    "fraction"
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
    unique(alias_lookup$analyte_id),
    unique(lookup$analyte_id)
  )

  if (length(bad_targets) > 0) {
    cli::cli_abort(
      c(
        "Alias lookup references unknown analyte IDs.",
        setNames(
          bad_targets,
          rep("x", length(bad_targets))
        )
      )
    )
  }

  alias_dupes <- alias_lookup %>%
    dplyr::count(alias) %>%
    dplyr::filter(n > 1)

  if (nrow(alias_dupes) > 0) {
    cli::cli_abort(
      c(
        "Duplicate aliases detected after normalization.",
        setNames(
          alias_dupes$alias,
          rep("x", nrow(alias_dupes))
        )
      )
    )
  }

  if (anyNA(alias_lookup$analyte_id)) {
    rlang::abort(
      "'analyte_id' cannot contain NA."
    )
  }

  if (!is.character(alias_lookup$analyte)) {
    rlang::abort(
      "'analyte' must be character."
    )
  }

  if (anyNA(alias_lookup$analyte)) {
    rlang::abort(
      "'analyte' cannot contain NA."
    )
  }

  if (!is.character(alias_lookup$alias)) {
    rlang::abort(
      "'alias' must be character."
    )
  }

  if (!is.character(alias_lookup$analyte_id)) {
    rlang::abort(
      "'analyte_id' must be character."
    )
  }

  if (anyNA(alias_lookup$alias)) {
    rlang::abort(
      "'alias' cannot contain NA."
    )
  }

  if (!is.character(alias_lookup$normalized_analyte)) {
    rlang::abort(
      "'normalized_analyte' must be character."
    )
  }

  if (anyNA(alias_lookup$normalized_analyte)) {
    rlang::abort(
      "'normalized_analyte' cannot contain NA."
    )
  }

  if (!is.character(alias_lookup$fraction)) {
    rlang::abort(
      "'fraction' must be character."
    )
  }

  valid_fractions <- c(
    "dissolved",
    "total",
    "filtered",
    "unfiltered"
  )

  bad_fraction <- alias_lookup %>%
    dplyr::filter(
      !is.na(fraction),
      !fraction %in% valid_fractions
    )

  if (nrow(bad_fraction) > 0) {
    rlang::abort(
      paste(
        "Invalid fraction values detected:",
        paste(
          unique(bad_fraction$fraction),
          collapse = ", "
        )
      )
    )
  }

  bad_aliases <- alias_lookup %>%
    dplyr::filter(
      alias != .normalize_analyte_text(alias)
    )

  if (nrow(bad_aliases) > 0) {
    rlang::abort(
      "All aliases must already be normalized."
    )
  }

  invisible(alias_lookup)
}

#' Warn on unsupported analytes
#'
#' Identifies analytes that could not be matched to the analyte
#' registry.
#'
#' @param x Output from [`.resolve_analyte`()] or
#'   [`.resolve_analyte_column`()] containing
#'   `source_analyte` and `is_known_analyte` columns.
#'
#' @returns
#' Invisibly returns the supplied object.
#'
#' @details
#' Unsupported analytes are determined using the
#' `is_known_analyte` flag generated during analyte resolution.
#'
#' When unsupported analytes are present, a warning is issued that
#' includes the first several unmatched analyte names.
#'
#' This function is intended for quality assurance during data import
#' and schema construction workflows.
#'
#' @seealso
#' [`.resolve_analyte`()],
#' [`.validate_supported_analytes`()]
#'
#' @keywords internal
.warn_unsupported_analytes <- function(x) {
  if (
    all(
      c(
        "source_analyte",
        "is_known_analyte"
      ) %in%
        names(x)
    )
  ) {
    source_col <- "source_analyte"
    known_col <- "is_known_analyte"
  } else if (
    all(
      c(
        ".source_analyte",
        ".is_known_analyte"
      ) %in%
        names(x)
    )
  ) {
    source_col <- ".source_analyte"
    known_col <- ".is_known_analyte"
  } else {
    rlang::abort(
      "Missing analyte resolution columns."
    )
  }

  unsupported <- x %>%
    dplyr::filter(
      !.data[[known_col]]
    ) %>%
    dplyr::distinct(
      .data[[source_col]]
    )

  if (nrow(unsupported) > 0) {
    cli::cli_warn(
      c(
        "{nrow(unsupported)} unsupported analytes detected.",
        "i" = paste(
          utils::head(
            unsupported[[source_col]],
            10
          ),
          collapse = ", "
        )
      )
    )
  }

  invisible(x)
}
