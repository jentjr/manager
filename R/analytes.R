#' Add resolved analyte metadata
#'
#' Resolves analyte names and attaches analyte metadata to a data
#' frame.
#'
#' This function serves as the primary analyte enrichment step used
#' during schema construction. Raw analyte names are standardized,
#' matched to the analyte registry, assigned metadata, and flagged
#' when no registry match is available.
#'
#' @param df A data frame containing analytical results.
#' @param analyte_col Column containing analyte names.
#'
#' @returns
#' A data frame enriched with analyte metadata.
#'
#' @details
#' The following schema columns are added:
#'
#' \itemize{
#'   \item `.source_analyte`
#'   \item `.analyte`
#'   \item `.analyte_id`
#'   \item `.normalized_analyte`
#'   \item `.fraction`
#'   \item `.is_known_analyte`
#' }
#'
#' Additional analyte metadata is attached from [analyte_tbl],
#' including analyte classifications, standard units, ion
#' information, molecular properties, and reporting metadata.
#'
#' Unknown analytes are retained in the output and reported through
#' [`.warn_unsupported_analytes`()].
#'
#' The resulting data frame is suitable for downstream workflows
#' including:
#'
#' \itemize{
#'   \item standards assignment;
#'   \item unit conversion;
#'   \item summary statistics;
#'   \item trend analysis;
#'   \item hydrochemical calculations;
#'   \item plotting and reporting.
#' }
#'
#' @seealso
#' [`.resolve_analyte_column`()],
#' [`.resolve_analyte`()],
#' [analyte_tbl]
#'
#' @keywords internal
.resolve_analyte_metadata <- function(
  df,
  analyte_col = "param_name",
  alias_lookup = analyte_alias_lookup,
  analyte_lookup = analyte_tbl
) {
  validate_analyte_tbl(
    analyte_lookup
  )

  .validate_alias_lookup(
    alias_lookup,
    analyte_lookup
  )

  if (!analyte_col %in% names(df)) {
    rlang::abort(
      sprintf(
        "Column '%s' not found.",
        analyte_col
      )
    )
  }

  out <- df %>%
    .resolve_analyte_column(
      analyte_col = !!rlang::sym(analyte_col),
      alias_lookup = alias_lookup,
      analyte_lookup = analyte_lookup
    )

  .warn_unsupported_analytes(
    out
  )

  out %>%
    dplyr::relocate(
      .source_analyte,
      .normalized_input,

      .analyte,
      .analyte_id,
      .normalized_analyte,

      .fraction,
      .is_known_analyte,

      .before = 1
    )
}

#' Resolve analyte names within a data frame
#'
#' Adds a column containing resolved analyte names according to the
#' specified analyte matching strategy. This provides a consistent
#' mechanism for handling raw laboratory analyte names, normalized
#' analyte names, and analyte aliases throughout the package.
#'
#' Three matching modes are supported:
#'
#' \describe{
#'   \item{exact}{
#'     Returns analyte names unchanged. For example,
#'     `"Calcium, dissolved"` and `"Calcium, total"` remain distinct.
#'   }
#'   \item{strict}{
#'     Applies text normalization only (lower-casing, punctuation removal,
#'     whitespace normalization), while preserving meaningful distinctions.
#'     For example, `"Calcium, dissolved"` becomes
#'     `"calcium dissolved"` and remains distinct from
#'     `"calcium total"`.
#'   }
#'   \item{normalized}{
#'     Applies text normalization and resolves aliases to a canonical
#'     analyte name using `alias_lookup`. For example,
#'     `"Calcium, dissolved"`, `"Calcium, total"`, and `"Ca"` may all
#'     resolve to `"calcium"`.
#'   }
#' }
#'
#' @param data A data frame containing analyte values.
#' @param analyte_col Column containing analyte names.
#' @param output_col Name of the output column containing resolved analyte
#'   names. Defaults to `"analyte_resolved"`.
#' @param analyte_matching Character string specifying the analyte matching
#'   strategy. Must be one of `"exact"`, `"strict"`, or `"normalized"`.
#' @param alias_lookup A lookup table containing at minimum `alias` and
#'   `analyte` columns used to resolve aliases when
#'   `analyte_matching = "normalized"`.
#'
#' @return
#' A data frame with an additional column containing resolved analyte names.
#'
#' @seealso
#' \code{\link{.resolve_analyte_name}}
#'
#' @noRd
.resolve_analyte_column <- function(
  data,
  analyte_col,
  alias_lookup = analyte_alias_lookup,
  analyte_lookup = analyte_tbl
) {
  analyte_col <- rlang::ensym(
    analyte_col
  )

  resolved <- .resolve_analyte(
    dplyr::pull(
      data,
      !!analyte_col
    ),
    alias_lookup = alias_lookup,
    analyte_lookup = analyte_lookup
  ) %>%
    dplyr::rename(
      .source_analyte = source_analyte,
      .normalized_input = normalized_input,
      .resolved_analyte = analyte,
      .analyte_id = analyte_id,
      .normalized_analyte = normalized_analyte,
      .fraction = fraction,
      .is_known_analyte = is_known_analyte,
      .short_name = short_name,
      .casrn = casrn,
      .analyte_group = analyte_group,
      .subcategory = subcategory,
      .regulatory_class = regulatory_class,
      .std_unit = std_unit,
      .ion_class = ion_class,
      .charge = charge,
      .molecular_weight = molecular_weight,
      .meq_factor = meq_factor,
      .is_major_ion = is_major_ion,
      .is_trace_metal = is_trace_metal,
      .is_radionuclide = is_radionuclide,
      .is_general_chemistry = is_general_chemistry,
      .is_field_parameter = is_field_parameter
    )

  dplyr::bind_cols(
    data,
    resolved
  ) %>%
    dplyr::mutate(
      .analyte = .resolved_analyte
    ) %>%
    dplyr::select(
      -.resolved_analyte
    )
}

#' @keywords internal
#' @noRd
.resolve_analyte_name <- function(
  x,
  analyte_matching = c(
    "exact",
    "strict",
    "normalized"
  ),
  alias_lookup = analyte_alias_lookup,
  analyte_lookup = analyte_tbl
) {
  resolved <- .resolve_analyte(
    x,
    alias_lookup = alias_lookup,
    analyte_lookup = analyte_lookup
  )

  .analyte_key(
    analyte = resolved$analyte,
    normalized_analyte = resolved$normalized_analyte,
    fraction = resolved$fraction,
    analyte_matching = analyte_matching
  )
}

#' Resolve analyte metadata
#'
#' Resolves analyte aliases and attaches analyte metadata.
#'
#' @param x Character vector of analyte names.
#' @param alias_lookup Alias lookup table.
#' @param analyte_lookup Analyte metadata table.
#'
#' @returns Tibble containing resolved analyte metadata.
#'
#' @keywords internal
.resolve_analyte <- function(
  x,
  alias_lookup = analyte_alias_lookup,
  analyte_lookup = analyte_tbl
) {
  source_analyte <- as.character(x)

  normalized <- .normalize_analyte_text(
    source_analyte
  )

  resolved <- tibble::tibble(
    source_analyte = source_analyte,
    alias = normalized
  ) %>%
    dplyr::left_join(
      alias_lookup,
      by = "alias",
      relationship = "many-to-one"
    ) %>%
    dplyr::mutate(
      fraction = dplyr::coalesce(
        fraction,
        .extract_fraction(source_analyte)
      )
    ) %>%
    dplyr::transmute(
      source_analyte,
      normalized_input = normalized,
      is_known_analyte = !is.na(analyte_id),
      analyte_id = dplyr::coalesce(
        analyte_id,
        normalized
      ),

      analyte = dplyr::coalesce(
        analyte,
        source_analyte
      ),

      normalized_analyte = dplyr::coalesce(
        normalized_analyte,
        analyte_id,
        normalized
      ),

      fraction
    )

  resolved %>%
    dplyr::left_join(
      analyte_lookup %>%
        dplyr::select(
          -dplyr::any_of(
            c(
              "analyte",
              "normalized_analyte",
              "fraction"
            )
          )
        ),
      by = "analyte_id",
      suffix = c("", ".lookup"),
      relationship = "many-to-one"
    )
}

#' Get analyte key
#'
#' @noRd
.analyte_key <- function(
  analyte,
  normalized_analyte,
  fraction,
  analyte_matching = c(
    "exact",
    "strict",
    "normalized"
  )
) {
  analyte_matching <- rlang::arg_match(
    analyte_matching
  )

  exact_analyte <- dplyr::if_else(
    is.na(fraction),
    analyte,
    paste(
      analyte,
      fraction,
      sep = ", "
    )
  )

  switch(
    analyte_matching,

    exact = exact_analyte,

    normalized = normalized_analyte,

    strict = paste(
      normalized_analyte,
      dplyr::coalesce(
        fraction,
        "unspecified"
      ),
      sep = "::"
    )
  )
}

#' Extract analyte fraction
#'
#' @param x Character vector of analyte names.
#'
#' @returns Character vector.
#'
#' @keywords internal
.extract_fraction <- function(x) {
  x <- .normalize_analyte_text(x)

  dplyr::case_when(
    stringr::str_detect(
      x,
      "\\bdis\\b|\\bdissolved\\b"
    ) ~ "dissolved",

    stringr::str_detect(
      x,
      "\\btot\\b|\\btotal\\b"
    ) ~ "total",

    stringr::str_detect(
      x,
      "\\bfiltered\\b"
    ) ~ "filtered",

    stringr::str_detect(
      x,
      "\\bunfiltered\\b"
    ) ~ "unfiltered",

    TRUE ~ NA_character_
  )
}

#' Normalize analyte text
#'
#' @param x Character vector.
#'
#' @return Normalized character vector.
#'
#' @noRd
.normalize_analyte_text <- function(x) {
  x <- as.character(x)

  x <- stringi::stri_trans_tolower(x)

  x <- stringr::str_replace_all(
    x,
    "[[:punct:]]+",
    " "
  )

  stringr::str_squish(x)
}

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
#' When unsupported analytes are present, a warning is issued
#' describing the affected analytes and any analyte-dependent
#' functionality that will be skipped, including metadata
#' enrichment and standard-unit conversion.
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
        "{nrow(unsupported)} analytes were not found in analyte metadata.",
        "i" = paste(
          utils::head(
            unsupported[[source_col]],
            10
          ),
          collapse = ", "
        ),
        "i" = paste(
          "Analyte metadata, standard-unit conversion,",
          "and analyte-specific calculations were skipped."
        )
      )
    )
  }
  
  invisible(x)
}

#' Validate analyte alias table
#'
#' Validates the structure and integrity of an analyte alias table.
#'
#' @param alias_tbl Analyte alias table.
#' @param analyte_tbl Canonical analyte metadata table.
#'
#' @returns
#' Invisibly returns `TRUE`.
#'
#' @keywords internal
validate_analyte_alias_tbl <- function(
    alias_tbl,
    analyte_tbl
) {
  required <- c(
    "analyte_id",
    "alias"
  )
  
  missing <- setdiff(
    required,
    names(alias_tbl)
  )
  
  if (length(missing) > 0) {
    rlang::abort(
      paste(
        "Missing alias columns:",
        paste(
          missing,
          collapse = ", "
        )
      )
    )
  }
  
  if (!is.character(alias_tbl$analyte_id)) {
    rlang::abort(
      "'analyte_id' must be character."
    )
  }
  
  if (!is.character(alias_tbl$alias)) {
    rlang::abort(
      "'alias' must be character."
    )
  }
  
  if (anyNA(alias_tbl$analyte_id)) {
    rlang::abort(
      "'analyte_id' cannot contain NA."
    )
  }
  
  if (anyNA(alias_tbl$alias)) {
    rlang::abort(
      "'alias' cannot contain NA."
    )
  }
  
  bad_ids <- setdiff(
    unique(alias_tbl$analyte_id),
    unique(analyte_tbl$analyte_id)
  )
  
  if (length(bad_ids) > 0) {
    rlang::abort(
      paste(
        "Unknown analyte IDs:",
        paste(
          bad_ids,
          collapse = ", "
        )
      )
    )
  }
  
  alias_conflicts <- alias_tbl %>%
    dplyr::mutate(
      alias = .normalize_analyte_text(alias)
    ) %>%
    dplyr::group_by(alias) %>%
    dplyr::summarise(
      n_ids = dplyr::n_distinct(analyte_id),
      .groups = "drop"
    ) %>%
    dplyr::filter(
      n_ids > 1
    )
  
  if (nrow(alias_conflicts) > 0) {
    rlang::abort(
      paste(
        "Aliases map to multiple analytes:",
        paste(
          alias_conflicts$alias,
          collapse = ", "
        )
      )
    )
  }
  
  invisible(TRUE)
}
