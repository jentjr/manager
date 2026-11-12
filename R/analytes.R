#' Attach analyte metadata
#'
#' Joins analyte metadata from an analyte lookup table onto an
#' \code{env_schema} object using the internal analyte identifier.
#'
#' This helper enriches schema data with analyte-level attributes such as
#' canonical names, short names, categories, groups, fractions,
#' regulatory classifications, preferred plotting labels, and any future
#' metadata stored in \code{analyte_tbl}.
#'
#' The join is performed on \code{.analyte_id}, allowing analyte naming
#' conventions to evolve independently of downstream analyses.
#'
#' @param df An \code{env_schema} object containing a
#'   \code{.analyte_id} column.
#' @param lookup An analyte metadata table, typically
#'   \code{analyte_tbl}.
#'
#' @returns An enriched \code{env_schema} object containing all original
#'   observations plus analyte metadata columns from \code{lookup}.
#'
#' @seealso [analyte_tbl], [validate_analyte_tbl()]
#'
#' @keywords internal
.attach_analyte_metadata <- function(
  df,
  lookup = analyte_tbl
) {
  validate_analyte_tbl(lookup)

  lookup <- lookup |>
    dplyr::rename(
      .analyte_id = analyte_id,
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
    ) |>
    dplyr::select(
      -dplyr::any_of("analyte")
    )

  dplyr::left_join(
    df,
    lookup,
    by = ".analyte_id",
    relationship = "many-to-one"
  )
}

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

  out <- df |>
    .resolve_analyte_column(
      analyte_col = !!rlang::sym(analyte_col),
      alias_lookup = alias_lookup,
      analyte_lookup = analyte_lookup
    )

  .warn_unsupported_analytes(
    out
  )

  out |>
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
  ) |>
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
  ) |>
    dplyr::mutate(
      .analyte = .resolved_analyte
    ) |>
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
  ) |>
    dplyr::left_join(
      alias_lookup,
      by = "alias",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      fraction = dplyr::coalesce(
        fraction,
        .extract_fraction(source_analyte)
      )
    ) |>
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

  resolved |>
    dplyr::left_join(
      analyte_lookup |>
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

#' Build analyte display labels
#'
#' @keywords internal
.build_analyte_labels <- function(df) {
  if (!".analysis_unit" %in% names(df)) {
    return(df)
  }

  analyte_name <- if (".normalized_analyte" %in% names(df)) {
    dplyr::coalesce(
      df$.normalized_analyte,
      df$.source_analyte
    )
  } else {
    df$.source_analyte
  }

  short_name <- if (".short_name" %in% names(df)) {
    dplyr::coalesce(
      df$.short_name,
      analyte_name
    )
  } else {
    analyte_name
  }

  has_unit <- !is.na(df$.analysis_unit) &
    trimws(df$.analysis_unit) != ""

  analyte_label <- dplyr::if_else(
    has_unit,
    paste0(
      analyte_name,
      " (",
      df$.analysis_unit,
      ")"
    ),
    analyte_name
  )

  short_analyte_label <- dplyr::if_else(
    has_unit,
    paste0(
      short_name,
      " (",
      df$.analysis_unit,
      ")"
    ),
    short_name
  )

  df |>
    dplyr::mutate(
      .analyte_label = analyte_label,
      .short_analyte_label = short_analyte_label
    )
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

  unsupported <- x |>
    dplyr::filter(
      !.data[[known_col]]
    ) |>
    dplyr::distinct(
      .data[[source_col]]
    )

  if (nrow(unsupported) > 0) {
    cli::cli_warn(
      c(
        "{nrow(unsupported)} analytes were not found in analyte metadata.",
        "i" = paste(
          unsupported[[source_col]] |>
            sort() |>
            head(10)
        ),
        collapse = ", "
      ),
      "i" = paste(
        "Analyte metadata, standard-unit conversion,",
        "and analyte-specific calculations were skipped."
      )
    )
  }

  invisible(x)
}

#' Generate fraction-specific analyte aliases
#'
#' Creates common environmental laboratory reporting variants for
#' analyte names by appending fraction descriptors.
#'
#' @param x Character vector of analyte names or abbreviations.
#'
#' @returns A character vector containing the original values and
#' common fraction-specific variants.
#'
#' @details
#' The following suffixes are generated:
#'
#' - `"dissolved"`
#' - `"total"`
#' - `"filtered"`
#' - `"unfiltered"`
#'
#' This helper is primarily used when constructing
#' [analyte_tbl] and [analyte_alias_lookup].
#'
#' @examples
#' .add_fraction_aliases(c("ca", "calcium"))
#'
#' # Returns aliases such as:
#' # "ca"
#' # "ca dissolved"
#' # "ca total"
#' # "calcium dissolved"
#'
#' @keywords internal
.add_fraction_aliases <- function(
  alias,
  fractions = c(
    "dissolved",
    "total",
    "filtered",
    "unfiltered"
  )
) {
  stopifnot(
    length(alias) == 1L
  )

  tibble::tibble(
    alias = c(
      alias,
      paste(alias, fractions)
    ),

    fraction = c(
      NA_character_,
      fractions
    )
  )
}

#' Build analyte alias lookup table
#'
#' Expands analyte aliases into a normalized lookup table used for
#' analyte standardization and metadata assignment.
#'
#' Each analyte alias is associated with a canonical `analyte_id`
#' and analyte name. Aliases are normalized using
#' `.normalize_analyte_text()` and validated to ensure that a single
#' alias does not map to more than one analyte.
#'
#' The resulting lookup table is used internally by functions that
#' standardize analyte names and attach analyte metadata to
#' environmental chemistry data.
#'
#' @param lookup An analyte metadata table, typically
#'   [analyte_tbl].
#'
#' @returns A tibble with columns:
#' \describe{
#'   \item{alias}{Normalized analyte alias.}
#'   \item{analyte_id}{Canonical analyte identifier.}
#'   \item{analyte}{Canonical analyte name.}
#' }
#'
#' @details
#' Aliases are expanded from the `aliases`
#' [analyte_alias_tbl]. Aliases are normalized using
#' `.normalize_analyte_text()`, and duplicate aliases mapping
#' to multiple analytes are not permitted.
#'
#' Alias conflicts generate an error to ensure that analyte
#' standardization remains deterministic.
#'
#' The lookup includes both primary analyte names and user-facing
#' aliases such as abbreviations, common synonyms, and fraction-
#' specific forms (for example, "calcium dissolved",
#' "calcium total", and "ca").
#'
#' @seealso
#' [analyte_tbl],
#' [`.normalize_analyte_text`()]
#'
#' @keywords internal
.build_alias_lookup <- function(
  analyte_tbl = analyte_tbl,
  analyte_alias_tbl = analyte_alias_tbl
) {
  validate_analyte_tbl(
    analyte_tbl
  )

  validate_analyte_alias_tbl(
    analyte_alias_tbl,
    analyte_tbl
  )

  expanded <- purrr::map2_dfr(
    analyte_alias_tbl$analyte_id,
    analyte_alias_tbl$alias,
    ~ .add_fraction_aliases(.y) |>
      dplyr::mutate(
        analyte_id = .x
      )
  )

  expanded |>
    dplyr::mutate(
      alias = .normalize_analyte_text(
        alias
      )
    ) |>
    dplyr::left_join(
      analyte_tbl |>
        dplyr::select(
          analyte_id,
          analyte,
          normalized_analyte
        ),
      by = "analyte_id",
      relationship = "many-to-one"
    ) |>
    dplyr::distinct(
      alias,
      .keep_all = TRUE
    ) |>
    dplyr::arrange(
      analyte_id,
      alias
    )
}
