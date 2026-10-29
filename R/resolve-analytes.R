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

#' Resolved analyte names
#'
#' @noRd
.resolve_analyte_name <- function(
  x,
  analyte_matching = c(
    "normalized",
    "strict",
    "exact"
  ),
  alias_lookup = analyte_alias_lookup,
  analyte_lookup = analyte_tbl
) {
  analyte_matching <-
    rlang::arg_match(
      analyte_matching
    )

  resolved <- .resolve_analyte(
    x,
    alias_lookup = alias_lookup,
    analyte_lookup = analyte_lookup
  )

  switch(
    analyte_matching,

    exact = as.character(x),

    strict = .get_analyte_key(
      resolved,
      analyte_matching = "strict"
    ),

    normalized = resolved$normalized_analyte
  )
}

#' Get analyte key
#'
#' @noRd
.get_analyte_key <- function(
  data,
  analyte_matching = c(
    "strict",
    "exact",
    "normalized"
  )
) {
  analyte_matching <-
    rlang::arg_match(
      analyte_matching
    )

  switch(
    analyte_matching,

    exact = data$analyte,

    normalized = data$normalized_analyte,

    strict = paste(
      data$normalized_analyte,
      dplyr::coalesce(
        data$fraction,
        "unspecified"
      ),
      sep = "::"
    )
  )
}

#' Get schema analyte key
#'
#' @noRd
.get_schema_analyte_key <- function(
  data,
  analyte_matching = c(
    "strict",
    "exact",
    "normalized"
  )
) {
  analyte_matching <-
    rlang::arg_match(
      analyte_matching
    )

  switch(
    analyte_matching,

    exact = data$.analyte,

    normalized = data$.normalized_analyte,

    strict = paste(
      data$.normalized_analyte,
      dplyr::coalesce(
        data$.fraction,
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
