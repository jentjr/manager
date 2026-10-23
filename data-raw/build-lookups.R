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

.extract_fraction <- function(x) {
  x <- .normalize_analyte_text(x)

  dplyr::case_when(
    stringr::str_detect(
      x,
      "\\bdis\\b|\\bdissolved\\b"
    ) ~ "dissolved",

    stringr::str_detect(
      x,
      "\\btotal\\b"
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
    ~ .add_fraction_aliases(.y) %>%
      dplyr::mutate(
        analyte_id = .x
      )
  )

  expanded %>%
    dplyr::mutate(
      alias = .normalize_analyte_text(
        alias
      )
    ) %>%
    dplyr::left_join(
      analyte_tbl %>%
        dplyr::select(
          analyte_id,
          analyte,
          normalized_analyte
        ),
      by = "analyte_id",
      relationship = "many-to-one"
    ) %>%
    dplyr::distinct(
      alias,
      .keep_all = TRUE
    ) %>%
    dplyr::arrange(
      analyte_id,
      alias
    )
}
