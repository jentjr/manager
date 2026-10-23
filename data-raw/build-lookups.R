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
.add_fraction_aliases <- function(x) {
  c(
    x,
    paste(x, "dissolved"),
    paste(x, "total"),
    paste(x, "filtered"),
    paste(x, "unfiltered")
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
#' Aliases are expanded from the `aliases` list-column in
#' [analyte_tbl]. Aliases are normalized using
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
  lookup = analyte_tbl
) {
  validate_analyte_tbl(lookup)

  alias_lookup <- purrr::pmap_dfr(
    list(
      analyte_id = lookup$analyte_id,
      analyte = lookup$analyte,
      aliases = lookup$aliases
    ),
    \(analyte_id, analyte, aliases) {
      tibble::tibble(
        alias = c(analyte, aliases),
        analyte_id = analyte_id,
        analyte = analyte
      )
    }
  ) %>%
    dplyr::mutate(
      alias = .normalize_analyte_text(alias)
    ) %>%
    dplyr::filter(
      !is.na(alias),
      alias != ""
    ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(
      analyte_id,
      alias
    )

  alias_conflicts <- alias_lookup %>%
    dplyr::summarise(
      analytes = paste(
        sort(unique(analyte_id)),
        collapse = ", "
      ),
      n_analytes = dplyr::n_distinct(analyte_id),
      .by = alias
    ) %>%
    dplyr::filter(n_analytes > 1)

  if (nrow(alias_conflicts) > 0) {
    cli::cli_abort(
      c(
        "Alias conflicts detected.",
        setNames(
          paste0(
            alias_conflicts$alias,
            " -> ",
            alias_conflicts$analytes
          ),
          rep("x", nrow(alias_conflicts))
        )
      )
    )
  }

  alias_lookup
}

#' Analyte alias lookup table
#'
#' Derived from analyte_tbl.
#'
#' @noRd
analyte_alias_lookup <- .build_alias_lookup(analyte_tbl)
