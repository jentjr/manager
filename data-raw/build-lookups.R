.add_fraction_aliases <- function(x) {
  c(
    x,
    paste(x, "dissolved"),
    paste(x, "total"),
    paste(x, "filtered"),
    paste(x, "unfiltered")
  )
}

.build_alias_lookup <- function(
  lookup = analyte_tbl
) {
  purrr::map2_dfr(
    lookup$analyte,
    lookup$aliases,
    \(analyte, aliases) {
      tibble::tibble(
        alias = c(analyte, aliases),
        analyte = analyte
      )
    }
  ) %>%
    dplyr::mutate(
      alias = .normalize_analyte_text(alias)
    ) %>%
    dplyr::distinct()
}

#' Analyte alias lookup table
#'
#' Derived from analyte_tbl.
#'
#' @noRd
analyte_alias_lookup <- .build_alias_lookup(analyte_tbl)
