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
  output_col = "analyte_resolved",
  analyte_matching = c(
    "normalized",
    "strict",
    "exact"
  ),
  alias_lookup = analyte_alias_lookup
) {
  analyte_matching <- rlang::arg_match(analyte_matching)

  analyte_col <- rlang::ensym(analyte_col)

  output_col <- rlang::sym(output_col)

  data %>%
    dplyr::mutate(
      !!output_col := .resolve_analyte_name(
        !!analyte_col,
        analyte_matching = analyte_matching,
        alias_lookup = alias_lookup
      )
    )
}
#' Resolve analyte names
#'
#' @param x Character vector of analyte names.
#' @param analyte_matching Matching strategy.
#' @param alias_lookup Alias lookup table.
#'
#' @return Character vector of resolved analyte names.
#'
#' @noRd
.resolve_analyte_name <- function(
  x,
  analyte_matching = c(
    "normalized",
    "strict",
    "exact"
  ),
  alias_lookup = analyte_alias_lookup
) {
  analyte_matching <- rlang::arg_match(analyte_matching)

  x <- as.character(x)

  #--------------------------------------------------------------
  # EXACT
  #--------------------------------------------------------------
  if (analyte_matching == "exact") {
    return(x)
  }

  #--------------------------------------------------------------
  # NORMALIZE TEXT
  #--------------------------------------------------------------
  normalized <- .normalize_analyte_text(x)

  #--------------------------------------------------------------
  # STRICT
  #--------------------------------------------------------------
  if (analyte_matching == "strict") {
    return(normalized)
  }

  #--------------------------------------------------------------
  # NORMALIZED
  #--------------------------------------------------------------
  normalized <- stringr::str_replace_all(
    normalized,
    "\\b(dissolved|total|filtered|unfiltered|lab|field)\\b",
    " "
  )

  normalized <- stringr::str_squish(normalized)

  matched <- alias_lookup$analyte[
    match(
      normalized,
      alias_lookup$alias
    )
  ]

  dplyr::coalesce(
    matched,
    normalized
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
