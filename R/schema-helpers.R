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

  lookup <- lookup %>%
    dplyr::rename(
      .analyte_id = analyte_id
    ) %>%
    dplyr::select(
      -.data$analyte
    )

  dplyr::left_join(
    df,
    lookup,
    by = ".analyte_id",
    relationship = "many-to-one"
  )
}

#' Build analyte display labels
#'
#' Creates formatted analyte labels for plotting, reporting, and
#' tabular output by combining analyte names with analytical units.
#'
#' Two label columns are generated:
#'
#' \itemize{
#'   \item \code{.analyte_label}:
#'   Canonical analyte name and unit.
#'   Example: \code{"Benzene (ug/L)"}.
#'
#'   \item \code{.short_analyte_label}:
#'   Preferred short analyte name and unit when available.
#'   Example: \code{"Benzene (µg/L)"}.
#' }
#'
#' If no \code{.analysis_unit} column exists, the input object is
#' returned unchanged.
#'
#' @param df An \code{env_schema} object.
#'
#' @returns The input \code{env_schema} with additional display-label
#'   columns.
#'
#' @keywords internal
.build_analyte_labels <- function(df) {
  if (!".analysis_unit" %in% names(df)) {
    return(df)
  }

  short_names <- if (".short_name" %in% names(df)) {
    dplyr::coalesce(
      df$.short_name,
      df$.analyte
    )
  } else {
    df$.analyte
  }

  df %>%
    dplyr::mutate(
      .analyte_label = paste0(
        .analyte,
        " (",
        .analysis_unit,
        ")"
      ),

      .short_analyte_label = paste0(
        short_names,
        " (",
        .analysis_unit,
        ")"
      )
    )
}

#' Copy env_schema attributes
#'
#' Copies schema metadata and custom attributes from one object to
#' another while preserving the target object's row names and column
#' names.
#'
#' This helper is primarily used by schema transformation functions that
#' create new data frames internally but should retain the original
#' \code{env_schema} attributes, including schema mappings, version
#' information, and other metadata stored as attributes.
#'
#' Standard data frame structural attributes such as \code{names} and
#' \code{row.names} are intentionally preserved from the target object.
#'
#' @param from Source object containing attributes to copy.
#' @param to Target object receiving attributes.
#'
#' @returns The \code{to} object with attributes inherited from
#'   \code{from}.
#'
#' @keywords internal
.copy_env_schema_attributes <- function(
  from,
  to
) {
  protected <- c(
    "names",
    "row.names"
  )

  atts <- attributes(from)

  atts <- atts[
    !names(atts) %in% protected
  ]

  attributes(to) <- utils::modifyList(
    attributes(to),
    atts
  )

  to
}
