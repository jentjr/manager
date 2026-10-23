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
