#' Return schema creation timestamp
#'
#' @param x An env_schema object.
#'
#' @returns POSIXct timestamp.
#'
#' @export
schema_created <- function(x) {
  if (!inherits(x, "env_schema")) {
    rlang::abort(
      "Object must inherit from 'env_schema'."
    )
  }

  attr(
    x,
    "env_schema_created",
    exact = TRUE
  )
}

#' Return metadata column names
#'
#' @param x An env_schema object.
#'
#' @returns Character vector.
#'
#' @export
schema_metadata <- function(x) {
  if (!inherits(x, "env_schema")) {
    rlang::abort(
      "Object must inherit from 'env_schema'."
    )
  }

  meta <- attr(
    x,
    "measurement_metadata",
    exact = TRUE
  )

  if (is.null(meta)) {
    return(character())
  }

  meta
}

#' Return original column mappings
#'
#' @param x An env_schema object.
#'
#' @returns Named list.
#'
#' @export
schema_mapping <- function(x) {
  if (!inherits(x, "env_schema")) {
    rlang::abort(
      "Object must inherit from 'env_schema'."
    )
  }

  attr(
    x,
    "env_mapping",
    exact = TRUE
  )
}

#' Return schema version
#'
#' @param x An env_schema object.
#'
#' @returns Character scalar.
#'
#' @export
schema_version <- function(x) {
  if (!inherits(x, "env_schema")) {
    rlang::abort(
      "Object must inherit from 'env_schema'."
    )
  }

  attr(
    x,
    "env_schema_version",
    exact = TRUE
  )
}

#' Return result classification counts
#'
#' @param x env_schema object.
#'
#' @returns Tibble.
#'
#' @export
schema_result_types <- function(x) {
  if (!inherits(x, "env_schema")) {
    rlang::abort(
      "Object must inherit from 'env_schema'."
    )
  }

  x %>%
    dplyr::count(
      .result_type,
      sort = TRUE
    )
}

location_var <- function(x) {
  validate_env_schema(x)
  ".location"
}

analyte_var <- function(x) {
  validate_env_schema(x)
  ".analyte"
}

result_var <- function(x) {
  validate_env_schema(x)
  ".analysis_result"
}

reporting_flag_var <- function(x) {
  validate_env_schema(x)
  ".reporting_flag"
}
