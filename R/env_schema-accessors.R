#' Return schema creation timestamp
#'
#' @param x An env_schema object.
#'
#' @returns POSIXct timestamp.
#'
#' @export
schema_created <- function(x) {
  validate_env_schema(x)

  attr(
    x,
    "env_schema_created",
    exact = TRUE
  )
}

#' Check if an object is an environmental schema
#'
#' @param x Object to check.
#'
#' @returns Logical scalar.
#'
#' @export
is_env_schema <- function(x) {
  inherits(x, "env_schema")
}

#' Return metadata column names
#'
#' @param x An env_schema object.
#'
#' @returns Character vector.
#'
#' @export
schema_metadata <- function(x) {
  validate_env_schema(x)

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
  validate_env_schema(x)

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
  validate_env_schema(x)

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
  validate_env_schema(x)

  x %>%
    dplyr::count(
      .result_type,
      sort = TRUE
    )
}
