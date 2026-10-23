#' Coerce to standards schema
#'
#' @param x Object to coerce.
#'
#' @returns
#' A standards_schema object.
#'
#' @export
as_standards_schema <- function(x) {
  if (is_standards_schema(x)) {
    return(x)
  }

  required_cols <- c(
    ".analyte",
    ".standard_type",
    ".standard_value",
    ".standard_unit"
  )

  missing_cols <- setdiff(
    required_cols,
    names(x)
  )

  if (length(missing_cols) > 0) {
    rlang::abort(
      paste(
        "Cannot coerce to standards_schema. Missing columns:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  if (!".location" %in% names(x)) {
    x$.location <- NA_character_
  }

  if (!".medium" %in% names(x)) {
    x$.medium <- NA_character_
  }

  if (!".source" %in% names(x)) {
    x$.source <- NA_character_
  }

  if (!".priority" %in% names(x)) {
    x$.priority <- ifelse(
      is.na(x$.location),
      100L,
      1L
    )
  }

  class(x) <- c(
    "standards_schema",
    class(x)
  )

  validate_standards_schema(x)

  x
}
