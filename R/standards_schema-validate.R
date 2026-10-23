#' Validate a standards schema
#'
#' @param x Object to validate.
#'
#' @returns Invisibly returns TRUE.
#'
#' @export
validate_standards_schema <- function(x) {
  if (!is_standards_schema(x)) {
    rlang::abort(
      "Object is not a standards_schema."
    )
  }

  required_cols <- c(
    ".analyte",
    ".standard_type",
    ".standard_value",
    ".standard_unit",
    ".location",
    ".medium",
    ".source",
    ".provenance",
    ".reference",
    ".effective_date",
    ".notes",
    ".priority"
  )

  missing_cols <- setdiff(
    required_cols,
    names(x)
  )

  if (length(missing_cols) > 0) {
    rlang::abort(
      paste(
        "Missing columns:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  if (!is.numeric(x$.standard_value)) {
    rlang::abort(
      "`.standard_value` must be numeric."
    )
  }

  if (!is.numeric(x$.priority)) {
    rlang::abort(
      "`.priority` must be numeric."
    )
  }

  if (any(is.na(x$.analyte))) {
    rlang::abort(
      "`.analyte` contains missing values."
    )
  }

  if (any(is.na(x$.standard_value))) {
    rlang::abort(
      "`.standard_value` contains missing values."
    )
  }

  if (any(is.na(x$.standard_unit))) {
    rlang::abort(
      "`.standard_unit` contains missing values."
    )
  }

  if (any(is.na(x$.priority))) {
    rlang::abort(
      "`.priority` contains missing values."
    )
  }

  invisible(TRUE)
}
