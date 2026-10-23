#' Validate an environmental schema
#'
#' @param x Object to validate.
#'
#' @returns Invisibly returns TRUE.
#'
#' @export
validate_env_schema <- function(x) {
  if (!is_env_schema(x)) {
    rlang::abort(
      "Object must inherit from 'env_schema'."
    )
  }

  required <- c(
    ".location",
    ".date",
    ".analyte_id",
    ".analyte",
    ".original_lab_result",
    ".analysis_result",
    ".original_lab_unit",
    ".analysis_unit"
  )

  missing <- setdiff(
    required,
    names(x)
  )

  if (length(missing) > 0) {
    rlang::abort(
      paste(
        "Missing required schema columns:",
        paste(missing, collapse = ", ")
      )
    )
  }

  if (!is.character(x$.analyte_id)) {
    rlang::abort(
      "'.analyte_id' must be character."
    )
  }

  if (!is.character(x$.location)) {
    rlang::abort(
      "'.location' must be character."
    )
  }

  if (!is.character(x$.analyte)) {
    rlang::abort(
      "'.analyte' must be character."
    )
  }

  if (
    !inherits(
      x$.date,
      c("Date", "POSIXct", "POSIXt")
    )
  ) {
    rlang::abort(
      "'.date' must be a Date or POSIXct column."
    )
  }

  if (!is.numeric(x$.original_lab_result)) {
    rlang::abort(
      "'.original_lab_result' must be numeric."
    )
  }

  if (!is.numeric(x$.analysis_result)) {
    rlang::abort(
      "'.analysis_result' must be numeric."
    )
  }

  if (".charge" %in% names(x) && !is.numeric(x$.charge)) {
    rlang::abort(
      "'.charge' must be numeric."
    )
  }

  if (".molecular_weight" %in% names(x) && !is.numeric(x$.molecular_weight)) {
    rlang::abort(
      "'.molecular_weight' must be numeric."
    )
  }

  if (".meq_factor" %in% names(x) && !is.numeric(x$.meq_factor)) {
    rlang::abort(
      "'.meq_factor' must be numeric."
    )
  }

  if (
    is.null(
      attr(
        x,
        "env_schema_version"
      )
    )
  ) {
    rlang::abort(
      "Missing env_schema_version attribute."
    )
  }

  invisible(TRUE)
}
