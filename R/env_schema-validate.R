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

  mapping <- get_env_mapping(x)

  required <- c(
    mapping$location,
    mapping$date,
    mapping$analyte,
    mapping$result
  )

  required <- stats::na.omit(required)

  missing <- setdiff(
    required,
    names(x)
  )

  if (length(missing) > 0) {
    rlang::abort(
      paste(
        "Missing mapped columns:",
        paste(missing, collapse = ", ")
      )
    )
  }

  # ------------------------------------------------------------
  # Required column types
  # ------------------------------------------------------------

  if (!".analyte_id" %in% names(x)) {
    rlang::abort(
      "Missing '.analyte_id'."
    )
  }

  if (!".analyte_id" %in% names(x)) {
    rlang::abort(
      "Missing required derived column '.analyte_id'."
    )
  }

  if (!is.character(x$.analyte_id)) {
    rlang::abort(
      "'.analyte_id' must be character."
    )
  }

  if (!is.character(location(x))) {
    rlang::abort(
      "Location column must be character."
    )
  }

  if (!is.character(analyte(x))) {
    rlang::abort(
      "'Analyte' must be character."
    )
  }

  if (
    !inherits(
      date(x),
      c(
        "Date",
        "POSIXct",
        "POSIXt"
      )
    )
  ) {
    rlang::abort(
      "'date' must be a Date or POSIXct column."
    )
  }

  if (!is.numeric(x$.original_lab_result)) {
    rlang::abort(
      "'.original_lab_result' must be numeric."
    )
  }

  if (!is.numeric(result(x))) {
    rlang::abort(
      "Result column must be numeric."
    )
  }

  # ------------------------------------------------------------
  # Optional environmental thresholds
  # ------------------------------------------------------------

  if (".reporting_limit" %in% names(x) && !is.numeric(x$.reporting_limit)) {
    rlang::abort(
      "'.reporting_limit' must be numeric."
    )
  }

  if (".detection_limit" %in% names(x) && !is.numeric(x$.detection_limit)) {
    rlang::abort(
      "'.detection_limit' must be numeric."
    )
  }

  if (
    ".quantitation_limit" %in% names(x) && !is.numeric(x$.quantitation_limit)
  ) {
    rlang::abort(
      "'.quantitation_limit' must be numeric."
    )
  }

  if (".achieved_mdc" %in% names(x) && !is.numeric(x$.achieved_mdc)) {
    rlang::abort(
      "'.achieved_mdc' must be numeric."
    )
  }

  # ------------------------------------------------------------
  # Result classification columns
  # ------------------------------------------------------------

  if (".detected" %in% names(x) && !is.logical(x$.detected)) {
    rlang::abort(
      "'.detected' must be logical."
    )
  }

  if (".censored" %in% names(x) && !is.logical(x$.censored)) {
    rlang::abort(
      "'.censored' must be logical."
    )
  }

  if (".estimated" %in% names(x) && !is.logical(x$.estimated)) {
    rlang::abort(
      "'.estimated' must be logical."
    )
  }

  if (".rejected" %in% names(x) && !is.logical(x$.rejected)) {
    rlang::abort(
      "'.rejected' must be logical."
    )
  }

  if (".usable" %in% names(x) && !is.logical(x$.usable)) {
    rlang::abort(
      "'.usable' must be logical."
    )
  }

  # ------------------------------------------------------------
  # Analyte metadata
  # ------------------------------------------------------------

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

  # ------------------------------------------------------------
  # Required attributes
  # ------------------------------------------------------------

  if (
    is.null(
      attr(
        x,
        "threshold_metadata"
      )
    )
  ) {
    rlang::abort(
      "Missing threshold_metadata attribute."
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

#' Validate minimum env_schema structure
#'
#' Used internally by vec_restore() and dplyr reconstruction.
#'
#' @keywords internal
validate_env_schema_structure <- function(x) {
  mapping <- attr(
    x,
    "env_mapping",
    exact = TRUE
  )

  if (is.null(mapping)) {
    rlang::abort(
      "Missing env_mapping attribute."
    )
  }

  required <- c(
    mapping$location,
    mapping$date,
    mapping$analyte,
    mapping$result
  )

  required <- stats::na.omit(required)

  missing <- setdiff(
    required,
    names(x)
  )

  if (length(missing) > 0) {
    rlang::abort(
      paste(
        "Missing mapped columns:",
        paste(missing, collapse = ", ")
      )
    )
  }
  invisible(TRUE)
}

#' Check if valid schema
#'
#' @keywords internal
is_valid_env_schema <- function(x) {
  if (!inherits(x, "env_schema")) {
    return(FALSE)
  }

  tryCatch(
    {
      validate_env_schema(x)
      TRUE
    },
    error = function(e) FALSE
  )
}

#' Check required env_schema columns structure
#'
#' @keywords internal
has_env_schema_structure <- function(x) {
  mapping <- attr(
    x,
    "env_mapping",
    exact = TRUE
  )

  if (is.null(mapping)) {
    return(FALSE)
  }

  required <- stats::na.omit(
    c(
      mapping$location,
      mapping$date,
      mapping$analyte,
      mapping$result
    )
  )

  required <- required[
    !is.null(required)
  ]

  all(required %in% names(x))
}
