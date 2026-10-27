# -------------------------------------------------------------------------
# Internal attribute helpers
# -------------------------------------------------------------------------

get_env_mapping <- function(x) {
  attr(
    x,
    "env_mapping",
    exact = TRUE
  )
}

get_schema_metadata <- function(x) {
  attr(
    x,
    "schema_metadata",
    exact = TRUE
  )
}

get_measurement_metadata <- function(x) {
  attr(
    x,
    "measurement_metadata",
    exact = TRUE
  )
}

get_schema_version <- function(x) {
  attr(
    x,
    "env_schema_version",
    exact = TRUE
  )
}

get_schema_created <- function(x) {
  attr(
    x,
    "env_schema_created",
    exact = TRUE
  )
}

# -------------------------------------------------------------------------
# Schema metadata accessors
# -------------------------------------------------------------------------

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

#' Return schema metadata
#'
#' @param x An env_schema object.
#'
#' @returns Character vector.
#'
#' @export
schema_metadata <- function(x) {
  if (!is_env_schema(x)) {
    rlang::abort(
      "Object must inherit from 'env_schema'."
    )
  }

  get_schema_metadata(x)
}

#' Return measurement metadata
#'
#' @param x An env_schema object.
#'
#' @returns Character vector.
#'
#' @export
measurement_metadata <- function(x) {
  if (!is_env_schema(x)) {
    rlang::abort(
      "Object must inherit from 'env_schema'."
    )
  }

  meta <- get_measurement_metadata(x)

  if (is.null(meta)) {
    return(character())
  }

  meta
}

#' Return schema column mappings
#'
#' @param x An env_schema object.
#'
#' @returns Named list.
#'
#' @export
schema_mapping <- function(x) {
  if (!is_env_schema(x)) {
    rlang::abort(
      "Object must inherit from 'env_schema'."
    )
  }

  get_env_mapping(x)
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
#' @param x An env_schema object.
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

# -------------------------------------------------------------------------
# Internal helpers
# -------------------------------------------------------------------------

#' @keywords internal
.get_mapped_column <- function(
  x,
  column
) {
  if (is.null(column)) {
    return(NULL)
  }

  x[[column]]
}

# -------------------------------------------------------------------------
# Internal mapping accessors
# -------------------------------------------------------------------------

location_var <- function(x) {
  get_env_mapping(x)$location
}

date_var <- function(x) {
  get_env_mapping(x)$date
}

analyte_var <- function(x) {
  get_env_mapping(x)$analyte
}

result_var <- function(x) {
  get_env_mapping(x)$result
}

unit_var <- function(x) {
  get_env_mapping(x)$unit
}

sample_id_var <- function(x) {
  get_env_mapping(x)$sample_id
}

reporting_flag_var <- function(x) {
  get_env_mapping(x)$reporting_flag
}

reporting_limit_var <- function(x) {
  get_env_mapping(x)$reporting_limit
}

detection_limit_var <- function(x) {
  get_env_mapping(x)$detection_limit
}

quantitation_limit_var <- function(x) {
  get_env_mapping(x)$quantitation_limit
}

achieved_mdc_var <- function(x) {
  get_env_mapping(x)$achieved_mdc
}

method_var <- function(x) {
  get_env_mapping(x)$method
}

# -------------------------------------------------------------------------
# Source-column accessor generics
# -------------------------------------------------------------------------

#' Return location values
#'
#' @param x Object.
#'
#' @export
location <- function(x) {
  UseMethod("location")
}

#' Return sample dates
#'
#' @param x Object.
#'
#' @export
date <- function(x) {
  UseMethod("date")
}

#' Return analyte values
#'
#' @param x Object.
#'
#' @export
analyte <- function(x) {
  UseMethod("analyte")
}

#' Return original result values
#'
#' @param x Object.
#'
#' @export
result <- function(x) {
  UseMethod("result")
}

#' Return original analytical units
#'
#' @param x Object.
#'
#' @export
unit <- function(x) {
  UseMethod("unit")
}

#' Return sample identifiers
#'
#' @param x Object.
#'
#' @export
sample_id <- function(x) {
  UseMethod("sample_id")
}

#' Return reporting qualifiers
#'
#' @param x Object.
#'
#' @export
reporting_flag <- function(x) {
  UseMethod("reporting_flag")
}

#' Return reporting limits
#'
#' @param x Object.
#'
#' @export
reporting_limit <- function(x) {
  UseMethod("reporting_limit")
}

#' Return detection limits
#'
#' @param x Object.
#'
#' @export
detection_limit <- function(x) {
  UseMethod("detection_limit")
}

#' Return quantitation limits
#'
#' @param x Object.
#'
#' @export
quantitation_limit <- function(x) {
  UseMethod("quantitation_limit")
}

#' Return achieved minimum detectable concentrations
#'
#' @param x Object.
#'
#' @export
achieved_mdc <- function(x) {
  UseMethod("achieved_mdc")
}

#' Return analytical methods
#'
#' @param x Object.
#'
#' @export
method <- function(x) {
  UseMethod("method")
}

# -------------------------------------------------------------------------
# Standardized manager-column accessor generics
# -------------------------------------------------------------------------

#' Return standardized analytical results
#'
#' @param x Object.
#'
#' @export
analysis_result <- function(x) {
  UseMethod("analysis_result")
}

#' Return standardized analytical units
#'
#' @param x Object.
#'
#' @export
analysis_unit <- function(x) {
  UseMethod("analysis_unit")
}

#' Return standardized analytical methods
#'
#' @param x Object.
#'
#' @export
analysis_method <- function(x) {
  UseMethod("analysis_method")
}

#' Return detection flags
#'
#' @param x Object.
#'
#' @export
detected <- function(x) {
  UseMethod("detected")
}

#' Return censoring flags
#'
#' @param x Object.
#'
#' @export
censored <- function(x) {
  UseMethod("censored")
}

#' Return estimation flags
#'
#' @param x Object.
#'
#' @export
estimated <- function(x) {
  UseMethod("estimated")
}

#' Return rejection flags
#'
#' @param x Object.
#'
#' @export
rejected <- function(x) {
  UseMethod("rejected")
}

#' Return usability flags
#'
#' @param x Object.
#'
#' @export
usable <- function(x) {
  UseMethod("usable")
}

# -------------------------------------------------------------------------
# env_schema source-column methods
# -------------------------------------------------------------------------

#' @export
location.env_schema <- function(x) {
  .get_mapped_column(
    x,
    location_var(x)
  )
}

#' @export
date.env_schema <- function(x) {
  .get_mapped_column(
    x,
    date_var(x)
  )
}

#' @export
analyte.env_schema <- function(x) {
  .get_mapped_column(
    x,
    analyte_var(x)
  )
}

#' @export
result.env_schema <- function(x) {
  .get_mapped_column(
    x,
    result_var(x)
  )
}

#' @export
unit.env_schema <- function(x) {
  .get_mapped_column(
    x,
    unit_var(x)
  )
}

#' @export
sample_id.env_schema <- function(x) {
  .get_mapped_column(
    x,
    sample_id_var(x)
  )
}

#' @export
reporting_flag.env_schema <- function(x) {
  .get_mapped_column(
    x,
    reporting_flag_var(x)
  )
}

#' @export
reporting_limit.env_schema <- function(x) {
  .get_mapped_column(
    x,
    reporting_limit_var(x)
  )
}

#' @export
detection_limit.env_schema <- function(x) {
  .get_mapped_column(
    x,
    detection_limit_var(x)
  )
}

#' @export
quantitation_limit.env_schema <- function(x) {
  .get_mapped_column(
    x,
    quantitation_limit_var(x)
  )
}

#' @export
achieved_mdc.env_schema <- function(x) {
  .get_mapped_column(
    x,
    achieved_mdc_var(x)
  )
}

#' @export
method.env_schema <- function(x) {
  .get_mapped_column(
    x,
    method_var(x)
  )
}

# -------------------------------------------------------------------------
# env_schema standardized-column methods
# -------------------------------------------------------------------------

#' @export
analysis_result.env_schema <- function(x) {
  x$.analysis_result
}

#' @export
analysis_unit.env_schema <- function(x) {
  x$.analysis_unit
}

#' @export
analysis_method.env_schema <- function(x) {
  x$.analysis_method
}

#' @export
detected.env_schema <- function(x) {
  x$.detected
}

#' @export
censored.env_schema <- function(x) {
  x$.censored
}

#' @export
estimated.env_schema <- function(x) {
  x$.estimated
}

#' @export
rejected.env_schema <- function(x) {
  x$.rejected
}

#' @export
usable.env_schema <- function(x) {
  x$.usable
}

# -------------------------------------------------------------------------
# Default methods
# -------------------------------------------------------------------------

.accessor_not_implemented <- function(
  generic,
  x
) {
  rlang::abort(
    paste0(
      "No ",
      generic,
      "() method for class: ",
      paste(class(x), collapse = ", ")
    )
  )
}

#' @export
location.default <- function(x) {
  .accessor_not_implemented("location", x)
}

#' @export
date.default <- function(x) {
  .accessor_not_implemented("date", x)
}

#' @export
analyte.default <- function(x) {
  .accessor_not_implemented("analyte", x)
}

#' @export
result.default <- function(x) {
  .accessor_not_implemented("result", x)
}

#' @export
unit.default <- function(x) {
  .accessor_not_implemented("unit", x)
}

#' @export
sample_id.default <- function(x) {
  .accessor_not_implemented("sample_id", x)
}

#' @export
reporting_flag.default <- function(x) {
  .accessor_not_implemented("reporting_flag", x)
}

#' @export
reporting_limit.default <- function(x) {
  .accessor_not_implemented("reporting_limit", x)
}

#' @export
detection_limit.default <- function(x) {
  .accessor_not_implemented("detection_limit", x)
}

#' @export
quantitation_limit.default <- function(x) {
  .accessor_not_implemented("quantitation_limit", x)
}

#' @export
achieved_mdc.default <- function(x) {
  .accessor_not_implemented("achieved_mdc", x)
}

#' @export
method.default <- function(x) {
  .accessor_not_implemented("method", x)
}

#' @export
analysis_result.default <- function(x) {
  .accessor_not_implemented("analysis_result", x)
}

#' @export
analysis_unit.default <- function(x) {
  .accessor_not_implemented("analysis_unit", x)
}

#' @export
analysis_method.default <- function(x) {
  .accessor_not_implemented("analysis_method", x)
}

#' @export
detected.default <- function(x) {
  .accessor_not_implemented("detected", x)
}

#' @export
censored.default <- function(x) {
  .accessor_not_implemented("censored", x)
}

#' @export
estimated.default <- function(x) {
  .accessor_not_implemented("estimated", x)
}

#' @export
rejected.default <- function(x) {
  .accessor_not_implemented("rejected", x)
}

#' @export
usable.default <- function(x) {
  .accessor_not_implemented("usable", x)
}
