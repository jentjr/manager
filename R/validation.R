#' Validate a scalar choice
#'
#' @keywords internal
.validate_choice <- function(
  x,
  choices,
  arg
) {
  if (
    !is.character(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !x %in% choices
  ) {
    rlang::abort(
      sprintf(
        "`%s` must be one of: %s.",
        arg,
        paste(choices, collapse = ", ")
      )
    )
  }

  invisible(x)
}

#' Validate a scalar logical flag
#'
#' @keywords internal
.check_flag <- function(
  x,
  name
) {
  if (
    !is.logical(x) ||
      length(x) != 1L ||
      is.na(x)
  ) {
    rlang::abort(
      sprintf(
        "%s must be TRUE or FALSE.",
        name
      )
    )
  }

  invisible(TRUE)
}

#' Validate required columns
#'
#' @keywords internal
.validate_required_cols <- function(
  data,
  cols
) {
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame.")
  }

  if (
    !is.character(cols) ||
      anyNA(cols) ||
      any(!nzchar(cols))
  ) {
    rlang::abort("`cols` must contain non-empty column names.")
  }

  missing_cols <- setdiff(cols, names(data))

  if (length(missing_cols) > 0L) {
    rlang::abort(
      paste0(
        "Missing required column(s): ",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  invisible(TRUE)
}

# Numeric values must be finite unless NA is explicitly allowed.
.validate_numeric_column <- function(
  x,
  name,
  allow_na = TRUE
) {
  if (!is.numeric(x)) {
    rlang::abort(
      sprintf("`%s` must be numeric.", name)
    )
  }

  if (!allow_na && anyNA(x)) {
    rlang::abort(
      sprintf("`%s` cannot contain NA values.", name)
    )
  }

  if (any(!is.na(x) & !is.finite(x))) {
    rlang::abort(
      sprintf("`%s` cannot contain Inf or NaN.", name)
    )
  }

  invisible(TRUE)
}

#' @keywords internal
.validate_logical_column <- function(
  x,
  name,
  allow_na = FALSE
) {
  if (!is.logical(x)) {
    rlang::abort(
      sprintf("'%s' must be logical.", name)
    )
  }

  if (!allow_na && anyNA(x)) {
    rlang::abort(
      sprintf("'%s' cannot contain NA values.", name)
    )
  }

  invisible(TRUE)
}

#' Validate display options
#'
#' @keywords internal
.validate_display_options <- function(x) {
  if (!is.list(x)) {
    rlang::abort("`display_options` must be a list.")
  }

  required <- c(
    "digits",
    "scientific",
    "trim_trailing_zeros",
    "censor_prefix",
    "report_style"
  )

  missing <- setdiff(required, names(x))

  if (length(missing) > 0L) {
    rlang::abort(
      paste0(
        "Missing display options: ",
        paste(missing, collapse = ", ")
      )
    )
  }

  if (
    !is.numeric(x$digits) ||
      length(x$digits) != 1L ||
      is.na(x$digits) ||
      !is.finite(x$digits) ||
      x$digits < 1 ||
      x$digits != as.integer(x$digits)
  ) {
    rlang::abort("`digits` must be a positive integer.")
  }

  for (option in c("scientific", "trim_trailing_zeros")) {
    value <- x[[option]]

    if (
      !is.logical(value) ||
        length(value) != 1L ||
        is.na(value)
    ) {
      rlang::abort(
        sprintf("`%s` must be TRUE or FALSE.", option)
      )
    }
  }

  if (
    !is.character(x$censor_prefix) ||
      length(x$censor_prefix) != 1L ||
      is.na(x$censor_prefix) ||
      !nzchar(x$censor_prefix)
  ) {
    rlang::abort(
      "`censor_prefix` must be a non-empty character value."
    )
  }

  if (
    !is.character(x$report_style) ||
      length(x$report_style) != 1L ||
      is.na(x$report_style) ||
      !x$report_style %in% c("standard", "verbose")
  ) {
    rlang::abort(
      "`report_style` must be either 'standard' or 'verbose'."
    )
  }

  invisible(TRUE)
}

#' Required derived env_schema columns
#'
#' @keywords internal
.required_env_schema_columns <- function() {
  c(
    ".analysis_result",
    ".analysis_unit",
    ".detected",
    ".censored",
    ".estimated",
    ".rejected",
    ".usable",
    ".original_lab_result",
    ".analyte_id"
  )
}

#' @keywords internal
.check_metadata_overlap <- function(...) {
  metadata <- list(...)

  if (length(metadata) == 0L) {
    return(invisible(TRUE))
  }

  for (i in seq_along(metadata)) {
    if (
      !is.character(metadata[[i]]) ||
        anyNA(metadata[[i]]) ||
        any(!nzchar(metadata[[i]]))
    ) {
      rlang::abort(
        "Metadata column assignments must be character vectors."
      )
    }
  }

  names_found <- unlist(metadata, use.names = FALSE)

  duplicated_names <- unique(
    names_found[duplicated(names_found)]
  )

  if (length(duplicated_names) > 0L) {
    rlang::abort(
      paste0(
        "Metadata columns assigned to multiple metadata categories: ",
        paste(duplicated_names, collapse = ", ")
      )
    )
  }

  invisible(TRUE)
}

#' @keywords internal
.validate_metadata_columns <- function(
  x,
  value,
  name
) {
  if (
    !is.character(value) ||
      anyNA(value) ||
      any(!nzchar(value)) ||
      anyDuplicated(value)
  ) {
    rlang::abort(
      sprintf(
        "`%s` must be a unique character vector of non-empty column names.",
        name
      )
    )
  }

  missing <- setdiff(value, names(x))

  if (length(missing) > 0L) {
    rlang::abort(
      paste0(
        "`",
        name,
        "` references missing column(s): ",
        paste(missing, collapse = ", ")
      )
    )
  }

  invisible(TRUE)
}

#' @keywords internal
.validate_schema_mapping <- function(
  mapping
) {
  if (!is.list(mapping)) {
    rlang::abort("`env_mapping` must be a list.")
  }

  required <- c(
    "location",
    "sample_date",
    "analyte",
    "result"
  )

  missing <- setdiff(required, names(mapping))

  if (length(missing) > 0L) {
    rlang::abort(
      paste0(
        "Missing mapping entries: ",
        paste(missing, collapse = ", ")
      )
    )
  }

  for (name in required) {
    value <- mapping[[name]]

    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !nzchar(value)
    ) {
      rlang::abort(
        sprintf(
          "Mapping entry `%s` must be one non-empty column name.",
          name
        )
      )
    }
  }

  invisible(TRUE)
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

  mapping <- schema_mapping(x)

  .validate_schema_mapping(mapping)

  # ------------------------------------------------------------------
  # Required mapped columns
  # ------------------------------------------------------------------

  required <- stats::na.omit(
    c(
      location_var(x),
      sample_date_var(x),
      analyte_var(x),
      result_var(x)
    )
  )

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

  # ------------------------------------------------------------------
  # Required manager-generated columns
  # ------------------------------------------------------------------

  missing_standard <- setdiff(
    .required_env_schema_columns(),
    names(x)
  )

  if (length(missing_standard) > 0) {
    rlang::abort(
      paste(
        "Missing required schema columns:",
        paste(
          missing_standard,
          collapse = ", "
        )
      )
    )
  }

  # ------------------------------------------------------------------
  # Metadata column references
  # ------------------------------------------------------------------

  .validate_metadata_columns(
    x,
    analysis_metadata(x),
    "analysis_metadata"
  )

  .validate_metadata_columns(
    x,
    threshold_metadata(x),
    "threshold_metadata"
  )

  .validate_metadata_columns(
    x,
    source_metadata(x),
    "source_metadata"
  )

  .check_metadata_overlap(
    analysis_metadata(x),
    threshold_metadata(x),
    source_metadata(x)
  )

  # ------------------------------------------------------------------
  # Metadata uniqueness
  # ------------------------------------------------------------------

  metadata_sets <- list(
    analysis_metadata = analysis_metadata(x),
    threshold_metadata = threshold_metadata(x),
    source_metadata = source_metadata(x)
  )

  for (nm in names(metadata_sets)) {
    vals <- metadata_sets[[nm]]

    if (anyDuplicated(vals)) {
      rlang::abort(
        paste(
          "Duplicate entries found in",
          nm
        )
      )
    }
  }

  # ------------------------------------------------------------------
  # Core column types
  # ------------------------------------------------------------------

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

  if (!is.character(x$.analyte_id)) {
    rlang::abort(
      "'.analyte_id' must be character."
    )
  }

  if (
    !inherits(
      sample_date(x),
      c(
        "Date",
        "POSIXct",
        "POSIXt"
      )
    )
  ) {
    rlang::abort(
      "'sample_date' must be a Date or POSIXct column."
    )
  }

  if (!is.null(analysis_date_var(x))) {
    if (
      !inherits(
        analysis_date(x),
        c(
          "Date",
          "POSIXct",
          "POSIXt"
        )
      )
    ) {
      rlang::abort(
        "'analysis_date' must be a Date or POSIXct column."
      )
    }
  }

  if (!is.numeric(result(x))) {
    rlang::abort(
      "Result column must be numeric."
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

  # ------------------------------------------------------------------
  # Threshold columns
  # ------------------------------------------------------------------

  if (!is.null(reporting_limit_var(x))) {
    if (!is.numeric(reporting_limit(x))) {
      rlang::abort(
        "Reporting limit column must be numeric."
      )
    }
  }

  if (!is.null(detection_limit_var(x))) {
    if (!is.numeric(detection_limit(x))) {
      rlang::abort(
        "Detection limit column must be numeric."
      )
    }
  }

  if (!is.null(quantitation_limit_var(x))) {
    if (!is.numeric(quantitation_limit(x))) {
      rlang::abort(
        "Quantitation limit column must be numeric."
      )
    }
  }

  if (!is.null(achieved_mdc_var(x))) {
    if (!is.numeric(achieved_mdc(x))) {
      rlang::abort(
        "Achieved MDC column must be numeric."
      )
    }
  }

  # ------------------------------------------------------------------
  # Display metadata
  # ------------------------------------------------------------------

  if (".display_result" %in% names(x) && !is.character(x$.display_result)) {
    rlang::abort(
      "'.display_result' must be character."
    )
  }

  if (".display_threshold" %in% names(x) && !is.numeric(x$.display_threshold)) {
    rlang::abort(
      "'.display_threshold' must be numeric."
    )
  }

  if (".display_threshold_type" %in% names(x)) {
    if (!is.character(x$.display_threshold_type)) {
      rlang::abort(
        "'.display_threshold_type' must be character."
      )
    }

    bad_types <- setdiff(
      unique(
        stats::na.omit(
          x$.display_threshold_type
        )
      ),
      unname(.display_threshold_types)
    )

    if (length(bad_types) > 0) {
      rlang::abort(
        paste(
          "Invalid display threshold type(s):",
          paste(bad_types, collapse = ", ")
        )
      )
    }
  }

  # ------------------------------------------------------------------
  # Result classification
  # ------------------------------------------------------------------

  for (col in c(
    ".detected",
    ".censored",
    ".estimated",
    ".rejected",
    ".usable"
  )) {
    .validate_logical_column(
      x[[col]],
      col
    )
  }

  # ------------------------------------------------------------------
  # Analyte metadata
  # ------------------------------------------------------------------

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

  # ------------------------------------------------------------------
  # Required attributes
  # ------------------------------------------------------------------

  if (is.null(display_options(x))) {
    rlang::abort(
      "Missing display_options attribute."
    )
  }

  .validate_display_options(
    display_options(x)
  )

  if (
    is.null(
      schema_version(x)
    )
  ) {
    rlang::abort(
      "Missing env_schema_version attribute."
    )
  }

  invisible(TRUE)
}


#' Minimal structural validation used during restoration.
#' Does not validate analytical content.
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
    rlang::abort("Missing env_mapping attribute.")
  }

  .validate_schema_mapping(mapping)

  # ------------------------------------------------------------
  # Required attributes
  # ------------------------------------------------------------

  required_attributes <- c(
    "display_options",
    "env_schema_version"
  )

  for (att in required_attributes) {
    if (is.null(attr(x, att, exact = TRUE))) {
      rlang::abort(
        paste(
          "Missing",
          att,
          "attribute."
        )
      )
    }
  }

  .validate_display_options(
    attr(
      x,
      "display_options",
      exact = TRUE
    )
  )

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

#' Validate unit strings
#'
#' Determines whether unit strings can be successfully interpreted
#' by the \pkg{units} package.
#'
#' Unit strings are first normalized using
#' [`.normalize_unit()`] before validation.
#'
#' @param x Character vector of unit strings.
#'
#' @return
#' A logical vector of the same length as `x` indicating whether
#' each unit string represents a valid unit recognized by
#' \pkg{units}.
#'
#' @details
#' Validation is performed using [units::as_units()].
#'
#' Missing values, empty strings, and unrecognized units are
#' considered invalid and return `FALSE`.
#'
#' This helper is primarily used internally by
#' [`standardize_units()`] and [`.convert_standard_units()`]
#' to identify invalid reporting or standard units before
#' attempting conversion.
#'
#' @noRd
.is_valid_unit <- function(x) {
  x <- .normalize_unit(x)

  .register_environmental_units()

  vapply(
    x,
    function(unit) {
      if (
        is.na(unit) ||
          !nzchar(unit)
      ) {
        return(FALSE)
      }

      tryCatch(
        {
          units::as_units(unit)
          TRUE
        },
        error = function(e) FALSE
      )
    },
    logical(1)
  )
}

#' Validate analyte metadata table
#'
#' @keywords internal
validate_analyte_tbl <- function(x) {
  if (!is.data.frame(x)) {
    rlang::abort("`analyte_tbl` must be a data frame.")
  }

  required <- c(
    "analyte_id",
    "analyte",
    "normalized_analyte",
    "short_name",
    "std_unit",
    "analyte_group",
    "fractionable"
  )

  .validate_required_cols(x, required)

  character_cols <- c(
    "analyte_id",
    "analyte",
    "normalized_analyte",
    "short_name",
    "std_unit",
    "analyte_group"
  )

  for (col in character_cols) {
    value <- x[[col]]

    if (!is.character(value)) {
      rlang::abort(
        sprintf("`%s` must be character.", col)
      )
    }

    if (anyNA(value) || any(!nzchar(trimws(value)))) {
      rlang::abort(
        sprintf(
          "`%s` cannot contain missing or empty values.",
          col
        )
      )
    }
  }

  if (!is.logical(x$fractionable)) {
    rlang::abort("`fractionable` must be logical.")
  }

  if (anyNA(x$fractionable)) {
    rlang::abort("`fractionable` cannot contain NA values.")
  }

  for (col in c("analyte_id", "analyte", "normalized_analyte")) {
    if (anyDuplicated(x[[col]])) {
      rlang::abort(
        sprintf(
          "Duplicate values detected in `%s`.",
          col
        )
      )
    }
  }

  invalid_units <- !.is_valid_unit(x$std_unit)

  if (any(invalid_units)) {
    rlang::abort(
      paste0(
        "Invalid standard unit(s): ",
        paste(
          unique(x$std_unit[invalid_units]),
          collapse = ", "
        )
      )
    )
  }

  invisible(TRUE)
}

#' Validate analyte alias table
#'
#' @keywords internal
validate_analyte_alias_tbl <- function(
  alias_tbl,
  analyte_tbl
) {
  if (!is.data.frame(alias_tbl)) {
    rlang::abort("`alias_tbl` must be a data frame.")
  }

  validate_analyte_tbl(analyte_tbl)

  required <- c("analyte_id", "alias")
  .validate_required_cols(alias_tbl, required)

  if (!is.character(alias_tbl$analyte_id)) {
    rlang::abort("`alias_tbl$analyte_id` must be character.")
  }

  if (!is.character(alias_tbl$alias)) {
    rlang::abort("`alias_tbl$alias` must be character.")
  }

  if (
    anyNA(alias_tbl$analyte_id) ||
      any(!nzchar(trimws(alias_tbl$analyte_id)))
  ) {
    rlang::abort(
      "`alias_tbl$analyte_id` cannot contain missing or empty values."
    )
  }

  if (
    anyNA(alias_tbl$alias) ||
      any(!nzchar(trimws(alias_tbl$alias)))
  ) {
    rlang::abort(
      "`alias_tbl$alias` cannot contain missing or empty values."
    )
  }

  bad_ids <- setdiff(
    unique(alias_tbl$analyte_id),
    unique(analyte_tbl$analyte_id)
  )

  if (length(bad_ids) > 0L) {
    rlang::abort(
      paste0(
        "Unknown analyte IDs: ",
        paste(bad_ids, collapse = ", ")
      )
    )
  }

  normalized_aliases <- .normalize_analyte_text(
    alias_tbl$alias
  )

  duplicate_aliases <- unique(
    normalized_aliases[
      duplicated(normalized_aliases)
    ]
  )

  if (length(duplicate_aliases) > 0L) {
    rlang::abort(
      paste0(
        "Duplicate normalized aliases detected: ",
        paste(duplicate_aliases, collapse = ", ")
      )
    )
  }

  invisible(TRUE)
}

#' Validate supported analytes
#'
#' Resolves analyte names using the configured analyte registry and
#' validates that analytes can be matched to known analyte metadata.
#'
#' This function performs validation of both the analyte metadata
#' table and alias lookup table before attempting analyte resolution.
#'
#' Unsupported analytes are reported using
#' [`.warn_unsupported_analytes`()].
#'
#' @param analytes Character vector of analyte names.
#' @param alias_lookup Alias lookup table, typically
#'   [analyte_alias_lookup].
#' @param lookup An analyte metadata table, typically [analyte_tbl].
#'
#' @returns
#' Invisibly returns the resolved analyte metadata produced by
#' [`.resolve_analyte`()].
#'
#' @details
#' The returned object includes:
#'
#' \itemize{
#'   \item resolved analyte identifiers;
#'   \item canonical analyte names;
#'   \item normalized analyte names;
#'   \item fraction assignments;
#'   \item analyte metadata;
#'   \item analyte match status.
#' }
#'
#' Unknown analytes are not discarded. Instead, they are flagged and
#' reported through a warning.
#'
#' @seealso
#' [`.resolve_analyte`()],
#' [`.warn_unsupported_analytes`()]
#'
#' @keywords internal
.validate_supported_analytes <- function(
  analytes,
  alias_lookup = analyte_alias_lookup,
  lookup = analyte_tbl
) {
  validate_analyte_tbl(lookup)

  .validate_alias_lookup(
    alias_lookup,
    lookup
  )

  resolved <- .resolve_analyte(
    analytes,
    alias_lookup = alias_lookup,
    analyte_lookup = lookup
  )

  .warn_unsupported_analytes(
    resolved
  )

  invisible(resolved)
}

#' Validate analytes and units
#'
#' Validates analyte and unit fields within an analytical dataset.
#'
#' The function verifies required columns, resolves analytes using the
#' package analyte registry, warns on unsupported analytes, and
#' standardizes unit values.
#'
#' @param df A data frame containing analytical results.
#' @param param_name Character string identifying the analyte column.
#' @param default_unit Character string identifying the unit column.
#' @param alias_lookup Analyte alias lookup table
#' @param lookup Anlayte table lookup
#'
#' @returns
#' A data frame containing resolved analyte metadata and normalized
#' unit values.
#'
#' @details
#' Validation steps include:
#'
#' \enumerate{
#'   \item verification of required columns;
#'   \item validation of the analyte registry;
#'   \item validation of the alias lookup table;
#'   \item analyte resolution;
#'   \item unsupported analyte checks;
#'   \item unit normalization.
#' }
#'
#' This helper is primarily used during environmental chemistry data
#' import and schema creation workflows.
#'
#' @seealso
#' [`.resolve_analyte_column`()],
#' [`.normalize_unit`()]
#'
#' @keywords internal
.validate_analyte_units <- function(
  df,
  param_name = "param_name",
  default_unit = "default_unit",
  alias_lookup = analyte_alias_lookup,
  lookup = analyte_tbl
) {
  .validate_required_cols(
    df,
    c(param_name, default_unit)
  )

  validate_analyte_tbl(lookup)

  validate_analyte_alias_tbl(
    alias_tbl = alias_lookup,
    analyte_tbl = lookup
  )

  out <- df |>
    .resolve_analyte_column(
      analyte_col = param_name,
      alias_lookup = alias_lookup,
      analyte_lookup = lookup
    )

  .warn_unsupported_analytes(out)

  out |>
    dplyr::mutate(
      !!default_unit := .normalize_unit(
        .data[[default_unit]]
      )
    )
}

#' Validate units for meq conversion
#'
#' @keywords internal
.validate_analyte_meq_units <- function(
  df,
  analyte_col = ".analyte_id",
  unit_col = ".analysis_unit"
) {
  .validate_required_cols(
    df,
    c(analyte_col, unit_col)
  )

  if (!is.character(df[[analyte_col]])) {
    rlang::abort(
      sprintf("`%s` must be character.", analyte_col)
    )
  }

  if (!is.character(df[[unit_col]])) {
    rlang::abort(
      sprintf("`%s` must be character.", unit_col)
    )
  }

  meq_lookup <- .get_meq_conversion_factors()

  .validate_required_cols(
    meq_lookup,
    c("analyte_id", "meq_factor", "std_unit")
  )

  if (!is.character(meq_lookup$analyte_id)) {
    rlang::abort("Meq lookup `analyte_id` must be character.")
  }

  if (!is.numeric(meq_lookup$meq_factor)) {
    rlang::abort("Meq lookup `meq_factor` must be numeric.")
  }

  if (!is.character(meq_lookup$std_unit)) {
    rlang::abort("Meq lookup `std_unit` must be character.")
  }

  duplicate_ids <- meq_lookup$analyte_id[
    duplicated(meq_lookup$analyte_id)
  ]

  if (length(duplicate_ids) > 0L) {
    rlang::abort(
      paste0(
        "Meq lookup contains duplicate analyte IDs: ",
        paste(unique(duplicate_ids), collapse = ", ")
      )
    )
  }

  meq_lookup <- meq_lookup |>
    dplyr::mutate(
      std_unit = .normalize_unit(std_unit)
    )

  if (
    anyNA(meq_lookup$std_unit) ||
      any(!nzchar(meq_lookup$std_unit))
  ) {
    rlang::abort(
      "Meq lookup `std_unit` must contain non-empty valid units."
    )
  }

  # Only validate lookup entries relevant to this conversion.
  # Unknown analytes and analytes without conversion factors are
  # intentionally allowed and produce NA output values.
  relevant_lookup <- meq_lookup |>
    dplyr::filter(
      analyte_id %in% unique(df[[analyte_col]])
    )

  # Validate only factors that are present. NA means that the
  # analyte is not convertible by design.
  invalid_factors <- relevant_lookup |>
    dplyr::filter(
      !is.na(meq_factor) &
        (!is.finite(meq_factor) | meq_factor <= 0)
    )

  if (nrow(invalid_factors) > 0L) {
    rlang::abort(
      paste0(
        "Meq lookup contains invalid conversion factor(s) for: ",
        paste(
          unique(invalid_factors$analyte_id),
          collapse = ", "
        )
      )
    )
  }

  # Validate units only for analytes that have a conversion factor.
  valid_lookup <- relevant_lookup |>
    dplyr::filter(
      !is.na(meq_factor)
    ) |>
    dplyr::select(
      analyte_id,
      std_unit
    )

  invalid_units <- df |>
    dplyr::mutate(
      .unit = .normalize_unit(.data[[unit_col]])
    ) |>
    dplyr::left_join(
      valid_lookup,
      by = stats::setNames("analyte_id", analyte_col),
      relationship = "many-to-one"
    ) |>
    dplyr::filter(
      !is.na(std_unit) &
        (is.na(.unit) |
          !nzchar(.unit) |
          .unit != std_unit)
    ) |>
    dplyr::distinct(
      .data[[analyte_col]],
      .unit,
      std_unit
    )

  if (nrow(invalid_units) > 0L) {
    examples <- utils::head(invalid_units, 10L)

    details <- paste(
      paste0(
        examples[[analyte_col]],
        " (",
        ifelse(
          is.na(examples$.unit) | !nzchar(examples$.unit),
          "<missing>",
          examples$.unit
        ),
        " != ",
        examples$std_unit,
        ")"
      ),
      collapse = ", "
    )

    cli::cli_abort(
      c(
        "Non-standard units found for meq conversion.",
        "x" = details,
        "i" = "Standardize analytical concentrations before meq conversion."
      )
    )
  }

  invisible(TRUE)
}

#' Validate alias lookup table
#'
#' @keywords internal
.validate_alias_lookup <- function(
  alias_lookup = analyte_alias_lookup,
  lookup = analyte_tbl
) {
  validate_analyte_tbl(lookup)

  required_cols <- c(
    "alias",
    "analyte_id",
    "analyte",
    "normalized_analyte",
    "fraction"
  )

  .validate_required_cols(
    alias_lookup,
    required_cols
  )

  character_cols <- c(
    "alias",
    "analyte_id",
    "analyte",
    "normalized_analyte",
    "fraction"
  )

  for (col in character_cols) {
    if (!is.character(alias_lookup[[col]])) {
      rlang::abort(
        sprintf("Alias lookup `%s` must be character.", col)
      )
    }
  }

  non_empty_cols <- c(
    "alias",
    "analyte_id",
    "analyte",
    "normalized_analyte"
  )

  for (col in non_empty_cols) {
    value <- alias_lookup[[col]]

    if (anyNA(value) || any(!nzchar(trimws(value)))) {
      rlang::abort(
        sprintf(
          "Alias lookup `%s` cannot contain missing or empty values.",
          col
        )
      )
    }
  }

  bad_targets <- setdiff(
    unique(alias_lookup$analyte_id),
    unique(lookup$analyte_id)
  )

  if (length(bad_targets) > 0L) {
    rlang::abort(
      paste0(
        "Alias lookup references unknown analyte IDs: ",
        paste(bad_targets, collapse = ", ")
      )
    )
  }

  normalized_alias <- .normalize_analyte_text(
    alias_lookup$alias
  )

  if (any(alias_lookup$alias != normalized_alias)) {
    rlang::abort(
      "All aliases must already be normalized."
    )
  }

  duplicate_aliases <- unique(
    alias_lookup$alias[
      duplicated(alias_lookup$alias)
    ]
  )

  if (length(duplicate_aliases) > 0L) {
    rlang::abort(
      paste0(
        "Duplicate aliases detected after normalization: ",
        paste(duplicate_aliases, collapse = ", ")
      )
    )
  }

  valid_fractions <- c(
    "dissolved",
    "total",
    "filtered",
    "unfiltered"
  )

  invalid_fraction <- !is.na(alias_lookup$fraction) &
    !alias_lookup$fraction %in% valid_fractions

  if (any(invalid_fraction)) {
    rlang::abort(
      paste0(
        "Invalid fraction values detected: ",
        paste(
          unique(alias_lookup$fraction[invalid_fraction]),
          collapse = ", "
        )
      )
    )
  }

  # Ensure copied metadata agrees with the canonical lookup.
  expected <- lookup |>
    dplyr::select(
      analyte_id,
      analyte,
      normalized_analyte
    ) |>
    dplyr::rename(
      expected_analyte = analyte,
      expected_normalized_analyte = normalized_analyte
    )

  mismatches <- alias_lookup |>
    dplyr::left_join(
      expected,
      by = "analyte_id",
      relationship = "many-to-one"
    ) |>
    dplyr::filter(
      analyte != expected_analyte |
        normalized_analyte != expected_normalized_analyte
    )

  if (nrow(mismatches) > 0L) {
    rlang::abort(
      "Alias lookup metadata does not agree with the canonical analyte table."
    )
  }

  invisible(alias_lookup)
}

#' Validate a standards schema
#'
#' @keywords internal
validate_standards_schema <- function(x) {
  if (!is_standards_schema(x)) {
    rlang::abort(
      "Object must inherit from 'standards_schema'."
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

  .validate_required_cols(x, required_cols)

  character_cols <- setdiff(
    required_cols,
    c(".standard_value", ".effective_date", ".priority")
  )

  for (col in character_cols) {
    value <- x[[col]]

    if (!is.character(value)) {
      rlang::abort(
        sprintf("`%s` must be character.", col)
      )
    }

    if (anyNA(value) || any(!nzchar(trimws(value)))) {
      rlang::abort(
        sprintf(
          "`%s` cannot contain missing or empty values.",
          col
        )
      )
    }
  }

  if (!is.numeric(x$.standard_value)) {
    rlang::abort("`.standard_value` must be numeric.")
  }

  if (
    anyNA(x$.standard_value) ||
      any(!is.finite(x$.standard_value))
  ) {
    rlang::abort(
      "`.standard_value` cannot contain NA, Inf, or NaN."
    )
  }

  if (!is.numeric(x$.priority)) {
    rlang::abort("`.priority` must be numeric.")
  }

  if (
    anyNA(x$.priority) ||
      any(!is.finite(x$.priority))
  ) {
    rlang::abort(
      "`.priority` cannot contain NA, Inf, or NaN."
    )
  }

  if (
    !inherits(
      x$.effective_date,
      c("Date", "POSIXct", "POSIXt")
    )
  ) {
    rlang::abort(
      "`.effective_date` must be a Date or POSIXct column."
    )
  }

  invisible(TRUE)
}

#' Validate est_dist arguments
#'
#' @keywords internal
.validate_est_dist_args <- function(
  alpha,
  method,
  choices,
  return_result
) {
  .validate_choice(
    method,
    c("sw", "sf", "ppcc", "proucl"),
    "method"
  )

  .check_flag(
    return_result,
    "return_result"
  )

  if (
    !is.numeric(alpha) ||
      length(alpha) != 1L ||
      is.na(alpha) ||
      !is.finite(alpha) ||
      alpha <= 0 ||
      alpha >= 1
  ) {
    rlang::abort(
      "`alpha` must be one finite numeric value strictly between 0 and 1."
    )
  }

  if (
    !is.character(method) ||
      length(method) != 1L ||
      is.na(method) ||
      !nzchar(method)
  ) {
    rlang::abort(
      "`method` must be one non-empty character value."
    )
  }

  if (
    !is.character(choices) ||
      length(choices) < 1L ||
      anyNA(choices) ||
      any(!nzchar(choices)) ||
      anyDuplicated(choices)
  ) {
    rlang::abort(
      "`choices` must be a non-empty vector of unique character values."
    )
  }

  invisible(TRUE)
}

#' Validate estimate_distribution Output
#'
#' Checks that a data frame contains the required structure produced
#' by estimate_distribution(), including:
#'
#' * a distribution column
#' * an n_used column
#' * a data list-column
#' * nested data frames containing analysis_result
#' * supported distribution values
#'
#' @keywords internal
.validate_est_dist_output <- function(df) {
  if (!is.data.frame(df)) {
    rlang::abort(
      "Distribution-estimation output must be a data frame."
    )
  }

  required_cols <- c(
    "distribution",
    "n_used",
    "data"
  )

  .validate_required_cols(df, required_cols)

  if (
    !is.character(df$distribution) ||
      anyNA(df$distribution)
  ) {
    rlang::abort(
      "`distribution` must be a character column without NA values."
    )
  }

  if (
    !is.numeric(df$n_used) ||
      anyNA(df$n_used) ||
      any(!is.finite(df$n_used)) ||
      any(df$n_used < 0) ||
      any(df$n_used != as.integer(df$n_used))
  ) {
    rlang::abort(
      "`n_used` must contain finite non-negative integers."
    )
  }

  if (!is.list(df$data)) {
    rlang::abort("`data` must be a list-column.")
  }

  valid_nested_data <- purrr::map_lgl(
    df$data,
    function(value) {
      is.data.frame(value) &&
        ".analysis_result" %in% names(value) &&
        is.numeric(value$.analysis_result)
    }
  )

  if (!all(valid_nested_data)) {
    rlang::abort(
      paste(
        "Each `data` element must be a data frame containing",
        "a numeric `.analysis_result` column."
      )
    )
  }

  valid_distributions <- c(
    "Normal",
    "Lognormal",
    "Nonparametric",
    "insufficient_data",
    "estimation_failed",
    "Descriptive Only"
  )

  invalid <- setdiff(
    unique(df$distribution),
    valid_distributions
  )

  if (length(invalid) > 0L) {
    rlang::abort(
      paste0(
        "Unsupported distribution(s): ",
        paste(invalid, collapse = ", ")
      )
    )
  }

  invisible(TRUE)
}

#' @keywords internal
.validate_censoring_data <- function(
  df,
  result_col,
  censored_col,
  reporting_limit_col
) {
  .validate_required_cols(
    df,
    c(
      result_col,
      censored_col,
      reporting_limit_col
    )
  )

  result <- df[[result_col]]
  censored <- df[[censored_col]]
  reporting_limit <- df[[reporting_limit_col]]

  if (!is.numeric(result)) {
    rlang::abort(
      sprintf("`%s` must be numeric.", result_col)
    )
  }

  if (any(!is.na(result) & !is.finite(result))) {
    rlang::abort(
      sprintf("`%s` cannot contain Inf or NaN.", result_col)
    )
  }

  if (!is.logical(censored)) {
    rlang::abort(
      sprintf("`%s` must be logical.", censored_col)
    )
  }

  if (anyNA(censored)) {
    rlang::abort(
      sprintf("`%s` cannot contain NA values.", censored_col)
    )
  }

  if (!is.numeric(reporting_limit)) {
    rlang::abort(
      sprintf("`%s` must be numeric.", reporting_limit_col)
    )
  }

  if (any(!is.na(reporting_limit) & !is.finite(reporting_limit))) {
    rlang::abort(
      sprintf(
        "`%s` cannot contain Inf or NaN.",
        reporting_limit_col
      )
    )
  }

  if (
    any(
      censored &
        (is.na(reporting_limit) | reporting_limit <= 0)
    )
  ) {
    rlang::abort(
      paste0(
        "Every censored observation must have a finite positive ",
        "reporting limit."
      )
    )
  }

  invisible(df)
}

#' @keywords internal
.validate_count_vector <- function(x, name) {
  if (
    !is.numeric(x) ||
      anyNA(x) ||
      any(!is.finite(x)) ||
      any(x < 0) ||
      any(x != floor(x))
  ) {
    rlang::abort(
      sprintf(
        "`%s` must contain finite, non-negative integers.",
        name
      )
    )
  }

  invisible(x)
}

#' @keywords internal
.validate_interval_distribution <- function(distribution) {
  supported <- c(
    "Normal",
    "Lognormal",
    "Nonparametric"
  )

  bad <- setdiff(
    unique(stats::na.omit(distribution)),
    supported
  )

  if (length(bad) > 0) {
    rlang::abort(
      paste(
        "Intervals are only available for:",
        paste(supported, collapse = ", "),
        "\nUnsupported distribution(s):",
        paste(bad, collapse = ", ")
      )
    )
  }

  invisible(distribution)
}

#' @keywords internal
#' @noRd
.validate_positive_integer <- function(x, name) {
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      x < 1 ||
      x != as.integer(x)
  ) {
    rlang::abort(
      sprintf("`%s` must be a positive integer.", name)
    )
  }

  invisible(x)
}

#' @keywords internal
#' @noRd
.validate_probability <- function(x, name) {
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      x <= 0 ||
      x >= 1
  ) {
    rlang::abort(
      sprintf("`%s` must be between 0 and 1.", name)
    )
  }

  invisible(x)
}

#' @keywords internal
#' @noRd
.validate_conf_int_args <- function(
  conf_level,
  ci_type,
  ci_param,
  nonparametric_args,
  censored_args
) {
  if (
    !is.numeric(conf_level) ||
      length(conf_level) != 1L ||
      is.na(conf_level) ||
      conf_level <= 0 ||
      conf_level >= 1
  ) {
    rlang::abort("`conf_level` must be between 0 and 1.")
  }

  rlang::arg_match0(
    ci_type,
    c("upper", "lower", "two-sided")
  )

  rlang::arg_match0(
    ci_param,
    c("mean", "median", "quantile", "geometric.mean")
  )

  if (!is.list(nonparametric_args) || is.null(names(nonparametric_args))) {
    rlang::abort("`nonparametric_args` must be a named list.")
  }

  if (!is.list(censored_args) || is.null(names(censored_args))) {
    rlang::abort("`censored_args` must be a named list.")
  }

  invisible(NULL)
}
