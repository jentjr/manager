#' Validate choices
#'
#' @param x Character vector.
#' @param choices Character vector
#' @param arg Character vector
#'
#' @keywords internal
.validate_choice <- function(x, choices, arg) {
  if (!x %in% choices) {
    stop(
      sprintf(
        "`%s` must be one of: %s",
        arg,
        paste(choices, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(x)
}

#' @keywords internal
.check_flag <- function(x, name) {
  if (
    !is.logical(x) ||
      length(x) != 1 ||
      is.na(x)
  ) {
    stop(
      name,
      " must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Validate required columns
#'
#' Internal helper used throughout the package to check that
#' required columns are present.
#'
#' @param data A data frame.
#' @param cols Character vector of required column names.
#'
#' @return Invisibly returns TRUE.
#'
#' @noRd
.validate_required_cols <- function(
  data,
  cols
) {
  missing_cols <- setdiff(
    cols,
    names(data)
  )

  if (length(missing_cols) > 0) {
    rlang::abort(
      paste(
        "Missing required column(s):",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
}

#' Validate display options
#'
#' @keywords internal
.validate_display_options <- function(x) {
  if (!is.list(x)) {
    rlang::abort(
      "display_options must be a list."
    )
  }

  required <- c(
    "digits",
    "scientific",
    "trim_trailing_zeros",
    "censor_prefix",
    "report_style"
  )

  missing <- setdiff(
    required,
    names(x)
  )

  if (length(missing) > 0) {
    rlang::abort(
      paste(
        "Missing display options:",
        paste(missing, collapse = ", ")
      )
    )
  }

  if (
    !is.numeric(x$digits) ||
      length(x$digits) != 1 ||
      x$digits < 1 ||
      x$digits != as.integer(x$digits)
  ) {
    rlang::abort(
      "'digits' must be a positive integer."
    )
  }

  if (
    !is.logical(x$scientific) ||
      length(x$scientific) != 1
  ) {
    rlang::abort(
      "'scientific' must be TRUE or FALSE."
    )
  }

  if (
    !is.logical(x$trim_trailing_zeros) ||
      length(x$trim_trailing_zeros) != 1
  ) {
    rlang::abort(
      "'trim_trailing_zeros' must be TRUE or FALSE."
    )
  }

  if (
    !is.character(x$censor_prefix) ||
      length(x$censor_prefix) != 1
  ) {
    rlang::abort(
      "'censor_prefix' must be character."
    )
  }

  if (!nzchar(x$censor_prefix)) {
    rlang::abort(
      "'censor_prefix' cannot be empty."
    )
  }

  if (
    !is.character(x$report_style) ||
      length(x$report_style) != 1 ||
      !x$report_style %in%
        c("standard", "verbose")
  ) {
    rlang::abort(
      "'report_style' must be 'standard' or 'verbose'."
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

  names_found <- unlist(
    metadata,
    use.names = FALSE
  )

  dupes <- unique(
    names_found[
      duplicated(names_found)
    ]
  )

  if (length(dupes) > 0) {
    rlang::abort(
      paste(
        "Metadata columns assigned to multiple metadata categories:",
        paste(dupes, collapse = ", ")
      )
    )
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

  required_mappings <- c(
    "location",
    "sample_date",
    "analyte",
    "result"
  )

  missing_mappings <- setdiff(
    required_mappings,
    names(mapping)
  )

  if (length(missing_mappings) > 0) {
    rlang::abort(
      paste(
        "Missing mapped columns:",
        paste(missing_mappings, collapse = ", ")
      )
    )
  }

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

  analysis_missing <- setdiff(
    analysis_metadata(x),
    names(x)
  )

  if (length(analysis_missing) > 0) {
    rlang::abort(
      paste(
        "Analysis metadata columns not found:",
        paste(analysis_missing, collapse = ", ")
      )
    )
  }

  threshold_missing <- setdiff(
    threshold_metadata(x),
    names(x)
  )

  if (length(threshold_missing) > 0) {
    rlang::abort(
      paste(
        "Threshold metadata columns not found:",
        paste(threshold_missing, collapse = ", ")
      )
    )
  }

  source_missing <- setdiff(
    source_metadata(x),
    names(x)
  )

  if (length(source_missing) > 0) {
    rlang::abort(
      paste(
        "Source metadata columns not found:",
        paste(source_missing, collapse = ", ")
      )
    )
  }

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
    if (col %in% names(x) && !is.logical(x[[col]])) {
      rlang::abort(
        paste0("'", col, "' must be logical.")
      )
    }
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
    rlang::abort(
      "Missing env_mapping attribute."
    )
  }

  required <- c(
    stats::na.omit(
      c(
        mapping$location,
        mapping$sample_date,
        mapping$analyte,
        mapping$result
      )
    ),
    .required_env_schema_columns()
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
#' Validates the structure and integrity of an analyte metadata table.
#'
#' An analyte metadata table provides the canonical analyte registry
#' used throughout the package for analyte resolution, metadata
#' assignment, standards matching, plotting, unit conversion, and
#' environmental data classification workflows.
#'
#' @param x An analyte metadata table, typically [analyte_tbl].
#'
#' @returns
#' Invisibly returns `TRUE` if validation succeeds.
#'
#' @details
#' The table must contain the following required columns:
#'
#' \itemize{
#'   \item `analyte_id`
#'   \item `analyte`
#'   \item `normalized_analyte`
#'   \item `short_name`
#'   \item `std_unit`
#'   \item `analyte_group`
#' }
#'
#' Validation includes:
#'
#' \itemize{
#'   \item presence of required columns;
#'   \item non-missing analyte identifiers;
#'   \item uniqueness of `analyte_id`;
#'   \item uniqueness of canonical analyte names;
#' }
#'
#' The `aliases` column must be stored as a list-column containing
#' one or more character aliases for each analyte.
#'
#' @seealso
#' [analyte_tbl],
#' [`.validate_alias_lookup`()]
#'
#' @keywords internal
validate_analyte_tbl <- function(x) {
  required <- c(
    "analyte_id",
    "analyte",
    "normalized_analyte",
    "short_name",
    "std_unit",
    "analyte_group"
  )

  missing <- setdiff(
    required,
    names(x)
  )

  if (length(missing) > 0) {
    rlang::abort(
      paste(
        "Missing analyte_tbl columns:",
        paste(
          missing,
          collapse = ", "
        )
      )
    )
  }

  if (!is.character(x$analyte_id)) {
    rlang::abort(
      "'analyte_id' must be character."
    )
  }

  if (anyNA(x$analyte_id)) {
    rlang::abort(
      "'analyte_id' cannot contain NA."
    )
  }

  if (!is.character(x$analyte)) {
    rlang::abort(
      "'analyte' must be character."
    )
  }

  if (anyNA(x$analyte)) {
    rlang::abort(
      "'analyte' cannot contain NA."
    )
  }

  if (
    anyDuplicated(
      x$analyte
    )
  ) {
    rlang::abort(
      "Duplicate analyte names detected."
    )
  }

  invisible(TRUE)
}

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
  default_unit = "default_unit"
) {
  required_cols <- c(
    param_name,
    default_unit
  )

  missing_cols <- setdiff(
    required_cols,
    names(df)
  )

  if (length(missing_cols) > 0) {
    rlang::abort(
      paste(
        "Missing required columns:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  validate_analyte_tbl(
    analyte_tbl
  )

  .validate_alias_lookup(
    analyte_alias_lookup,
    analyte_tbl
  )

  out <- df %>%
    .resolve_analyte_column(
      analyte_col = param_name
    )

  .warn_unsupported_analytes(
    out
  )

  out %>%
    dplyr::mutate(
      !!default_unit := .normalize_unit(
        .data[[default_unit]]
      )
    )
}

#' Validate units for meq conversion
#'
#' Ensures analytes eligible for meq conversion are reported using
#' their expected standardized reporting units.
#'
#' @param df Data frame containing analytical results.
#'
#' @param analyte_col Character string identifying the canonical
#'   analyte column. Default is `"analyte"`.
#'
#' @param unit_col Character string identifying the
#'   reporting unit column. Default is `".analysis_unit"`.
#'
#' @return
#' Invisibly returns `TRUE`.
#'
#' @details
#' Validation is performed using the metadata returned by
#' [`.get_meq_conversion_factors()`].
#'
#' Only analytes having a non-missing `meq_factor` are validated.
#'
#' @section Validation:
#'
#' An error is raised when a convertible analyte is reported using a
#' unit that differs from the expected standard unit.
#'
#' This typically indicates that
#' [`.standardize_units()`] has not yet been executed.
#'
#' @noRd
.validate_analyte_meq_units <- function(
  df,
  analyte_col = ".analyte_id",
  unit_col = ".analysis_unit"
) {
  .validate_required_cols(
    df,
    c(
      analyte_col,
      unit_col
    )
  )

  meq_lookup <- .get_meq_conversion_factors() %>%
    dplyr::filter(
      !is.na(meq_factor)
    )

  invalid_units <- df %>%
    dplyr::mutate(
      !!unit_col := .normalize_unit(
        .data[[unit_col]]
      )
    ) %>%
    dplyr::left_join(
      meq_lookup,
      by = stats::setNames(
        "analyte_id",
        analyte_col
      ),
      relationship = "many-to-one"
    ) %>%
    dplyr::filter(
      !is.na(meq_factor),
      !is.na(.data[[unit_col]]),
      .data[[unit_col]] != std_unit
    ) %>%
    dplyr::distinct(
      .data[[analyte_col]],
      .data[[unit_col]],
      std_unit
    )

  if (nrow(invalid_units) > 0) {
    examples <- invalid_units %>%
      utils::head(10)

    cli::cli_abort(
      c(
        "Non-standard units found for meq conversion.",
        "x" = paste(
          paste0(
            examples[[analyte_col]],
            " (",
            examples[[unit_col]],
            " != ",
            examples$std_unit,
            ")"
          ),
          collapse = ", "
        ),
        "i" = "Analytical concentrations must be standardized before meq conversion."
      )
    )
  }

  invisible(TRUE)
}

#' Validate alias lookup table
#'
#' Validates the structure and integrity of an analyte alias lookup
#' table.
#'
#' The alias lookup table is used during analyte resolution to map
#' normalized analyte aliases to canonical analyte identifiers and
#' metadata.
#'
#' @param alias_lookup Alias lookup table, typically
#'   [analyte_alias_lookup].
#' @param lookup An analyte metadata table, typically [analyte_tbl].
#'
#' @returns
#' Invisibly returns the validated alias lookup table.
#'
#' @details
#' Required columns are:
#'
#' \itemize{
#'   \item `alias`
#'   \item `analyte_id`
#'   \item `analyte`
#'   \item `normalized_analyte`
#'   \item `fraction`
#' }
#'
#' Validation includes:
#'
#' \itemize{
#'   \item required column checks;
#'   \item referential integrity against `analyte_tbl`;
#'   \item duplicate alias detection;
#'   \item character type validation;
#'   \item missing value checks;
#'   \item fraction validation;
#'   \item alias normalization checks.
#' }
#'
#' Valid fraction values are:
#'
#' \itemize{
#'   \item `"dissolved"`
#'   \item `"total"`
#'   \item `"filtered"`
#'   \item `"unfiltered"`
#' }
#'
#' All aliases are expected to have already been normalized using
#' [`.normalize_analyte_text`()].
#'
#' @seealso
#' [analyte_tbl],
#' [analyte_alias_lookup],
#' [`.normalize_analyte_text`()]
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

  missing_cols <- setdiff(
    required_cols,
    names(alias_lookup)
  )

  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "Alias lookup table is invalid.",
        "Missing columns:" = paste(
          missing_cols,
          collapse = ", "
        )
      )
    )
  }

  bad_targets <- setdiff(
    unique(alias_lookup$analyte_id),
    unique(lookup$analyte_id)
  )

  if (length(bad_targets) > 0) {
    cli::cli_abort(
      c(
        "Alias lookup references unknown analyte IDs.",
        setNames(
          bad_targets,
          rep("x", length(bad_targets))
        )
      )
    )
  }

  alias_dupes <- alias_lookup %>%
    dplyr::count(alias) %>%
    dplyr::filter(n > 1)

  if (nrow(alias_dupes) > 0) {
    cli::cli_abort(
      c(
        "Duplicate aliases detected after normalization.",
        setNames(
          alias_dupes$alias,
          rep("x", nrow(alias_dupes))
        )
      )
    )
  }

  if (anyNA(alias_lookup$analyte_id)) {
    rlang::abort(
      "'analyte_id' cannot contain NA."
    )
  }

  if (!is.character(alias_lookup$analyte)) {
    rlang::abort(
      "'analyte' must be character."
    )
  }

  if (anyNA(alias_lookup$analyte)) {
    rlang::abort(
      "'analyte' cannot contain NA."
    )
  }

  if (!is.character(alias_lookup$alias)) {
    rlang::abort(
      "'alias' must be character."
    )
  }

  if (!is.character(alias_lookup$analyte_id)) {
    rlang::abort(
      "'analyte_id' must be character."
    )
  }

  if (anyNA(alias_lookup$alias)) {
    rlang::abort(
      "'alias' cannot contain NA."
    )
  }

  if (!is.character(alias_lookup$normalized_analyte)) {
    rlang::abort(
      "'normalized_analyte' must be character."
    )
  }

  if (anyNA(alias_lookup$normalized_analyte)) {
    rlang::abort(
      "'normalized_analyte' cannot contain NA."
    )
  }

  if (!is.character(alias_lookup$fraction)) {
    rlang::abort(
      "'fraction' must be character."
    )
  }

  valid_fractions <- c(
    "dissolved",
    "total",
    "filtered",
    "unfiltered"
  )

  bad_fraction <- alias_lookup %>%
    dplyr::filter(
      !is.na(fraction),
      !fraction %in% valid_fractions
    )

  if (nrow(bad_fraction) > 0) {
    rlang::abort(
      paste(
        "Invalid fraction values detected:",
        paste(
          unique(bad_fraction$fraction),
          collapse = ", "
        )
      )
    )
  }

  bad_aliases <- alias_lookup %>%
    dplyr::filter(
      alias != .normalize_analyte_text(alias)
    )

  if (nrow(bad_aliases) > 0) {
    rlang::abort(
      "All aliases must already be normalized."
    )
  }

  invisible(alias_lookup)
}

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

#' Validate est_dist arguments
#'
#' Internal helper for validating distribution estimation arguments.
#'
#' @param alpha Significance level.
#' @param method Goodness-of-fit test method.
#' @param choices Candidate distributions.
#' @param return_result Logical indicating whether to return the
#' EnvStats result object.
#'
#' @return Invisibly returns TRUE.
#'
#' @noRd
.validate_est_dist_args <- function(
  alpha,
  method,
  choices,
  return_result
) {
  if (!is.logical(return_result) || length(return_result) != 1) {
    rlang::abort(
      "return_result must be TRUE or FALSE."
    )
  }

  if (
    !is.numeric(alpha) ||
      length(alpha) != 1 ||
      alpha <= 0 ||
      alpha >= 1
  ) {
    rlang::abort(
      "alpha must be a single numeric value between 0 and 1."
    )
  }

  if (!is.character(method) || length(method) != 1) {
    rlang::abort(
      "method must be a single character value."
    )
  }

  if (!is.character(choices) || length(choices) < 1) {
    rlang::abort(
      "choices must be a non-empty character vector."
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
  required_cols <- c(
    "distribution",
    "n_used",
    "data"
  )

  missing_cols <- setdiff(
    required_cols,
    names(df)
  )

  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "Input must be the output of {.fn estimate_distribution}.",
        "x" = "Missing column(s): {.val {missing_cols}}."
      )
    )
  }

  if (!inherits(df$data, "list")) {
    cli::cli_abort(
      c(
        "Input must be the output of {.fn estimate_distribution}.",
        "x" = "Column {.field data} must be a list-column."
      )
    )
  }

  empty_data <- purrr::map_lgl(
    df$data,
    is.null
  )

  if (any(empty_data)) {
    cli::cli_abort(
      c(
        "Invalid nested data detected.",
        "x" = "{sum(empty_data)} row(s) contain a NULL {.field data} object."
      )
    )
  }

  has_analysis_result <- purrr::map_lgl(
    df$data,
    ~ ".analysis_result" %in% names(.x)
  )

  if (!all(has_analysis_result)) {
    cli::cli_abort(
      c(
        "Invalid nested data detected.",
        "x" = "Each nested data frame must contain {.field .analysis_result}."
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

  invalid_distributions <- setdiff(
    unique(stats::na.omit(df$distribution)),
    valid_distributions
  )

  if (length(invalid_distributions) > 0) {
    cli::cli_abort(
      c(
        "Unsupported distribution detected.",
        "x" = "Found {.val {invalid_distributions}}.",
        "i" = "Valid values are {.val {valid_distributions}}."
      )
    )
  }

  invisible(df)
}
