#' Construct an environmental data schema
#'
#' Creates an `env_schema` object by attaching environmental column
#' mappings and schema metadata to a data frame.
#'
#' Source column names are preserved and their environmental meaning
#' is stored in schema metadata, allowing downstream plotting,
#' reporting, trend analysis, geochemical calculations,
#' censored-data methods, and radiochemistry workflows to operate on
#' a consistent environmental data model.
#'
#' @param df Data frame.
#' @param site Site identifier column.
#' @param location Location identifier column.
#' @param sample_date Sample date column.
#' @param analyte Analyte column.
#' @param result Analytical result column.
#' @param unit Unit column.
#' @param sample_id Sample identifier column.
#' @param reporting_flag Laboratory qualifier column.
#' @param reporting_limit Reporting limit column.
#' @param detection_limit Detection limit column.
#' @param quantitation_limit Quantitation limit column
#'   (PQL, LOQ, SQL, etc.).
#' @param achieved_mdc Achieved minimum detectable concentration
#'   (radiochemistry).
#' @param measurement_uncertainty Measurement uncertainty column.
#' @param method Analytical method column.
#' @param analysis_date Analysis date column.
#' @param matrix Matrix column.
#' @param lab_name Laboratory column.
#' @param analysis_metadata Optional tidyselect specification identifying
#'   analysis-context metadata columns (for example analytical methods,
#'   laboratories, matrices, measurement uncertainty,
#'   or other analytical metadata).
#' @param source_metadata Optional tidyselect specification
#'   identifying source-system-specific metadata columns.
#' @param qualifier_table Optional qualifier lookup table.
#' @param analyte_lookup Optional analyte metadata lookup table.
#'   Defaults to \code{analyte_tbl}. Used to standardize analyte
#'   names, assign analyte identifiers, and attach analyte metadata.
#'
#' @returns
#' A tibble with class `env_schema`.
#'
#' The returned object retains all original source columns and stores
#' environmental column mappings in object metadata.
#'
#' Additional manager-generated columns may include:
#'
#' \describe{
#'   \item{.analysis_result}{
#'   Standardized analytical value used for calculations.
#'   }
#'
#'   \item{.censored}{
#'   Logical censoring indicator.
#'   }
#'
#'   \item{.display_threshold}{
#'   Threshold used when constructing display-ready censored values.
#'   Reporting limits are preferred, followed by quantitation limits,
#'   detection limits, and finally the analytical result itself.
#'   }
#'
#'   \item{.display_threshold_type}{
#'   Source of the display threshold. One of
#'   `"reporting_limit"`,
#'   `"quantitation_limit"`,
#'   `"detection_limit"`, or
#'   `"analysis_result"`.
#'   }
#'
#'   \item{.display_result}{
#'   Display-ready reporting result suitable for tables and exports.
#'   }
#' }
#'
#' @details
#' `env_schema()` does not rename or modify source data columns.
#'
#' Instead, the environmental meaning of each source column is stored
#' as schema metadata through an internal column-mapping structure.
#'
#' For example:
#'
#' \preformatted{
#' env_schema(
#'   data,
#'   location = location_id,
#'   sample_date = sample_date,
#'   analyte = parameter,
#'   result = concentration
#' )
#' }
#'
#' records that `location_id` contains location identifiers,
#' `sample_date` contains sample dates,
#' `parameter` contains analyte names,
#' and `concentration` contains analytical results.
#'
#'
#' The constructor may also create standardized derived columns used
#' internally by manager for result classification, analyte metadata,
#' reporting workflows, plotting, and statistical calculations.
#'
#' Stored schema mappings can be inspected using
#' `schema_mapping()`.
#'
#' @examples
#' \dontrun{
#' gw <- env_schema(
#'   my_data,
#'   location = location_id,
#'   sample_date = sample_date,
#'   analyte = param_name,
#'   result = analysis_result,
#'   unit = default_unit
#' )
#'
#' schema_mapping(gw)
#' }
#'
#' @export
env_schema <- function(
  df,
  site = NULL,
  location,
  sample_date,
  analyte,
  result,
  unit = NULL,
  sample_id = NULL,
  reporting_flag = NULL,

  reporting_limit = NULL,
  detection_limit = NULL,
  quantitation_limit = NULL,

  achieved_mdc = NULL,
  measurement_uncertainty = NULL,

  method = NULL,
  analysis_date = NULL,
  matrix = NULL,
  lab_name = NULL,

  analysis_metadata = NULL,
  source_metadata = NULL,
  qualifier_table = NULL,

  display_options = default_display_options(),

  analyte_lookup = analyte_tbl
) {
  if (!is.data.frame(df)) {
    rlang::abort(
      "`df` must be a data frame."
    )
  }

  analysis_metadata <- rlang::enquo(analysis_metadata)
  source_metadata <- rlang::enquo(source_metadata)

  if (is.null(qualifier_table)) {
    qualifier_table <- .qualifier_tbl
  }

  required_mapping <- rlang::enquos(
    location,
    sample_date,
    analyte,
    result
  )

  if (any(vapply(required_mapping, rlang::quo_is_missing, logical(1)))) {
    rlang::abort(
      "location, sample_date, analyte, and result mappings are required."
    )
  }

  .validate_display_options(
    display_options
  )

  column_mapping <- rlang::enquos(
    .site = site,
    .location = location,
    .sample_date = sample_date,
    .analyte = analyte,

    .result = result,
    .unit = unit,

    .sample_id = sample_id,
    .reporting_flag = reporting_flag,

    .reporting_limit = reporting_limit,
    .detection_limit = detection_limit,
    .quantitation_limit = quantitation_limit,

    .achieved_mdc = achieved_mdc,

    .measurement_uncertainty = measurement_uncertainty,

    .method = method,
    .analysis_date = analysis_date,
    .matrix = matrix,
    .lab_name = lab_name
  )

  column_mapping <- column_mapping[
    !vapply(
      column_mapping,
      rlang::quo_is_null,
      logical(1)
    )
  ]

  source_cols <- vapply(
    column_mapping,
    function(x) {
      expr <- rlang::quo_get_expr(x)

      if (is.null(expr)) {
        return(NA_character_)
      }

      if (!rlang::is_symbol(expr)) {
        rlang::abort(
          paste(
            "Schema mappings must be simple column names.",
            "Received:",
            rlang::expr_deparse(expr)
          )
        )
      }

      rlang::as_name(expr)
    },
    character(1)
  )

  env_mapping <- list(
    site = unname(source_cols[".site"]),
    location = unname(source_cols[".location"]),
    sample_date = unname(source_cols[".sample_date"]),
    analyte = unname(source_cols[".analyte"]),
    result = unname(source_cols[".result"]),

    unit = unname(source_cols[".unit"]),
    sample_id = unname(source_cols[".sample_id"]),

    reporting_flag = unname(source_cols[".reporting_flag"]),

    reporting_limit = unname(source_cols[".reporting_limit"]),

    detection_limit = unname(source_cols[".detection_limit"]),

    quantitation_limit = unname(source_cols[".quantitation_limit"]),

    achieved_mdc = unname(source_cols[".achieved_mdc"]),

    measurement_uncertainty = unname(source_cols[".measurement_uncertainty"]),

    method = unname(source_cols[".method"]),

    analysis_date = unname(source_cols[".analysis_date"]),

    matrix = unname(source_cols[".matrix"]),

    lab_name = unname(source_cols[".lab_name"])
  )

  env_mapping <- env_mapping[
    !vapply(
      env_mapping,
      function(x) {
        length(x) == 1 && is.na(x)
      },
      logical(1)
    )
  ]

  missing_cols <- setdiff(
    source_cols,
    names(df)
  )

  if (length(missing_cols) > 0) {
    rlang::abort(
      paste(
        "Missing columns:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  out <- tibble::as_tibble(df)

  # --------------------------------------------------------------
  # Qualifier classification
  # --------------------------------------------------------------

  if (
    !all(
      c(
        "reporting_flag",
        "result_type",
        "detected",
        "censored",
        "estimated",
        "rejected"
      ) %in%
        names(qualifier_table)
    )
  ) {
    rlang::abort(
      "qualifier_table must contain required columns."
    )
  }

  if (!is.null(env_mapping$reporting_flag)) {
    qualifier_info <- .classify_results(
      out[[env_mapping$reporting_flag]],
      lookup = qualifier_table
    )
    out <- dplyr::bind_cols(
      out,
      qualifier_info |>
        dplyr::select(
          result_type,
          detected,
          censored,
          estimated,
          rejected
        ) |>
        dplyr::rename(
          .result_type = result_type,
          .detected = detected,
          .censored = censored,
          .estimated = estimated,
          .rejected = rejected
        )
    )
  } else {
    out <- out |>
      dplyr::mutate(
        .result_type = "detect",
        .detected = TRUE,
        .censored = FALSE,
        .estimated = FALSE,
        .rejected = FALSE
      )
  }

  # --------------------------------------------------------------
  # Derived fields
  # --------------------------------------------------------------

  result_col <- env_mapping$result

  out <- out |>
    dplyr::mutate(
      .usable = !.rejected,

      .original_lab_result = .data[[result_col]],

      .analysis_result = .data[[result_col]]
    )

  if (!is.null(env_mapping$method)) {
    method_col <- env_mapping$method

    out <- out |>
      dplyr::mutate(
        .analysis_method = .data[[method_col]]
      )
  } else {
    out <- out |>
      dplyr::mutate(
        .analysis_method = "raw"
      )
  }

  if (!is.null(env_mapping$unit)) {
    unit_col <- env_mapping$unit

    out <- out |>
      dplyr::mutate(
        .original_lab_unit = .data[[unit_col]],

        .analysis_unit = .data[[unit_col]]
      )
  } else {
    out <- out |>
      dplyr::mutate(
        .original_lab_unit = NA_character_,
        .analysis_unit = NA_character_
      )
  }

  # --------------------------------------------------------------
  # Relative uncertainty
  # --------------------------------------------------------------

  if (!is.null(env_mapping$measurement_uncertainty)) {
    unc_col <- env_mapping$measurement_uncertainty

    out <- out |>
      dplyr::mutate(
        .relative_uncertainty = dplyr::if_else(
          is.na(.original_lab_result) |
            .original_lab_result == 0,
          NA_real_,
          .data[[unc_col]] /
            abs(.original_lab_result)
        )
      )
  }

  # --------------------------------------------------------------
  # Analyte metadata
  # --------------------------------------------------------------

  if (!is.null(analyte_lookup)) {
    validate_analyte_tbl(
      analyte_lookup
    )

    out <- out |>
      .resolve_analyte_metadata(
        analyte_col = env_mapping$analyte,
        alias_lookup = analyte_alias_lookup,
        analyte_lookup = analyte_lookup
      )
  }

  # --------------------------------------------------------------
  # Unit standardization
  # --------------------------------------------------------------

  out <- out |>
    .standardize_schema_results() |>
    .build_analyte_labels()

  # --------------------------------------------------------------
  # Display standardization
  # --------------------------------------------------------------
  display_metadata <-
    .create_display_metadata(
      df = out,
      env_mapping = env_mapping,
      display_options = display_options
    )

  out <- dplyr::bind_cols(
    out,
    display_metadata
  )

  # --------------------------------------------------------------
  # Metadata columns
  # --------------------------------------------------------------

  analysis_metadata_cols <- character()

  if (!rlang::quo_is_null(analysis_metadata)) {
    analysis_metadata_cols <- names(
      tidyselect::eval_select(
        analysis_metadata,
        df
      )
    )
  }

  source_metadata_cols <- character()

  if (!rlang::quo_is_null(source_metadata)) {
    source_metadata_cols <- names(
      tidyselect::eval_select(
        source_metadata,
        df
      )
    )
  }
  # --------------------------------------------------------------
  # Column order
  # --------------------------------------------------------------

  priority_cols <- c(
    unlist(
      env_mapping[c(
        "site",
        "location",
        "analyte",
        "sample_date",
        "result",
        "unit",
        "reporting_flag",
        "reporting_limit",
        "detection_limit"
      )],
      use.names = FALSE
    ),
    ".analysis_result",
    ".analysis_unit",
    ".censored"
  )

  priority_cols <- unique(
    priority_cols[
      !is.na(priority_cols) &
        nzchar(priority_cols)
    ]
  )

  out <- dplyr::relocate(
    out,
    dplyr::any_of(priority_cols)
  )

  # --------------------------------------------------------------
  # Attributes
  # --------------------------------------------------------------

  threshold_metadata <- env_mapping[
    intersect(
      names(env_mapping),
      c(
        "reporting_limit",
        "detection_limit",
        "quantitation_limit",
        "achieved_mdc"
      )
    )
  ]

  threshold_metadata <- unname(
    unlist(
      threshold_metadata,
      use.names = FALSE
    )
  )

  out <- new_env_schema(
    out,
    env_mapping = env_mapping,
    analysis_metadata = analysis_metadata_cols,
    threshold_metadata = threshold_metadata,
    source_metadata = source_metadata_cols,
    schema_metadata = names(env_mapping),
    display_options = display_options,
    schema_version = "0.5.0",
    analyte_lookup = analyte_lookup
  )

  validate_env_schema(out)

  out
}

#' @export
default_display_options <- function() {
  list(
    digits = 6,

    scientific = FALSE,

    trim_trailing_zeros = TRUE,

    censor_prefix = "<",

    report_style = "standard"
  )
}

#' @keywords internal
.display_threshold_types <- c(
  reporting_limit = "reporting_limit",
  quantitation_limit = "quantitation_limit",
  detection_limit = "detection_limit",
  analysis_result = "analysis_result"
)

#' @keywords internal
.display_threshold_labels <- c(
  reporting_limit = "RL",
  quantitation_limit = "PQL",
  detection_limit = "DL",
  analysis_result = "Result"
)


#' @keywords internal
.format_display_threshold <- function(
  x,
  options = default_display_options()
) {
  x <- as.numeric(x)

  out <- format(
    signif(
      x,
      digits = options$digits
    ),
    scientific = options$scientific,
    trim = TRUE
  )

  if (
    isTRUE(
      options$trim_trailing_zeros
    )
  ) {
    out <- stringr::str_replace(
      out,
      "\\.$",
      ""
    )

    out <- stringr::str_replace(
      out,
      "(\\.[0-9]*?)0+$",
      "\\1"
    )
  }

  out
}

#' Build display-ready analytical results and metadata
#'
#' @param df Data frame.
#' @param env_mapping Schema mapping list.
#' @param display_options description
#'
#' @returns
#' A tibble containing:
#' \describe{
#'   \item{.display_threshold}{Display threshold value.}
#'   \item{.display_threshold_type}{Source of the threshold.}
#'   \item{.display_result}{Display-ready result string.}
#' }
#'
#' @keywords internal
.create_display_metadata <- function(
  df,
  env_mapping,
  display_options = default_display_options()
) {
  reporting_limit <- if (!is.null(env_mapping$reporting_limit)) {
    df[[env_mapping$reporting_limit]]
  } else {
    rep(NA_real_, nrow(df))
  }

  quantitation_limit <- if (!is.null(env_mapping$quantitation_limit)) {
    df[[env_mapping$quantitation_limit]]
  } else {
    rep(NA_real_, nrow(df))
  }

  detection_limit <- if (!is.null(env_mapping$detection_limit)) {
    df[[env_mapping$detection_limit]]
  } else {
    rep(NA_real_, nrow(df))
  }

  display_threshold <- dplyr::coalesce(
    reporting_limit,
    quantitation_limit,
    detection_limit,
    df$.analysis_result
  )

  display_threshold_type <- dplyr::case_when(
    !is.na(reporting_limit) ~
      unname(.display_threshold_types["reporting_limit"]),

    is.na(reporting_limit) &
      !is.na(quantitation_limit) ~
      unname(.display_threshold_types["quantitation_limit"]),

    is.na(reporting_limit) &
      is.na(quantitation_limit) &
      !is.na(detection_limit) ~
      unname(.display_threshold_types["detection_limit"]),

    TRUE ~
      unname(.display_threshold_types["analysis_result"])
  )

  formatted_threshold <- .format_display_threshold(
    display_threshold,
    options = display_options
  )

  formatted_result <- .format_display_threshold(
    df$.analysis_result,
    options = display_options
  )

  tibble::tibble(
    .display_threshold = display_threshold,

    .display_threshold_type = display_threshold_type,

    .display_result = dplyr::case_when(
      is.na(display_threshold) ~
        NA_character_,

      !df$.censored ~
        formatted_result,

      display_options$report_style == "standard" ~
        paste0(
          display_options$censor_prefix,
          formatted_threshold
        ),

      display_options$report_style == "verbose" ~
        paste0(
          display_options$censor_prefix,
          " ",
          .display_threshold_labels[
            display_threshold_type
          ],
          " ",
          formatted_threshold
        ),

      TRUE ~
        paste0(
          display_options$censor_prefix,
          formatted_threshold
        )
    )
  )
}

#' Classify reporting qualifiers
#'
#' Classifies analytical results based on reporting qualifiers and
#' assigns result type, detection status, censoring status,
#' estimation flags, and usability flags.
#'
#' @param flag Reporting flag vector.
#' @param lookup Qualifier lookup table.
#'
#' @returns A tibble containing:
#' \itemize{
#'   \item \code{result_type}
#'   \item \code{detected}
#'   \item \code{censored}
#'   \item \code{estimated}
#'   \item \code{rejected}
#' }
#'
#' @keywords internal
.classify_results <- function(
  flag,
  lookup = .qualifier_tbl
) {
  flag <- stringr::str_squish(
    as.character(flag)
  )

  x <- tibble::tibble(
    reporting_flag = toupper(flag)
  )

  out <- x |>
    dplyr::left_join(
      lookup,
      by = "reporting_flag"
    )

  # ------------------------------------------------------------
  # Unqualified results are assumed to be detects
  # ------------------------------------------------------------

  out$result_type[
    is.na(out$reporting_flag)
  ] <- "detect"

  out$detected[
    is.na(out$reporting_flag)
  ] <- TRUE

  out$censored[
    is.na(out$reporting_flag)
  ] <- FALSE

  out$estimated[
    is.na(out$reporting_flag)
  ] <- FALSE

  out$rejected[
    is.na(out$reporting_flag)
  ] <- FALSE

  # ------------------------------------------------------------
  # Unknown qualifiers
  # ------------------------------------------------------------

  unknown <- !is.na(out$reporting_flag) &
    is.na(out$result_type)

  out$result_type[unknown] <- "unknown"

  out$detected[unknown] <- FALSE

  out$censored[unknown] <- FALSE

  out$estimated[unknown] <- FALSE

  out$rejected[unknown] <- FALSE

  out
}

#' Resolve laboratory reporting qualifiers
#'
#' Combines qualifier information from laboratory flags and
#' less-than indicators into a single reporting flag suitable
#' for classification with `.classify_results()`.
#'
#' Priority:
#' 1. FLAGS
#' 2. LT_MEASURE
#' 3. NA
#'
#' @param flags Character vector of laboratory qualifiers.
#' @param lt_measure Character vector of less-than indicators.
#'
#' @returns Character vector.
#'
#' @keywords internal
.resolve_reporting_flag <- function(
  flags = NULL,
  lt_measure = NULL
) {
  flags <- trimws(
    as.character(flags)
  )

  lt_measure <- trimws(
    as.character(lt_measure)
  )

  flags[
    flags %in%
      c(
        "",
        "NA"
      )
  ] <- NA_character_

  out <- flags

  missing_flag <- is.na(out)

  out[
    missing_flag &
      lt_measure %in% "<"
  ] <- "U"

  out
}

#' Default qualifier lookup table
#'
#' @keywords internal
.qualifier_tbl <- tibble::tribble(
  ~reporting_flag , ~result_type , ~detected , ~censored , ~estimated , ~rejected ,

  "<"             , "nondetect"  , FALSE     , TRUE      , FALSE      , FALSE     ,
  "ND"            , "nondetect"  , FALSE     , TRUE      , FALSE      , FALSE     ,
  "U"             , "nondetect"  , FALSE     , TRUE      , FALSE      , FALSE     ,
  "U1"            , "nondetect"  , FALSE     , TRUE      , FALSE      , FALSE     ,

  "J"             , "estimated"  , TRUE      , FALSE     , TRUE       , FALSE     ,
  "J1"            , "estimated"  , TRUE      , FALSE     , TRUE       , FALSE     ,
  "UJ"            , "estimated"  , FALSE     , TRUE      , TRUE       , FALSE     ,
  "JB"            , "estimated"  , TRUE      , FALSE     , TRUE       , FALSE     ,
  "EST"           , "estimated"  , TRUE      , FALSE     , TRUE       , FALSE     ,

  "R"             , "rejected"   , FALSE     , FALSE     , FALSE      , TRUE      ,
  "REJ"           , "rejected"   , FALSE     , FALSE     , FALSE      , TRUE
)

#' Return qualifier lookup table
#'
#' @returns Tibble of supported qualifier mappings.
#'
#' @export
qualifier_table <- function() {
  .qualifier_tbl
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

#' Check required env_schema columns structure
#'
#' @keywords internal
has_env_schema_structure <- function(x) {
  tryCatch(
    {
      validate_env_schema_structure(x)
      TRUE
    },
    error = function(e) FALSE
  )
}

#' @export
as_env_schema <- function(x, ...) {
  UseMethod("as_env_schema")
}

#' @export
as_env_schema.env_schema <- function(x, ...) {
  x
}

#' @export
as_env_schema.data.frame <- function(x, ...) {
  if (has_env_schema_structure(x)) {
    return(
      restore_env_schema(
        x = x,
        to = x
      )
    )
  }

  env_schema(
    x,
    ...
  )
}

#' @export
as_env_schema.default <- function(x, ...) {
  rlang::abort(
    "`x` must be a data.frame or env_schema."
  )
}

#' @export
tbl_sum.env_schema <- function(x) {
  out <- c(
    "env_schema",
    Rows = nrow(x)
  )

  if (!is.null(site_var(x))) {
    out <- c(
      out,
      Sites = dplyr::n_distinct(site(x))
    )
  }

  c(
    out,
    Locations = dplyr::n_distinct(location(x)),
    Analytes = dplyr::n_distinct(analyte(x)),
    Version = schema_version(x)
  )
}


#' Restore env_schema attributes
#'
#' Reattaches all env_schema metadata from a template object
#' to a modified data frame.
#'
#' @param x Data frame to restore.
#' @param to Template env_schema object.
#' @param validate Logical. Should the restored object be
#'   validated before returning?
#'
#' @returns An object of class `env_schema`.
#'
#' @keywords internal
restore_env_schema <- function(
  x,
  to,
  validate = TRUE
) {
  metadata_names <- names(x)

  mapping <- attr(to, "env_mapping", exact = TRUE)

  mapping <- mapping[
    unlist(mapping, use.names = FALSE) %in% metadata_names
  ]

  out <- new_env_schema(
    x,
    env_mapping = mapping,
    analysis_metadata = intersect(
      analysis_metadata(to),
      metadata_names
    ),
    threshold_metadata = intersect(
      threshold_metadata(to),
      metadata_names
    ),
    source_metadata = intersect(
      source_metadata(to),
      metadata_names
    ),
    analyte_metadata = intersect(
      known_analyte_metadata(),
      metadata_names
    ),
    schema_metadata = attr(
      to,
      "schema_metadata",
      exact = TRUE
    ),
    display_options = attr(
      to,
      "display_options",
      exact = TRUE
    ),
    schema_version = attr(
      to,
      "env_schema_version",
      exact = TRUE
    ),
    schema_created = attr(
      to,
      "env_schema_created",
      exact = TRUE
    ),
    analyte_lookup_source = attr(
      to,
      "analyte_lookup_source",
      exact = TRUE
    ),
    analyte_lookup_version = attr(
      to,
      "analyte_lookup_version",
      exact = TRUE
    ),
    analyte_lookup_n = attr(
      to,
      "analyte_lookup_n",
      exact = TRUE
    ),
    analyte_lookup_cols = attr(
      to,
      "analyte_lookup_cols",
      exact = TRUE
    )
  )

  # Preserve grouped_df/tibble classes and maintain a vctrs-compatible
  # class hierarchy.
  restored_classes <- unique(c(
    class(x),
    class(out)
  ))

  class(out) <- c(
    setdiff(
      restored_classes,
      c("env_schema", "data.frame")
    ),
    "env_schema",
    "data.frame"
  )

  if (!is.null(attr(x, "groups", exact = TRUE))) {
    attr(out, "groups") <- attr(x, "groups", exact = TRUE)
  }

  if (validate) {
    validate_env_schema(out)
  }

  out
}

#' Low-level env_schema constructor
#'
#' @keywords internal
new_env_schema <- function(
  x,
  env_mapping = NULL,
  analysis_metadata = character(),
  threshold_metadata = character(),
  schema_metadata = character(),
  analyte_metadata = NULL,

  source_metadata = character(),

  display_options = default_display_options(),

  schema_version = "0.5.0",
  schema_created = Sys.time(),

  analyte_lookup = NULL,

  analyte_lookup_source = NULL,
  analyte_lookup_version = NULL,
  analyte_lookup_n = NULL,
  analyte_lookup_cols = NULL
) {
  x <- tibble::as_tibble(x)

  if (is.null(analysis_metadata)) {
    analysis_metadata <- character()
  }

  if (is.null(threshold_metadata)) {
    threshold_metadata <- character()
  }

  if (is.null(source_metadata)) {
    source_metadata <- character()
  }

  if (is.null(schema_metadata)) {
    schema_metadata <- character()
  }

  if (is.null(analyte_metadata)) {
    analyte_cols <- c(
      ".source_analyte",
      ".normalized_input",

      ".analyte",
      ".analyte_id",
      ".normalized_analyte",

      ".fraction",
      ".is_known_analyte",

      ".short_name",
      ".casrn",

      ".analyte_group",
      ".subcategory",
      ".regulatory_class",

      ".std_unit",

      ".ion_class",
      ".charge",
      ".molecular_weight",
      ".meq_factor",

      ".analysis_result_meq",
      ".analysis_unit_meq",

      ".is_major_ion",
      ".is_trace_metal",
      ".is_radionuclide",
      ".is_general_chemistry",
      ".is_field_parameter",

      ".analyte_label",
      ".short_analyte_label"
    )

    analyte_metadata <- intersect(
      analyte_cols,
      names(x)
    )
  }

  if (
    is.null(analyte_lookup_source) &&
      !is.null(analyte_lookup)
  ) {
    analyte_lookup_source <-
      if (
        isTRUE(
          all.equal(
            analyte_lookup,
            analyte_tbl,
            check.attributes = FALSE
          )
        )
      ) {
        "package"
      } else {
        "user"
      }
  }

  if (
    is.null(analyte_lookup_version) &&
      !is.null(analyte_lookup)
  ) {
    analyte_lookup_version <-
      utils::packageVersion("manager")
  }

  if (
    is.null(analyte_lookup_n) &&
      !is.null(analyte_lookup)
  ) {
    analyte_lookup_n <- nrow(analyte_lookup)
  }

  if (
    is.null(analyte_lookup_cols) &&
      !is.null(analyte_lookup)
  ) {
    analyte_lookup_cols <- names(analyte_lookup)
  }

  .validate_display_options(display_options)

  structure(
    x,

    env_mapping = env_mapping,

    analysis_metadata = analysis_metadata,
    threshold_metadata = threshold_metadata,
    schema_metadata = schema_metadata,
    analyte_metadata = analyte_metadata,

    source_metadata = source_metadata,

    display_options = display_options,

    env_schema_version = schema_version,
    env_schema_created = schema_created,

    analyte_lookup_source = analyte_lookup_source,
    analyte_lookup_version = analyte_lookup_version,
    analyte_lookup_n = analyte_lookup_n,
    analyte_lookup_cols = analyte_lookup_cols,

    class = c(
      "env_schema",
      class(x)
    )
  )
}

#' @keywords internal
#' @noRd
known_analyte_metadata <- function() {
  c(
    ".source_analyte",
    ".normalized_input",
    ".analyte",
    ".analyte_id",
    ".normalized_analyte",
    ".fractionable",
    ".fraction",
    ".is_known_analyte",
    ".short_name",
    ".casrn",
    ".analyte_group",
    ".subcategory",
    ".regulatory_class",
    ".std_unit",
    ".ion_class",
    ".charge",
    ".molecular_weight",
    ".meq_factor",
    ".analysis_result_meq",
    ".analysis_unit_meq",
    ".is_major_ion",
    ".is_trace_metal",
    ".is_radionuclide",
    ".is_general_chemistry",
    ".is_field_parameter",
    ".analyte_label",
    ".short_analyte_label"
  )
}

#' @export
set_display_options <- function(
  x,
  digits = NULL,
  scientific = NULL,
  trim_trailing_zeros = NULL,
  censor_prefix = NULL,
  report_style = NULL
) {
  validate_env_schema(x)

  opts <- display_options(x)

  if (!is.null(digits)) {
    opts$digits <- digits
  }

  if (!is.null(scientific)) {
    opts$scientific <- scientific
  }

  if (!is.null(trim_trailing_zeros)) {
    opts$trim_trailing_zeros <- trim_trailing_zeros
  }

  if (!is.null(censor_prefix)) {
    opts$censor_prefix <- censor_prefix
  }

  if (!is.null(report_style)) {
    opts$report_style <- report_style
  }

  attr(
    x,
    "display_options"
  ) <- opts

  refresh_display_results(x)
}

#' @keywords internal
refresh_display_results <- function(x) {
  validate_env_schema(x)

  display_meta <- .create_display_metadata(
    df = x,
    env_mapping = schema_mapping(x),
    display_options = display_options(x)
  )

  x$.display_threshold <-
    display_meta$.display_threshold

  x$.display_threshold_type <-
    display_meta$.display_threshold_type

  x$.display_result <-
    display_meta$.display_result

  restore_env_schema(
    x,
    x,
    validate = FALSE
  )
}

#' @keywords internal
#' @noRd
.restore_geochemical_schema <- function(
  x,
  template
) {
  out <- restore_env_schema(
    x = x,
    to = template,
    validate = FALSE
  )

  mapping <- attr(
    out,
    "env_mapping",
    exact = TRUE
  )

  mapping$analyte <- "analyte"
  mapping$result <- ".analysis_result"
  mapping$unit <- ".analysis_unit"

  attr(
    out,
    "env_mapping"
  ) <- mapping

  validate_env_schema(out)

  out
}


# -------------------------------------------------------------------------
# Internal attribute helpers
# -------------------------------------------------------------------------

#' @keywords internal
get_env_mapping <- function(x) {
  attr(
    x,
    "env_mapping",
    exact = TRUE
  )
}

#' @keywords internal
get_schema_metadata <- function(x) {
  attr(
    x,
    "schema_metadata",
    exact = TRUE
  )
}

#' @keywords internal
get_analysis_metadata <- function(x) {
  attr(
    x,
    "analysis_metadata",
    exact = TRUE
  )
}

#' @keywords internal
get_schema_version <- function(x) {
  attr(
    x,
    "env_schema_version",
    exact = TRUE
  )
}

#' @keywords internal
get_schema_created <- function(x) {
  attr(
    x,
    "env_schema_created",
    exact = TRUE
  )
}

#' @keywords internal
get_source_metadata <- function(x) {
  attr(
    x,
    "source_metadata",
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
analysis_metadata <- function(x) {
  if (!is_env_schema(x)) {
    rlang::abort(
      "Object must inherit from 'env_schema'."
    )
  }

  meta <- get_analysis_metadata(x)

  if (is.null(meta)) {
    return(character())
  }

  meta
}

#' Return threshold metadata
#'
#' @param x An env_schema object.
#'
#' @returns Character vector.
#'
#' @export
threshold_metadata <- function(x) {
  if (!is_env_schema(x)) {
    rlang::abort(
      "Object must inherit from 'env_schema'."
    )
  }

  meta <- attr(
    x,
    "threshold_metadata",
    exact = TRUE
  )

  if (is.null(meta)) {
    return(character())
  }

  if (is.list(meta)) {
    meta <- unlist(
      meta,
      use.names = FALSE
    )
  }

  meta <- meta[!is.na(meta)]

  meta
}

#' Return display formatting options
#'
#' Returns display and reporting formatting settings attached
#' to an env_schema object.
#'
#' @param x An env_schema object.
#'
#' @returns Named list.
#'
#' @export
display_options <- function(x) {
  if (!inherits(x, "env_schema")) {
    rlang::abort(
      "Object must inherit from 'env_schema'."
    )
  }

  attr(
    x,
    "display_options",
    exact = TRUE
  )
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

  x |>
    dplyr::count(
      .result_type,
      sort = TRUE
    )
}

#' Return source metadata columns
#'
#' Returns source-system-specific metadata columns attached to an
#' env_schema object.
#'
#' These columns are not part of the core schema mapping and are
#' typically added by source-specific importers such as
#' `read_manages4()`.
#'
#' @param x An env_schema object.
#'
#' @returns Character vector of source metadata column names.
#'
#' @export
source_metadata <- function(x) {
  if (!is_env_schema(x)) {
    rlang::abort(
      "Object must inherit from 'env_schema'."
    )
  }

  meta <- get_source_metadata(x)

  if (is.null(meta)) {
    return(character())
  }

  meta
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
# Mapping accessors
# -------------------------------------------------------------------------

#' Schema Variable Accessors
#'
#' Access stored schema mappings.
#'
#' @name schema_accessors
#' @title Schema Variable Accessors
#'
#' @param x An `env_schema` object.
#'
#' @return A character scalar.
#'
#' @keywords internal
NULL

#' @rdname schema_accessors
#' @export
analyte_id_var <- function(x) {
  ".analyte_id"
}

#' @rdname schema_accessors
#' @export
site_var <- function(x) {
  get_env_mapping(x)$site
}

#' @rdname schema_accessors
#' @export
location_var <- function(x) {
  get_env_mapping(x)$location
}

#' @rdname schema_accessors
#' @export
sample_date_var <- function(x) {
  get_env_mapping(x)$sample_date
}

#' @rdname schema_accessors
#' @export
analysis_date_var <- function(x) {
  get_env_mapping(x)$analysis_date
}

#' @rdname schema_accessors
#' @export
analyte_var <- function(x) {
  get_env_mapping(x)$analyte
}

#' @rdname schema_accessors
#' @export
result_var <- function(x) {
  get_env_mapping(x)$result
}


#' @rdname schema_accessors
#' @export
analysis_result_var <- function(x) {
  ".analysis_result"
}

#' @rdname schema_accessors
#' @export
boxcox_transform_var <- function(x) {
  ".boxcox_transform"
}

#' @rdname schema_accessors
#' @export
analysis_unit_var <- function(x) {
  ".analysis_unit"
}

#' @rdname schema_accessors
#' @export
unit_var <- function(x) {
  get_env_mapping(x)$unit
}

#' @rdname schema_accessors
#' @export
sample_id_var <- function(x) {
  get_env_mapping(x)$sample_id
}

#' @rdname schema_accessors
#' @export
reporting_flag_var <- function(x) {
  get_env_mapping(x)$reporting_flag
}

#' @rdname schema_accessors
#' @export
reporting_limit_var <- function(x) {
  get_env_mapping(x)$reporting_limit
}

#' @rdname schema_accessors
#' @export
detection_limit_var <- function(x) {
  get_env_mapping(x)$detection_limit
}

#' @rdname schema_accessors
#' @export
quantitation_limit_var <- function(x) {
  get_env_mapping(x)$quantitation_limit
}

#' @rdname schema_accessors
#' @export
achieved_mdc_var <- function(x) {
  get_env_mapping(x)$achieved_mdc
}

#' @rdname schema_accessors
#' @export
method_var <- function(x) {
  get_env_mapping(x)$method
}

#' @rdname schema_accessors
#' @export
analysis_result_meq_var <- function(x) {
  ".analysis_result_meq"
}

#' @rdname schema_accessors
#' @export
analysis_unit_meq_var <- function(x) {
  ".analysis_unit_meq"
}

#' @rdname schema_accessors
#' @export
censored_var <- function(x) {
  ".censored"
}

# -------------------------------------------------------------------------
# Source-column accessor generics
# -------------------------------------------------------------------------

#' Return resolved analyte values
#'
#' Returns analyte identifiers according to the requested
#' analyte matching strategy.
#'
#' \describe{
#'   \item{exact}{
#'     Returns resolved analyte names and preserves
#'     dissolved/total distinctions.
#'   }
#'   \item{strict}{
#'     Returns analyte keys incorporating fraction information.
#'   }
#'   \item{normalized}{
#'     Returns canonical analyte identifiers suitable for
#'     grouping related analytes.
#'   }
#' }
#'
#' @param x Object.
#' @param analyte_matching Matching strategy.
#'
#' @export
analysis_analyte <- function(
  x,
  analyte_matching = c(
    "normalized",
    "strict",
    "exact"
  )
) {
  UseMethod("analysis_analyte")
}

#' Return site values
#'
#' @param x Object.
#'
#' @export
site <- function(x) {
  UseMethod("site")
}

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
sample_date <- function(x) {
  UseMethod("sample_date")
}

#' Return sample analysis dates
#'
#' @param x Object.
#'
#' @export
analysis_date <- function(x) {
  UseMethod("analysis_date")
}

#' Return analyte values
#'
#' @param x Object.
#'
#' @export
analyte <- function(x) {
  UseMethod("analyte")
}

#' Return analyte_id values
#'
#' @param x Object.
#'
#' @export
analyte_id <- function(x) {
  UseMethod("analyte_id")
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

#' Return analyte labels
#'
#' @param x Object.
#'
#' @export
analyte_label <- function(x) {
  UseMethod("analyte_label")
}

#' Return short analyte labels
#'
#' @param x Object.
#'
#' @export
short_analyte_label <- function(x) {
  UseMethod("short_analyte_label")
}

#' Return display-ready analytical results
#'
#' @param x Object.
#'
#' @export
display_result <- function(x) {
  UseMethod("display_result")
}

#' Return display-ready analytical threshold
#'
#' @param x Object.
#'
#' @export
display_threshold <- function(x) {
  UseMethod("display_threshold")
}

#' Return display-ready analytical threshold type
#'
#' @param x Object.
#'
#' @export
display_threshold_type <- function(x) {
  UseMethod("display_threshold_type")
}

# -------------------------------------------------------------------------
# env_schema source-column methods
# -------------------------------------------------------------------------

#' @export
analysis_analyte.env_schema <- function(
  x,
  analyte_matching = c(
    "exact",
    "strict",
    "normalized"
  )
) {
  .analyte_key(
    analyte = x$.analyte,
    normalized_analyte = x$.normalized_analyte,
    fraction = x$.fraction,
    analyte_matching = analyte_matching
  )
}

#' @export
site.env_schema <- function(x) {
  .get_mapped_column(
    x,
    site_var(x)
  )
}

#' @export
location.env_schema <- function(x) {
  .get_mapped_column(
    x,
    location_var(x)
  )
}

#' @export
sample_date.env_schema <- function(x) {
  .get_mapped_column(
    x,
    sample_date_var(x)
  )
}

#' @export
analysis_date.env_schema <- function(x) {
  .get_mapped_column(
    x,
    analysis_date_var(x)
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
analyte_id.env_schema <- function(x) {
  .get_mapped_column(
    x,
    analyte_id_var()
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

#' @export
display_result.env_schema <- function(x) {
  x$.display_result
}

#' @export
display_threshold.env_schema <- function(x) {
  x$.display_threshold
}

#' @export
display_threshold_type.env_schema <- function(x) {
  x$.display_threshold_type
}

# -------------------------------------------------------------------------
# env_schema standardized-column methods
# -------------------------------------------------------------------------

#' @export
analysis_result.env_schema <- function(x) {
  x[[analysis_result_var()]]
}

#' @export
analysis_unit.env_schema <- function(x) {
  x[[analysis_unit_var()]]
}

#' Extract milliequivalent results
#'
#' @param x An env_schema object.
#'
#' @returns Numeric vector.
#'
#' @export
analysis_result_meq <- function(x) {
  col <- analysis_result_meq_var()

  if (!col %in% names(x)) {
    rlang::abort(
      "'.analysis_result_meq' is not present."
    )
  }

  x[[col]]
}

#' Extract milliequivalent units
#'
#' @param x An env_schema object.
#'
#' @returns Character vector.
#'
#' @export
analysis_unit_meq <- function(x) {
  col <- analysis_unit_meq_var()

  if (!col %in% names(x)) {
    rlang::abort(
      "'.analysis_unit_meq' is not present."
    )
  }

  x[[col]]
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

#' @export
analyte_label.env_schema <- function(x) {
  x$.analyte_label
}

#' @export
short_analyte_label.env_schema <- function(x) {
  x$.short_analyte_label
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
analysis_analyte.default <- function(
  x,
  analyte_matching = c(
    "normalized",
    "strict",
    "exact"
  )
) {
  .accessor_not_implemented(
    "analysis_analyte",
    x
  )
}

#' @export
site.default <- function(x) {
  .accessor_not_implemented(
    "site",
    x
  )
}

#' @export
location.default <- function(x) {
  .accessor_not_implemented("location", x)
}

#' @export
sample_date.default <- function(x) {
  .accessor_not_implemented("sample_date", x)
}

#' @export
analysis_date.default <- function(x) {
  .accessor_not_implemented(
    "analysis_date",
    x
  )
}

#' @export
analyte.default <- function(x) {
  .accessor_not_implemented("analyte", x)
}

#' @export
analyte_id.default <- function(x) {
  .accessor_not_implemented(
    "analyte_id",
    x
  )
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

#' @export
analyte_label.default <- function(x) {
  .accessor_not_implemented(
    "analyte_label",
    x
  )
}

#' @export
short_analyte_label.default <- function(x) {
  .accessor_not_implemented(
    "short_analyte_label",
    x
  )
}

#' @export
display_result.default <- function(x) {
  .accessor_not_implemented(
    "display_result",
    x
  )
}

#' @export
display_threshold.default <- function(x) {
  .accessor_not_implemented(
    "display_threshold",
    x
  )
}

#' @export
display_threshold_type.default <- function(x) {
  .accessor_not_implemented(
    "display_threshold_type",
    x
  )
}

#' @exportS3Method dplyr::group_by env_schema
group_by.env_schema <- function(
  .data,
  ...,
  .add = FALSE,
  .drop = dplyr::group_by_drop_default(.data)
) {
  out <- dplyr::group_by(
    tibble::as_tibble(.data),
    ...,
    .add = .add,
    .drop = .drop
  )

  restored <- restore_env_schema(
    out,
    .data,
    validate = FALSE
  )

  restored
}

#' @export
ungroup.env_schema <- function(x, ...) {
  out <- dplyr::ungroup(
    tibble::as_tibble(x)
  )

  restore_env_schema(
    out,
    x,
    validate = FALSE
  )
}

#' @importFrom vctrs vec_restore
NULL

#' @export
vec_restore.env_schema <- function(
  x,
  to,
  ...
) {
  restore_env_schema(
    x,
    to,
    validate = FALSE
  )
}

#' @export
dplyr_reconstruct.env_schema <- function(
  data,
  template
) {
  restore_env_schema(
    data,
    template,
    validate = FALSE
  )
}

#' @export
select.env_schema <- function(.data, ...) {
  out <- dplyr::select(
    tibble::as_tibble(.data),
    ...
  )

  restore_env_schema(
    out,
    .data,
    validate = FALSE
  )
}

#' Copy env_schema attributes
#'
#' Copies schema metadata and custom attributes from one object to
#' another while preserving the target object's row names and column
#' names.
#'
#' This helper is primarily used by schema transformation functions that
#' create new data frames internally but should retain the original
#' \code{env_schema} attributes, including schema mappings, version
#' information, and other metadata stored as attributes.
#'
#' Standard data frame structural attributes such as \code{names} and
#' \code{row.names} are intentionally preserved from the target object.
#'
#' @param from Source object containing attributes to copy.
#' @param to Target object receiving attributes.
#'
#' @returns The \code{to} object with attributes inherited from
#'   \code{from}.
#'
#' @keywords internal
.copy_env_schema_attributes <- function(
  from,
  to
) {
  protected <- c(
    "names",
    "row.names"
  )

  atts <- attributes(from)

  atts <- atts[
    !names(atts) %in% protected
  ]

  attributes(to) <- utils::modifyList(
    attributes(to),
    atts
  )

  to
}

#' Build env_schema call safely
#'
#' @keywords internal
.build_env_schema_call <- function(
  data,
  site = NULL,
  location,
  sample_date,
  analyte,
  result,
  unit = NULL,
  sample_id = NULL,
  reporting_flag = NULL,
  reporting_limit = NULL,
  detection_limit = NULL,
  quantitation_limit = NULL,
  achieved_mdc = NULL,
  measurement_uncertainty = NULL,
  method = NULL,
  analysis_date = NULL,
  matrix = NULL,
  lab_name = NULL,
  analysis_metadata = NULL,
  source_metadata = NULL,
  qualifier_table = NULL,
  analyte_lookup = analyte_tbl
) {
  args <- list(
    df = data,
    location = rlang::enquo(location),
    sample_date = rlang::enquo(sample_date),
    analyte = rlang::enquo(analyte),
    result = rlang::enquo(result),
    qualifier_table = qualifier_table,
    analyte_lookup = analyte_lookup
  )

  optional_args <- list(
    site = rlang::enquo(site),
    unit = rlang::enquo(unit),
    sample_id = rlang::enquo(sample_id),
    reporting_flag = rlang::enquo(reporting_flag),
    reporting_limit = rlang::enquo(reporting_limit),
    detection_limit = rlang::enquo(detection_limit),
    quantitation_limit = rlang::enquo(quantitation_limit),
    achieved_mdc = rlang::enquo(achieved_mdc),
    measurement_uncertainty = rlang::enquo(measurement_uncertainty),
    method = rlang::enquo(method),
    analysis_date = rlang::enquo(analysis_date),
    matrix = rlang::enquo(matrix),
    lab_name = rlang::enquo(lab_name),
    analysis_metadata = rlang::enquo(analysis_metadata),
    source_metadata = rlang::enquo(source_metadata)
  )

  optional_args <- optional_args[
    !vapply(
      optional_args,
      rlang::quo_is_null,
      logical(1)
    )
  ]

  args <- c(
    args,
    optional_args
  )

  rlang::exec(
    env_schema,
    !!!args
  )
}

#' Attach source metadata columns
#'
#' @param x env_schema object.
#' @param columns Character vector of source metadata columns.
#'
#' @returns env_schema
#'
#' @keywords internal
set_source_metadata <- function(
  x,
  columns
) {
  validate_env_schema(x)

  missing_cols <- setdiff(
    columns,
    names(x)
  )

  if (length(missing_cols) > 0) {
    rlang::abort(
      paste(
        "Source metadata columns not found:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  attr(
    x,
    "source_metadata"
  ) <- columns

  out <- restore_env_schema(
    x = x,
    to = x,
    validate = FALSE
  )

  validate_env_schema(out)

  out
}
