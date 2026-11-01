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
#' @param location Location identifier column.
#' @param date Sample date column.
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
#' @param metadata Optional tidyselect specification identifying
#'   columns containing method-specific metadata.
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
#' Additional manager-generated columns may be added, including
#' result-classification fields, normalized analyte metadata,
#' analytical labels, uncertainty metrics, and reporting attributes.
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
#'   date = sample_date,
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
#'   date = sample_date,
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
  location,
  date,
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

  metadata = NULL,
  qualifier_table = NULL,
  analyte_lookup = analyte_tbl
) {
  stopifnot(is.data.frame(df))

  metadata <- rlang::enquo(metadata)

  if (is.null(qualifier_table)) {
    qualifier_table <- .qualifier_tbl
  }

  required_mapping <- rlang::enquos(
    location,
    date,
    analyte,
    result
  )

  if (any(vapply(required_mapping, rlang::quo_is_missing, logical(1)))) {
    rlang::abort(
      "location, date, analyte, and result mappings are required."
    )
  }

  column_mapping <- rlang::enquos(
    .location = location,
    .date = date,
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
    location = unname(source_cols[".location"]),
    date = unname(source_cols[".date"]),
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
    out <- out %>%
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

  out <- out %>%
    dplyr::mutate(
      .usable = !.rejected,

      .original_lab_result = .data[[result_col]],

      .analysis_result = .data[[result_col]]
    )

  if (!is.null(env_mapping$method)) {
    method_col <- env_mapping$method

    out <- out %>%
      dplyr::mutate(
        .analysis_method = .data[[method_col]]
      )
  } else {
    out <- out %>%
      dplyr::mutate(
        .analysis_method = "raw"
      )
  }

  if (!is.null(env_mapping$unit)) {
    unit_col <- env_mapping$unit

    out <- out %>%
      dplyr::mutate(
        .original_lab_unit = .data[[unit_col]],

        .analysis_unit = .data[[unit_col]]
      )
  } else {
    out <- out %>%
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

    out <- out %>%
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

    out <- out %>%
      .resolve_analyte_metadata(
        analyte_col = env_mapping$analyte,
        alias_lookup = analyte_alias_lookup,
        analyte_lookup = analyte_lookup
      ) %>%
      .build_analyte_labels()
  }

  # --------------------------------------------------------------
  # Unit standardization
  # --------------------------------------------------------------

  out <- out |>
    .standardize_schema_results()

  # --------------------------------------------------------------
  # Metadata columns
  # --------------------------------------------------------------

  metadata_cols <- character()

  if (!rlang::quo_is_null(metadata)) {
    metadata_cols <- names(
      tidyselect::eval_select(
        metadata,
        df
      )
    )
  }

  # --------------------------------------------------------------
  # Attributes
  # --------------------------------------------------------------

  threshold_metadata <- env_mapping[
    c(
      "reporting_limit",
      "detection_limit",
      "quantitation_limit",
      "achieved_mdc"
    )
  ]

  threshold_metadata <- threshold_metadata[
    !vapply(
      threshold_metadata,
      function(x) {
        length(x) == 1 && is.na(x)
      },
      logical(1)
    )
  ]

  out <- new_env_schema(
    out,
    env_mapping = env_mapping,
    measurement_metadata = metadata_cols,
    threshold_metadata = threshold_metadata,
    schema_metadata = names(env_mapping),
    schema_version = "0.5.0",
    analyte_lookup = analyte_lookup
  )

  validate_env_schema(out)

  out
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
  
  out <- x %>%
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
  
  "J"             , "estimated"  , TRUE      , FALSE     , TRUE       , FALSE     ,
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
  c(
    "env_schema",
    Rows = nrow(x),
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
  
  mapping <- attr(
    to,
    "env_mapping",
    exact = TRUE
  )
  
  mapping <- mapping[
    unlist(mapping, use.names = FALSE) %in%
      names(x)
  ]
  
  out <- new_env_schema(
    x,
    
    env_mapping = mapping,
    
    measurement_metadata = intersect(
      attr(
        to,
        "measurement_metadata",
        exact = TRUE
      ),
      metadata_names
    ),
    
    threshold_metadata = intersect(
      attr(
        to,
        "threshold_metadata",
        exact = TRUE
      ),
      metadata_names
    ),
    
    schema_metadata = intersect(
      attr(
        to,
        "schema_metadata",
        exact = TRUE
      ),
      metadata_names
    ),
    
    analyte_metadata = intersect(
      known_analyte_metadata(),
      metadata_names
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
  
  class(out) <- unique(
    c(
      setdiff(class(x), c("tbl_df", "tbl", "data.frame")),
      class(out)
    )
  )
  
  dplyr_attrs <- attributes(x)
  
  if (!is.null(dplyr_attrs$groups)) {
    attr(out, "groups") <- dplyr_attrs$groups
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
    measurement_metadata = character(),
    threshold_metadata = character(),
    schema_metadata = character(),
    analyte_metadata = NULL,
    schema_version = "0.5.0",
    schema_created = Sys.time(),
    
    analyte_lookup = NULL,
    
    analyte_lookup_source = NULL,
    analyte_lookup_version = NULL,
    analyte_lookup_n = NULL,
    analyte_lookup_cols = NULL
) {
  x <- tibble::as_tibble(x)
  
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
  
  structure(
    x,
    
    env_mapping = env_mapping,
    
    measurement_metadata = measurement_metadata,
    threshold_metadata = threshold_metadata,
    schema_metadata = schema_metadata,
    analyte_metadata = analyte_metadata,
    
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

analyte_id_var <- function(x) {
  ".analyte_id"
}

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

analysis_result_var <- function() {
  ".analysis_result"
}

analysis_unit_var <- function() {
  ".analysis_unit"
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

#' MEQ result column
#'
#' @param x An env_schema object.
#'
#' @returns Character scalar.
#'
#' @export
analysis_result_meq_var <- function(x) {
  ".analysis_result_meq"
}

#' MEQ unit column
#'
#' @param x An env_schema object.
#'
#' @returns Character scalar.
#'
#' @export
analysis_unit_meq_var <- function(x) {
  ".analysis_unit_meq"
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
analyte_id <- function(x) {
  x[[analyte_id_var(x)]]
}

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
  col <- analysis_result_meq_var(x)
  
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
  col <- analysis_unit_meq_var(x)
  
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
  
  restore_env_schema(
    out,
    .data,
    validate = FALSE
  )
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
print.env_schema <- function(x, ...) {
  cat("<env_schema>\n")
  
  cat(
    nrow(x),
    "rows x",
    ncol(x),
    "columns\n"
  )
  
  cat(
    "Locations:",
    dplyr::n_distinct(location(x)),
    "\n"
  )
  
  cat(
    "Analytes:",
    dplyr::n_distinct(analyte(x)),
    "\n"
  )
  
  cat(
    "Valid:",
    has_env_schema_structure(x),
    "\n"
  )
  
  cat(
    "Version:",
    attr(
      x,
      "env_schema_version",
      exact = TRUE
    ),
    "\n"
  )
  
  invisible(x)
}

#' Standardize env_schema analytical results
#'
#' Converts .analysis_result values to analyte-specific
#' standard reporting units and updates .analysis_unit.
#'
#' Original laboratory values are preserved in
#' .original_lab_result and .original_lab_unit.
#'
#' @param df Data frame containing schema analyte metadata.
#'
#' @returns Data frame.
#'
#' @keywords internal
.standardize_schema_results <- function(df) {
  if (!".std_unit" %in% names(df)) {
    return(df)
  }
  
  required_cols <- c(
    ".analysis_result",
    ".analysis_unit",
    ".std_unit"
  )
  
  .validate_required_cols(
    df,
    required_cols
  )
  
  df$.analysis_unit <- .normalize_unit(
    df$.analysis_unit
  )
  
  df$.std_unit <- .normalize_unit(
    df$.std_unit
  )
  
  conversion_groups <- df |>
    dplyr::filter(
      !is.na(.std_unit)
    ) |>
    dplyr::distinct(
      .analysis_unit,
      .std_unit
    )
  
  for (i in seq_len(nrow(conversion_groups))) {
    from_unit <- conversion_groups$.analysis_unit[i]
    to_unit <- conversion_groups$.std_unit[i]
    
    if (identical(from_unit, to_unit)) {
      next
    }
    
    if (
      is.na(from_unit) ||
      is.na(to_unit)
    ) {
      next
    }
    
    idx <-
      !is.na(df$.analysis_unit) &
      !is.na(df$.std_unit) &
      df$.analysis_unit == from_unit &
      df$.std_unit == to_unit
    
    if (!any(idx)) {
      next
    }
    
    df$.analysis_result[idx] <-
      .safe_convert_units(
        value = df$.analysis_result[idx],
        from_unit = from_unit,
        to_unit = to_unit
      )
  }
  
  df$.analysis_unit <- dplyr::coalesce(
    df$.std_unit,
    df$.analysis_unit
  )
  
  df
}

#' Convert standards to reporting units
#'
#' Converts regulatory or screening standard values into the same
#' reporting units used by analytical results.
#'
#' @param df Data frame containing standard values and units.
#'
#' @param data_unit_col Character string identifying the reporting
#'   unit column associated with analytical results. Default is
#'   `"default_unit"`.
#'
#' @return
#' A data frame with standard values converted to match the
#' reporting units contained in `data_unit_col`.
#'
#' @details
#' Standard values are converted from `standard_unit` to the unit
#' specified in `data_unit_col`.
#'
#' Successful conversions update both:
#'
#' \describe{
#'   \item{standard_value}{
#'     Standard concentration converted to the reporting unit.
#'   }
#'   \item{standard_unit}{
#'     Updated to match the reporting unit.
#'   }
#' }
#'
#' Invalid unit definitions are identified using
#' [`.is_valid_unit()`] and reported through warnings.
#'
#' Unit conversion is performed row-wise because individual rows
#' may require different source and destination unit pairs.
#'
#' Conversion failures do not stop execution and instead return
#' `NA_real_` for affected standards.
#'
#' @section Validation:
#'
#' The function validates:
#'
#' \itemize{
#'   \item required columns exist;
#'   \item source and destination units are valid;
#'   \item standard values are non-missing before conversion.
#' }
#'
#' @noRd
.convert_standard_units <- function(
    df,
    data_unit_col = ".analysis_unit"
) {
  .validate_required_cols(
    df,
    c(
      ".standard_value",
      ".standard_unit",
      data_unit_col
    )
  )
  
  from_unit <- .normalize_unit(
    df$.standard_unit
  )
  
  to_unit <- .normalize_unit(
    df[[data_unit_col]]
  )
  
  valid_from <- .is_valid_unit(from_unit)
  
  valid_to <- .is_valid_unit(to_unit)
  
  invalid_idx <-
    (!valid_from | !valid_to) &
    !is.na(from_unit) &
    !is.na(to_unit)
  
  if (any(invalid_idx)) {
    invalid_pairs <- unique(
      paste(
        from_unit[invalid_idx],
        "->",
        to_unit[invalid_idx]
      )
    )
    
    cli::cli_warn(
      c(
        "Invalid unit conversions detected.",
        "i" = paste(
          invalid_pairs,
          collapse = ", "
        )
      )
    )
  }
  
  convert_idx <-
    !is.na(df$.standard_value) &
    valid_from &
    valid_to &
    from_unit != to_unit
  
  if (!any(convert_idx)) {
    return(df)
  }
  
  converted_values <- purrr::pmap_dbl(
    list(
      value = df$.standard_value[convert_idx],
      from_unit = from_unit[convert_idx],
      to_unit = to_unit[convert_idx]
    ),
    .safe_convert_unit
  )
  
  convert_rows <- which(convert_idx)
  
  successful_rows <- convert_rows[
    !is.na(converted_values)
  ]
  
  if (length(successful_rows) == 0) {
    return(df)
  }
  
  df$.standard_value[
    successful_rows
  ] <- converted_values[
    !is.na(converted_values)
  ]
  
  df$.standard_unit[
    successful_rows
  ] <- to_unit[
    successful_rows
  ]
  
  df
}

#' Convert analytical values between units
#'
#' Converts numeric values from one unit to another using the
#' \pkg{units} package.
#'
#' @param value Numeric vector of values.
#'
#' @param from_unit Character string specifying the source unit.
#'
#' @param to_unit Character string specifying the destination unit.
#'
#' @return
#' Numeric vector converted to the requested units.
#'
#' @details
#' Unit conversion is performed using
#' [units::set_units()] and [units::drop_units()].
#'
#' @noRd
.convert_units <- function(
    value,
    from_unit,
    to_unit
) {
  units::drop_units(
    units::set_units(
      units::set_units(
        value,
        from_unit,
        mode = "standard"
      ),
      to_unit,
      mode = "standard"
    )
  )
}

#' Safely convert a single value between units
#'
#' Performs a unit conversion while suppressing conversion errors.
#'
#' @param value Numeric value to convert.
#'
#' @param from_unit Character string specifying the source unit.
#'
#' @param to_unit Character string specifying the destination unit.
#'
#' @return
#' Converted numeric value.
#'
#' Returns `NA_real_` if conversion cannot be performed.
#'
#' @details
#' This helper wraps [`.convert_units()`] in a
#' [base::tryCatch()] block and converts conversion failures into
#' warnings.
#'
#' Typical causes of conversion failure include:
#'
#' \itemize{
#'   \item incompatible dimensions (for example `"mg/L"` to
#'   `"feet"`);
#'   \item invalid unit strings;
#'   \item unsupported unit definitions.
#' }
#'
#' This helper is intended for row-wise conversions where source
#' and destination units may vary between observations.
#'
#' @noRd
.safe_convert_unit <- function(
    value,
    from_unit,
    to_unit
) {
  if (
    is.na(from_unit) ||
    is.na(to_unit)
  ) {
    return(NA_real_)
  }
  
  tryCatch(
    .convert_units(
      value = value,
      from_unit = from_unit,
      to_unit = to_unit
    ),
    error = function(e) {
      cli::cli_warn(
        paste0(
          "Skipped incompatible unit conversion: ",
          from_unit,
          " -> ",
          to_unit
        )
      )
      
      NA_real_
    }
  )
}

#' Safely convert a vector of values between units
#'
#' Performs vectorized unit conversion while suppressing
#' conversion errors.
#'
#' @param value Numeric vector to convert.
#'
#' @param from_unit Character string specifying the source unit.
#'
#' @param to_unit Character string specifying the destination unit.
#'
#' @return
#' A numeric vector containing converted values.
#'
#' If conversion fails, a vector of `NA_real_` values having the
#' same length as `value` is returned.
#'
#' @details
#' This helper wraps [`.convert_units()`] in a
#' [base::tryCatch()] block and converts conversion failures into
#' warnings.
#'
#' Unlike [`.safe_convert_unit()`], this function is intended for
#' grouped conversions where all values share a common source and
#' destination unit.
#'
#' It is used by [`standardize_units()`] to efficiently convert
#' analytical results by unit-pair group rather than converting
#' observations one at a time.
#'
#' @noRd
.safe_convert_units <- function(
    value,
    from_unit,
    to_unit
) {
  if (
    is.na(from_unit) ||
    is.na(to_unit)
  ) {
    return(
      rep(
        NA_real_,
        length(value)
      )
    )
  }
  
  tryCatch(
    .convert_units(
      value = value,
      from_unit = from_unit,
      to_unit = to_unit
    ),
    error = function(e) {
      cli::cli_warn(
        paste0(
          "Skipped incompatible unit conversion: ",
          from_unit,
          " -> ",
          to_unit
        )
      )
      
      rep(
        NA_real_,
        length(value)
      )
    }
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

#' Normalize analytical units
#'
#' Normalizes unit strings to a consistent format for comparison and
#' unit conversion.
#'
#' The function:
#' \itemize{
#'   \item trims leading and trailing whitespace;
#'   \item converts microgram symbols (`µg`, `μg`) to `"ug"`;
#'   \item converts all text to lower case.
#' }
#'
#' Examples:
#'
#' \itemize{
#'   \item `" µg/L "` becomes `"ug/l"`
#'   \item `"MG/L"` becomes `"mg/l"`
#' }
#'
#' @param x Character vector of analytical units.
#'
#' @return
#' A normalized character vector of units.
#'
#' @noRd
.normalize_unit <- function(x) {
  x <- as.character(x)
  
  x %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("µg", "ug") %>%
    stringr::str_replace_all("μg", "ug") %>%
    stringr::str_to_lower()
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

#' Attach analyte metadata
#'
#' Joins analyte metadata from an analyte lookup table onto an
#' \code{env_schema} object using the internal analyte identifier.
#'
#' This helper enriches schema data with analyte-level attributes such as
#' canonical names, short names, categories, groups, fractions,
#' regulatory classifications, preferred plotting labels, and any future
#' metadata stored in \code{analyte_tbl}.
#'
#' The join is performed on \code{.analyte_id}, allowing analyte naming
#' conventions to evolve independently of downstream analyses.
#'
#' @param df An \code{env_schema} object containing a
#'   \code{.analyte_id} column.
#' @param lookup An analyte metadata table, typically
#'   \code{analyte_tbl}.
#'
#' @returns An enriched \code{env_schema} object containing all original
#'   observations plus analyte metadata columns from \code{lookup}.
#'
#' @seealso [analyte_tbl], [validate_analyte_tbl()]
#'
#' @keywords internal
.attach_analyte_metadata <- function(
    df,
    lookup = analyte_tbl
) {
  validate_analyte_tbl(lookup)
  
  lookup <- lookup %>%
    dplyr::rename(
      .analyte_id = analyte_id,
      .short_name = short_name,
      .casrn = casrn,
      .analyte_group = analyte_group,
      .subcategory = subcategory,
      .regulatory_class = regulatory_class,
      .std_unit = std_unit,
      .ion_class = ion_class,
      .charge = charge,
      .molecular_weight = molecular_weight,
      .meq_factor = meq_factor,
      .is_major_ion = is_major_ion,
      .is_trace_metal = is_trace_metal,
      .is_radionuclide = is_radionuclide,
      .is_general_chemistry = is_general_chemistry,
      .is_field_parameter = is_field_parameter
    ) %>%
    dplyr::select(-analyte)
  
  dplyr::left_join(
    df,
    lookup,
    by = ".analyte_id",
    relationship = "many-to-one"
  )
}

#' Build analyte display labels
#'
#' @keywords internal
.build_analyte_labels <- function(df) {
  if (!".analysis_unit" %in% names(df)) {
    return(df)
  }
  
  analyte_name <- if (".normalized_analyte" %in% names(df)) {
    dplyr::coalesce(
      df$.normalized_analyte,
      df$.source_analyte
    )
  } else {
    df$.source_analyte
  }
  
  short_name <- if (".short_name" %in% names(df)) {
    dplyr::coalesce(
      df$.short_name,
      analyte_name
    )
  } else {
    analyte_name
  }
  
  has_unit <- !is.na(df$.analysis_unit) &
    trimws(df$.analysis_unit) != ""
  
  analyte_label <- dplyr::if_else(
    has_unit,
    paste0(
      analyte_name,
      " (",
      df$.analysis_unit,
      ")"
    ),
    analyte_name
  )
  
  short_analyte_label <- dplyr::if_else(
    has_unit,
    paste0(
      short_name,
      " (",
      df$.analysis_unit,
      ")"
    ),
    short_name
  )
  
  df %>%
    dplyr::mutate(
      .analyte_label = analyte_label,
      .short_analyte_label = short_analyte_label
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
    location,
    date,
    analyte,
    result,
    unit = NULL,
    sample_id = NULL,
    reporting_flag = NULL,
    reporting_limit = NULL,
    detection_limit = NULL,
    detection_threshold = NULL,
    detection_threshold_type = NULL,
    measurement_uncertainty = NULL,
    method = NULL,
    analysis_date = NULL,
    matrix = NULL,
    lab_name = NULL,
    metadata = NULL,
    qualifier_table = NULL,
    analyte_lookup = analyte_tbl
) {
  args <- list(
    df = data,
    location = rlang::enquo(location),
    date = rlang::enquo(date),
    analyte = rlang::enquo(analyte),
    result = rlang::enquo(result),
    qualifier_table = qualifier_table,
    analyte_lookup = analyte_lookup
  )
  
  optional_args <- list(
    unit = rlang::enquo(unit),
    sample_id = rlang::enquo(sample_id),
    reporting_flag = rlang::enquo(reporting_flag),
    reporting_limit = rlang::enquo(reporting_limit),
    detection_limit = rlang::enquo(detection_limit),
    detection_threshold = rlang::enquo(detection_threshold),
    detection_threshold_type = rlang::enquo(detection_threshold_type),
    measurement_uncertainty = rlang::enquo(measurement_uncertainty),
    method = rlang::enquo(method),
    analysis_date = rlang::enquo(analysis_date),
    matrix = rlang::enquo(matrix),
    lab_name = rlang::enquo(lab_name),
    metadata = rlang::enquo(metadata)
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
