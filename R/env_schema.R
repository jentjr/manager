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
