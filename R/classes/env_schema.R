#' Construct an environmental data schema
#'
#' Standardizes environmental analytical data into a common structure
#' for plotting, reporting, trend analysis, geochemical calculations,
#' censored-data methods, and radiochemistry workflows.
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
#'
#' @returns A tibble with class `env_schema`.
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

  rename_map <- rlang::enquos(
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

  rename_map <- rename_map[
    !vapply(
      rename_map,
      rlang::quo_is_null,
      logical(1)
    )
  ]

  source_cols <- vapply(
    rename_map,
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

  source_cols <- stats::na.omit(source_cols)

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

  out <- dplyr::rename(
    df,
    !!!rename_map
  )

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

  if (".reporting_flag" %in% names(out)) {
    qualifier_info <- .classify_results(
      out$.reporting_flag,
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

  out <- out %>%
    dplyr::mutate(
      .usable = !.rejected,

      .original_lab_result = .result,

      .analysis_result = .result
    )

  if (".method" %in% names(out)) {
    out <- out %>%
      dplyr::mutate(
        .analysis_method = .method
      )
  } else {
    out <- out %>%
      dplyr::mutate(
        .analysis_method = "raw"
      )
  }

  if (".unit" %in% names(out)) {
    out <- out %>%
      dplyr::mutate(
        .original_lab_unit = .unit,
        .analysis_unit = .unit
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

  if (
    all(
      c(
        ".measurement_uncertainty",
        ".original_lab_result"
      ) %in%
        names(out)
    )
  ) {
    out <- out %>%
      dplyr::mutate(
        .relative_uncertainty = dplyr::if_else(
          is.na(.original_lab_result) |
            .original_lab_result == 0,
          NA_real_,
          .measurement_uncertainty /
            abs(.original_lab_result)
        )
      )
  }

  # --------------------------------------------------------------
  # Remove intermediate columns
  # --------------------------------------------------------------

  out <- out %>%
    dplyr::select(
      -dplyr::any_of(
        c(
          ".result",
          ".unit",
          ".method"
        )
      )
    )

  # --------------------------------------------------------------
  # Analyte metadata
  # --------------------------------------------------------------

  if (!is.null(analyte_lookup)) {
    validate_analyte_tbl(
      analyte_lookup
    )

    out <- out %>%
      .resolve_analyte_metadata(
        analyte_col = ".analyte",
        alias_lookup = analyte_alias_lookup,
        analyte_lookup = analyte_lookup
      ) %>%
      .build_analyte_labels()

    if (".short_name" %in% names(out)) {
      missing_metadata <- !out$.is_known_analyte

      if (any(missing_metadata)) {
        missing_analytes <- unique(
          out$.source_analyte[missing_metadata]
        )

        cli::cli_warn(
          c(
            "Analyte metadata missing.",
            "i" = paste(
              utils::head(
                missing_analytes,
                10
              ),
              collapse = ", "
            )
          )
        )
      }
    }
  }

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

  env_mapping <- list(
    source = lapply(
      rename_map,
      function(x) {
        rlang::as_name(
          rlang::get_expr(x)
        )
      }
    ),

    derived = list(
      analyte_id = ".analyte_id",

      source_analyte = ".source_analyte",
      normalized_input = ".normalized_input",

      normalized_analyte = ".normalized_analyte",
      fraction = ".fraction",
      is_known_analyte = ".is_known_analyte",

      original_lab_result = ".original_lab_result",
      original_lab_unit = ".original_lab_unit",

      usable = ".usable",

      analysis_result = ".analysis_result",
      analysis_unit = ".analysis_unit",
      analysis_method = ".analysis_method",

      reporting_limit = ".reporting_limit",
      detection_limit = ".detection_limit",
      quantitation_limit = ".quantitation_limit",
      achieved_mdc = ".achieved_mdc",

      analyte_label = ".analyte_label",
      short_analyte_label = ".short_analyte_label"
    ),

    analyte_metadata = list(
      short_name = ".short_name",
      casrn = ".casrn",
      analyte_group = ".analyte_group",
      subcategory = ".subcategory",
      regulatory_class = ".regulatory_class",
      std_unit = ".std_unit",
      ion_class = ".ion_class",
      charge = ".charge",
      molecular_weight = ".molecular_weight",
      meq_factor = ".meq_factor"
    )
  )

  out <- new_env_schema(
    out,

    env_mapping = env_mapping,

    measurement_metadata = metadata_cols,

    threshold_metadata = c(
      ".reporting_limit",
      ".detection_limit",
      ".quantitation_limit",
      ".achieved_mdc"
    )[
      c(
        ".reporting_limit",
        ".detection_limit",
        ".quantitation_limit",
        ".achieved_mdc"
      ) %in%
        names(out)
    ],

    schema_metadata = c(
      ".sample_id",
      ".reporting_flag",
      ".reporting_limit",
      ".detection_limit",
      ".quantitation_limit",
      ".achieved_mdc",
      ".measurement_uncertainty",
      ".analysis_method",
      ".analysis_date",
      ".matrix",
      ".lab_name"
    )[
      c(
        ".sample_id",
        ".reporting_flag",
        ".reporting_limit",
        ".detection_limit",
        ".quantitation_limit",
        ".achieved_mdc",
        ".measurement_uncertainty",
        ".analysis_method",
        ".analysis_date",
        ".matrix",
        ".lab_name"
      ) %in%
        names(out)
    ],

    schema_version = "0.4.0",

    analyte_lookup = analyte_lookup
  )

  validate_env_schema(out)

  out
}
