#' Transform Piper chemistry into plotting coordinates
#'
#' @noRd
.transform_piper_data <- function(
  df,
  location_id = "location_id",
  sample_date = "sample_date",
  param_name = "param_name",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  analyte_matching = c(
    "normalized",
    "strict",
    "exact"
  ),
  x_cation = "Calcium, dissolved",
  y_cation = "Magnesium, dissolved",
  z_cation = c(
    "Sodium, dissolved",
    "Potassium, dissolved"
  ),
  x_anion = c(
    "Chloride, total",
    "Fluoride, total"
  ),
  y_anion = "Alkalinity, total (lab)",
  z_anion = "Sulfate, total",
  scale_analyte = NULL
) {
  analyte_matching <- rlang::arg_match(
    analyte_matching
  )

  chemistry_df <- .prepare_major_ion_chemistry(
    df = df,
    location_id = location_id,
    sample_date = sample_date,
    param_name = param_name,
    analysis_result = analysis_result,
    default_unit = default_unit,
    analyte_matching = analyte_matching
  ) %>%
    .create_geochemical_components(
      list(
        piper_x_cation = .resolve_geochemical_analytes(
          x_cation,
          analyte_matching
        ),

        piper_y_cation = .resolve_geochemical_analytes(
          y_cation,
          analyte_matching
        ),

        piper_z_cation = .resolve_geochemical_analytes(
          z_cation,
          analyte_matching
        ),

        piper_x_anion = .resolve_geochemical_analytes(
          x_anion,
          analyte_matching
        ),

        piper_y_anion = .resolve_geochemical_analytes(
          y_anion,
          analyte_matching
        ),

        piper_z_anion = .resolve_geochemical_analytes(
          z_anion,
          analyte_matching
        )
      )
    )

  scale_df <- NULL
  scale_col <- NULL

  if (!is.null(scale_analyte)) {
    scale_col <- .resolve_geochemical_analytes(
      scale_analyte,
      analyte_matching
    )[1]

    scale_df <- .prepare_geochemical_data(
      df = df,
      analytes = scale_analyte,
      location_id = location_id,
      sample_date = sample_date,
      param_name = param_name,
      analysis_result = analysis_result,
      default_unit = default_unit,
      analyte_matching = analyte_matching
    )
  }

  # ---------------------------------------------------
  # CATION TRIANGLE
  # ---------------------------------------------------

  cations <- .calculate_cation_triangle(
    left = chemistry_df$piper_x_cation,
    top = chemistry_df$piper_y_cation,
    right = chemistry_df$piper_z_cation
  )

  # ---------------------------------------------------
  # ANION TRIANGLE
  # ---------------------------------------------------

  anions <- .calculate_anion_triangle(
    left = chemistry_df$piper_y_anion,
    top = chemistry_df$piper_z_anion,
    right = chemistry_df$piper_x_anion
  )

  # ---------------------------------------------------
  # DIAMOND
  # ---------------------------------------------------

  slope <- sqrt(3)

  b1 <- cations$cation_y -
    slope * cations$cation_x

  b2 <- anions$anion_y +
    slope * anions$anion_x

  diamond_x <-
    (b2 - b1) /
    (2 * slope)

  diamond_y <-
    slope * diamond_x + b1

  diamond <- tibble::tibble(
    diamond_x = diamond_x,
    diamond_y = diamond_y
  )

  result <- dplyr::bind_cols(
    chemistry_df,
    cations,
    anions,
    diamond
  )

  if (!is.null(scale_df)) {
    scale_values <- scale_df %>%
      dplyr::select(
        dplyr::all_of(
          c(
            location_id,
            sample_date
          )
        ),
        analyte,
        analysis_result_std
      ) %>%
      tidyr::pivot_wider(
        names_from = analyte,
        values_from = analysis_result_std
      )

    result <- result %>%
      dplyr::left_join(
        scale_values,
        by = c(
          location_id,
          sample_date
        )
      )
  }

  result %>%
    dplyr::select(
      dplyr::all_of(
        c(
          location_id,
          sample_date
        )
      ),
      cation_x,
      cation_y,
      anion_x,
      anion_y,
      diamond_x,
      diamond_y,
      dplyr::any_of(scale_col)
    )
}

#' Transform Schoeller data
#'
#' @noRd
.transform_schoeller_data <- function(
  chemistry_df
) {
  .validate_required_cols(
    chemistry_df,
    c(
      "location_id",
      "sample_date",
      "analyte",
      "analysis_result_meq"
    )
  )

  chemistry_df %>%
    dplyr::mutate(
      schoeller_axis = dplyr::case_when(
        analyte == "calcium" ~ "Ca",
        analyte == "magnesium" ~ "Mg",
        analyte == "chloride" ~ "Cl",
        analyte == "sulfate" ~ "SO4",
        analyte == "bicarbonate" ~ "Alk",
        analyte %in%
          c(
            "sodium",
            "potassium"
          ) ~ "Na + K",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(
      !is.na(schoeller_axis)
    ) %>%
    dplyr::group_by(
      location_id,
      sample_date,
      schoeller_axis
    ) %>%
    dplyr::summarise(
      analysis_result = sum(
        analysis_result_meq,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      schoeller_axis = factor(
        schoeller_axis,
        levels = c(
          "Ca",
          "Mg",
          "Na + K",
          "Cl",
          "SO4",
          "Alk"
        ),
        ordered = TRUE
      )
    )
}

#' Transform chemistry for Stiff diagram
#'
#' @noRd
.transform_stiff_data <- function(
  chemistry_df,
  scale_df = NULL,
  scale_analyte = NULL
) {
  .validate_required_cols(
    chemistry_df,
    c(
      "location_id",
      "sample_date",
      "analyte",
      "analysis_result_meq"
    )
  )

  components <- .create_geochemical_components(
    chemistry_df,
    .major_ion_components()
  )

  results <- components %>%
    purrr::pmap_dfr(
      function(
        location_id,
        sample_date,
        calcium,
        magnesium,
        sodium,
        potassium,
        sodium_potassium,
        chloride,
        sulfate,
        alkalinity
      ) {
        tibble::tibble(
          location_id = location_id,
          sample_date = sample_date,

          vertex = c(
            1,
            2,
            3,
            4,
            5,
            6,
            7
          ),

          ion = c(
            "Na + K",
            "Cl",
            "HCO[3]",
            "SO[4]",
            "Mg",
            "Ca",
            "Na + K"
          ),

          stiff_x = c(
            -sodium_potassium,
            chloride,
            alkalinity,
            sulfate,
            -magnesium,
            -calcium,
            -sodium_potassium
          ),

          stiff_y = c(
            3,
            3,
            2,
            1,
            1,
            2,
            3
          )
        ) %>%
          dplyr::mutate(
            label_side = dplyr::if_else(
              ion %in%
                c(
                  "Na + K",
                  "Ca",
                  "Mg"
                ),
              "left",
              "right"
            )
          )
      }
    ) %>%
    dplyr::ungroup()

  if (!is.null(scale_df)) {
    scale_var <- .resolve_geochemical_analytes(
      scale_analyte
    )

    scale_values <- scale_df %>%
      dplyr::select(
        location_id,
        sample_date,
        scale_value = analysis_result_std
      )

    results <- results %>%
      dplyr::left_join(
        scale_values,
        by = c(
          "location_id",
          "sample_date"
        )
      )
  }

  results %>%
    dplyr::arrange(
      location_id,
      sample_date,
      vertex
    )
}

#' Transform chemistry for Durov diagram
#'
#' @noRd
.transform_durov_data <- function(
  chemistry_df
) {
  components <- .create_geochemical_components(
    chemistry_df,
    .major_ion_components()
  )

  cations <- .calculate_cation_triangle(
    left = components$calcium,
    top = components$magnesium,
    right = components$sodium_potassium
  )

  anions <- .calculate_anion_triangle(
    left = components$alkalinity,
    top = components$sulfate,
    right = components$chloride
  )

  h <- sqrt(3) / 2 * 100

  square <- tibble::tibble(
    durov_x = cations$cation_right_pct +
      cations$cation_top_pct / 2,

    durov_y = anions$anion_right_pct +
      anions$anion_top_pct / 2,

    durov_x_plot = 60 +
      cations$cation_right_pct +
      cations$cation_top_pct / 2,

    durov_y_plot = h +
      anions$anion_right_pct +
      anions$anion_top_pct / 2
  )

  invalid_samples <- sum(
    !stats::complete.cases(
      square[,
        c(
          "durov_x_plot",
          "durov_y_plot"
        )
      ]
    )
  )

  if (invalid_samples > 0) {
    cli::cli_warn(
      paste(
        invalid_samples,
        "sample(s) were omitted because insufficient",
        "major-ion chemistry was available."
      )
    )
  }

  result <- dplyr::bind_cols(
    components,
    cations,
    anions,
    square
  )

  required_cols <- c(
    "location_id",
    "sample_date",
    "durov_x_plot",
    "durov_y_plot"
  )

  missing_cols <- setdiff(
    required_cols,
    names(result)
  )

  if (length(missing_cols) > 0) {
    rlang::abort(
      paste(
        "Durov transform is missing required columns:",
        paste(
          missing_cols,
          collapse = ", "
        )
      )
    )
  }

  result
}
