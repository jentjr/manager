#' @export
charge_balance <- function(df, ...) {
  UseMethod("charge_balance")
}

#' @export
charge_balance.data.frame <- function(
    df,
    location,
    analyte,
    result,
    unit,
    date,
    ...
) {
  df <- env_schema(
    df,
    location = {{ location }},
    analyte = {{ analyte }},
    result = {{ result }},
    unit = {{ unit }},
    date = {{ date }}
  )
  
  charge_balance(df, ...)
}

#' Charge balance calculation
#'
#' @export
charge_balance.env_schema <- function(
    df,
    analyte_matching = c(
      "exact",
      "strict",
      "normalized"
    )
) {
  analyte_matching <- rlang::arg_match(
    analyte_matching
  )
  
  chemistry_df <- .prepare_major_ion_chemistry(
    df = df,
    analyte_matching = analyte_matching
  )
  
  .calculate_charge_balance(
    chemistry_df
  )
}

#' Major ion components
#'
#' @noRd
.major_ion_components <- function() {
  list(
    calcium = "calcium",
    
    magnesium = "magnesium",
    
    sodium = "sodium",
    
    potassium = "potassium",
    
    sodium_potassium = c(
      "sodium",
      "potassium"
    ),
    
    chloride = "chloride",
    
    sulfate = "sulfate",
    
    alkalinity = "bicarbonate"
  )
}

#' Create geochemical components
#'
#' @noRd
.create_geochemical_components <- function(
    chemistry_df,
    components,
    value_col = NULL
) {
  if (is.null(value_col)) {
    value_col <- analysis_result_meq_var(
      chemistry_df
    )
  }
  
  location_col <- location_var(
    chemistry_df
  )
  
  date_col <- date_var(
    chemistry_df
  )
  
  analyte_id_col <- analyte_id_var(
    chemistry_df
  )
  
  keys <- chemistry_df %>%
    dplyr::distinct(
      .data[[location_col]],
      .data[[date_col]]
    )
  
  component_dfs <- purrr::imap(
    components,
    function(analytes, component_name) {
      chemistry_df %>%
        dplyr::filter(
          .data[[analyte_id_col]] %in% analytes
        ) %>%
        dplyr::group_by(
          .data[[location_col]],
          .data[[date_col]]
        ) %>%
        dplyr::summarise(
          value = {
            x <- .data[[value_col]]
            
            if (all(is.na(x))) {
              NA_real_
            } else {
              sum(
                x,
                na.rm = TRUE
              )
            }
          },
          .groups = "drop"
        ) %>%
        dplyr::rename(
          !!component_name := value
        )
    }
  )
  
  result <- keys
  
  for (component_df in component_dfs) {
    result <- result %>%
      dplyr::left_join(
        component_df,
        by = c(
          location_col,
          date_col
        )
      )
  }
  
  result
}

#' Major cations
#'
#' @noRd
.major_cations <- function() {
  c(
    "calcium",
    "magnesium",
    "sodium",
    "potassium"
  )
}

#' Major anions
#'
#' @noRd
.major_anions <- function() {
  c(
    "chloride",
    "sulfate",
    "bicarbonate"
  )
}

#' Calculate charge balance
#'
#' @noRd
.calculate_charge_balance <- function(
    chemistry_df
) {
  totals <- .create_geochemical_components(
    chemistry_df,
    
    list(
      total_cations = c(
        "calcium",
        "magnesium",
        "sodium",
        "potassium"
      ),
      
      total_anions = c(
        "chloride",
        "sulfate",
        "bicarbonate"
      )
    ),
    
    value_col = analysis_result_meq_var(
      chemistry_df
    )
  )
  
  totals %>%
    dplyr::mutate(
      charge_balance_pct = 100 *
        (total_cations - total_anions) /
        (total_cations + total_anions)
    )
}

#' Calculate major ion groups
#'
#' @noRd
.calculate_major_ions <- function(
    chemistry_df
) {
  .create_geochemical_components(
    chemistry_df,
    .major_ion_components(),
    value_col = analysis_result_meq_var(
      chemistry_df
    )
  )
}

#' Calculate ternary coordinates
#'
#' @noRd
.calculate_ternary_coordinates <- function(
    left,
    top,
    right,
    x_offset = 0,
    prefix = NULL
) {
  total <- rowSums(
    cbind(
      left,
      top,
      right
    ),
    na.rm = TRUE
  )
  
  total[total <= 0] <- NA_real_
  
  left_pct <- 100 * left / total
  
  top_pct <- 100 * top / total
  
  right_pct <- 100 * right / total
  
  out <- tibble::tibble(
    left_pct = left_pct,
    
    top_pct = top_pct,
    
    right_pct = right_pct,
    
    x = x_offset +
      right_pct +
      top_pct / 2,
    
    y = sqrt(3) / 2 * top_pct
  )
  
  if (!is.null(prefix)) {
    names(out) <- paste0(
      prefix,
      "_",
      names(out)
    )
  }
  
  out
}

#' Calculate cation ternary coordinates
#'
#' @noRd
.calculate_cation_triangle <- function(
    left,
    top,
    right
) {
  .calculate_ternary_coordinates(
    left = left,
    top = top,
    right = right,
    x_offset = 0,
    prefix = "cation"
  )
}

#' Calculate anion ternary coordinates
#'
#' @noRd
.calculate_anion_triangle <- function(
    left,
    top,
    right,
    x_offset = 120
) {
  .calculate_ternary_coordinates(
    left = left,
    top = top,
    right = right,
    x_offset = x_offset,
    prefix = "anion"
  )
}

#' Prepare geochemical data
#'
#' Resolve analytes, validate units, standardize analytical results,
#' average duplicate analytes, and return a standardized long-format
#' chemistry data set.
#'
#' @noRd
.prepare_geochemical_data <- function(
    df,
    analytes,
    additional_analytes = NULL,
    analyte_matching = c(
      "exact",
      "strict",
      "normalized"
    )
) {
  analyte_matching <- rlang::arg_match(
    analyte_matching
  )
  
  if (!inherits(df, "env_schema")) {
    rlang::abort(
      "df must inherit from 'env_schema'."
    )
  }
  
  validate_env_schema(df)
  
  location_col <- location_var(df)
  date_col <- date_var(df)
  analyte_id_col <- analyte_id_var(
    df
  )
  
  analytes <- unique(
    c(
      analytes,
      additional_analytes
    )
  )
  
  resolved_analytes <- .resolve_geochemical_analytes(
    analytes,
    analyte_matching
  )
  
  display_analyte <- analysis_analyte(
    df,
    analyte_matching = analyte_matching
  )
  
  canonical_id <- analyte_id(df)
  
  out <- df |>
    dplyr::mutate(
      analyte = display_analyte,
      !!analyte_id_col := canonical_id
    ) |>
    dplyr::filter(
      analyte %in% resolved_analytes
    ) |>
    dplyr::group_by(
      .data[[location_col]],
      .data[[date_col]],
      analyte,
      .data[[analyte_id_col]]
    ) |>
    dplyr::summarise(
      .original_lab_result = mean(
        .original_lab_result,
        na.rm = TRUE
      ),
      
      .original_lab_unit = dplyr::first(
        .original_lab_unit
      ),
      
      .analysis_result = mean(
        .analysis_result,
        na.rm = TRUE
      ),
      
      .analysis_unit = dplyr::first(
        .analysis_unit
      ),
      
      .groups = "drop"
    )
  
  .restore_geochemical_schema(
    out,
    df
  )
}

#' Prepare major ion chemistry
#'
#' @noRd
.prepare_major_ion_chemistry <- function(
    df,
    analytes = c(
      "Calcium, dissolved",
      "Magnesium, dissolved",
      "Sodium, dissolved",
      "Potassium, dissolved",
      "Chloride, total",
      "Sulfate, total",
      "Alkalinity, total (lab)"
    ),
    analyte_matching = c(
      "exact",
      "strict",
      "normalized"
    )
) {
  analyte_matching <- rlang::arg_match(
    analyte_matching
  )
  
  out <- .prepare_geochemical_data(
    df = df,
    analytes = analytes,
    analyte_matching = analyte_matching
  ) |>
    convert_analytes_to_meq()
  
  .restore_geochemical_schema(
    out,
    df
  )
}

#' Resolve requested analytes
#'
#' @noRd
.resolve_geochemical_analytes <- function(
    analytes,
    analyte_matching = c(
      "exact",
      "strict",
      "normalized"
    )
) {
  .resolve_analyte_name(
    analytes,
    analyte_matching = analyte_matching
  ) |>
    unique()
}

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
      dplyr::mutate(
        .analysis_result = analysis_result(.)
      ) %>%
      dplyr::select(
        dplyr::all_of(
          c(
            location_id,
            sample_date
          )
        ),
        analyte,
        .analysis_result
      ) %>%
      tidyr::pivot_wider(
        names_from = analyte,
        values_from = .analysis_result
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
        scale_value = .analysis_result
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
