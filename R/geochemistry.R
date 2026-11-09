#' Major Ion Chemistry
#'
#' Extracts, standardizes, and converts major-ion chemistry data for
#' hydrogeochemical analysis.
#'
#' `major_ion_chemistry()` is an S3 generic that supports both raw
#' data frames and `env_schema` objects.
#'
#' For data frames, environmental column mappings must be supplied.
#' For `env_schema` objects, mapped schema fields are used
#' automatically.
#'
#' Major-ion analytes are standardized, duplicate measurements are
#' consolidated, and analytical results are converted to
#' milliequivalents per liter (meq/L).
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return
#' An `env_schema` object containing standardized major-ion
#' chemistry data.
#'
#' @examples
#' data(gw)
#'
#' chem <- major_ion_chemistry(gw)
#'
#' chem
#'
#' @export
major_ion_chemistry <- function(df, ...) {
  UseMethod("major_ion_chemistry")
}

#' @rdname major_ion_chemistry
#'
#' @param location Location identifier column.
#' @param analyte Analyte column.
#' @param result Analytical result column.
#' @param unit Reporting unit column.
#' @param sample_date Sample date column.
#'
#' @details
#' For data frames, `major_ion_chemistry()` first converts the
#' supplied data to an `env_schema` object using the specified
#' column mappings and then dispatches to
#' [major_ion_chemistry.env_schema()].
#'
#' @export
major_ion_chemistry.data.frame <- function(
  df,
  location,
  analyte,
  result,
  unit,
  sample_date,
  ...
) {
  df <- env_schema(
    df,
    location = {{ location }},
    analyte = {{ analyte }},
    result = {{ result }},
    unit = {{ unit }},
    sample_date = {{ sample_date }}
  )

  major_ion_chemistry(
    df,
    ...
  )
}

#' @rdname major_ion_chemistry
#'
#' @param analytes Major-ion analytes to retain.
#'
#' @param analyte_matching Analyte matching strategy. One of
#' `"exact"`, `"strict"`, or `"normalized"`.
#'
#' @details
#' `major_ion_chemistry()` serves as the primary preprocessing
#' function for geochemical analyses and diagrams.
#'
#' The function:
#'
#' \itemize{
#'   \item resolves analyte names;
#'   \item filters requested analytes;
#'   \item consolidates duplicate measurements;
#'   \item standardizes analytical metadata;
#'   \item converts concentrations to milliequivalents per liter
#'   (meq/L).
#' }
#'
#' Duplicate measurements within a location and sample-date
#' combination are averaged prior to conversion.
#'
#' @section Analyte Matching:
#'
#' \describe{
#'
#'   \item{exact}{
#'   Uses analyte names exactly as reported.
#'   }
#'
#'   \item{strict}{
#'   Applies text normalization while preserving analytical
#'   distinctions such as dissolved versus total fractions.
#'   }
#'
#'   \item{normalized}{
#'   Resolves analyte aliases to canonical analyte names using
#'   package analyte metadata.
#'   }
#' }
#'
#' @section Output Structure:
#'
#' One record is returned for each unique analyte, location, and
#' sample-date combination.
#'
#' Analytical concentrations are available in both native units and
#' standardized milliequivalent units suitable for geochemical
#' calculations.
#'
#' @examples
#' data(gw)
#'
#' major_ion_chemistry(gw)
#'
#' major_ion_chemistry(
#'   gw,
#'   analyte_matching = "normalized"
#' )
#'
#' @export
major_ion_chemistry.env_schema <- function(
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
  validate_env_schema(df)

  analyte_matching <- rlang::arg_match(
    analyte_matching
  )

  .prepare_major_ion_chemistry(
    df = df,
    analytes = analytes,
    analyte_matching = analyte_matching
  )
}

#' Major Ion Components
#'
#' Calculates aggregated major-ion component concentrations for
#' hydrogeochemical evaluation.
#'
#' `major_ions()` is an S3 generic that supports both raw data
#' frames and `env_schema` objects.
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return
#' A data frame containing one record per location and sample-date
#' combination.
#'
#' @export
major_ions <- function(df, ...) {
  UseMethod("major_ions")
}

#' @rdname major_ions
#'
#' @param location Location identifier column.
#' @param analyte Analyte column.
#' @param result Analytical result column.
#' @param unit Reporting unit column.
#' @param sample_date Sample date column.
#'
#' @details
#' For data frames, `major_ions()` first converts the supplied data
#' to an `env_schema` object and then dispatches to
#' [major_ions.env_schema()].
#'
#' @export
major_ions.data.frame <- function(
  df,
  location,
  analyte,
  result,
  unit,
  sample_date,
  ...
) {
  df <- env_schema(
    df,
    location = {{ location }},
    analyte = {{ analyte }},
    result = {{ result }},
    unit = {{ unit }},
    sample_date = {{ sample_date }}
  )

  major_ions(
    df,
    ...
  )
}

#' @rdname major_ions
#'
#' @param analyte_matching Analyte matching strategy. One of
#' `"exact"`, `"strict"`, or `"normalized"`.
#'
#' @details
#' Major-ion concentrations are calculated from analytical results
#' converted to milliequivalents per liter (meq/L).
#'
#' @section Output Variables:
#'
#' Returned variables include:
#'
#' \itemize{
#'   \item `calcium`
#'   \item `magnesium`
#'   \item `sodium`
#'   \item `potassium`
#'   \item `sodium_potassium`
#'   \item `chloride`
#'   \item `sulfate`
#'   \item `alkalinity`
#' }
#'
#' All concentrations are expressed in milliequivalents per liter
#' (meq/L).
#'
#' @examples
#' data(gw)
#'
#' major_ions(gw)
#'
#' @export
major_ions.env_schema <- function(
  df,
  analyte_matching = c(
    "exact",
    "strict",
    "normalized"
  )
) {
  validate_env_schema(df)

  analyte_matching <- rlang::arg_match(
    analyte_matching
  )

  chemistry_df <- .prepare_major_ion_chemistry(
    df = df,
    analyte_matching = analyte_matching
  )

  .calculate_major_ions(
    chemistry_df
  )
}

#' Charge Balance Calculation
#'
#' Calculates the ionic charge balance of major-ion chemistry data.
#'
#' `charge_balance()` is an S3 generic that supports both raw data
#' frames and `env_schema` objects.
#'
#' For data frames, environmental column mappings must be supplied.
#' For `env_schema` objects, mapped schema fields are used
#' automatically.
#'
#' Charge balance is calculated using concentrations converted to
#' milliequivalents per liter (meq/L) and provides a quality-control
#' evaluation of analytical completeness and consistency.
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return
#' A data frame containing:
#'
#' \itemize{
#'   \item location identifiers;
#'   \item sample dates;
#'   \item total cations (meq/L);
#'   \item total anions (meq/L);
#'   \item charge balance percentage.
#' }
#'
#' @examples
#' data(gw)
#'
#' charge_balance(gw)
#'
#' @export
charge_balance <- function(df, ...) {
  UseMethod("charge_balance")
}

#' @rdname charge_balance
#'
#' @param location Location identifier column.
#' @param analyte Analyte column.
#' @param result Analytical result column.
#' @param unit Reporting unit column.
#' @param sample_date Sample date column.
#'
#' @details
#' For data frames, `charge_balance()` first converts the supplied
#' data to an `env_schema` object using the specified column mappings
#' and then dispatches to [charge_balance.env_schema()].
#'
#' @export
charge_balance.data.frame <- function(
  df,
  location,
  analyte,
  result,
  unit,
  sample_date,
  ...
) {
  df <- env_schema(
    df,
    location = {{ location }},
    analyte = {{ analyte }},
    result = {{ result }},
    unit = {{ unit }},
    sample_date = {{ sample_date }}
  )

  charge_balance(df, ...)
}

#' @rdname charge_balance
#'
#' @param analyte_matching Analyte matching strategy. One of
#' `"exact"`, `"strict"`, or `"normalized"`.
#'
#' @details
#' Charge balance is calculated from the sum of major dissolved
#' cations and anions after conversion to milliequivalents per liter
#' (meq/L).
#'
#' Major ions used in the calculation include:
#'
#' \itemize{
#'   \item calcium;
#'   \item magnesium;
#'   \item sodium;
#'   \item potassium;
#'   \item chloride;
#'   \item sulfate;
#'   \item bicarbonate (alkalinity).
#' }
#'
#' The charge balance percentage is calculated as:
#'
#' \deqn{
#' 100 \times
#' \frac{\sum cations - \sum anions}
#' {\sum cations + \sum anions}
#' }
#'
#' Values near zero generally indicate internally consistent
#' analytical results, whereas large positive or negative values may
#' indicate analytical error, missing constituents, transcription
#' errors, or unit-conversion problems.
#'
#' For `env_schema` objects, analyte, result, unit, sample_date, and
#' location fields are obtained directly from schema mappings.
#'
#' @section Analyte Matching:
#'
#' \describe{
#'   \item{exact}{
#'   Uses analyte names exactly as reported.
#'   }
#'
#'   \item{strict}{
#'   Applies text normalization while preserving analytical
#'   distinctions such as dissolved versus total fractions.
#'   }
#'
#'   \item{normalized}{
#'   Resolves analyte aliases to canonical analyte names using
#'   package analyte metadata.
#'   }
#' }
#'
#' @section Output Structure:
#'
#' One record is returned for each unique location and sample-date
#' combination.
#'
#' When multiple measurements of the same analyte occur within a
#' location and sample-date group, values are averaged during
#' geochemical standardization prior to charge-balance calculation.
#'
#' Returned cation and anion totals are expressed in
#' milliequivalents per liter (meq/L).
#'
#' @section Interpretation:
#'
#' Typical charge balance error guidelines are:
#'
#' \describe{
#'   \item{< ±5\%}{
#'   Excellent agreement.
#'   }
#'
#'   \item{±5\% to ±10\%}{
#'   Generally acceptable for most environmental investigations.
#'   }
#'
#'   \item{> ±10\%}{
#'   May indicate incomplete chemistry or analytical issues and
#'   should be reviewed.
#'   }
#' }
#'
#' These thresholds should be interpreted in the context of project
#' objectives, laboratory methods, and sample chemistry.
#'
#' @section Validation:
#'
#' Input data are validated for:
#'
#' \itemize{
#'   \item analyte resolution;
#'   \item major-ion chemistry availability;
#'   \item supported analytical units;
#'   \item successful conversion to milliequivalents;
#'   \item duplicate analyte measurements.
#' }
#'
#' @examples
#' data(gw)
#'
#' charge_balance(gw)
#'
#' charge_balance(
#'   gw,
#'   analyte_matching = "normalized"
#' )
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
  validate_env_schema(df)

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

#' Piper Diagram Coordinates
#'
#' Creates Piper-diagram coordinate data from major-ion chemistry.
#'
#' `piper_data()` is an S3 generic that supports both raw data
#' frames and `env_schema` objects.
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return
#' A data frame containing Piper plotting coordinates.
#'
#' @export
piper_data <- function(df, ...) {
  UseMethod("piper_data")
}

#' @rdname piper_data
#'
#' @param location Location identifier column.
#' @param analyte Analyte column.
#' @param result Analytical result column.
#' @param unit Reporting unit column.
#' @param sample_date Sample date column.
#'
#' @export
piper_data.data.frame <- function(
  df,
  location,
  analyte,
  result,
  unit,
  sample_date,
  ...
) {
  df <- env_schema(
    df,
    location = {{ location }},
    analyte = {{ analyte }},
    result = {{ result }},
    unit = {{ unit }},
    sample_date = {{ sample_date }}
  )

  piper_data(
    df,
    ...
  )
}

#' @rdname piper_data
#'
#' @inheritParams piper_plot.env_schema
#'
#' @details
#' Major-ion concentrations are converted to milliequivalents per
#' liter (meq/L), normalized to ionic percentages, and transformed
#' into Piper plotting coordinates.
#'
#' @section Output Variables:
#'
#' \itemize{
#'   \item `cation_x`
#'   \item `cation_y`
#'   \item `anion_x`
#'   \item `anion_y`
#'   \item `diamond_x`
#'   \item `diamond_y`
#' }
#'
#' One row is returned for each unique location and sample-date
#' combination.
#'
#' @examples
#' data(gw)
#'
#' piper_data(gw)
#'
#' @export
piper_data.env_schema <- function(
  df,
  analyte_matching = c(
    "strict",
    "exact",
    "normalized"
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
  validate_env_schema(df)

  .transform_piper_data(
    df = df,
    analyte_matching = analyte_matching,
    x_cation = x_cation,
    y_cation = y_cation,
    z_cation = z_cation,
    x_anion = x_anion,
    y_anion = y_anion,
    z_anion = z_anion,
    scale_analyte = scale_analyte
  )
}

#' Durov Diagram Coordinates
#'
#' Creates Durov-diagram coordinate data from major-ion chemistry.
#'
#' `durov_data()` is an S3 generic that supports both raw data
#' frames and `env_schema` objects.
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return
#' A data frame containing Durov plotting coordinates.
#'
#' @export
durov_data <- function(df, ...) {
  UseMethod("durov_data")
}

#' @rdname durov_data
#'
#' @param location Location identifier column.
#' @param analyte Analyte column.
#' @param result Analytical result column.
#' @param unit Reporting unit column.
#' @param sample_date Sample date column.
#'
#' @export
durov_data.data.frame <- function(
  df,
  location,
  analyte,
  result,
  unit,
  sample_date,
  ...
) {
  df <- env_schema(
    df,
    location = {{ location }},
    analyte = {{ analyte }},
    result = {{ result }},
    unit = {{ unit }},
    sample_date = {{ sample_date }}
  )

  durov_data(
    df,
    ...
  )
}

#' @rdname durov_data
#'
#' @param analyte_matching Analyte matching strategy. One of
#' `"exact"`, `"strict"`, or `"normalized"`.
#'
#' @details
#' Major-ion concentrations are converted to normalized ionic
#' percentages and transformed into Durov projection coordinates.
#'
#' @section Output Variables:
#'
#' Returned data include:
#'
#' \itemize{
#'   \item major-ion component totals;
#'   \item cation-triangle coordinates;
#'   \item anion-triangle coordinates;
#'   \item `durov_x`;
#'   \item `durov_y`;
#'   \item `durov_x_plot`;
#'   \item `durov_y_plot`.
#' }
#'
#' @examples
#' data(gw)
#'
#' durov_data(gw)
#'
#' @export
durov_data.env_schema <- function(
  df,
  analyte_matching = c(
    "normalized",
    "strict",
    "exact"
  )
) {
  validate_env_schema(df)

  analyte_matching <- rlang::arg_match(
    analyte_matching
  )

  chemistry_df <- .prepare_major_ion_chemistry(
    df = df,
    analyte_matching = analyte_matching
  )

  .transform_durov_data(
    chemistry_df
  )
}

#' Schoeller Diagram Data
#'
#' Creates Schoeller-diagram plotting data from major-ion chemistry.
#'
#' `schoeller_data()` is an S3 generic that supports both raw data
#' frames and `env_schema` objects.
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return
#' A tidy data frame containing Schoeller-diagram plotting data.
#'
#' @export
schoeller_data <- function(df, ...) {
  UseMethod("schoeller_data")
}

#' @rdname schoeller_data
#'
#' @param location Location identifier column.
#' @param analyte Analyte column.
#' @param result Analytical result column.
#' @param unit Reporting unit column.
#' @param sample_date Sample date column.
#'
#' @details
#' For data frames, `schoeller_data()` first converts the supplied
#' data to an `env_schema` object and then dispatches to
#' [schoeller_data.env_schema()].
#'
#' @export
schoeller_data.data.frame <- function(
  df,
  location,
  analyte,
  result,
  unit,
  sample_date,
  ...
) {
  df <- env_schema(
    df,
    location = {{ location }},
    analyte = {{ analyte }},
    result = {{ result }},
    unit = {{ unit }},
    sample_date = {{ sample_date }}
  )

  schoeller_data(
    df,
    ...
  )
}

#' @rdname schoeller_data
#'
#' @param analyte_matching Analyte matching strategy. One of
#' `"exact"`, `"strict"`, or `"normalized"`.
#'
#' @details
#' Major-ion concentrations are converted to milliequivalents per
#' liter (meq/L) and transformed into the categorical structure
#' required for Schoeller diagrams.
#'
#' Sodium and potassium are combined into a single `"Na + K"`
#' category consistent with standard Schoeller-diagram
#' conventions.
#'
#' @section Output Variables:
#'
#' Returned variables include:
#'
#' \itemize{
#'   \item location identifier;
#'   \item sample date;
#'   \item `schoeller_axis`;
#'   \item `analysis_result`.
#' }
#'
#' Schoeller axis levels are returned in the following order:
#'
#' \itemize{
#'   \item `Ca`
#'   \item `Mg`
#'   \item `Na + K`
#'   \item `Cl`
#'   \item `SO4`
#'   \item `Alk`
#' }
#'
#' The returned `analysis_result` values are expressed in
#' milliequivalents per liter (meq/L).
#'
#' @section Output Structure:
#'
#' Multiple rows are returned for each sample. Each row represents
#' a single Schoeller-axis category for a given location and sample
#' date.
#'
#' @examples
#' data(gw)
#'
#' schoeller <- schoeller_data(gw)
#'
#' schoeller
#'
#' @export
schoeller_data.env_schema <- function(
  df,
  analyte_matching = c(
    "exact",
    "strict",
    "normalized"
  )
) {
  validate_env_schema(df)

  analyte_matching <- rlang::arg_match(
    analyte_matching
  )

  chemistry_df <- .prepare_major_ion_chemistry(
    df = df,
    analyte_matching = analyte_matching
  )

  .transform_schoeller_data(
    chemistry_df
  )
}

#' Stiff Diagram Coordinates
#'
#' Creates Stiff-diagram polygon coordinates from major-ion
#' chemistry.
#'
#' `stiff_data()` is an S3 generic that supports both raw data
#' frames and `env_schema` objects.
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return
#' A data frame containing Stiff-diagram polygon vertices.
#'
#' @export
stiff_data <- function(df, ...) {
  UseMethod("stiff_data")
}

#' @rdname stiff_data
#'
#' @param location Location identifier column.
#' @param analyte Analyte column.
#' @param result Analytical result column.
#' @param unit Reporting unit column.
#' @param sample_date Sample date column.
#'
#' @export
stiff_data.data.frame <- function(
  df,
  location,
  analyte,
  result,
  unit,
  sample_date,
  ...
) {
  df <- env_schema(
    df,
    location = {{ location }},
    analyte = {{ analyte }},
    result = {{ result }},
    unit = {{ unit }},
    sample_date = {{ sample_date }}
  )

  stiff_data(
    df,
    ...
  )
}

#' @rdname stiff_data
#'
#' @inheritParams stiff_plot.env_schema
#'
#' @details
#' Major-ion chemistry is converted to milliequivalent
#' concentrations and transformed into Stiff-diagram polygon
#' coordinates.
#'
#' Unlike most geochemical summary functions, multiple rows are
#' returned for each sample because each location/date combination
#' is expanded into a sequence of polygon vertices.
#'
#' @section Output Variables:
#'
#' Returned variables include:
#'
#' \itemize{
#'   \item `vertex`
#'   \item `ion`
#'   \item `stiff_x`
#'   \item `stiff_y`
#'   \item `label_side`
#' }
#'
#' When `scale_analyte` is supplied, a `scale_value` column is also
#' returned.
#'
#' @examples
#' data(gw)
#'
#' stiff_data(gw)
#'
#' @export
stiff_data.env_schema <- function(
  df,
  analyte_matching = c(
    "exact",
    "strict",
    "normalized"
  ),
  calcium = "Calcium, dissolved",
  magnesium = "Magnesium, dissolved",
  sodium = "Sodium, dissolved",
  potassium = "Potassium, dissolved",
  chloride = "Chloride, total",
  sulfate = "Sulfate, total",
  alkalinity = "Alkalinity, total (lab)",
  scale_analyte = NULL
) {
  validate_env_schema(df)

  analyte_matching <- rlang::arg_match(
    analyte_matching
  )
  chemistry_df <- .prepare_major_ion_chemistry(
    df = df,
    analyte_matching = analyte_matching,
    analytes = c(
      calcium,
      magnesium,
      sodium,
      potassium,
      chloride,
      sulfate,
      alkalinity
    )
  )

  scale_df <- NULL

  if (!is.null(scale_analyte)) {
    scale_df <- .prepare_geochemical_data(
      df = df,
      analytes = scale_analyte
    )
  }

  .transform_stiff_data(
    chemistry_df = chemistry_df,
    scale_df = scale_df,
    scale_analyte = scale_analyte
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

#' Create geochemical component summaries
#'
#' Aggregates chemistry data into derived geochemical component
#' groups.
#'
#' Components may represent individual ions or combinations of
#' multiple analytes. Values are summed within each location and
#' sample-date combination.
#'
#' @param chemistry_df Standardized geochemical data set.
#' @param components Named list of analyte identifiers used to
#' construct each component.
#' @param value_col Column containing values to aggregate.
#'
#' @return
#' A data frame containing one row per location and sample date with
#' one column generated for each requested component.
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

  sample_date_col <- sample_date_var(
    chemistry_df
  )

  analyte_id_col <- analyte_id_var(
    chemistry_df
  )

  keys <- chemistry_df |>
    dplyr::distinct(
      .data[[location_col]],
      .data[[sample_date_col]]
    )

  component_dfs <- purrr::imap(
    components,
    function(analytes, component_name) {
      chemistry_df |>
        dplyr::filter(
          .data[[analyte_id_col]] %in% analytes
        ) |>
        dplyr::group_by(
          .data[[location_col]],
          .data[[sample_date_col]]
        ) |>
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
        ) |>
        dplyr::rename(
          !!component_name := value
        )
    }
  )

  result <- keys

  for (component_df in component_dfs) {
    result <- result |>
      dplyr::left_join(
        component_df,
        by = c(
          location_col,
          sample_date_col
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

#' Calculate ionic charge balance
#'
#' Calculates total cation charge, total anion charge, and charge
#' balance error for standardized major-ion chemistry data.
#'
#' Charge balance is calculated using analyte concentrations
#' expressed in milliequivalents per liter (meq/L).
#'
#' @param chemistry_df Standardized major-ion chemistry data.
#'
#' @return
#' A data frame containing total cations, total anions, and charge
#' balance percentage for each sample.
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

  totals |>
    dplyr::mutate(
      charge_balance_pct = 100 *
        (total_cations - total_anions) /
        (total_cations + total_anions)
    )
}

#' Calculate major-ion groups
#'
#' Calculates commonly used major-ion component groups from
#' standardized chemistry data.
#'
#' Major-ion concentrations are aggregated by sample and expressed
#' in milliequivalents per liter (meq/L).
#'
#' @param chemistry_df Standardized major-ion chemistry data.
#'
#' @return
#' A data frame containing major-ion component totals for each
#' sample.
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

#' Calculate ternary plotting coordinates
#'
#' Converts three-component compositions into normalized ternary
#' percentages and Cartesian plotting coordinates.
#'
#' Component values are normalized to percentages of the total
#' composition before coordinate transformation.
#'
#' @param left Left-axis component values.
#' @param top Top-axis component values.
#' @param right Right-axis component values.
#' @param x_offset Horizontal offset applied to generated
#' coordinates.
#' @param prefix Optional column-name prefix.
#'
#' @return
#' A tibble containing normalized percentages and Cartesian ternary
#' coordinates.
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
#' Converts normalized cation compositions into Piper or Durov
#' cation-triangle coordinates.
#'
#' @param left Calcium component values.
#' @param top Magnesium component values.
#' @param right Sodium-plus-potassium component values.
#'
#' @return
#' A tibble containing cation percentages and plotting
#' coordinates.
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
#' Converts normalized anion compositions into Piper or Durov
#' anion-triangle coordinates.
#'
#' @param left Alkalinity component values.
#' @param top Sulfate component values.
#' @param right Chloride-plus-fluoride component values.
#' @param x_offset Horizontal plotting offset.
#'
#' @return
#' A tibble containing anion percentages and plotting
#' coordinates.
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

#' Prepare geochemical chemistry data
#'
#' Resolves analyte names, standardizes analytical results,
#' consolidates duplicate analyte measurements, and returns a
#' standardized chemistry data set suitable for geochemical
#' calculations and visualization.
#'
#' Results are grouped by location, sample date, analyte, and
#' canonical analyte identifier.
#'
#' Duplicate measurements within a sample are averaged.
#'
#' @param df An env_schema object.
#' @param analytes Target analytes to retain.
#' @param additional_analytes Optional additional analytes.
#' @param analyte_matching Analyte matching strategy.
#'
#' @return
#' A standardized env_schema object containing only the requested
#' analytes.
#'
#' @section Processing Steps:
#'
#' \itemize{
#'   \item analyte names are resolved;
#'   \item requested analytes are filtered;
#'   \item duplicate measurements are summarized;
#'   \item standardized analytical fields are preserved;
#'   \item schema metadata are restored.
#' }
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
  sample_date_col <- sample_date_var(df)
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
      .data[[sample_date_col]],
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
      .detected = any(
        .detected,
        na.rm = TRUE
      ),

      .censored = any(
        .censored,
        na.rm = TRUE
      ),

      .estimated = any(
        .estimated,
        na.rm = TRUE
      ),

      .rejected = all(
        .rejected
      ),

      .usable = any(
        .usable,
        na.rm = TRUE
      ),

      .groups = "drop"
    )

  .restore_geochemical_schema(
    out,
    df
  )
}

#' Prepare major-ion chemistry
#'
#' Extracts major-ion analytes, standardizes analytical results,
#' and converts concentrations to milliequivalents per liter
#' (meq/L).
#'
#' This function serves as the primary preprocessing step for
#' charge-balance calculations and geochemical diagrams.
#'
#' @param df An env_schema object.
#' @param analytes Major-ion analytes to retain.
#' @param analyte_matching Analyte matching strategy.
#'
#' @return
#' An env_schema object containing standardized major-ion chemistry
#' data expressed in milliequivalents per liter (meq/L).
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

#' Resolve geochemical analyte names
#'
#' Resolves user-specified analyte names according to the selected
#' matching strategy and returns unique canonical analyte names.
#'
#' @param analytes Character vector of requested analytes.
#' @param analyte_matching Analyte matching strategy.
#'
#' @return
#' Character vector of resolved analyte names.
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

#' Transform chemistry data for Piper diagrams
#'
#' Converts major-ion chemistry into cation-triangle,
#' anion-triangle, and central-diamond coordinates used by Piper
#' diagrams.
#'
#' Major-ion concentrations are converted to normalized ionic
#' percentages before coordinate transformation.
#'
#' @param df Environmental chemistry data.
#' @param analyte_matching Analyte matching strategy.
#' @param x_cation,y_cation,z_cation Cation components.
#' @param x_anion,y_anion,z_anion Anion components.
#' @param scale_analyte Optional analyte used for point scaling.
#'
#' @return
#' A data frame containing Piper plotting coordinates and optional
#' scaling variables.
#'
#'
#' @details
#' Requested analytes are resolved according to the selected
#' analyte-matching strategy during chemistry preparation.
#' Geochemical component aggregation and Piper-coordinate
#' calculations are subsequently performed using canonical
#' analyte identifiers (`analyte_id`) to ensure consistent
#' behavior across matching modes.
#'
#' @noRd
.transform_piper_data <- function(
  df,
  analyte_matching = c(
    "strict",
    "exact",
    "normalized"
  ),
  x_cation,
  y_cation,
  z_cation,
  x_anion,
  y_anion,
  z_anion,
  scale_analyte = NULL
) {
  validate_env_schema(df)

  location_col <- location_var(df)
  sample_date_col <- sample_date_var(df)

  analyte_matching <- rlang::arg_match(
    analyte_matching
  )

  # ---------------------------------------------------
  # Resolve requested analytes
  # ---------------------------------------------------

  x_cation_resolved <- .resolve_geochemical_analytes(
    x_cation,
    analyte_matching
  )

  y_cation_resolved <- .resolve_geochemical_analytes(
    y_cation,
    analyte_matching
  )

  z_cation_resolved <- .resolve_geochemical_analytes(
    z_cation,
    analyte_matching
  )

  x_anion_resolved <- .resolve_geochemical_analytes(
    x_anion,
    analyte_matching
  )

  y_anion_resolved <- .resolve_geochemical_analytes(
    y_anion,
    analyte_matching
  )

  z_anion_resolved <- .resolve_geochemical_analytes(
    z_anion,
    analyte_matching
  )

  requested_analytes <- unique(c(
    x_cation_resolved,
    y_cation_resolved,
    z_cation_resolved,
    x_anion_resolved,
    y_anion_resolved,
    z_anion_resolved
  ))

  # ---------------------------------------------------
  # Build chemistry data
  # ---------------------------------------------------

  chemistry_df <- .prepare_major_ion_chemistry(
    df = df,
    analytes = requested_analytes,
    analyte_matching = analyte_matching
  )

  analyte_col <- analyte_var(
    chemistry_df
  )

  analyte_id_col <- analyte_id_var(
    chemistry_df
  )

  # ---------------------------------------------------
  # Resolve plotting components to analyte IDs
  # ---------------------------------------------------
  #
  # Geochemical preprocessing uses analyte matching to
  # identify and retain the requested analytical results.
  #
  # Subsequent Piper calculations operate on canonical
  # analyte identifiers rather than matched analyte names.
  #
  # This separation is important because analyte matching
  # may resolve user-supplied analytes to names such as:
  #
  #   calcium::dissolved
  #   chloride::total
  #
  # whereas geochemical component aggregation operates on
  # analyte_id values such as:
  #
  #   calcium
  #   chloride
  #   bicarbonate
  #
  # Converting resolved analytes to analyte_id values
  # ensures that Piper calculations remain consistent
  # across exact, strict, and normalized matching modes
  # and prevents component aggregation failures caused by
  # mismatched analyte namespaces.

  analyte_ids <- chemistry_df |>
    dplyr::distinct(
      .data[[analyte_col]],
      .data[[analyte_id_col]]
    )

  # Return unique analyte_id values corresponding to the
  # resolved analytes retained in the chemistry data set.
  component_ids <- function(analytes) {
    analyte_ids |>
      dplyr::filter(
        .data[[analyte_col]] %in% analytes
      ) |>
      dplyr::pull(
        .data[[analyte_id_col]]
      ) |>
      unique()
  }

  chemistry_df <- .create_geochemical_components(
    chemistry_df,
    list(
      piper_x_cation = component_ids(
        x_cation_resolved
      ),

      piper_y_cation = component_ids(
        y_cation_resolved
      ),

      piper_z_cation = component_ids(
        z_cation_resolved
      ),

      piper_x_anion = component_ids(
        x_anion_resolved
      ),

      piper_y_anion = component_ids(
        y_anion_resolved
      ),

      piper_z_anion = component_ids(
        z_anion_resolved
      )
    )
  )

  # ---------------------------------------------------
  # Optional scaling analyte
  # ---------------------------------------------------

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
      analyte_matching = analyte_matching
    )
  }

  # ---------------------------------------------------
  # Cation triangle
  # ---------------------------------------------------

  cations <- .calculate_cation_triangle(
    left = chemistry_df$piper_x_cation,
    top = chemistry_df$piper_y_cation,
    right = chemistry_df$piper_z_cation
  )

  # ---------------------------------------------------
  # Anion triangle
  # ---------------------------------------------------

  anions <- .calculate_anion_triangle(
    left = chemistry_df$piper_y_anion,
    top = chemistry_df$piper_z_anion,
    right = chemistry_df$piper_x_anion
  )

  # ---------------------------------------------------
  # Diamond
  # ---------------------------------------------------

  slope <- sqrt(3)

  b1 <- cations$cation_y -
    slope * cations$cation_x

  b2 <- anions$anion_y +
    slope * anions$anion_x

  diamond_x <- (b2 - b1) /
    (2 * slope)

  diamond_y <- slope * diamond_x + b1

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
    scale_values <- scale_df |>
      dplyr::mutate(
        .analysis_result = analysis_result(
          scale_df
        )
      ) |>
      dplyr::select(
        dplyr::all_of(
          c(
            location_col,
            sample_date_col
          )
        ),
        analyte,
        .analysis_result
      ) |>
      tidyr::pivot_wider(
        names_from = analyte,
        values_from = .analysis_result
      )

    result <- result |>
      dplyr::left_join(
        scale_values,
        by = c(
          location_col,
          sample_date_col
        )
      )
  }

  result |>
    dplyr::select(
      dplyr::all_of(
        c(
          location_col,
          sample_date_col
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

#' Transform chemistry for Schoeller diagrams
#'
#' Converts major-ion chemistry into Schoeller-diagram plotting
#' data.
#'
#' Sodium and potassium are combined into a single "Na + K"
#' category consistent with standard Schoeller diagram practice.
#'
#' @param chemistry_df Standardized major-ion chemistry data.
#'
#' @return
#' A tidy data frame containing Schoeller-axis categories and
#' milliequivalent concentrations.
#'
#' @noRd
.transform_schoeller_data <- function(
  chemistry_df
) {
  location_col <- location_var(
    chemistry_df
  )

  sample_date_col <- sample_date_var(
    chemistry_df
  )
  analyte_id_col <- analyte_id_var(
    chemistry_df
  )

  analysis_result_meq_col <- analysis_result_meq_var(
    chemistry_df
  )

  .validate_required_cols(
    chemistry_df,
    c(
      location_col,
      sample_date_col,
      analyte_id_var(chemistry_df),
      analysis_result_meq_var(chemistry_df)
    )
  )

  chemistry_df |>
    dplyr::mutate(
      schoeller_axis = dplyr::case_when(
        .data[[analyte_id_col]] == "calcium" ~ "Ca",
        .data[[analyte_id_col]] == "magnesium" ~ "Mg",
        .data[[analyte_id_col]] == "chloride" ~ "Cl",
        .data[[analyte_id_col]] == "sulfate" ~ "SO4",
        .data[[analyte_id_col]] == "bicarbonate" ~ "Alk",
        .data[[analyte_id_col]] %in%
          c(
            "sodium",
            "potassium"
          ) ~ "Na + K",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(
      !is.na(schoeller_axis)
    ) |>
    dplyr::group_by(
      .data[[location_col]],
      .data[[sample_date_col]],
      schoeller_axis
    ) |>
    dplyr::summarise(
      analysis_result = sum(
        .data[[analysis_result_meq_col]],
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
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

#' Transform chemistry for Stiff diagrams
#'
#' Converts major-ion chemistry into Stiff-diagram polygon
#' coordinates.
#'
#' Cations are represented on the left side of the diagram and
#' anions on the right side using milliequivalent concentrations.
#'
#' @param chemistry_df Standardized major-ion chemistry data.
#' @param scale_df Optional scaling data.
#' @param scale_analyte Optional analyte used for polygon
#' colouring.
#'
#' @return
#' A data frame containing Stiff-diagram vertices and optional
#' scaling values.
#'
#' @noRd
.transform_stiff_data <- function(
  chemistry_df,
  scale_df = NULL,
  scale_analyte = NULL
) {
  location_col <- location_var(chemistry_df)

  sample_date_col <- sample_date_var(chemistry_df)

  .validate_required_cols(
    chemistry_df,
    c(
      location_col,
      sample_date_col,
      analyte_id_var(chemistry_df),
      analysis_result_meq_var(chemistry_df)
    )
  )

  if (nrow(chemistry_df) == 0) {
    return(tibble::tibble())
  }

  components <- .create_geochemical_components(
    chemistry_df,
    .major_ion_components()
  )

  results <- components |>
    purrr::pmap_dfr(
      function(...) {
        vals <- list(...)

        loc_value <- vals[[location_col]]
        date_value <- vals[[sample_date_col]]

        calcium <- vals$calcium
        magnesium <- vals$magnesium
        sodium_potassium <- vals$sodium_potassium
        chloride <- vals$chloride
        sulfate <- vals$sulfate
        alkalinity <- vals$alkalinity

        tibble::tibble(
          !!location_col := loc_value,
          !!date_col := date_value,

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
        ) |>
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
    ) |>
    dplyr::ungroup()

  if (!is.null(scale_df)) {
    scale_var <- .resolve_geochemical_analytes(
      scale_analyte
    )

    scale_values <- scale_df |>
      dplyr::select(
        dplyr::all_of(
          c(
            location_col,
            sample_date_col
          )
        ),
        scale_value = .analysis_result
      )

    results <- results |>
      dplyr::left_join(
        scale_values,
        by = c(
          location_col,
          sample_date_col
        )
      )
  }

  results |>
    dplyr::arrange(
      .data[[location_col]],
      .data[[sample_date_col]],
      vertex
    )
}

#' Transform chemistry for Durov diagrams
#'
#' Converts major-ion chemistry into cation-triangle,
#' anion-triangle, and Durov-field coordinates.
#'
#' Ionic concentrations are normalized to percentages before
#' transformation into Durov plotting coordinates.
#'
#' Samples lacking sufficient major-ion chemistry are omitted from
#' the resulting projection field.
#'
#' @param chemistry_df Standardized major-ion chemistry data.
#'
#' @return
#' A data frame containing ternary coordinates, Durov projection
#' coordinates, and major-ion component summaries.
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
    location_var(chemistry_df),
    sample_date_var(chemistry_df),
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
