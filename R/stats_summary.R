#' Summary Statistics
#'
#' Calculates descriptive statistics for environmental monitoring
#' data.
#'
#' For `env_schema` objects, calculations are performed using the
#' standardized `.analysis_result` and `.censored` columns rather
#' than the original source data columns. This ensures summary
#' statistics are based on the processed analytical values used
#' throughout the package.
#'
#' If the input is grouped with [dplyr::group_by()], summary
#' statistics are calculated within the existing groups. For ungrouped
#' inputs, summaries are calculated separately for each
#' location, analyte, and unit.
#'
#' @param df A data frame or `env_schema` object.
#' @param na.rm Logical indicating whether missing values should
#'   be removed when calculating summary statistics.
#' @param ... Additional arguments passed to methods.
#'
#' @returns
#' A tibble containing one row per summary group.
#'
#' The returned table includes:
#' \describe{
#'   \item{n}{Number of records in the group, including records with missing result values.}
#'   \item{percent_censored}{Percentage of records in the group classified as censored.}
#'   \item{mean}{Arithmetic mean concentration.}
#'   \item{median}{Median concentration.}
#'   \item{sd}{Sample standard deviation.}
#'   \item{var}{Sample variance.}
#'   \item{min}{Minimum concentration.}
#'   \item{max}{Maximum concentration.}
#'   \item{cv}{Coefficient of variation.}
#'   \item{25\%}{25th percentile.}
#'   \item{75\%}{75th percentile.}
#'   \item{IQR}{Interquartile range.}
#'   \item{mad}{Median absolute deviation.}
#' }
#'
#' @details
#' For ungrouped `data.frame` inputs, summaries are calculated by:
#'
#' \itemize{
#'   \item location
#'   \item analyte
#'   \item reporting unit
#' }
#'
#' For ungrouped `env_schema` inputs, summaries are calculated by:
#'
#' \itemize{
#'   \item the mapped location variable returned by `location_var()`
#'   \item the mapped analyte variable returned by `analyte_var()`
#'   \item the standardized analysis unit (`.analysis_unit`)
#' }
#'
#' Missing result values are excluded from statistical calculations
#' when `na.rm = TRUE`, but still contribute to the record count
#' (`n`), which represents the number of records in each summary
#' group.
#'
#' Empty inputs return an empty tibble with the standard summary columns.
#'
#' @examples
#' stats_summary(gw)
#'
#' gw |>
#'   dplyr::group_by(location_id) |>
#'   stats_summary()
#'
#' x <- env_schema(
#'   raw_data,
#'   location = site_id,
#'   date = sample_date,
#'   analyte = parameter,
#'   result = result_value
#' )
#'
#' stats_summary(x)
#'
#' @export
stats_summary <- function(df, ...) {
  UseMethod("stats_summary")
}

#' @rdname stats_summary
#' @param location_id Location column name.
#' @param param_name Analyte column name.
#' @param censored Censoring indicator column name.
#' @param analysis_result Analytical result column name.
#' @param default_unit Reporting unit column name.
#' @export
stats_summary.data.frame <- function(
  df,
  location_id = "location_id",
  param_name = "param_name",
  censored = "censored",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  na.rm = TRUE
) {
  group_cols <- if (dplyr::is_grouped_df(df)) {
    dplyr::group_vars(df)
  } else {
    c(
      location_id,
      param_name,
      default_unit
    )
  }

  .stats_summary_core(
    df = df,
    group_cols = group_cols,
    result_col = analysis_result,
    censored_col = censored,
    na.rm = na.rm
  )
}

#' @rdname stats_summary
#'
#' @section env_schema Method:
#' For `env_schema` objects, `stats_summary()` automatically uses
#' the schema mappings and standardized analytical fields:
#'
#' \itemize{
#'   \item `.analysis_result`
#'   \item `.censored`
#'   \item `.analysis_unit`
#' }
#'
#' This allows summaries to remain consistent regardless of the
#' original source column names or schema mappings used by the input data.
#'
#' @export
stats_summary.env_schema <- function(
  df,
  na.rm = TRUE,
  ...
) {
  group_cols <- if (dplyr::is_grouped_df(df)) {
    dplyr::group_vars(df)
  } else {
    c(
      location_var(df),
      analyte_var(df),
      ".analysis_unit"
    )
  }

  .stats_summary_core(
    df = df,
    group_cols = group_cols,
    result_col = ".analysis_result",
    censored_col = ".censored",
    na.rm = na.rm
  )
}

#' Summary statistics core
#'
#' @keywords internal
.stats_summary_core <- function(
  df,
  group_cols,
  result_col,
  censored_col,
  na.rm = TRUE
) {
  required_cols <- c(
    group_cols,
    result_col,
    censored_col
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

  if (length(group_cols) > 0) {
    df <- dplyr::group_by(
      df,
      dplyr::across(
        dplyr::all_of(group_cols)
      ),
      .add = FALSE
    )
  }

  if (nrow(df) == 0) {
    return(
      tibble::tibble(
        n = integer(),
        percent_censored = numeric(),
        mean = numeric(),
        median = numeric(),
        sd = numeric(),
        var = numeric(),
        min = numeric(),
        max = numeric(),
        cv = numeric(),
        `25%` = numeric(),
        `75%` = numeric(),
        IQR = numeric(),
        mad = numeric()
      )
    )
  }

  df |>
    dplyr::summarise(
      n = dplyr::n(),

      percent_censored = mean(
        .data[[censored_col]],
        na.rm = na.rm
      ) *
        100,

      mean = mean(
        .data[[result_col]],
        na.rm = na.rm
      ),

      median = stats::median(
        .data[[result_col]],
        na.rm = na.rm
      ),

      sd = stats::sd(
        .data[[result_col]],
        na.rm = na.rm
      ),

      var = stats::var(
        .data[[result_col]],
        na.rm = na.rm
      ),

      min = min(
        .data[[result_col]],
        na.rm = na.rm
      ),

      max = max(
        .data[[result_col]],
        na.rm = na.rm
      ),

      cv = cv(
        .data[[result_col]],
        na.rm = na.rm
      ),

      `25%` = unname(
        stats::quantile(
          .data[[result_col]],
          probs = 0.25,
          na.rm = na.rm
        )
      ),

      `75%` = unname(
        stats::quantile(
          .data[[result_col]],
          probs = 0.75,
          na.rm = na.rm
        )
      ),

      IQR = stats::IQR(
        .data[[result_col]],
        na.rm = na.rm
      ),

      mad = stats::mad(
        .data[[result_col]],
        na.rm = na.rm
      ),

      .groups = "drop"
    )
}
