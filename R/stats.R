#' Estimate Distributions Using Statistical Decision Guidance
#'
#' Estimates an appropriate distributional model for each group using
#' censoring-aware statistical decision rules.
#'
#' `estimate_distribution()` automatically evaluates the percentage of
#' censored observations and available sample size using
#' [derive_decision()]. A [statistical_decision()] object is then used
#' to select the recommended distribution estimation pathway for each
#' group.
#'
#' Depending on the degree of censoring, groups are evaluated using:
#'
#' \itemize{
#'   \item `"parametric"` distribution fitting
#'   \item `"ros_censored"` censored distribution fitting
#'   \item `"nonparametric"` distribution classification
#'   \item `"descriptive_only"` evaluation
#'   \item `"insufficient_data"` when too few observations are available
#' }
#'
#' Distribution fitting is performed independently within each group and
#' requires the input data to already be grouped using
#' [dplyr::group_by()].
#'
#' @param df Grouped data frame or `env_schema` object.
#'
#' @param distribution_config Configuration list supplied to
#' distribution fitting functions.
#'
#' If `"auto"` (the default), configuration settings are obtained from
#' the group's [statistical_decision()] object.
#'
#' @param result_col Name of the numeric analytical-result column.
#' Defaults to `"analysis_result"` for ordinary data frames and the
#' schema-defined analytical-result column for `env_schema` objects.
#'
#' @param censored_col Name of the logical censoring-indicator column.
#' Defaults to `"censored"` for ordinary data frames and the schema-defined
#' censoring column for `env_schema` objects.
#'
#' @returns
#' A tibble containing one row per group.
#'
#' The returned table includes statistical decision metadata together
#' with the distribution evaluation result.
#'
#' \describe{
#'   \item{n_detect}{
#'     Number of non-censored observations.
#'   }
#'
#'   \item{percent_censored}{
#'     Percentage of observations classified as censored.
#'   }
#'
#'   \item{censoring_class}{
#'     Censoring classification used for method selection.
#'   }
#'
#'   \item{distribution_method}{
#'     Recommended distribution estimation approach.
#'   }
#'
#'   \item{trend_method}{
#'     Recommended trend evaluation method.
#'   }
#'
#'   \item{trend_guidance}{
#'     Recommended interpretation of trend analyses.
#'   }
#'
#'   \item{censored_method}{
#'     Recommended censored-data handling method.
#'   }
#'
#'   \item{distribution_config}{
#'     Distribution-fitting configuration used during evaluation.
#'     This is a named list of arguments passed to
#'     [EnvStats::distChoose()] or
#'     [EnvStats::distChooseCensored()].
#'   }
#'
#'   \item{interval_config}{
#'     Recommended interval estimation settings.
#'   }
#'
#'   \item{decision_basis}{
#'     Basis used to select the statistical approach.
#'   }
#'
#'   \item{distribution}{
#'     Selected distribution or recommended distributional approach.
#'   }
#'
#'   \item{n_used}{
#'     Number of observations used during evaluation.
#'   }
#'
#'   \item{result}{
#'     Distribution fitting result object.
#'   }
#' }
#'
#' @details
#' Method selection follows the active guidance profile used by
#' [statistical_decision()].
#'
#' Under the default EPA-style guidance:
#'
#' \tabular{lll}{
#' Condition \tab Recommendation \tab Action \cr
#' n_used < 4 \tab insufficient_data \tab no distribution fitting \cr
#' <15\% censored \tab parametric \tab fit candidate distributions \cr
#' 15-50\% censored \tab ros_censored \tab censored distribution fitting \cr
#' 50-90\% censored \tab nonparametric \tab classify as nonparametric \cr
#' >=90\% censored \tab descriptive_only \tab descriptive evaluation only
#' }
#'
#' Parametric evaluations use candidate distributions specified by
#' `distribution_config`, typically through
#' [EnvStats::distChoose()]. By default, distribution fitting evaluates
#' normal and lognormal candidate distributions using the `"sf"` selection method.
#'
#' Censored evaluations use
#' [EnvStats::distChooseCensored()] with left-censoring assumptions.
#'
#' Nonparametric and descriptive-only groups do not perform formal
#' distribution fitting and instead return recommendation metadata for
#' downstream analyses.
#'
#' @examples
#' gw |>
#'   dplyr::group_by(
#'     location_id,
#'     param_name,
#'     default_unit
#'   ) |>
#'   estimate_distribution()
#'
#' gw |>
#'   dplyr::group_by(param_name) |>
#'   estimate_distribution()
#'
#' estimate_distribution(
#'   gw |>
#'     dplyr::group_by(
#'       location_id,
#'       param_name
#'     )
#' )
#'
#' @seealso
#' [statistical_decision()],
#' [derive_decision()],
#' [guidance_profile()],
#' [guidance_options()]
#'
#' @export
estimate_distribution <- function(df, ...) {
  UseMethod("estimate_distribution")
}

#' @export
estimate_distribution.data.frame <- function(
  df,
  distribution_config = "auto",
  result_col = NULL,
  censored_col = NULL,
  ...
) {
  .estimate_distribution_grouped(
    df = df,
    distribution_config = distribution_config,
    result_col = result_col,
    censored_col = censored_col
  )
}

#' @keywords internal
.estimate_distribution_grouped <- function(
  df,
  distribution_config = "auto",
  result_col = NULL,
  censored_col = NULL
) {
  if (!dplyr::is_grouped_df(df)) {
    rlang::abort(
      paste(
        "estimate_distribution() requires grouped data.",
        "Call dplyr::group_by() before calling it."
      )
    )
  }

  if (
    !identical(distribution_config, "auto") &&
      !is.list(distribution_config)
  ) {
    rlang::abort(
      "`distribution_config` must be either \"auto\" or a named list."
    )
  }

  columns <- .resolve_analysis_columns(
    df = df,
    result_col = result_col,
    censored_col = censored_col
  )

  dplyr::group_modify(
    .data = df,
    .f = function(.x, .y) {
      .estimate_distribution_group(
        df = .x,
        distribution_config = distribution_config,
        result_col = columns$result_col,
        censored_col = columns$censored_col
      )
    }
  ) |>
    dplyr::ungroup()
}

#' @export
estimate_distribution.grouped_df <- function(
  df,
  distribution_config = "auto",
  result_col = NULL,
  censored_col = NULL,
  ...
) {
  .estimate_distribution_grouped(
    df = df,
    distribution_config = distribution_config,
    result_col = result_col,
    censored_col = censored_col
  )
}

#' @export
estimate_distribution.env_schema <- function(
  df,
  distribution_config = "auto",
  result_col = NULL,
  censored_col = NULL,
  ...
) {
  .estimate_distribution_grouped(
    df = df,
    distribution_config = distribution_config,
    result_col = result_col,
    censored_col = censored_col
  )
}

#' @export
estimate_distribution.grouped_env_schema <- function(
  df,
  distribution_config = "auto",
  result_col = NULL,
  censored_col = NULL,
  ...
) {
  .estimate_distribution_grouped(
    df = df,
    distribution_config = distribution_config,
    result_col = result_col,
    censored_col = censored_col
  )
}

#' @keywords internal
.estimate_distribution_group <- function(
  df,
  distribution_config,
  result_col,
  censored_col
) {
  decision_obj <- derive_decision(
    df = df,
    result_col = result_col,
    censored_col = censored_col
  )

  if (identical(distribution_config, "auto")) {
    distribution_config <-
      decision_obj$distribution_config[[1]]
  }

  method <-
    decision_obj$distribution_method[[1]]

  out <- switch(
    method,

    insufficient_data = {
      .attach_nested_data(
        result = .make_dist_result(
          distribution = "insufficient_data",
          n_used = decision_obj$n_used[[1L]],
          return_result = TRUE
        ),
        data = df
      )
    },

    parametric = {
      .est_dist_group(
        df,
        return_result = TRUE,
        distribution_config = distribution_config,
        result_col = result_col
      )
    },

    ros_censored = {
      .est_dist_censored_group(
        df,
        return_result = TRUE,
        distribution_config = distribution_config,
        result_col = result_col,
        censored_col = censored_col
      )
    },

    nonparametric = {
      .attach_nested_data(
        result = .make_dist_result(
          distribution = "Nonparametric",
          n_used = decision_obj$n_used[[1L]],
          return_result = TRUE
        ),
        data = df
      )
    },

    descriptive_only = {
      .attach_nested_data(
        result = .make_dist_result(
          distribution = "Descriptive Only",
          n_used = decision_obj$n_used[[1L]],
          return_result = TRUE
        ),
        data = df
      )
    },
    rlang::abort(
      sprintf("Unsupported distribution method: `%s`.", method)
    )
  )

  dplyr::bind_cols(
    dplyr::select(
      decision_obj,
      -n_used
    ),
    out
  )
}

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
#'   \item{n_used}{ Number of non-missing analytical results used in statistical
#'   evaluations and decision selection.}
#'   \item{n_detect}{Number of non-censored analytical results.}
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
#'   \item{censoring_class}{
#'         EPA Unified Guidance style censoring classification:
#'        low_censoring, moderate_censoring, high_censoring, or extreme_censoring.
#' }
#'    \item{distribution_method}{
#'          Recommended distributional approach based on the degree of censoring.
#' }
#'     \item{trend_method}{
#'          Recommended trend method.
#' }
#'     \item{trend_guidance}{
#'          Recommended interpretation of trend results.
#'            "formal_trend_test" indicates trend inference is generally
#'             supported. "screening_only" indicates trend results should
#'             be considered exploratory because of extensive censoring.
#' }
#'      \item{censored_method}{
#'          Recommended censored-data handling method for trend analyses.
#' }
#'      \item{decision_basis}{
#'          Basis for the statistical recommendation.
#'            Examples include:
#'              fewer_than_4_observations,
#'              low_censoring,
#'              moderate_censoring,
#'              high_censoring,
#'              and extreme_censoring.
#'}
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
#'
#' @param location_id Location column name.
#' @param param_name Analyte column name.
#' @param censored Censoring indicator column name.
#' @param analysis_result Analytical result column name.
#' @param default_unit Reporting unit column name.
#' @param result_col Numerical column on which stats_summary is computed.
#' @param censored_col Censoring indicator column name used to calculate
#'   detection counts and censoring percentages.
#' @export
stats_summary.data.frame <- function(
  df,
  location_id = "location_id",
  param_name = "param_name",
  censored = "censored",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  result_col = analysis_result,
  censored_col = censored,
  na.rm = TRUE
) {
  group_cols <- if (dplyr::is_grouped_df(df)) {
    dplyr::group_vars(df)
  } else {
    c(location_id, param_name, default_unit)
  }

  .stats_summary_core(
    df = df,
    group_cols = group_cols,
    result_col = result_col,
    censored_col = censored_col,
    na.rm = na.rm
  )
}

#' @rdname stats_summary
#'
#' @section env_schema Method:
#'
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
  result_col = NULL,
  censored_col = NULL,
  na.rm = TRUE,
  ...
) {
  if (is.null(result_col)) {
    result_col <- analysis_result_var(df)
  }

  if (is.null(censored_col)) {
    censored_col <- censored_var(df)
  }
  group_cols <- if (dplyr::is_grouped_df(df)) {
    dplyr::group_vars(df)
  } else {
    c(
      location_var(df),
      analyte_var(df),
      analysis_unit_var(df)
    )
  }

  .stats_summary_core(
    df = df,
    group_cols = group_cols,
    result_col = result_col,
    censored_col = censored_col,
    na.rm = na.rm
  )
}

#' @rdname stats_summary
#'
#' @export
stats_summary.grouped_df <- function(df, ...) {
  if (inherits(df, "env_schema")) {
    stats_summary.env_schema(df, ...)
  } else {
    stats_summary.data.frame(df, ...)
  }
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
  required_cols <- unique(c(group_cols, result_col, censored_col))

  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0L) {
    rlang::abort(
      paste(
        "Missing required columns:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  result_values <- df[[result_col]]
  censored_values <- df[[censored_col]]

  if (!is.numeric(result_values)) {
    rlang::abort(
      sprintf("`%s` must be numeric.", result_col)
    )
  }

  if (!is.logical(censored_values)) {
    rlang::abort(
      sprintf("`%s` must be a logical column.", censored_col)
    )
  }

  # Preserve existing groups. This is important for env_schema objects,
  # whose class includes grouped_df.
  if (!dplyr::is_grouped_df(df) && length(group_cols) > 0L) {
    df <- dplyr::group_by(
      df,
      dplyr::across(dplyr::all_of(group_cols)),
      .add = FALSE
    )
  }

  if (nrow(df) == 0L) {
    return(.empty_stats_summary())
  }

  df |>
    dplyr::summarise(
      n = dplyr::n(),

      n_used = sum(
        !is.na(.data[[result_col]])
      ),

      n_detect = sum(
        !is.na(.data[[result_col]]) &
          !is.na(.data[[censored_col]]) &
          !.data[[censored_col]]
      ),

      percent_censored = {
        keep <- !is.na(.data[[result_col]]) &
          !is.na(.data[[censored_col]])

        censor_values <- .data[[censored_col]][keep]

        if (length(censor_values) == 0L) {
          NA_real_
        } else {
          mean(censor_values) * 100
        }
      },

      mean = .safe_stat(
        mean(.data[[result_col]], na.rm = na.rm)
      ),

      median = .safe_stat(
        stats::median(.data[[result_col]], na.rm = na.rm)
      ),

      sd = .safe_stat(
        stats::sd(.data[[result_col]], na.rm = na.rm)
      ),

      var = .safe_stat(
        stats::var(.data[[result_col]], na.rm = na.rm)
      ),

      min = .safe_stat(
        min(.data[[result_col]], na.rm = na.rm)
      ),

      max = .safe_stat(
        max(.data[[result_col]], na.rm = na.rm)
      ),

      cv = {
        values <- .data[[result_col]]

        if (na.rm) {
          values <- values[!is.na(values)]
        }

        if (
          length(values) < 2L ||
            anyNA(values)
        ) {
          NA_real_
        } else {
          value_mean <- mean(values)

          if (
            !is.finite(value_mean) ||
              value_mean == 0
          ) {
            NA_real_
          } else {
            stats::sd(values) / value_mean
          }
        }
      },

      `25%` = .safe_stat(
        unname(stats::quantile(
          .data[[result_col]],
          probs = 0.25,
          na.rm = na.rm
        ))
      ),

      `75%` = .safe_stat(
        unname(stats::quantile(
          .data[[result_col]],
          probs = 0.75,
          na.rm = na.rm
        ))
      ),

      IQR = .safe_stat(
        stats::IQR(
          .data[[result_col]],
          na.rm = na.rm
        )
      ),

      mad = .safe_stat(
        stats::mad(
          .data[[result_col]],
          na.rm = na.rm
        )
      ),

      .groups = "drop"
    ) |>
    (\(summary_data) {
      decision <- statistical_decision(
        n_used = summary_data$n_used,
        n_detect = summary_data$n_detect,
        percent_censored = summary_data$percent_censored
      )

      dplyr::bind_cols(
        summary_data,
        dplyr::select(
          decision,
          -dplyr::all_of(c(
            "n_used",
            "n_detect",
            "percent_censored"
          ))
        )
      )
    })()
}

#' Safely normalize invalid scalar statistics to NA.
#'
#' @keywords internal
.safe_stat <- function(value) {
  if (
    length(value) == 0L ||
      is.na(value) ||
      is.nan(value) ||
      is.infinite(value)
  ) {
    return(NA_real_)
  }

  as.numeric(value)
}

#' Trend Summary Statistics
#'
#' Computes trend statistics and monitoring-period summary metrics
#' for environmental time-series data.
#'
#' For `env_schema` objects, trend calculations are performed using
#' the standardized `.analysis_result` values and schema mappings.
#' This ensures trends are evaluated consistently regardless of the
#' original source column names.
#'
#' If the input is grouped with [dplyr::group_by()], trend summaries
#' are calculated within the existing groups. For ungrouped inputs,
#' summaries are calculated separately for each location and analyte.
#'
#' @param df A data frame or `env_schema` object.
#' @param method Trend method. One of:
#'   \itemize{
#'   \item `"lm"` Linear regression
#'   \item `"loess"` Local regression smoother
#'   \item `"gam"` Generalized additive model
#'   \item `"mann_kendall"` Mann-Kendall trend test
#'   }
#' @param censored_method Method used to handle censored data.
#'   One of:
#'   \itemize{
#'   \item `"cenken"` Censored Kendall trend estimator
#'   \item `"as_reported"` Use analytical results as reported
#'   }
#' @param conf.level Confidence level used for trend estimates.
#' @param ... Additional arguments passed to methods.
#'
#' @returns
#' A tibble containing one row per trend group.
#'
#' The returned table includes:
#' \describe{
#'   \item{n}{Number of records in the group.}
#'   \item{first_date}{Earliest sample date.}
#'   \item{last_date}{Latest sample date.}
#'   \item{date_range_days}{Monitoring period length in days.}
#'   \item{date_range_years}{Monitoring period length in years.}
#'   \item{sampling_frequency}{Approximate samples collected per year.}
#'   \item{minimum_concentration}{Minimum analytical result.}
#'   \item{median_concentration}{Median analytical result.}
#'   \item{mean_concentration}{Mean analytical result.}
#'   \item{maximum_concentration}{Maximum analytical result.}
#'   \item{standard_deviation}{Sample standard deviation.}
#'   \item{coefficient_of_variation}{Standard deviation divided by mean.}
#'   \item{trend_method}{Trend estimation method used.}
#'   \item{censored_method}{Censored-data handling method used.}
#'   \item{slope}{Estimated trend slope.}
#'   \item{p_value}{Trend test p-value.}
#'   \item{conf_low}{Lower confidence limit for the slope estimate.}
#'   \item{conf_high}{Upper confidence limit for the slope estimate.}
#'   \item{significant}{TRUE when the trend is statistically significant
#'   at the specified confidence level.}
#'   \item{trend_direction}{Increasing, Decreasing, or Stable.}
#' }
#'
#' @details
#' Trend calculations require at least three non-missing observations
#' and at least two distinct analytical result values. Groups that do not meet
#' these requirements return summary metrics, but trend-specific fields
#' such as `slope`, `p_value`, `conf_low`, and `conf_high` are returned as `NA`.
#'
#' Slope estimates and significance statistics are only available for
#' trend methods that support formal trend estimation. For smoothing
#' methods such as `"loess"` and `"gam"`, these fields are returned as `NA`.
#'
#' For Mann-Kendall analyses using `censored_method = "cenken"`,
#' confidence limits for the Sen slope are not currently available
#' and are returned as `NA`.
#'
#' For ungrouped `env_schema` inputs, trends are calculated by:
#'
#' \itemize{
#'   \item the mapped location variable returned by `location_var()`
#'   \item the mapped analyte variable returned by `analyte_var()`
#' }
#'
#' Confidence intervals are available for some trend methods. When a
#' confidence interval cannot be estimated, `conf_low` and `conf_high`
#' are returned as `NA`.
#'
#' @examples
#' trend_summary(gw)
#'
#' gw |>
#'   dplyr::group_by(location_id) |>
#'   trend_summary()
#'
#' trend_summary(
#'   gw,
#'   method = "mann_kendall",
#'   censored_method = "cenken"
#' )
#'
#' @export
trend_summary <- function(df, ...) {
  UseMethod("trend_summary")
}

#' @rdname trend_summary
#'
#' @param location_id Location column name.
#' @param param_name Analyte column name.
#' @param sample_date Sample date column name.
#' @param analysis_result Analytical result column name.
#' @export
trend_summary.data.frame <- function(
  df,
  location_id = "location_id",
  param_name = "param_name",
  sample_date = "sample_date",
  analysis_result = "analysis_result",
  method = "mann_kendall",
  censored_method = "auto",
  conf.level = 0.95
) {
  env_schema(
    df,
    location = !!rlang::sym(location_id),
    sample_date = !!rlang::sym(sample_date),
    analyte = !!rlang::sym(param_name),
    result = !!rlang::sym(analysis_result)
  ) |>
    trend_summary(
      method = method,
      censored_method = censored_method,
      conf.level = conf.level
    )
}

#' @rdname trend_summary
#'
#' @section env_schema Method:
#'
#' For `env_schema` objects, `trend_summary()` automatically uses
#' the schema mappings and standardized analytical fields:
#'
#' \itemize{
#'   \item `.analysis_result`
#'   \item `.censored`
#'   \item mapped location variable
#'   \item mapped analyte variable
#' }
#'
#' This allows trend summaries to remain consistent regardless
#' of the original source column names or schema mappings used by
#' the input data.
#'
#' @export
trend_summary.env_schema <- function(
  df,
  result_col = NULL,
  censored_col = NULL,
  method = "mann_kendall",
  censored_method = "auto",
  conf.level = 0.95,
  ...
) {
  if (is.null(result_col)) {
    result_col <- if (".analysis_result" %in% names(df)) {
      ".analysis_result"
    } else {
      analysis_result_var(df)
    }
  }

  if (is.null(censored_col)) {
    censored_col <- if (".censored" %in% names(df)) {
      ".censored"
    } else {
      censored_var(df)
    }
  }

  validate_env_schema(df)

  alpha <- 1 - conf.level

  sample_date_col <- sample_date_var(df)

  group_cols <- .get_trend_group_cols(df)

  df |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(group_cols)
      )
    ) |>
    dplyr::group_modify(
      ~ .trend_summary_group(
        .x,
        sample_date_col = sample_date_col,
        result_col = result_col,
        censored_col = censored_col,
        method = method,
        censored_method = censored_method,
        alpha = alpha,
        conf.level = conf.level
      )
    ) |>
    dplyr::ungroup()
}

#' @keywords internal
.trend_summary_group <- function(
  df,
  sample_date_col,
  result_col,
  censored_col,
  method,
  censored_method,
  alpha,
  conf.level
) {
  n <- nrow(df)

  sample_date_values <- stats::na.omit(
    df[[sample_date_col]]
  )

  if (length(sample_date_values) > 0) {
    first_date <- min(sample_date_values)

    last_date <- max(sample_date_values)

    date_range_days <- as.numeric(
      difftime(
        last_date,
        first_date,
        units = "days"
      )
    )

    date_range_years <- date_range_days / 365.25
  } else {
    first_date <- as.Date(NA)

    last_date <- as.Date(NA)

    date_range_days <- NA_real_

    date_range_years <- NA_real_
  }

  sampling_frequency <- dplyr::case_when(
    is.na(date_range_years) ~ NA_real_,
    date_range_years <= 0 ~ NA_real_,
    TRUE ~ n / date_range_years
  )

  values <- stats::na.omit(
    df[[result_col]]
  )

  if (length(values) > 0) {
    minimum_concentration <- min(values)

    median_concentration <- stats::median(values)

    mean_concentration <- mean(values)

    maximum_concentration <- max(values)

    standard_deviation <- stats::sd(values)

    coefficient_of_variation <- dplyr::case_when(
      is.na(mean_concentration) ~ NA_real_,
      mean_concentration == 0 ~ NA_real_,
      TRUE ~ standard_deviation / mean_concentration
    )
  } else {
    minimum_concentration <- NA_real_
    median_concentration <- NA_real_
    mean_concentration <- NA_real_
    maximum_concentration <- NA_real_
    standard_deviation <- NA_real_
    coefficient_of_variation <- NA_real_
  }

  tr <- .compute_trend(
    df = df,
    sample_date_col = sample_date_col,
    result_col = result_col,
    censored_col = censored_col,
    method = method,
    censored_method = censored_method,
    conf.level = conf.level
  )

  if (is.null(tr)) {
    return(
      tibble::tibble(
        n = n,
        first_date = first_date,
        last_date = last_date,
        date_range_days = date_range_days,
        date_range_years = date_range_years,
        sampling_frequency = sampling_frequency,
        minimum_concentration = minimum_concentration,
        median_concentration = median_concentration,
        mean_concentration = mean_concentration,
        maximum_concentration = maximum_concentration,
        standard_deviation = standard_deviation,
        coefficient_of_variation = coefficient_of_variation,
        trend_method = method,
        censored_method = censored_method,
        slope = NA_real_,
        p_value = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_,
        significant = NA,
        trend_direction = NA_character_
      )
    )
  }

  tibble::tibble(
    n = n,
    first_date = first_date,
    last_date = last_date,
    date_range_days = date_range_days,
    date_range_years = date_range_years,
    sampling_frequency = sampling_frequency,
    minimum_concentration = minimum_concentration,
    median_concentration = median_concentration,
    mean_concentration = mean_concentration,
    maximum_concentration = maximum_concentration,
    standard_deviation = standard_deviation,
    coefficient_of_variation = coefficient_of_variation,
    trend_method = method,
    censored_method = tr$censored_method,
    slope = tr$slope,
    p_value = tr$p_value,
    conf_low = tr$conf_low,
    conf_high = tr$conf_high,
    significant = dplyr::case_when(
      is.na(tr$p_value) ~ NA,
      TRUE ~ tr$p_value <= alpha
    ),
    trend_direction = dplyr::case_when(
      is.na(tr$slope) ~ NA_character_,
      tr$p_value > alpha ~ "Stable",
      tr$slope > 0 ~ "Increasing",
      tr$slope < 0 ~ "Decreasing",
      TRUE ~ "Stable"
    )
  )
}

#' @keywords internal
#' @noRd
.prepare_trend_data <- function(
  df,
  sample_date_col,
  result_col,
  censored_col,
  censored_method = "auto"
) {
  required_cols <- c(
    sample_date_col,
    result_col,
    censored_col
  )

  .validate_required_cols(df, required_cols)

  out <- df |>
    dplyr::filter(
      !is.na(.data[[sample_date_col]]),
      !is.na(.data[[result_col]]),
      !is.na(.data[[censored_col]])
    ) |>
    dplyr::arrange(
      .data[[sample_date_col]]
    )

  if (nrow(out) < 3L) {
    return(NULL)
  }

  if (dplyr::n_distinct(out[[result_col]]) < 2L) {
    return(NULL)
  }

  out
}

#' Mann-Kendall Trend Test
#'
#' Performs a Mann-Kendall monotonic trend test using
#' [EnvStats::kendallTrendTest()].
#'
#' For `env_schema` objects, the standardized analytical results
#' stored in `.analysis_result` are used together with the mapped
#' date variable.
#'
#' @param df A data frame or `env_schema` object.
#' @param conf.level Confidence level for confidence interval
#'   calculations.
#' @param ... Additional arguments passed to methods.
#'
#' @returns
#' An object returned by [EnvStats::kendallTrendTest()].
#'
#' The result includes:
#' \itemize{
#'   \item trend test statistics
#'   \item p-values
#'   \item Sen slope estimate
#'   \item confidence intervals when available
#' }
#'
#' @details
#' The Mann-Kendall test is a nonparametric method commonly used
#' to evaluate monotonic trends in environmental monitoring data.
#'
#' The test estimates the Sen slope and evaluates whether a monotonic
#' increasing or decreasing trend is present through time.
#'
#' Confidence intervals may not be available for small sample sizes.
#'
#' @examples
#' kendall_trend(gw)
#'
#' kendall_trend(
#'   gw,
#'   conf.level = 0.90
#' )
#'
#' @export
kendall_trend <- function(df, ...) {
  UseMethod("kendall_trend")
}

#' @rdname kendall_trend
#'
#' @param location_id Location column name.
#' @param param_name Analyte column name.
#' @param sample_date Sample date column name.
#' @param analysis_result Analytical result column name.
#' @export
kendall_trend.data.frame <- function(
  df,
  location_id = "location_id",
  param_name = "param_name",
  sample_date = "sample_date",
  analysis_result = "analysis_result",
  conf.level = 0.95
) {
  env_schema(
    df,
    location = !!rlang::sym(location_id),
    sample_date = !!rlang::sym(sample_date),
    analyte = !!rlang::sym(param_name),
    result = !!rlang::sym(analysis_result)
  ) |>
    kendall_trend(
      conf.level = conf.level
    )
}


#' @rdname kendall_trend
#'
#' @section env_schema Method:
#'
#' For `env_schema` objects, the test is performed using:
#'
#' \itemize{
#'   \item the mapped date variable returned by `date_var()`
#'   \item standardized analytical results stored in `.analysis_result`
#' }
#'
#' @export
kendall_trend.env_schema <- function(
  df,
  result_col = NULL,
  censored_col = NULL,
  conf.level = 0.95,
  ...
) {
  if (is.null(result_col)) {
    result_col <- analysis_result_var(df)
  }

  if (is.null(censored_col)) {
    censored_col <- censored_var(df)
  }
  validate_env_schema(df)

  sample_date_col <- sample_date_var(df)

  EnvStats::kendallTrendTest(
    x = df[[result_col]],
    y = df[[sample_date_col]],
    conf.level = conf.level
  )
}


#' Extract lm stats information
#'
#' Internal helper used by plotting functions.
#'
#' @keywords internal
#'
#' @noRd
.extract_lm_stats <- function(
  fit,
  conf.level = 0.95
) {
  fit_summary <- suppressWarnings(
    base::summary(fit)
  )

  coef_tbl <- fit_summary$coefficients

  slope <- NA_real_
  p_value <- NA_real_
  conf_low <- NA_real_
  conf_high <- NA_real_

  if (
    !is.null(coef_tbl) &&
      nrow(coef_tbl) >= 2 &&
      length(stats::coef(fit)) >= 2
  ) {
    slope <- unname(stats::coef(fit)[2])

    p_value <- coef_tbl[2, 4]

    slope_ci <- tryCatch(
      suppressWarnings(
        stats::confint(
          fit,
          level = conf.level
        )
      ),
      error = function(e) NULL
    )

    if (
      !is.null(slope_ci) &&
        nrow(slope_ci) >= 2
    ) {
      conf_low <- slope_ci[2, 1]
      conf_high <- slope_ci[2, 2]
    }
  }

  list(
    slope = slope,
    p_value = p_value,
    conf_low = conf_low,
    conf_high = conf_high
  )
}

#' Resolve the trend method for a data set
#'
#' @keywords internal
.resolve_trend_censored_method <- function(
  censored,
  censored_method = "auto"
) {
  if (
    !is.logical(censored) ||
      anyNA(censored)
  ) {
    rlang::abort(
      "`censored` must be a logical vector without missing values."
    )
  }

  censored_method <- rlang::arg_match(
    censored_method,
    c("auto", "cenken", "as_reported")
  )

  has_censored <- any(censored)

  if (censored_method == "auto") {
    return(
      if (has_censored) {
        "cenken"
      } else {
        "as_reported"
      }
    )
  }

  if (
    censored_method == "as_reported" &&
      has_censored
  ) {
    rlang::abort(
      paste(
        "`censored_method = \"as_reported\"` cannot be used",
        "when censored observations are present."
      )
    )
  }

  censored_method
}

#' Compute Trend Information
#'
#' Internal helper used by plotting functions.
#'
#' @param df Data frame
#' @param method Trend method
#' @param conf.level Confidence level
#'
#' @keywords internal
#'
#' @noRd
.compute_trend <- function(
  df,
  sample_date_col,
  result_col = NULL,
  censored_col = NULL,
  method = "mann_kendall",
  censored_method = "auto",
  conf.level = 0.95
) {
  if (is.null(result_col)) {
    result_col <- analysis_result_var(df)
  }

  if (is.null(censored_col)) {
    censored_col <- censored_var(df)
  }
  df <- .prepare_trend_data(
    df = df,
    sample_date_col = sample_date_col,
    result_col = result_col,
    censored_col = censored_col,
    censored_method = censored_method
  )

  if (is.null(df)) {
    return(NULL)
  }

  method <- rlang::arg_match(
    method,
    c(
      "none",
      "lm",
      "loess",
      "gam",
      "mann_kendall"
    )
  )

  if (!censored_col %in% names(df)) {
    rlang::abort(
      sprintf(
        "Censoring column `%s` was not found.",
        censored_col
      )
    )
  }

  censored <- df[[censored_col]]

  if (!is.logical(censored)) {
    rlang::abort(
      sprintf(
        "`%s` must be logical.",
        censored_col
      )
    )
  }

  censored_method <- .resolve_trend_censored_method(
    censored = censored,
    censored_method = censored_method
  )

  has_censored <- any(censored, na.rm = TRUE)

  # Ordinary regression and smoothing methods do not account for
  # censored observations. Do not silently treat nondetects as detects.
  if (
    has_censored &&
      method %in% c("lm", "loess", "gam")
  ) {
    rlang::abort(
      c(
        paste0(
          "`method = \"",
          method,
          "\"` is not supported for censored data."
        ),
        "i" = "Use `method = \"mann_kendall\"` with `censored_method = \"cenken\"`."
      )
    )
  }

  if (censored_method == "auto") {
    decision <- derive_decision(
      df,
      result_col = result_col,
      censored_col = censored_col
    )

    censored_method <-
      decision$censored_method[[1]]
  }

  if (method == "none") {
    return(NULL)
  }

  z <- stats::qnorm(
    1 - (1 - conf.level) / 2
  )

  if (method == "lm") {
    df$.date_num <- as.numeric(
      df[[sample_date_col]]
    )

    fit <- stats::lm(
      stats::reformulate(
        ".date_num",
        response = result_col
      ),
      data = df
    )

    pred <- stats::predict(
      fit,
      interval = "confidence",
      level = conf.level
    )

    trend_df <- dplyr::bind_cols(
      df,
      as.data.frame(pred)
    )

    trend_stats <- .extract_lm_stats(
      fit,
      conf.level
    )

    return(
      list(
        method = method,
        censored_method = censored_method,
        model = fit,
        slope = trend_stats$slope,
        p_value = trend_stats$p_value,
        conf_low = trend_stats$conf_low,
        conf_high = trend_stats$conf_high,
        trend_df = trend_df
      )
    )
  }

  if (method == "loess") {
    df$.date_num <- as.numeric(
      df[[sample_date_col]]
    )

    fit <- stats::loess(
      stats::reformulate(
        ".date_num",
        response = result_col
      ),
      data = df
    )

    pred <- stats::predict(
      fit,
      se = TRUE
    )

    trend_df <- df |>
      dplyr::mutate(
        fit = pred$fit,
        se = pred$se.fit,
        lwr = fit - z * se,
        upr = fit + z * se
      )

    return(
      list(
        method = method,
        censored_method = censored_method,
        model = fit,
        trend_df = trend_df
      )
    )
  }

  if (method == "gam") {
    df$.date_num <- as.numeric(
      df[[sample_date_col]]
    )

    fit <- tryCatch(
      mgcv::gam(
        stats::as.formula(
          paste(result_col, "~ s(.date_num)")
        ),
        data = df
      ),
      error = function(e) NULL
    )

    if (is.null(fit)) {
      return(NULL)
    }

    pred <- stats::predict(
      fit,
      se.fit = TRUE
    )

    trend_df <- df |>
      dplyr::mutate(
        fit = pred$fit,
        se = pred$se.fit,
        lwr = fit - z * se,
        upr = fit + z * se
      )

    return(
      list(
        method = method,
        censored_method = censored_method,
        model = fit,
        trend_df = trend_df
      )
    )
  }

  if (
    method == "mann_kendall" &&
      censored_method == "as_reported"
  ) {
    fit <- EnvStats::kendallTrendTest(
      x = df[[result_col]],
      y = df[[sample_date_col]],
      conf.level = conf.level
    )

    x_num <- as.numeric(
      df[[sample_date_col]]
    )

    x0 <- min(x_num)

    slope <- unname(
      fit$estimate[["slope"]]
    )

    intercept <- stats::median(
      df[[result_col]] -
        slope * (x_num - x0),
      na.rm = TRUE
    )

    trend_df <- df |>
      dplyr::mutate(
        fit = intercept +
          slope * (x_num - x0)
      )

    conf_low <- NA_real_
    conf_high <- NA_real_

    if (
      !is.null(fit$conf.int) &&
        length(fit$conf.int) >= 2
    ) {
      conf_low <- fit$conf.int[1]
      conf_high <- fit$conf.int[2]
    }

    return(
      list(
        method = method,
        censored_method = censored_method,
        slope = slope,
        p_value = fit$p.value,
        conf_low = conf_low,
        conf_high = conf_high,
        trend_df = trend_df
      )
    )
  }

  if (
    method == "mann_kendall" &&
      censored_method == "cenken"
  ) {
    fit <- suppressWarnings(
      tryCatch(
        NADA2::cenken(
          y = df[[result_col]],
          ycen = df[[censored_col]],
          x = as.numeric(df[[sample_date_col]])
        ),
        error = function(e) NULL
      )
    )

    if (is.null(fit)) {
      return(NULL)
    }

    slope <- fit$slope

    p_value <- fit$p

    x_num <- as.numeric(
      df[[sample_date_col]]
    )

    x0 <- min(x_num)

    intercept <- fit$intercept

    trend_df <- df |>
      dplyr::mutate(
        fit = intercept +
          slope * (x_num - x0)
      )

    return(
      list(
        method = method,
        censored_method = censored_method,
        slope = slope,
        p_value = p_value,
        conf_low = NA_real_,
        conf_high = NA_real_,
        trend_df = trend_df
      )
    )
  }
}

#' Add Trend Layer to Plots
#'
#' @noRd
.add_trend <- function(
  p,
  df,
  result_col = NULL,
  censored_col = NULL,
  trend = "none",
  trend_ci = FALSE,
  trend_conf_level = 0.95,
  trend_label = "none",
  trend_label_sig_only = TRUE,
  censored_method = "cenken",
  plot_type = "split"
) {
  if (is.null(result_col)) {
    result_col <- analysis_result_var(df)
  }

  if (is.null(censored_col)) {
    censored_col <- censored_var(df)
  }

  censored_method <- rlang::arg_match(
    censored_method,
    c(
      "cenken",
      "as_reported"
    )
  )

  location_col <- location_var(df)
  analyte_col <- analyte_var(df)
  sample_date_col <- sample_date_var(df)

  if (trend == "none") {
    return(p)
  }

  if (trend %in% c("loess", "gam")) {
    trend_label <- "none"
  }

  alpha <- 1 - trend_conf_level

  if (plot_type == "split") {
    trend_df <- .compute_group_trends(
      df = df,
      result_col = result_col,
      method = trend,
      censored_method = censored_method,
      conf.level = trend_conf_level
    )

    if (nrow(trend_df) == 0) {
      return(p)
    }

    if (
      trend_ci &&
        all(c("lwr", "upr") %in% names(trend_df))
    ) {
      p <- p +
        ggplot2::geom_ribbon(
          data = trend_df,
          ggplot2::aes(
            x = .data[[sample_date_col]],
            group = .data[[location_col]],
            ymin = lwr,
            ymax = upr
          ),
          fill = "red",
          alpha = 0.15,
          inherit.aes = FALSE
        )
    }

    p <- p +
      ggplot2::geom_line(
        data = trend_df,
        ggplot2::aes(
          x = .data[[sample_date_col]],
          y = fit,
          group = .data[[location_col]]
        ),
        colour = "red",
        linewidth = 1,
        inherit.aes = FALSE
      )

    if (trend_label != "none") {
      labels <- .compute_group_labels(
        trend_df = trend_df,
        trend = trend,
        trend_label = trend_label,
        sample_date_col = sample_date_col,
        location_col = location_col,
        analyte_col = analyte_col
      )

      if (
        trend_label_sig_only &&
          trend == "mann_kendall"
      ) {
        labels <- labels |>
          dplyr::filter(
            !is.na(.trend_p_value),
            .trend_p_value <= alpha
          )
      }

      if (nrow(labels) > 0) {
        p <- p +
          ggplot2::geom_text(
            data = labels,
            ggplot2::aes(
              x = label_x,
              y = label_y,
              label = label
            ),
            colour = "red",
            size = 3,
            hjust = -0.05,
            inherit.aes = FALSE,
            show.legend = FALSE
          )
      }
    }

    return(p)
  }

  trend_df <- .compute_group_trends(
    df = df,
    result_col = result_col,
    method = trend,
    censored_method = censored_method,
    conf.level = trend_conf_level
  )

  if (nrow(trend_df) == 0) {
    return(p)
  }

  if (
    trend_ci &&
      all(
        c("lwr", "upr") %in% names(trend_df)
      )
  ) {
    p <- p +
      ggplot2::geom_ribbon(
        data = trend_df,
        ggplot2::aes(
          x = .data[[sample_date_col]],
          ymin = lwr,
          ymax = upr,
          group = interaction(
            .data[[location_col]],
            .data[[analyte_col]]
          ),
          fill = .data[[analyte_col]]
        ),
        alpha = 0.15,
        colour = NA,
        inherit.aes = FALSE,
        show.legend = FALSE
      )
  }

  p <- p +
    ggplot2::geom_line(
      data = trend_df,
      ggplot2::aes(
        x = .data[[sample_date_col]],
        y = fit,
        group = interaction(
          .data[[location_col]],
          .data[[analyte_col]]
        ),
        colour = .data[[analyte_col]]
      ),
      linewidth = 1,
      inherit.aes = FALSE
    )

  if (trend_label != "none") {
    labels <- .compute_group_labels(
      trend_df = trend_df,
      trend = trend,
      trend_label = trend_label,
      sample_date_col = sample_date_col,
      location_col = location_col,
      analyte_col = analyte_col
    )

    if (
      trend_label_sig_only &&
        trend == "mann_kendall"
    ) {
      labels <- labels |>
        dplyr::filter(
          !is.na(.trend_p_value),
          .trend_p_value <= alpha
        )
    }

    if (nrow(labels) > 0) {
      p <- p +
        ggplot2::geom_text(
          data = labels,
          ggplot2::aes(
            x = label_x,
            y = label_y,
            label = label,
            colour = .data[[analyte_col]]
          ),
          size = 3,
          hjust = -0.05,
          inherit.aes = FALSE,
          show.legend = FALSE
        )
    }
  }

  p
}

#' Compute trend models for grouped series
#'
#' @keywords internal
#' @noRd
.compute_group_trends <- function(
  df,
  result_col = NULL,
  censored_col = NULL,
  method = "mann_kendall",
  censored_method = "cenken",
  conf.level = 0.95
) {
  if (is.null(result_col)) {
    result_col <- analysis_result_var(df)
  }

  if (is.null(censored_col)) {
    censored_col <- censored_var(df)
  }

  sample_date_col <- sample_date_var(df)

  group_cols <- .get_trend_group_cols(df)

  df |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(group_cols)
      )
    ) |>
    dplyr::group_modify(
      ~ {
        tr <- .compute_trend(
          df = .x,
          sample_date_col = sample_date_col,
          result_col = result_col,
          censored_col = censored_col,
          method = method,
          censored_method = censored_method,
          conf.level = conf.level
        )

        if (is.null(tr)) {
          return(
            tibble::tibble()
          )
        }

        tr$trend_df |>
          dplyr::mutate(
            .trend_method = method,

            .trend_slope = if (is.null(tr$slope)) {
              NA_real_
            } else {
              tr$slope
            },

            .trend_p_value = if (is.null(tr$p_value)) {
              NA_real_
            } else {
              tr$p_value
            },

            .trend_conf_low = if (is.null(tr$conf_low)) {
              NA_real_
            } else {
              tr$conf_low
            },

            .trend_conf_high = if (is.null(tr$conf_high)) {
              NA_real_
            } else {
              tr$conf_high
            }
          )
      }
    ) |>
    dplyr::ungroup()
}

#' Compute trend labels
#' @keywords internal
#' @noRd
.compute_group_labels <- function(
  trend_df,
  trend = "mann_kendall",
  trend_label = "compact",
  sample_date_col,
  location_col,
  analyte_col
) {
  trend_df |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(
          c(
            location_col,
            analyte_col
          )
        )
      )
    ) |>
    dplyr::arrange(
      .data[[sample_date_col]],
      .by_group = TRUE
    ) |>
    dplyr::slice_tail(
      n = 1
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      label_x = .data[[sample_date_col]],

      label_y = fit,

      label = purrr::pmap_chr(
        list(
          .trend_slope,
          .trend_p_value,
          .trend_conf_low,
          .trend_conf_high
        ),
        ~ .format_trend_label(
          slope = ..1,
          p_value = ..2,
          conf_low = ..3,
          conf_high = ..4,
          trend_label = trend_label,
          trend = trend
        )
      )
    )
}

#' @keywords internal
#' @noRd
.format_trend_label <- function(
  slope,
  p_value,
  conf_low,
  conf_high,
  trend_label,
  trend
) {
  if (
    trend_label == "none" ||
      is.na(p_value)
  ) {
    return("")
  }

  direction <- dplyr::case_when(
    slope > 0 ~ "Increasing",
    slope < 0 ~ "Decreasing",
    TRUE ~ "Stable"
  )

  if (trend_label == "compact") {
    return(
      sprintf(
        "%s\np=%.3f",
        direction,
        p_value
      )
    )
  }

  if (
    trend == "mann_kendall" &&
      (is.na(conf_low) ||
        is.na(conf_high))
  ) {
    return(
      sprintf(
        paste(
          "Mann-Kendall",
          "p=%.3f",
          "Sen=%.3g",
          sep = "\n"
        ),
        p_value,
        slope
      )
    )
  }

  if (trend == "mann_kendall") {
    return(
      sprintf(
        paste(
          "Mann-Kendall",
          "p=%.3f",
          "Sen=%.3g",
          "CI=[%.3g, %.3g]",
          sep = "\n"
        ),
        p_value,
        slope,
        conf_low,
        conf_high
      )
    )
  }

  if (
    is.na(conf_low) ||
      is.na(conf_high)
  ) {
    return(
      sprintf(
        paste(
          "Slope=%.3g",
          "p=%.3f",
          sep = "\n"
        ),
        slope,
        p_value
      )
    )
  }

  sprintf(
    paste(
      "Slope=%.3g",
      "p=%.3f",
      "CI=[%.3g, %.3g]",
      sep = "\n"
    ),
    slope,
    p_value,
    conf_low,
    conf_high
  )
}

#' @keywords internal
#' @noRd
.get_trend_group_cols <- function(df) {
  if (dplyr::is_grouped_df(df)) {
    return(
      dplyr::group_vars(df)
    )
  }

  c(
    location_var(df),
    analyte_var(df)
  )
}

#' @keywords internal
#' @noRd
.get_outlier_group_cols <- function(df) {
  if (dplyr::is_grouped_df(df)) {
    return(
      dplyr::group_vars(df)
    )
  }

  c(
    location_var(df),
    analyte_var(df),
    analysis_unit_var(df)
  )
}

#' @keywords internal
#' @noRd
.build_outlier_groups <- function(df) {
  group_cols <- .get_outlier_group_cols(df)

  df |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(group_cols)
      ),
      .add = FALSE
    ) |>
    tidyr::nest()
}

#' @keywords internal
#' @noRd
.extract_htest_results <- function(
  df,
  test_col
) {
  df |>
    dplyr::mutate(
      alternative = purrr::map(
        .data[[test_col]],
        ~ {
          if (is.null(.x)) {
            NA_character_
          } else {
            .x$alternative
          }
        }
      ),

      p_value = purrr::map(
        .data[[test_col]],
        ~ {
          if (is.null(.x)) {
            NA_real_
          } else {
            .x$p.value
          }
        }
      )
    )
}

#' Dixon Test
#'
#' Performs Dixon's Q test for potential outliers in environmental
#' monitoring data.
#'
#' `dixon_test()` is an S3 generic that supports both raw
#' `data.frame` inputs and `env_schema` objects.
#'
#' If the input is grouped using [dplyr::group_by()], Dixon tests are
#' performed within the existing groups. For ungrouped `env_schema`
#' inputs, tests are performed separately for each:
#'
#' \itemize{
#'   \item location
#'   \item analyte
#'   \item analysis unit
#' }
#'
#' Dixon's test is intended for relatively small sample sizes and is
#' generally appropriate for groups containing between three and
#' thirty observations.
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional arguments passed to methods.
#'
#' @returns
#' A tibble containing one row per outlier evaluation group.
#'
#' @details
#' Dixon's test evaluates whether the most extreme observation is
#' statistically inconsistent with the remaining observations in the
#' group.
#'
#' The method is most appropriate for small datasets and should be
#' interpreted cautiously for environmental monitoring programs with
#' larger sample sizes.
#'
#' @section Groundwater Monitoring:
#'
#' Dixon's and Grubbs' tests are generally most appropriate
#' for relatively small datasets and may be sensitive to
#' distributional assumptions.
#'
#' For larger groundwater monitoring datasets,
#' `rosner_test()` is often preferred.
#'
#' @examples
#' dixon_test(gw_data)
#'
#' schema <- env_schema(
#'   gw_data,
#'   location = location_id,
#'   sample_date = sample_date,
#'   analyte = param_name,
#'   result = analysis_result,
#'   unit = default_unit
#' )
#'
#' dixon_test(schema)
#'
#' schema |>
#'   dplyr::group_by(location_id) |>
#'   dixon_test()
#'
#' @export
dixon_test <- function(df, ...) {
  UseMethod("dixon_test")
}

#' @rdname dixon_test
#'
#' @param location_id Location column name.
#' @param param_name Analyte column name.
#' @param sample_date Sample date column name.
#' @param analysis_result Analytical result column name.
#' @param default_unit Reporting unit column name.
#'
#' @export
dixon_test.data.frame <- function(
  df,
  location_id = "location_id",
  param_name = "param_name",
  sample_date = "sample_date",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  ...
) {
  df <- env_schema(
    df,
    location = !!rlang::sym(location_id),
    analyte = !!rlang::sym(param_name),
    sample_date = !!rlang::sym(sample_date),
    result = !!rlang::sym(analysis_result),
    unit = !!rlang::sym(default_unit)
  )

  dixon_test(
    df,
    ...
  )
}

#' @rdname dixon_test
#'
#' @param type Variant of Dixon's test.
#' @param opposite Logical indicating whether the lowest value
#' should be tested rather than the highest value.
#' @param two_sided Logical indicating whether a two-sided test
#' should be performed.
#' @param keep_data_object Logical indicating whether the complete
#' test object should be retained in the output.
#'
#' @section env_schema Method:
#'
#' For `env_schema` objects, Dixon tests are performed using
#' standardized analytical results stored in `.analysis_result`.
#'
#' Grouping is determined automatically from the current
#' `dplyr::group_by()` structure or, for ungrouped data, uses:
#'
#' \itemize{
#'   \item location
#'   \item analyte
#'   \item analysis unit
#' }
#'
#' @export
dixon_test.env_schema <- function(
  df,
  type = 0,
  opposite = FALSE,
  two_sided = TRUE,
  keep_data_object = FALSE,
  ...
) {
  validate_env_schema(df)

  df <- df |>
    .build_outlier_groups() |>
    dplyr::mutate(
      dixon = purrr::map(
        data,
        ~ {
          x <- stats::na.omit(
            .x$.analysis_result
          )

          x <- x[
            is.finite(x)
          ]

          if (
            length(x) < 3 ||
              length(x) > 30 ||
              dplyr::n_distinct(x) < 3
          ) {
            return(NULL)
          }

          outliers::dixon.test(
            x,
            type = type,
            opposite = opposite,
            two.sided = two_sided
          )
        }
      )
    )

  if (isTRUE(keep_data_object)) {
    outliers <- df |>
      .extract_htest_results(
        test_col = "dixon"
      ) |>
      dplyr::select(-data)
  } else {
    outliers <- df |>
      .extract_htest_results(
        test_col = "dixon"
      ) |>
      dplyr::select(-data, -dixon) |>
      tidyr::unnest(
        cols = c(
          alternative,
          p_value
        )
      )
  }

  return(outliers)
}

#' Grubbs Test
#'
#' Performs Grubbs' test for potential outliers in environmental
#' monitoring data.
#'
#' `grubbs_test()` is an S3 generic supporting both `data.frame`
#' and `env_schema` objects.
#'
#' If the input is grouped using [dplyr::group_by()], Grubbs'
#' tests are performed within the existing groups.
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional arguments passed to methods.
#'
#' @returns
#' A tibble containing one row per outlier evaluation group.
#'
#' @details
#' Grubbs' test evaluates whether the most extreme value differs
#' significantly from the remaining observations.
#'
#' Grubbs' test assumes approximately normally distributed data.
#'
#' For many environmental datasets, Rosner's test may be more
#' appropriate when larger sample sizes are available.
#'
#' @section Groundwater Monitoring:
#'
#' Dixon's and Grubbs' tests are generally most appropriate
#' for relatively small datasets and may be sensitive to
#' distributional assumptions.
#'
#' For larger groundwater monitoring datasets,
#' `rosner_test()` is often preferred.
#'
#' @examples
#' grubbs_test(gw_data)
#'
#' @export
grubbs_test <- function(df, ...) {
  UseMethod("grubbs_test")
}

#' @rdname grubbs_test
#'
#' @param location_id Location column name.
#' @param param_name Analyte column name.
#' @param sample_date Sample date column.
#' @param analysis_result Analytical result column name.
#' @param default_unit Reporting unit column name.
#'
#' @export
grubbs_test.data.frame <- function(
  df,
  location_id = "location_id",
  param_name = "param_name",
  sample_date = "sample_date",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  ...
) {
  df <- env_schema(
    df,
    location = !!rlang::sym(location_id),
    analyte = !!rlang::sym(param_name),
    sample_date = !!rlang::sym(sample_date),
    result = !!rlang::sym(analysis_result),
    unit = !!rlang::sym(default_unit)
  )

  grubbs_test(
    df,
    ...
  )
}

#' @rdname grubbs_test
#'
#' @param type Grubbs test variant.
#' @param opposite Logical indicating whether the lowest value
#' should be tested rather than the highest value.
#' @param two_sided Logical indicating whether a two-sided test
#' should be performed.
#' @param keep_data_object Logical indicating whether the complete
#' test object should be retained.
#'
#' @section env_schema Method:
#'
#' For `env_schema` objects, tests are performed using
#' `.analysis_result` values and respect existing
#' `dplyr::group_by()` groups.
#'
#' @export
grubbs_test.env_schema <- function(
  df,
  type = 10,
  opposite = FALSE,
  two_sided = FALSE,
  keep_data_object = FALSE,
  ...
) {
  validate_env_schema(df)

  df <- df |>
    .build_outlier_groups() |>
    dplyr::mutate(
      grubbs = purrr::map(
        .x = data,
        ~ outliers::grubbs.test(
          .x$.analysis_result,
          type = type,
          opposite = opposite,
          two.sided = two_sided
        )
      )
    )

  if (isTRUE(keep_data_object)) {
    outliers <- df |>
      .extract_htest_results(
        test_col = "grubbs"
      ) |>
      dplyr::select(-data)
  } else {
    outliers <- df |>
      .extract_htest_results(
        test_col = "grubbs"
      ) |>
      dplyr::select(-data, -grubbs) |>
      tidyr::unnest(
        cols = c(
          alternative,
          p_value
        )
      )
  }

  return(outliers)
}

#' Rosner Test
#'
#' Performs Rosner's test for one or more potential outliers in
#' environmental monitoring data.
#'
#' `rosner_test()` is an S3 generic supporting both `data.frame`
#' and `env_schema` objects.
#'
#' If the input is grouped using [dplyr::group_by()], Rosner tests
#' are performed within the existing groups.
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional arguments passed to methods.
#'
#' @returns
#' A tibble containing one row per outlier evaluation group.
#'
#' @details
#' Rosner's test evaluates multiple candidate outliers
#' simultaneously and is commonly used in environmental and
#' groundwater statistical evaluations.
#'
#' When groups contain insufficient data, no outlier evaluation is
#' performed and missing values are returned.
#'
#' @section Groundwater Monitoring:
#'
#' Rosner's test is often preferred for groundwater monitoring
#' datasets because it can evaluate multiple potential outliers and
#' generally performs better than Dixon's or Grubbs' tests when
#' sample sizes are sufficiently large.
#'
#' @examples
#' schema <- env_schema(
#'   gw_data,
#'   location = location_id,
#'   sample_date = sample_date,
#'   analyte = param_name,
#'   result = analysis_result,
#'   unit = default_unit
#' )
#'
#' rosner_test(schema)
#'
#' schema |>
#'   dplyr::group_by(location_id) |>
#'   rosner_test()
#'
#' @export
rosner_test <- function(df, ...) {
  UseMethod("rosner_test")
}

#' @rdname rosner_test
#'
#' @param location_id Location column name.
#' @param param_name Analyte column name.
#' @param sample_date Sample date column.
#' @param analysis_result Analytical result column name.
#' @param default_unit Reporting unit column name.
#'
#' @export
rosner_test.data.frame <- function(
  df,
  location_id = "location_id",
  param_name = "param_name",
  sample_date = "sample_date",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  ...
) {
  df <- env_schema(
    df,
    location = !!rlang::sym(location_id),
    analyte = !!rlang::sym(param_name),
    sample_date = !!rlang::sym(sample_date),
    result = !!rlang::sym(analysis_result),
    unit = !!rlang::sym(default_unit)
  )

  rosner_test(
    df,
    ...
  )
}

#' @rdname rosner_test
#'
#' @param k Maximum number of candidate outliers evaluated.
#' @param alpha Significance level.
#' @param warn Logical indicating whether Rosner test warnings
#' should be returned.
#' @param keep_data_object Logical indicating whether the complete
#' Rosner result object should be returned.
#'
#' @section env_schema Method:
#'
#' For `env_schema` objects, Rosner tests use standardized
#' `.analysis_result` values.
#'
#' Existing `dplyr::group_by()` groups are preserved. When the
#' input is not grouped, tests are performed by:
#'
#' \itemize{
#'   \item location
#'   \item analyte
#'   \item analysis unit
#' }
#'
#' @returns
#' A tibble containing one row per outlier evaluation group.
#'
#' The returned table includes:
#' \describe{
#'   \item{Value}{Observation evaluated by Rosner's procedure.}
#'   \item{Outlier}{Logical indicator identifying observations
#'   classified as outliers.}
#' }
#'
#' @export
rosner_test.env_schema <- function(
  df,
  k = 3,
  alpha = 0.05,
  warn = TRUE,
  keep_data_object = FALSE,
  ...
) {
  validate_env_schema(df)

  df <- df |>
    .build_outlier_groups() |>
    dplyr::mutate(
      rosner = purrr::map(
        data,
        purrr::possibly(
          function(.x) {
            x <- stats::na.omit(
              .x$.analysis_result
            )

            x <- x[
              is.finite(x)
            ]

            if (length(x) < 3) {
              return(NULL)
            }

            if (dplyr::n_distinct(x) < 2) {
              return(NULL)
            }

            k_use <- min(
              k,
              length(x) - 2
            )

            if (k_use < 1) {
              return(NULL)
            }

            suppressWarnings(
              EnvStats::rosnerTest(
                x,
                k = k_use,
                alpha = alpha,
                warn = warn
              )
            )
          },
          otherwise = NULL
        )
      )
    )

  if (isTRUE(keep_data_object)) {
    outliers <- df |>
      dplyr::mutate(
        value = purrr::map(.x = rosner, ~ .x$all.stats[c("Value", "Outlier")])
      ) |>
      dplyr::select(-data)
  } else {
    outliers <- df |>
      dplyr::mutate(
        value = purrr::map(.x = rosner, ~ .x$all.stats[c("Value", "Outlier")])
      ) |>
      dplyr::select(-data, -rosner) |>
      tidyr::unnest(
        cols = c(value)
      )
  }

  return(outliers)
}

#' Tukey Outlier Screening
#'
#' Identifies potential outliers using Tukey's interquartile range
#' (IQR) method.
#'
#' Observations greater than:
#'
#' \deqn{
#' Q3 + k \times IQR
#' }
#'
#' or less than:
#'
#' \deqn{
#' Q1 - k \times IQR
#' }
#'
#' are flagged as potential outliers.
#'
#' `tukey_outlier()` supports both `data.frame`
#' and `env_schema` objects.
#'
#' If the input is grouped using [dplyr::group_by()], screening is
#' performed within each group.
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional arguments passed to methods.
#'
#' @returns
#' The original observations with an added logical
#' `outlier` column identifying observations that exceed
#' the specified Tukey IQR thresholds.
#'
#' @section Groundwater Monitoring:
#'
#' Tukey's method should generally be considered a screening tool
#' rather than a formal hypothesis test. Potential outliers
#' identified using this method should be evaluated further before
#' exclusion from statistical analyses.
#'
#' @export
tukey_outlier <- function(df, ...) {
  UseMethod("tukey_outlier")
}

#' @rdname tukey_outlier
#'
#' @export
tukey_outlier.data.frame <- function(
  df,
  location_id = "location_id",
  param_name = "param_name",
  sample_date = "sample_date",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  ...
) {
  df <- env_schema(
    df,
    location = !!rlang::sym(location_id),
    analyte = !!rlang::sym(param_name),
    sample_date = !!rlang::sym(sample_date),
    result = !!rlang::sym(analysis_result),
    unit = !!rlang::sym(default_unit)
  )

  tukey_outlier(
    df,
    ...
  )
}

#' @rdname tukey_outlier
#'
#' @param k IQR multiplier.
#' @param na.rm Logical indicating whether missing values should be
#' removed.
#' @param type Quantile algorithm used in the calculation.
#'
#' @section env_schema Method:
#'
#' For `env_schema` objects, screening is performed using
#' standardized `.analysis_result` values and respects existing
#' `dplyr::group_by()` groups.
#'
#' @export
tukey_outlier.env_schema <- function(
  df,
  k = 3,
  na.rm = FALSE,
  type = 6,
  ...
) {
  validate_env_schema(df)

  df <- df |>
    .build_outlier_groups() |>
    dplyr::mutate(
      outlier = purrr::map(
        .x = data,
        ~ dplyr::case_when(
          .x$.analysis_result >
            .tukey_high_cutoff(
              .x$.analysis_result,
              k = k,
              na.rm = na.rm,
              type = type
            ) ~ TRUE,
          .x$.analysis_result <
            .tukey_low_cutoff(
              .x$.analysis_result,
              k = k,
              na.rm = na.rm,
              type = type
            ) ~ TRUE,
          TRUE ~ FALSE
        )
      )
    ) |>
    tidyr::unnest(cols = c(data, outlier))

  return(df)
}

#' @keywords internal
.tukey_low_cutoff <- function(x, k = 3, na.rm = FALSE, type = 6) {
  low <- quantile(x, na.rm = na.rm, type = type)[["25%"]] -
    k * IQR(x, na.rm = na.rm, type = type)
  return(low)
}

#' @keywords internal
.tukey_high_cutoff <- function(x, k = 3, na.rm = FALSE, type = 6, ...) {
  high <- quantile(x, na.rm = na.rm, type = type)[["75%"]] +
    k * IQR(x, na.rm = na.rm, type = type)
  return(high)
}

#' Box-Cox Transformation
#'
#' Applies a Box-Cox transformation to analytical results within groups.
#'
#' Transformation parameters are estimated independently for each group
#' using [EnvStats::boxcox()]. Transformed values are calculated using
#' [EnvStats::boxcoxTransform()].
#'
#' Existing analytical results are preserved. Transformed values are
#' returned in a new `.boxcox_transform` column together with the
#' estimated transformation parameter `.boxcox_lambda`.
#'
#' If the input is grouped using [dplyr::group_by()], transformations
#' are performed within the existing groups.
#'
#' For ungrouped `env_schema` objects, transformations are performed by:
#'
#' \itemize{
#'   \item location
#'   \item analyte
#'   \item analysis unit
#' }
#'
#' For ungrouped `data.frame` inputs, data are first converted to an
#' env_schema object and transformations are performed separately by
#' location, analyte, and analysis unit.
#'
#' @param df A data frame or `env_schema` object.
#' @param objective_name Character string specifying the optimization
#'   criterion passed to [EnvStats::boxcox()].
#' @param keep_data_object Logical indicating whether the complete
#'   Box-Cox result object should be returned.
#' @param ... Additional arguments passed to methods.
#'
#' @returns
#' The original data with additional columns:
#'
#' \describe{
#'   \item{.boxcox_transform}{
#'   Box-Cox transformed analytical result.
#'   }
#'
#'   \item{.boxcox_lambda}{
#'   Estimated Box-Cox transformation parameter.
#'   }
#'
#'   \item{boxcox}{
#'   Full Box-Cox optimization result object when
#'   `keep_data_object = TRUE`.
#'   }
#' }
#'
#' @examples
#' gw |>
#'   dplyr::group_by(
#'     location_id,
#'     param_name
#'   ) |>
#'   boxcox_transform()
#'
#' gw |>
#'   boxcox_transform()
#'
#' @export
boxcox_transform <- function(df, ...) {
  UseMethod("boxcox_transform")
}

#' @rdname boxcox_transform
#'
#' @export
boxcox_transform.data.frame <- function(
  df,
  location_id = "location_id",
  param_name = "param_name",
  sample_date = "sample_date",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  objective_name = "PPCC",
  keep_data_object = FALSE,
  ...
) {
  df <- env_schema(
    df,
    location = !!rlang::sym(location_id),
    analyte = !!rlang::sym(param_name),
    sample_date = !!rlang::sym(sample_date),
    result = !!rlang::sym(analysis_result),
    unit = !!rlang::sym(default_unit)
  )

  result_col <- analysis_result_var(df)

  boxcox_transform(
    df = df,
    result_col = result_col,
    objective_name = objective_name,
    keep_data_object = keep_data_object,
    ...
  )
}

#' @rdname boxcox_transform
#'
#' @section env_schema Method:
#'
#' For `env_schema` objects, Box-Cox transformations are performed using
#' standardized analytical results stored in `.analysis_result`.
#'
#' Existing `dplyr::group_by()` groups are preserved.
#'
#' When the input is not grouped, transformations are performed by:
#'
#' \itemize{
#'   \item location
#'   \item analyte
#'   \item analysis unit
#' }
#'
#' Schema metadata are preserved.
#'
#' @export
boxcox_transform.env_schema <- function(
  df,
  result_col = NULL,
  objective_name = "PPCC",
  keep_data_object = FALSE,
  ...
) {
  if (is.null(result_col)) {
    result_col <- analysis_result_var(df)
  }

  validate_env_schema(df)

  template <- df
  was_grouped <- dplyr::is_grouped_df(df)

  if (!was_grouped) {
    df <- df |>
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(
            c(
              location_var(df),
              analyte_var(df),
              analysis_unit_var(df)
            )
          )
        )
      )
  }

  out <- .boxcox_transform_core(
    df = df,
    result_col = result_col,
    objective_name = objective_name,
    keep_data_object = keep_data_object
  )

  if (!was_grouped) {
    out <- dplyr::ungroup(out)
  }

  out <- restore_env_schema(
    out,
    template,
    validate = FALSE
  )

  out
}

#' @keywords internal
.boxcox_transform_core <- function(
  df,
  result_col,
  objective_name = "PPCC",
  keep_data_object = FALSE
) {
  grouped <- dplyr::is_grouped_df(df)

  out <- df |>
    tidyr::nest() |>
    dplyr::mutate(
      boxcox = purrr::map(
        data,
        purrr::possibly(
          function(.x) {
            EnvStats::boxcox(
              .x[[result_col]],
              optimize = TRUE,
              objective.name = objective_name
            )
          },
          otherwise = NULL
        )
      ),

      .boxcox_lambda = purrr::map_dbl(
        boxcox,
        ~ if (is.null(.x)) {
          NA_real_
        } else {
          .x$lambda
        }
      ),

      data = purrr::map2(
        data,
        .boxcox_lambda,
        ~ {
          if (is.na(.y)) {
            dplyr::mutate(
              .x,
              .boxcox_transform = NA_real_
            )
          } else {
            dplyr::mutate(
              .x,
              .boxcox_transform = EnvStats::boxcoxTransform(
                .x[[result_col]],
                lambda = .y
              )
            )
          }
        }
      )
    )

  if (!keep_data_object) {
    out <- dplyr::select(
      out,
      -boxcox
    )
  }

  out <- out |>
    tidyr::unnest(
      cols = data
    )

  if (!grouped) {
    out <- dplyr::ungroup(out)
  }

  out
}

#' Compare Analytical Results Among Groups
#'
#' Performs multiple-group statistical comparisons on environmental
#' analytical results.
#'
#' `compare_groups()` automatically selects a recommended comparison
#' procedure using [derive_decision()] and
#' [statistical_decision()].
#'
#' Recommended methods are based primarily on the degree of censoring:
#'
#' \itemize{
#'   \item `anova` for low-censoring datasets
#'   \item `kruskal` for moderate or high censoring
#'   \item `insufficient_data` when too few observations are available
#' }
#'
#' Explicit alternatives are available through:
#'
#' \itemize{
#'   \item [anova_test()]
#'   \item [welch_test()]
#'   \item [kruskal_test()]
#' }
#'
#' Existing grouping created with [dplyr::group_by()] is respected.
#' Comparisons are performed independently within each grouping level.
#'
#' For ungrouped `env_schema` objects, comparisons are performed
#' separately for each analyte and analysis unit.
#'
#' @param df A data frame or `env_schema` object.
#' @param compare Character column name identifying the grouping
#'   factor to compare.
#' @param result_col Analytical result column.
#' @param keep_data_object Logical indicating whether the complete
#' statistical test object should be retained in the returned output.
#' @param variance_alpha Significance level used by Levene's test to
#' determine whether group variances are considered unequal.
#' Defaults to `0.05`.
#' @param levene_center Centering method for Levene's test. `"median"`
#' performs the Brown-Forsythe version and is the default. `"mean"` performs
#' the classical mean-centered version.
#' @param ... Additional arguments passed to methods.
#'
#' @returns
#' A tibble containing one row per comparison group.
#'
#' The returned table includes:
#'
#' \describe{
#'   \item{comparison_method}{
#'     Statistical procedure used.
#'   }
#'
#'   \item{statistic}{
#'     Test statistic returned by the selected procedure.
#'   }
#'
#'   \item{p_value}{
#'     P-value associated with the test statistic.
#'   }
#'
#'   \item{test}{
#'     Full statistical test object when
#'     `keep_data_object = TRUE`.
#'   }
#' }
#'
#' @details
#'
#' Automatic method selection follows the active
#' [statistical_decision()] framework.
#'
#' Under the default EPA-style profile:
#'
#' \tabular{llll}{
#' Condition \tab Variance test \tab Recommendation \tab Method \cr
#' <15\% censored \tab p >= alpha \tab Equal variance \tab ANOVA \cr
#' <15\% censored \tab p < alpha \tab Unequal variance \tab Welch \cr
#' 15-90\% \tab Not used \tab Nonparametric comparison \tab Kruskal-Wallis \cr
#' >=90\% \tab Not used \tab Insufficient information \tab No comparison
#' }
#'
#' Comparisons require at least two distinct comparison groups.
#'
#' If the comparison variable contains only one level,
#' test statistics and p-values are returned as `NA`.
#'
#' The comparison variable cannot also be an active grouping variable.
#'
#' Existing grouping controls the strata within which comparisons
#' are performed.
#'
#' For example:
#'
#' \preformatted{
#' gw |>
#'   dplyr::group_by(param_name) |>
#'   compare_groups(compare = "location_id")
#' }
#'
#' compares locations separately within each analyte.
#'
#' \preformatted{
#' gw |>
#'   dplyr::group_by(well_class) |>
#'   compare_groups(compare = "location_id")
#' }
#'
#' compares locations separately within each well classification.
#'
#' Under the default EPA-style guidance profile,
#' `compare_groups()` recommends classical ANOVA for low-censored
#' datasets and Kruskal-Wallis tests for datasets with moderate
#' or high censoring.
#'
#' Explicit method-selection functions are available for users who
#' wish to override the automatic recommendation framework:
#'
#' \itemize{
#'   \item [anova_test()]
#'   \item [welch_test()]
#'   \item [kruskal_test()]
#' }
#'
#' @seealso
#' [statistical_decision()],
#' [derive_decision()],
#' [anova_test()],
#' [welch_test()],
#' [kruskal_test()]
#'
#' @examples
#' compare_groups(
#'   gw,
#'   compare = "location_id"
#' )
#'
#' anova_test(
#'   gw,
#'   compare = "location_id"
#' )
#'
#' welch_test(
#'   gw,
#'   compare = "location_id"
#' )
#'
#' kruskal_test(
#'   gw,
#'   compare = "location_id"
#' )
#'
#' gw |>
#'   dplyr::group_by(param_name) |>
#'   compare_groups(
#'     compare = "location_id"
#'   )
#'
#' gw |>
#'   dplyr::group_by(well_class) |>
#'   compare_groups(
#'     compare = "location_id"
#'   )
#'
#' @export
compare_groups <- function(df, ...) {
  UseMethod("compare_groups")
}

#' @rdname compare_groups
#'
#' @export
compare_groups.data.frame <- function(
  df,
  location_id = "location_id",
  param_name = "param_name",
  sample_date = "sample_date",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  compare = location_id,
  keep_data_object = FALSE,
  ...
) {
  df <- env_schema(
    df,
    location = !!rlang::sym(location_id),
    analyte = !!rlang::sym(param_name),
    sample_date = !!rlang::sym(sample_date),
    result = !!rlang::sym(analysis_result),
    unit = !!rlang::sym(default_unit)
  )

  compare_groups(
    df,
    compare = compare,
    keep_data_object = keep_data_object,
    ...
  )
}

#' @rdname compare_groups
#'
#' @export
compare_groups.grouped_df <- function(df, ...) {
  if (inherits(df, "env_schema")) {
    compare_groups.env_schema(df, ...)
  } else {
    compare_groups.data.frame(df, ...)
  }
}

#' @rdname compare_groups
#'
#' @export
compare_groups.env_schema <- function(
  df,
  compare = NULL,
  result_col = NULL,
  censored_col = NULL,
  keep_data_object = FALSE,
  ...
) {
  if (is.null(compare)) {
    compare <- location_var(df)
  }
  if (is.null(result_col)) {
    result_col <- analysis_result_var(df)
  }

  if (is.null(censored_col)) {
    censored_col <- censored_var(df)
  }

  validate_env_schema(df)

  .compare_groups_core(
    df = df,
    compare = compare,
    result_col = result_col,
    censored_col = censored_col,
    method = "auto",
    keep_data_object = keep_data_object
  )
}

#' @keywords internal
.compare_groups_core <- function(
  df,
  compare,
  result_col,
  censored_col,
  method = c(
    "auto",
    "anova",
    "welch",
    "kruskal",
    "cenperm2",
    "cen2means",
    "cenanova",
    "censored_nonparametric"
  ),
  keep_data_object = FALSE
) {
  method <- match.arg(
    method,
    c(
      "auto",
      "anova",
      "welch",
      "kruskal",
      "cen2perm",
      "cen2means",
      "cenanova",
      "censored_nonparametric"
    )
  )

  if (
    !is.character(compare) ||
      length(compare) != 1L ||
      is.na(compare) ||
      !nzchar(compare)
  ) {
    rlang::abort("`compare` must be a single column name.")
  }

  if (!compare %in% names(df)) {
    rlang::abort(
      sprintf("Comparison column `%s` was not found.", compare)
    )
  }

  group_cols <- if (dplyr::is_grouped_df(df)) {
    dplyr::group_vars(df)
  } else {
    c(
      analyte_var(df),
      analysis_unit_var(df)
    )
  }

  group_cols <- unique(
    group_cols[group_cols %in% names(df)]
  )

  if (compare %in% group_cols) {
    rlang::abort(
      sprintf(
        "`compare` ('%s') cannot also be a grouping variable.",
        compare
      )
    )
  }

  nested <- if (length(group_cols) == 0L) {
    tibble::tibble(data = list(df))
  } else {
    df |>
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(group_cols)
        )
      ) |>
      tidyr::nest()
  }

  nested |>
    dplyr::mutate(
      decision = purrr::map(
        data,
        derive_decision,
        result_col = result_col,
        censored_col = censored_col
      ),

      comparison_method = if (identical(method, "auto")) {
        purrr::map_chr(
          decision,
          ~ .x$comparison_method[[1L]]
        )
      } else {
        rep(method, dplyr::n())
      },

      test = purrr::pmap(
        list(data, comparison_method),
        function(data, selected_method) {
          if (
            selected_method %in%
              c(
                "cenanova",
                "cenperm2",
                "censored_nonparametric"
              )
          ) {
            .run_censored_comparison_test(
              df = data,
              method = selected_method,
              compare = compare,
              result_col = result_col,
              censored_col = censored_col
            )
          } else {
            .run_comparison_test(
              df = data,
              method = selected_method,
              compare = compare,
              result_col = result_col
            )
          }
        }
      )
    ) |>
    .extract_comparison_results(
      keep_data_object = keep_data_object
    )
}

#' @keywords internal
.run_comparison_test <- function(
  df,
  method,
  compare,
  result_col
) {
  keep <- !is.na(df[[compare]]) &
    !is.na(df[[result_col]])

  df <- df[keep, , drop = FALSE]

  if (dplyr::n_distinct(df[[compare]]) < 2L) {
    return(NULL)
  }

  df[[compare]] <- droplevels(factor(df[[compare]]))

  frm <- stats::reformulate(
    compare,
    response = result_col
  )

  switch(
    method,

    anova = stats::aov(
      frm,
      data = df
    ),

    welch = stats::oneway.test(
      frm,
      data = df,
      var.equal = FALSE
    ),

    kruskal = stats::kruskal.test(
      frm,
      data = df
    ),

    rlang::abort(
      sprintf("Unsupported comparison method: `%s`.", method)
    )
  )
}

#' @rdname compare_groups
#'
#' @section ANOVA Method:
#'
#' `anova_test()` performs a classical one-way analysis of variance
#' using [stats::aov()].
#'
#' This method assumes:
#'
#' \itemize{
#'   \item approximately normal residuals
#'   \item homogeneous variances among groups
#'   \item independent observations
#' }
#'
#' This function bypasses automatic method selection and always
#' performs ANOVA.
#'
#' @export
anova_test <- function(df, ...) {
  UseMethod("anova_test")
}

#' @rdname compare_groups
#'
#' @export
anova_test.data.frame <- function(
  df,
  location = "location_id",
  analyte = "param_name",
  sample_date = "sample_date",
  result = "analysis_result",
  unit = "default_unit",
  compare = location,
  keep_data_object = FALSE,
  ...
) {
  df <- env_schema(
    df,
    location = !!rlang::sym(location),
    analyte = !!rlang::sym(analyte),
    sample_date = !!rlang::sym(sample_date),
    result = !!rlang::sym(result),
    unit = !!rlang::sym(unit)
  )

  result_col <- analysis_result_var(df)
  censored_col <- censored_var(df)

  .compare_groups_core(
    df = df,
    compare = compare,
    result_col = result_col,
    censored_col = censored_col,
    method = "anova",
    keep_data_object = keep_data_object
  )
}

#' @rdname compare_groups
#'
#' @export
anova_test.env_schema <- function(
  df,
  compare = location_var(df),
  result_col = analysis_result_var(df),
  censored_col = censored_var(df),
  keep_data_object = FALSE,
  ...
) {
  .compare_groups_core(
    df = df,
    compare = compare,
    result_col = result_col,
    censored_col = censored_col,
    method = "anova",
    keep_data_object = keep_data_object
  )
}

#' @rdname compare_groups
#'
#' @section Welch Method:
#'
#' `welch_test()` performs Welch's unequal-variance analysis of
#' variance using [stats::oneway.test()].
#'
#' Welch's ANOVA relaxes the equal-variance assumption required by
#' classical ANOVA and is provided as an explicit analyst override.
#'
#' This method is not selected automatically by
#' [compare_groups()].
#'
#' @export
welch_test <- function(df, ...) {
  UseMethod("welch_test")
}

#' @rdname compare_groups
#'
#' @export
welch_test.data.frame <- function(
  df,
  location = "location_id",
  analyte = "param_name",
  sample_date = "sample_date",
  result = "analysis_result",
  unit = "default_unit",
  compare = location,
  keep_data_object = FALSE,
  ...
) {
  df <- env_schema(
    df,
    location = !!rlang::sym(location),
    analyte = !!rlang::sym(analyte),
    sample_date = !!rlang::sym(sample_date),
    result = !!rlang::sym(result),
    unit = !!rlang::sym(unit)
  )

  result_col <- analysis_result_var(df)
  censored_col <- censored_var(df)

  .compare_groups_core(
    df = df,
    compare = compare,
    result_col = result_col,
    censored_col = censored_col,
    method = "welch",
    keep_data_object = keep_data_object
  )
}

#' @rdname compare_groups
#'
#' @export
welch_test.env_schema <- function(
  df,
  compare = location_var(df),
  result_col = analysis_result_var(df),
  censored_col = censored_var(df),
  keep_data_object = FALSE,
  ...
) {
  .compare_groups_core(
    df = df,
    compare = compare,
    result_col = result_col,
    censored_col = censored_col,
    method = "welch",
    keep_data_object = keep_data_object
  )
}

#' @rdname compare_groups
#'
#' @section Kruskal-Wallis Method:
#'
#' `kruskal_test()` performs a Kruskal-Wallis rank-sum test using
#' [stats::kruskal.test()].
#'
#' This nonparametric method is appropriate when normality or
#' variance assumptions are not satisfied and is commonly
#' recommended for datasets with moderate or extensive censoring.
#'
#' This function bypasses automatic method selection and always
#' performs a Kruskal-Wallis comparison.
#'
#' @export
kruskal_test <- function(df, ...) {
  UseMethod("kruskal_test")
}

#' @rdname compare_groups
#'
#' @export
kruskal_test.data.frame <- function(
  df,
  location = "location_id",
  analyte = "param_name",
  sample_date = "sample_date",
  result = "analysis_result",
  unit = "default_unit",
  compare = location,
  keep_data_object = FALSE,
  ...
) {
  df <- env_schema(
    df,
    location = !!rlang::sym(location),
    analyte = !!rlang::sym(analyte),
    sample_date = !!rlang::sym(sample_date),
    result = !!rlang::sym(result),
    unit = !!rlang::sym(unit)
  )

  result_col <- analysis_result_var(df)
  censored_col <- censored_var(df)

  .compare_groups_core(
    df = df,
    compare = compare,
    result_col = result_col,
    censored_col = censored_col,
    method = "kruskal",
    keep_data_object = keep_data_object
  )
}

#' @rdname compare_groups
#'
#' @export
kruskal_test.env_schema <- function(
  df,
  compare = location_var(df),
  result_col = analysis_result_var(df),
  censored_col = censored_var(df),
  keep_data_object = FALSE,
  ...
) {
  .compare_groups_core(
    df = df,
    compare = compare,
    result_col = result_col,
    censored_col = censored_col,
    method = "kruskal",
    keep_data_object = keep_data_object
  )
}

#' Extract a test statistic and p-value
#'
#' Supports standard `aov` and `htest` objects, as well as censored-test
#' result objects returned by NADA/NADA2.
#'
#' @keywords internal
.extract_test_results <- function(test) {
  missing_result <- list(
    statistic = NA_real_,
    p_value = NA_real_
  )

  if (is.null(test)) {
    return(missing_result)
  }

  if (inherits(test, "aov")) {
    table <- summary(test)[[1L]]

    return(list(
      statistic = .as_scalar_numeric(table[["F value"]][1L]),
      p_value = .as_scalar_numeric(table[["Pr(>F)"]][1L])
    ))
  }

  if (inherits(test, "htest")) {
    return(list(
      statistic = .as_scalar_numeric(test$statistic),
      p_value = .as_scalar_numeric(test$p.value)
    ))
  }

  # NADA/NADA2 result objects use package-specific field names.
  statistic <- test$statistic %||%
    test$test.statistic %||%
    test$F.statistic

  p_value <- test$p.value %||%
    test$p %||%
    test$P.Value

  list(
    statistic = .as_scalar_numeric(statistic),
    p_value = .as_scalar_numeric(p_value)
  )
}

#' @keywords internal
.extract_comparison_results <- function(
  x,
  keep_data_object = FALSE
) {
  out <- dplyr::ungroup(x)

  extracted <- purrr::map(
    out$test,
    .extract_test_results
  )

  out$statistic <- purrr::map_dbl(
    extracted,
    ~ .x$statistic
  )

  out$p_value <- purrr::map_dbl(
    extracted,
    ~ .x$p_value
  )

  if (!keep_data_object) {
    out <- dplyr::select(
      out,
      -dplyr::any_of(c("data", "test", "decision"))
    )
  }

  out
}

#' Run NADA2 censored ANOVA
#'
#' @keywords internal
.run_cenanova <- function(
  values,
  censored,
  groups,
  log_transform = TRUE,
  printstat = FALSE
) {
  if (!is.numeric(values)) {
    rlang::abort("`values` must be numeric.")
  }

  if (!is.logical(censored)) {
    rlang::abort("`censored` must be logical.")
  }

  if (
    length(values) != length(censored) ||
      length(values) != length(groups)
  ) {
    rlang::abort(
      "`values`, `censored`, and `groups` must have equal lengths."
    )
  }

  keep <- !is.na(values) &
    !is.na(censored) &
    !is.na(groups)

  values <- values[keep]
  censored <- censored[keep]
  groups <- droplevels(factor(groups[keep]))

  if (nlevels(groups) < 2L) {
    return(NULL)
  }

  # NADA2::cenanova() should not be called for small samples.
  if (length(values) < 20L || length(values) > 5000L) {
    return(NULL)
  }

  # NADA2 cannot fit a censored model when there are no censored
  # observations.
  if (!any(censored)) {
    return(NULL)
  }

  if (sum(!censored) < 4L) {
    return(NULL)
  }

  group_counts <- table(groups)

  if (any(group_counts < 2L)) {
    return(NULL)
  }

  detected_by_group <- tapply(
    !censored,
    groups,
    sum
  )

  if (any(detected_by_group < 1L)) {
    return(NULL)
  }

  result <- NULL

  # NADA2 has a bug with printstat = FALSE: some versions reference
  # an internal object that is only created by the printing branch.
  # Capture the output while always using printstat = TRUE.
  utils::capture.output(
    result <- NADA2::cenanova(
      x1 = values,
      x2 = censored,
      group = groups,
      LOG = isTRUE(log_transform),
      printstat = TRUE
    )
  )

  result
}

#' @keywords internal
.extract_censored_test_results <- function(test) {
  if (is.null(test)) {
    return(
      list(
        statistic = NA_real_,
        p_value = NA_real_
      )
    )
  }

  statistic <- test$statistic %||%
    test$test.statistic %||%
    test$F.statistic

  p_value <- test$p.value %||%
    test$p %||%
    test$P.Value

  list(
    statistic = .as_scalar_numeric(statistic),
    p_value = .as_scalar_numeric(p_value)
  )
}

#' @keywords internal
.as_scalar_numeric <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_real_)
  }

  value <- suppressWarnings(as.numeric(x[[1L]]))

  if (length(value) == 0L || is.na(value)) {
    NA_real_
  } else {
    unname(value[[1L]])
  }
}

#' Levene's Test for Homogeneity of Variance
#'
#' @param df A data frame, grouped data frame, or `env_schema` object.
#' @param result_col Name of the numeric analytical-result column.
#' @param group_col Optional grouping column or columns. If omitted for a
#'   grouped data frame, the columns created by `dplyr::group_by()` are used.
#' @param center Centering method. `"median"` performs the Brown-Forsythe
#'   version of Levene's test and is generally more robust.
#'
#' @returns A tibble containing the Levene test results.
#'
#' @export
levene_test <- function(
  df,
  result_col = NULL,
  group_col = NULL,
  center = "median",
  alpha = 0.05
) {
  UseMethod("levene_test")
}

#' @export
levene_test.data.frame <- function(
  df,
  result_col = NULL,
  group_col = NULL,
  center = "median",
  alpha = 0.05
) {
  columns <- .resolve_levene_columns(
    df = df,
    result_col = result_col,
    group_col = group_col
  )

  .levene_test_core(
    df = df,
    result_col = columns$result_col,
    group_col = columns$group_col,
    center = center,
    alpha = alpha
  )
}

#' @export
levene_test.grouped_df <- function(
  df,
  result_col = NULL,
  group_col = NULL,
  center = "median",
  alpha = 0.05
) {
  columns <- .resolve_levene_columns(
    df = df,
    result_col = result_col,
    group_col = group_col
  )

  # group_vars() are metadata for the test and should not be passed
  # as grouping structure to downstream operations.
  df <- dplyr::ungroup(df)

  .levene_test_core(
    df = df,
    result_col = columns$result_col,
    group_col = columns$group_col,
    center = center,
    alpha = alpha
  )
}

#' @export
levene_test.env_schema <- function(
  df,
  result_col = NULL,
  group_col = NULL,
  center = "median"
) {
  columns <- .resolve_levene_columns(
    df = df,
    result_col = result_col,
    group_col = group_col
  )

  .levene_test_core(
    df = dplyr::ungroup(df),
    result_col = columns$result_col,
    group_col = columns$group_col,
    center = center,
    alpha = alpha
  )
}

#' @export
levene_test.grouped_env_schema <- function(
  df,
  result_col = NULL,
  group_col = NULL,
  center = "median"
) {
  levene_test.env_schema(
    df = df,
    result_col = result_col,
    group_col = group_col,
    center = center,
    alpha = alpha
  )
}

#' @keywords internal
.resolve_levene_columns <- function(
  df,
  result_col = NULL,
  group_col = NULL
) {
  if (is.null(result_col)) {
    result_col <- if (inherits(df, "env_schema")) {
      analysis_result_var(df)
    } else {
      "analysis_result"
    }
  }

  if (is.null(group_col)) {
    if (!dplyr::is_grouped_df(df)) {
      rlang::abort(
        "`group_col` must be supplied when `df` is not grouped."
      )
    }

    group_col <- dplyr::group_vars(df)
  }

  if (length(group_col) == 0L) {
    rlang::abort(
      "At least one grouping column is required for Levene's test."
    )
  }

  required_cols <- c(result_col, group_col)
  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0L) {
    rlang::abort(
      paste0(
        "Missing required columns: ",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  list(
    result_col = result_col,
    group_col = group_col
  )
}

#' @keywords internal
.levene_test_core <- function(
  df,
  result_col,
  group_col,
  center = c("median", "mean"),
  alpha = 0.05
) {
  center <- match.arg(center)

  if (
    !is.numeric(alpha) ||
      length(alpha) != 1L ||
      is.na(alpha) ||
      alpha <= 0 ||
      alpha >= 1
  ) {
    rlang::abort("`alpha` must be between 0 and 1.")
  }

  values <- df[[result_col]]

  # Use all grouping columns together when multiple grouping variables
  # were supplied to group_by().
  groups <- interaction(
    df[group_col],
    drop = TRUE,
    lex.order = TRUE,
    sep = " : "
  )

  keep <- !is.na(values) & !is.na(groups)

  values <- values[keep]
  groups <- droplevels(groups[keep])

  if (length(values) == 0L) {
    return(tibble::tibble(
      levene_statistic = NA_real_,
      levene_p_value = NA_real_,
      levene_df_between = NA_integer_,
      levene_df_within = NA_integer_,
      levene_n = 0L,
      levene_n_groups = 0L,
      unequal_variance = NA
    ))
  }

  if (nlevels(groups) < 2L) {
    return(tibble::tibble(
      levene_statistic = NA_real_,
      levene_p_value = NA_real_,
      levene_df_between = NA_integer_,
      levene_df_within = NA_integer_,
      levene_n = length(values),
      levene_n_groups = nlevels(groups),
      unequal_variance = NA
    ))
  }

  group_sizes <- table(groups)

  if (any(group_sizes < 2L)) {
    rlang::abort(
      "Each group must contain at least two non-missing observations."
    )
  }

  centers <- tapply(
    values,
    groups,
    switch(
      center,
      median = stats::median,
      mean = mean
    )
  )

  absolute_deviations <- abs(
    values - centers[as.character(groups)]
  )

  levene_fit <- stats::lm(
    absolute_deviations ~ groups
  )

  anova_result <- stats::anova(levene_fit)

  statistic <- unname(anova_result$`F value`[[1L]])
  p_value <- unname(anova_result$`Pr(>F)`[[1L]])

  tibble::tibble(
    levene_statistic = statistic,
    levene_p_value = p_value,
    levene_df_between = as.integer(anova_result$Df[[1L]]),
    levene_df_within = as.integer(anova_result$Df[[2L]]),
    levene_n = length(values),
    levene_n_groups = nlevels(groups),
    unequal_variance = p_value < alpha
  )
}

#' @keywords internal
.compute_levene_test <- function(
  values,
  groups,
  center = c("median", "mean"),
  alpha = 0.05
) {
  center <- match.arg(center)

  if (!is.numeric(values)) {
    rlang::abort("`values` must be numeric.")
  }

  if (length(values) != length(groups)) {
    rlang::abort("`values` and `groups` must have equal lengths.")
  }

  keep <- !is.na(values) & !is.na(groups)

  values <- values[keep]
  groups <- droplevels(as.factor(groups[keep]))

  if (length(values) == 0L || nlevels(groups) < 2L) {
    return(tibble::tibble(
      levene_statistic = NA_real_,
      levene_p_value = NA_real_,
      levene_df_between = NA_integer_,
      levene_df_within = NA_integer_,
      levene_n = length(values),
      levene_n_groups = nlevels(groups),
      unequal_variance = NA
    ))
  }

  group_sizes <- table(groups)

  if (any(group_sizes < 2L)) {
    return(tibble::tibble(
      levene_statistic = NA_real_,
      levene_p_value = NA_real_,
      levene_df_between = NA_integer_,
      levene_df_within = NA_integer_,
      levene_n = length(values),
      levene_n_groups = nlevels(groups),
      unequal_variance = NA
    ))
  }

  centers <- tapply(
    values,
    groups,
    switch(
      center,
      median = stats::median,
      mean = mean
    )
  )

  absolute_deviations <- abs(
    values - centers[as.character(groups)]
  )

  fit <- stats::lm(
    absolute_deviations ~ groups
  )

  result <- stats::anova(fit)
  p_value <- unname(result$`Pr(>F)`[[1L]])

  tibble::tibble(
    levene_statistic = unname(result$`F value`[[1L]]),
    levene_p_value = p_value,
    levene_df_between = as.integer(result$Df[[1L]]),
    levene_df_within = as.integer(result$Df[[2L]]),
    levene_n = length(values),
    levene_n_groups = nlevels(groups),
    unequal_variance = p_value < alpha
  )
}

#' @keywords internal
.run_censored_comparison_test <- function(
  df,
  method,
  compare,
  result_col,
  censored_col,
  log_transform = TRUE,
  printstat = FALSE
) {
  required_cols <- c(
    compare,
    result_col,
    censored_col
  )

  missing_cols <- setdiff(
    required_cols,
    names(df)
  )

  if (length(missing_cols) > 0L) {
    rlang::abort(
      paste0(
        "Missing required columns: ",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  values <- df[[result_col]]
  censored <- df[[censored_col]]
  groups <- df[[compare]]

  if (!is.numeric(values)) {
    rlang::abort(
      sprintf("`%s` must be numeric.", result_col)
    )
  }

  if (!is.logical(censored)) {
    rlang::abort(
      sprintf("`%s` must be logical.", censored_col)
    )
  }

  keep <- !is.na(values) &
    !is.na(censored) &
    !is.na(groups)

  values <- values[keep]
  censored <- censored[keep]
  groups <- droplevels(factor(groups[keep]))

  n_groups <- nlevels(groups)

  if (n_groups < 2L) {
    return(NULL)
  }

  # NADA2 expects TRUE for censored observations and FALSE for detected
  if (anyNA(censored)) {
    rlang::abort(
      "The censoring indicator cannot contain missing values."
    )
  }

  group_counts <- table(groups)
  detected_by_group <- tapply(
    !censored,
    groups,
    sum
  )

  # Require at least two observations per comparison group.
  if (any(group_counts < 2L)) {
    return(NULL)
  }

  # Require at least one detected observation in each group.
  # This prevents groups composed entirely of censored values
  # from being treated as ordinary comparison groups.
  if (any(detected_by_group < 1L)) {
    return(NULL)
  }

  if (sum(!censored) < 4L) {
    return(NULL)
  }

  if (identical(method, "censored_nonparametric")) {
    method <- if (n_groups == 2L) {
      "cenperm2"
    } else {
      "cenanova"
    }
  }

  switch(
    method,

    cenperm2 = {
      if (n_groups != 2L) {
        rlang::abort(
          "`cenperm2` requires exactly two groups."
        )
      }

      NADA2::cenperm2(
        y1 = values,
        y2 = censored,
        grp = groups,
        printstat = printstat
      )
    },

    cenanova = .run_cenanova(
      values = values,
      censored = censored,
      groups = groups,
      log_transform = log_transform,
      printstat = printstat
    ),

    rlang::abort(
      sprintf(
        "Unsupported censored comparison method: `%s`.",
        method
      )
    )
  )
}

#' @keywords internal
.est_dist_group <- function(
  df,
  return_result = FALSE,
  distribution_config,
  result_col
) {
  x <- stats::na.omit(
    df[[result_col]]
  )

  n <- length(x)

  fit_result <- purrr::possibly(
    .fit_dist,
    otherwise = NULL
  )(
    x = x,
    distribution_config = distribution_config
  )

  result <- .handle_dist_result(
    result = fit_result,
    n = n,
    return_result = return_result
  )

  .attach_nested_data(
    result = result,
    data = df
  )
}

#' @keywords internal
.est_dist_worker <- function(
  df,
  return_result,
  distribution_config,
  result_col
) {
  df |>
    dplyr::group_modify(
      ~ .est_dist_group(
        df = .x,
        return_result = return_result,
        distribution_config = distribution_config,
        result_col = result_col
      )
    )
}

#' @keywords internal
.est_dist_censored_group <- function(
  df,
  return_result = FALSE,
  distribution_config,
  result_col,
  censored_col
) {
  x <- df[[result_col]]

  censored <- df[[censored_col]]

  keep <- !is.na(x)

  x <- x[keep]
  censored <- censored[keep]

  n <- length(x)

  if (any(is.na(censored))) {
    rlang::abort(
      ".censored cannot contain missing values."
    )
  }

  fit_result <- if (!any(censored)) {
    purrr::possibly(
      .fit_dist,
      otherwise = NULL
    )(
      x = x,
      distribution_config = distribution_config
    )
  } else {
    purrr::possibly(
      .fit_dist_censored,
      otherwise = NULL
    )(
      x = x,
      censored = censored,
      distribution_config = distribution_config
    )
  }

  result <- .handle_dist_result(
    result = fit_result,
    n = n,
    return_result = return_result
  )

  .attach_nested_data(
    result = result,
    data = df
  )
}

#' @keywords internal
.est_dist_censored_worker <- function(
  df,
  return_result,
  distribution_config,
  result_col,
  censored_col
) {
  df |>
    dplyr::group_modify(
      ~ .est_dist_censored_group(
        df = .x,
        return_result = return_result,
        distribution_config = distribution_config,
        result_col = result_col,
        censored_col = censored_col
      )
    )
}

#' Fit candidate distributions
#'
#' Internal wrapper around EnvStats::distChoose().
#'
#' @inheritParams est_dist
#' @param x Numeric vector.
#'
#' @return An EnvStats distChoose object.
#'
#' @keywords internal
#' @noRd
.fit_dist <- function(
  x,
  distribution_config
) {
  do.call(
    EnvStats::distChoose,
    c(
      list(y = x),
      distribution_config
    )
  )
}

#' Fit candidate distributions to censored data
#'
#' Internal wrapper around EnvStats::distChooseCensored().
#'
#' @param x Numeric vector.
#' @param censored Logical censoring indicator.
#' @param alpha Significance level.
#' @param method Goodness-of-fit test method.
#' @param choices Candidate distributions.
#'
#' @return An EnvStats distChooseCensored object.
#'
#' @keywords internal
#'
#' @noRd
.fit_dist_censored <- function(
  x,
  censored,
  distribution_config
) {
  do.call(
    EnvStats::distChooseCensored,
    c(
      list(
        x = x,
        censored = censored,
        censoring.side = "left"
      ),
      distribution_config
    )
  )
}

#' Construct distribution estimation result
#'
#' Internal helper used by est_dist() and est_dist_censored().
#'
#' @param distribution Selected distribution.
#' @param n Number of observations used in estimation.
#' @param result EnvStats result object.
#' @param return_result Logical indicating whether to include the
#' result object in the output.
#'
#' @return A tibble with distribution metadata.
#'
#' @keywords internal
#'
#' @noRd
.make_dist_result <- function(
  distribution,
  n_used,
  result = NULL,
  return_result = FALSE
) {
  out <- tibble::tibble(
    distribution = distribution,
    n_used = n_used
  )

  if (return_result) {
    out$result <- list(result)
  }

  out
}

#' Helper function for est_dist() and est_dist_censored()
#'
#' @keywords internal
#'
#' @noRd
.handle_dist_result <- function(
  result,
  n,
  return_result
) {
  if (is.null(result)) {
    return(
      .make_dist_result(
        "estimation_failed",
        n_used = n,
        return_result = return_result
      )
    )
  }

  .make_dist_result(
    distribution = paste(
      result$decision,
      collapse = ", "
    ),
    n_used = n,
    result = result,
    return_result = return_result
  )
}

#' @keywords internal
#' @noRd
.pred_int_call <- function(
  values,
  distribution,
  simultaneous = FALSE,
  method = "Bonferroni",
  n_mean = 1,
  k = 1,
  m = 2,
  r = 1,
  rule = "k.of.m",
  pi_type = "upper",
  conf_level = 0.95
) {
  if (!is.numeric(values)) {
    rlang::abort("Prediction-interval values must be numeric.")
  }

  if (simultaneous) {
    rule <- match.arg(
      rule,
      c("k.of.m", "CA", "Modified.CA")
    )

    return(
      switch(
        distribution,

        Normal = EnvStats::predIntNormSimultaneous(
          x = values,
          n.mean = n_mean,
          k = k,
          m = m,
          r = r,
          rule = rule,
          pi.type = pi_type,
          conf.level = conf_level
        ),

        Lognormal = EnvStats::predIntLnormSimultaneous(
          x = values,
          n.geomean = n_mean,
          k = k,
          m = m,
          r = r,
          rule = rule,
          pi.type = pi_type,
          conf.level = conf_level
        ),

        Nonparametric = EnvStats::predIntNparSimultaneous(
          x = values,
          n.median = n_mean,
          k = k,
          m = m,
          r = r,
          rule = rule,
          pi.type = pi_type
        ),

        rlang::abort(
          sprintf(
            "Unsupported prediction-interval distribution: `%s`.",
            distribution
          )
        )
      )
    )
  }

  switch(
    distribution,

    Normal = EnvStats::predIntNorm(
      x = values,
      k = k,
      pi.type = pi_type,
      conf.level = conf_level,
      method = method,
      n.mean = n_mean
    ),

    Lognormal = EnvStats::predIntLnorm(
      x = values,
      k = k,
      pi.type = pi_type,
      conf.level = conf_level,
      method = method,
      n.geomean = n_mean
    ),

    Nonparametric = EnvStats::predIntNpar(
      x = values,
      k = k,
      pi.type = pi_type
    ),

    rlang::abort(
      sprintf(
        "Unsupported prediction-interval distribution: `%s`.",
        distribution
      )
    )
  )
}

#' Compute prediction intervals
#'
#' @keywords internal
#' @noRd
.compute_pred_int <- function(
  df,
  simultaneous = FALSE,
  method = "Bonferroni",
  n_mean = 1,
  k = 1,
  m = 1,
  r = 1,
  rule = "k.of.m",
  pi_type = "upper",
  conf_level = 0.95
) {
  .valid_pred_int_rules <- c(
    "k.of.m",
    "CA",
    "Modified.CA"
  )

  if (simultaneous) {
    rule <- match.arg(
      rule,
      choices = .valid_pred_int_rules
    )
  }

  .validate_interval_distribution(df$distribution)
  .validate_est_dist_output(df)

  rlang::arg_match0(
    pi_type,
    c("upper", "lower", "two-sided")
  )

  .validate_positive_integer(n_mean, "n_mean")
  .validate_positive_integer(k, "k")

  if (simultaneous) {
    .validate_positive_integer(m, "m")
    .validate_positive_integer(r, "r")

    if (
      length(rule) != 1L ||
        !is.character(rule) ||
        is.na(rule)
    ) {
      rlang::abort("`rule` must be a non-missing character scalar.")
    }
  }

  .validate_probability(conf_level, "conf_level")

  pred_int <- purrr::map2(
    df$data,
    df$distribution,
    function(data, distribution) {
      .pred_int_call(
        values = data$.analysis_result,
        distribution = distribution,
        simultaneous = simultaneous,
        method = method,
        n_mean = n_mean,
        k = k,
        m = m,
        r = r,
        rule = rule,
        pi_type = pi_type,
        conf_level = conf_level
      )
    }
  )

  df |>
    dplyr::mutate(pred_int = pred_int) |>
    dplyr::mutate(
      sample_size = purrr::map_dbl(
        pred_int,
        "sample.size"
      ),
      conf_level = purrr::map_dbl(
        pred_int,
        ~ .x$interval$conf.level
      ),
      lpl = purrr::map_dbl(
        pred_int,
        ~ unname(.x$interval$limits["LPL"])
      ),
      upl = purrr::map_dbl(
        pred_int,
        ~ unname(.x$interval$limits["UPL"])
      )
    ) |>
    dplyr::select(
      -data,
      -pred_int
    )
}

#' Compute one confidence interval
#'
#' @keywords internal
#' @noRd
.compute_conf_int_one <- function(
  values,
  distribution,
  censored = NULL,
  method,
  ci_type,
  ci_param,
  conf_level,
  nonparametric_args,
  censored_args
) {
  has_censored <- !is.null(censored) && any(censored)

  if (has_censored && distribution == "Nonparametric") {
    rlang::abort(
      paste(
        "Censored nonparametric confidence intervals are not",
        "provided by this dispatcher."
      )
    )
  }

  if (has_censored) {
    function_name <- switch(
      distribution,
      Normal = "enormCensored",
      Lognormal = "elnormCensored",
      rlang::abort(
        sprintf(
          "Censored confidence intervals are not supported for `%s`.",
          distribution
        )
      )
    )

    if (
      !exists(
        function_name,
        envir = asNamespace("EnvStats"),
        inherits = FALSE
      )
    ) {
      rlang::abort(
        sprintf(
          "`EnvStats::%s()` is not available in the installed EnvStats version.",
          function_name
        )
      )
    }

    args <- c(
      list(
        x = values,
        censored = censored,
        method = method,
        ci = TRUE,
        ci.type = ci_type,
        ci.param = ci_param,
        conf.level = conf_level
      ),
      censored_args
    )

    return(
      do.call(
        getExportedValue("EnvStats", function_name),
        args
      )
    )
  }

  switch(
    distribution,

    Normal = EnvStats::enorm(
      x = values,
      method = method,
      ci = TRUE,
      ci.type = ci_type,
      ci.param = ci_param,
      conf.level = conf_level
    ),

    Lognormal = EnvStats::elnorm(
      x = values,
      method = method,
      ci = TRUE,
      ci.method = "exact",
      ci.type = ci_type,
      ci.param = ci_param,
      conf.level = conf_level
    ),

    Nonparametric = do.call(
      EnvStats::eqnpar,
      c(
        list(
          x = values,
          approx.conf.level = conf_level
        ),
        nonparametric_args
      )
    ),

    rlang::abort(
      sprintf(
        "Unsupported confidence-interval distribution: `%s`.",
        distribution
      )
    )
  )
}

#' Compute confidence intervals
#'
#' @keywords internal
#' @noRd
.compute_conf_int <- function(
  df,
  method = "mvue",
  ci_type = "lower",
  ci_param = "mean",
  conf_level = 0.95,
  nonparametric_args = list(
    p = 0.5,
    type = 7,
    lcl.rank = NULL,
    ucl.rank = NULL,
    lb = -Inf,
    ub = Inf,
    ci.method = "interpolate",
    min.coverage = TRUE,
    tol = 0
  ),
  censored_col = NULL,
  censored_args = list(
    censoring.side = "left"
  ),
  analysis_result = ".analysis_result"
) {
  .validate_est_dist_output(df)

  if (!distribution %in% names(df)) {
    rlang::abort("The input must contain a `distribution` column.")
  }

  if (
    !is.character(analysis_result) ||
      length(analysis_result) != 1L
  ) {
    rlang::abort("`analysis_result` must be a column name.")
  }

  .validate_conf_int_args(
    conf_level = conf_level,
    ci_type = ci_type,
    ci_param = ci_param,
    nonparametric_args = nonparametric_args,
    censored_args = censored_args
  )

  if (
    !is.null(censored_col) &&
      !all(vapply(
        df$data,
        function(x) censored_col %in% names(x),
        logical(1)
      ))
  ) {
    rlang::abort(
      sprintf(
        "Column `%s` was not found in every nested data frame.",
        censored_col
      )
    )
  }

  conf_int <- purrr::map2(
    df$data,
    df$distribution,
    function(data, selected_distribution) {
      values <- data[[analysis_result]]

      censored <- if (is.null(censored_col)) {
        NULL
      } else {
        data[[censored_col]]
      }

      keep <- !is.na(values)

      values <- values[keep]

      if (!is.null(censored)) {
        censored <- censored[keep]

        if (!is.logical(censored)) {
          rlang::abort(
            sprintf(
              "`%s` must be logical.",
              censored_col
            )
          )
        }

        if (anyNA(censored)) {
          rlang::abort(
            sprintf(
              "`%s` cannot contain missing values.",
              censored_col
            )
          )
        }
      }

      .compute_conf_int_one(
        values = values,
        distribution = selected_distribution,
        censored = censored,
        method = method,
        ci_type = ci_type,
        ci_param = ci_param,
        conf_level = conf_level,
        nonparametric_args = nonparametric_args,
        censored_args = censored_args
      )
    }
  )

  df |>
    dplyr::mutate(
      .conf_int = conf_int,
      sample_size = purrr::map_dbl(
        .conf_int,
        ~ .x$sample.size
      ),
      conf_level = purrr::map_dbl(
        .conf_int,
        ~ .x$interval$conf.level
      ),
      lcl = purrr::map_dbl(
        .conf_int,
        ~ unname(.x$interval$limits["LCL"])
      ),
      ucl = purrr::map_dbl(
        .conf_int,
        ~ unname(.x$interval$limits["UCL"])
      )
    ) |>
    dplyr::select(
      -data,
      -.conf_int
    )
}

#' Calculate confidence intervals
#'
#' Calculates confidence intervals for each distribution-estimation result in
#' `df`. Normal, Lognormal, and Nonparametric distributions are dispatched to
#' the corresponding EnvStats estimation function. Censored Normal and
#' Lognormal data are dispatched to the corresponding censored-data method
#' when `censored_col` identifies censored observations.
#'
#' The input must be generated by [est_dist()] with
#' `keep_data_object = TRUE`.
#'
#' @param df A data frame returned by [est_dist()] with
#'   `keep_data_object = TRUE`.
#' @param method Character string specifying the estimation method passed to
#'   EnvStats.
#' @param ci_type Character string specifying the confidence-interval type.
#'   Supported values are `"two-sided"`, `"upper"`, and `"lower"`.
#' @param ci_param Character string specifying the parameter for which the
#'   interval is calculated, such as `"mean"`, `"median"`, `"quantile"`, or
#'   `"geometric.mean"`, depending on the selected EnvStats function.
#' @param conf_level A scalar between 0 and 1 specifying the confidence level.
#' @param analysis_result Character scalar giving the name of the result column
#'   inside each nested data frame.
#' @param censored_col Optional character scalar giving the name of a logical
#'   censoring-indicator column inside each nested data frame. `TRUE` identifies
#'   a censored observation.
#' @param censored_args Named list of additional arguments passed to the
#'   censored EnvStats function. For left-censored groundwater data, use
#'   `list(censoring.side = "left")`.
#' @param nonparametric_args Named list of additional arguments passed to
#'   [EnvStats::eqnpar()].
#'
#' @return A data frame containing the grouping columns from `df`, the
#'   estimated distribution, sample size, confidence level, and confidence
#'   limits:
#'   \itemize{
#'     \item `lcl`: lower confidence limit;
#'     \item `ucl`: upper confidence limit;
#'     \item `sample_size`: number of observations used;
#'     \item `conf_level`: confidence level.
#'   }
#'
#' @seealso [est_dist()], [tol_int()],
#'   [EnvStats::enorm()], [EnvStats::elnorm()], [EnvStats::eqnpar()]
#'
#' @examples
#' \donttest{
#' data("gw_data")
#'
#' background_data <- gw_data |>
#'   dplyr::filter(
#'     location_id %in% c("MW-1", "MW-2", "MW-3", "MW-4"),
#'     param_name %in% c(
#'       "Sulfate, total",
#'       "Arsenic, dissolved",
#'       "Boron, dissolved"
#'     ),
#'     sample_date >= lubridate::ymd("2007-12-20"),
#'     sample_date <= lubridate::ymd("2012-01-01")
#'   ) |>
#'   dplyr::group_by(
#'     location_id,
#'     param_name,
#'     default_unit
#'   ) |>
#'   percent_lt() |>
#'   est_dist(keep_data_object = TRUE)
#'
#' conf_int(
#'   background_data,
#'   ci_type = "upper",
#'   ci_param = "mean",
#'   conf_level = 0.99
#' )
#' }
#'
#' @export
conf_int <- function(
  df,
  method = "mvue",
  ci_type = "lower",
  ci_param = "mean",
  conf_level = 0.95,
  analysis_result = ".analysis_result",
  censored_col = NULL,
  censored_args = list(
    censoring.side = "left"
  ),
  nonparametric_args = list(
    p = 0.5,
    type = 7,
    lcl.rank = NULL,
    ucl.rank = NULL,
    lb = -Inf,
    ub = Inf,
    ci.method = "interpolate",
    min.coverage = TRUE,
    tol = 0
  )
) {
  .validate_interval_distribution(df$distribution)

  .compute_conf_int(
    df = df,
    method = method,
    ci_type = ci_type,
    ci_param = ci_param,
    conf_level = conf_level,
    analysis_result = analysis_result,
    censored_col = censored_col,
    censored_args = censored_args,
    nonparametric_args = nonparametric_args
  )
}

#' Calculate prediction intervals
#'
#' Calculates prediction intervals for each distribution-estimation result in
#' `df`. Normal, Lognormal, and Nonparametric distributions are dispatched to
#' the corresponding EnvStats prediction-interval function.
#'
#' The input must be generated by [est_dist()] with
#' `keep_data_object = TRUE`.
#'
#' @param df A data frame returned by [est_dist()] with
#'   `keep_data_object = TRUE`.
#' @param method Character string specifying the method used to calculate a
#'   parametric prediction interval. The value is passed to
#'   [EnvStats::predIntNorm()] or [EnvStats::predIntLnorm()].
#' @param n_mean Positive integer specifying the number of observations used to
#'   calculate each future mean or geometric mean. The default, `1`, calculates
#'   an interval for individual future observations.
#' @param k Positive integer specifying the number of future observations or
#'   means the interval should contain.
#' @param pi_type Character string specifying the interval type. Supported
#'   values are `"two-sided"`, `"upper"`, and `"lower"`.
#' @param conf_level A scalar between 0 and 1 specifying the confidence level.
#'
#' @return A data frame containing the grouping columns from `df`, the
#'   estimated distribution, sample size, confidence level, and prediction
#'   limits:
#'   \itemize{
#'     \item `lpl`: lower prediction limit;
#'     \item `upl`: upper prediction limit;
#'     \item `sample_size`: number of observations used;
#'     \item `conf_level`: confidence level.
#'   }
#'
#' @seealso [pred_int_sim()], [est_dist()],
#'   [EnvStats::predIntNorm()], [EnvStats::predIntLnorm()],
#'   [EnvStats::predIntNpar()]
#'
#' @examples
#' \donttest{
#' data("gw_data")
#'
#' wells <- c("MW-1", "MW-2", "MW-3", "MW-4")
#' params <- c(
#'   "Sulfate, total",
#'   "Arsenic, dissolved",
#'   "Boron, dissolved"
#' )
#'
#' background <- lubridate::ymd(
#'   c("2007-12-20", "2012-01-01"),
#'   tz = "UTC"
#' )
#'
#' background_data <- gw_data |>
#'   dplyr::filter(
#'     location_id %in% wells,
#'     param_name %in% params,
#'     sample_date >= background[1],
#'     sample_date <= background[2]
#'   ) |>
#'   dplyr::group_by(
#'     location_id,
#'     param_name,
#'     default_unit
#'   ) |>
#'   percent_lt() |>
#'   est_dist(keep_data_object = TRUE)
#'
#' pred_int(
#'   background_data,
#'   pi_type = "upper",
#'   conf_level = 0.99
#' )
#' }
#'
#' @export
pred_int <- function(
  df,
  method = "Bonferroni",
  n_mean = 1,
  k = 1,
  pi_type = "upper",
  conf_level = 0.95
) {
  .compute_pred_int(
    df = df,
    simultaneous = FALSE,
    method = method,
    n_mean = n_mean,
    k = k,
    pi_type = pi_type,
    conf_level = conf_level
  )
}

#' Calculate simultaneous prediction intervals
#'
#' Calculates simultaneous prediction intervals for each
#' distribution-estimation result in `df`. The calculation is delegated to the
#' corresponding EnvStats simultaneous prediction-interval function.
#'
#' The input must be generated by [est_dist()] with
#' `keep_data_object = TRUE`.
#'
#' @param df A data frame returned by [est_dist()] with
#'   `keep_data_object = TRUE`.
#' @param n_mean Positive integer specifying the number of observations used to
#'   calculate each future mean or geometric mean.
#' @param k Positive integer specifying the number of future observations or
#'   means in each future set.
#' @param m Positive integer specifying the number of future sets.
#' @param r Positive integer specifying the number of future sets that must be
#'   covered when using an `r.of.m` coverage rule.
#' @param rule Character string specifying the simultaneous-coverage rule
#'   accepted by EnvStats, such as `"all.of.m"` or `"r.of.m"`.
#' @param pi_type Character string specifying the interval type. Supported
#'   values are `"two-sided"`, `"upper"`, and `"lower"`.
#' @param conf_level A scalar between 0 and 1 specifying the confidence level.
#'
#' @return A data frame containing the grouping columns from `df`, the
#'   estimated distribution, sample size, confidence level, and simultaneous
#'   prediction limits.
#'
#' @seealso [pred_int()], [est_dist()],
#'   [EnvStats::predIntNormSimultaneous()],
#'   [EnvStats::predIntLnormSimultaneous()],
#'   [EnvStats::predIntNparSimultaneous()]
#'
#' @examples
#' \donttest{
#' data("gw_data")
#'
#' background_data <- gw_data |>
#'   dplyr::filter(
#'     location_id %in% c("MW-1", "MW-2", "MW-3", "MW-4"),
#'     param_name %in% c(
#'       "Sulfate, total",
#'       "Arsenic, dissolved",
#'       "Boron, dissolved"
#'     ),
#'     sample_date >= lubridate::ymd("2007-12-20"),
#'     sample_date <= lubridate::ymd("2012-01-01")
#'   ) |>
#'   dplyr::group_by(
#'     location_id,
#'     param_name,
#'     default_unit
#'   ) |>
#'   percent_lt() |>
#'   est_dist(keep_data_object = TRUE)
#'
#' pred_int_sim(
#'   background_data,
#'   k = 1,
#'   m = 4,
#'   r = 4,
#'   rule = "r.of.m",
#'   pi_type = "upper",
#'   conf_level = 0.99
#' )
#' }
#'
#' @export
pred_int_sim <- function(
  df,
  n_mean = 1,
  k = 1,
  m = 2,
  r = 1,
  rule = "k.of.m",
  pi_type = "upper",
  conf_level = 0.95
) {
  .compute_pred_int(
    df = df,
    simultaneous = TRUE,
    n_mean = n_mean,
    k = k,
    m = m,
    r = r,
    rule = rule,
    pi_type = pi_type,
    conf_level = conf_level
  )
}

#' Calculate tolerance intervals
#'
#' Calculates tolerance intervals for each distribution-estimation result in
#' `df`. Uncensored observations are dispatched to the corresponding EnvStats
#' tolerance-interval function. When `censored_col` is supplied and contains
#' censored observations, the appropriate censored EnvStats function is used
#' when available.
#'
#' The input must be generated by [est_dist()] with
#' `keep_data_object = TRUE`.
#'
#' @param df A data frame returned by [est_dist()] with
#'   `keep_data_object = TRUE`.
#' @param analysis_result Character scalar giving the name of the result column
#'   inside each nested data frame.
#' @param distribution Character scalar giving the name of the distribution
#'   column in `df`.
#' @param censored_col Optional character scalar giving the name of a logical
#'   censoring-indicator column inside each nested data frame. `TRUE` identifies
#'   a censored observation.
#' @param coverage A scalar between 0 and 1 specifying the desired population
#'   coverage. For `cov_type = "expectation"`, EnvStats may ignore this
#'   argument.
#' @param cov_type Coverage type. Supported values are `"content"` and
#'   `"expectation"`.
#' @param ti_type Tolerance-interval type. Supported values are `"two-sided"`,
#'   `"upper"`, and `"lower"`.
#' @param method Character string specifying the method used for parametric
#'   tolerance intervals.
#' @param conf_level A scalar between 0 and 1 specifying the confidence level
#'   of the tolerance interval.
#' @param censoring_side Character string specifying the censoring direction
#'   passed to EnvStats. Use `"left"` for typical groundwater nondetect data.
#'
#' @return A data frame containing the grouping columns from `df`, the
#'   estimated distribution, sample size, confidence level, and tolerance
#'   limits:
#'   \itemize{
#'     \item `ltl`: lower tolerance limit;
#'     \item `utl`: upper tolerance limit;
#'     \item `sample_size`: number of observations used;
#'     \item `conf_level`: confidence level.
#'   }
#'
#' @seealso [est_dist()], [conf_int()],
#'   [EnvStats::tolIntNorm()], [EnvStats::tolIntLnorm()],
#'   [EnvStats::tolIntNpar()], [EnvStats::tolIntNormCensored()]
#'
#' @examples
#' \donttest{
#' data("gw_data")
#'
#' background_data <- gw_data |>
#'   dplyr::filter(
#'     location_id %in% c("MW-1", "MW-2", "MW-3", "MW-4"),
#'     param_name %in% c(
#'       "Sulfate, total",
#'       "Arsenic, dissolved",
#'       "Boron, dissolved"
#'     ),
#'     sample_date >= lubridate::ymd("2007-12-20"),
#'     sample_date <= lubridate::ymd("2012-01-01")
#'   ) |>
#'   dplyr::group_by(
#'     location_id,
#'     param_name,
#'     default_unit
#'   ) |>
#'   percent_lt() |>
#'   est_dist(keep_data_object = TRUE)
#'
#' tol_int(
#'   background_data,
#'   coverage = 0.95,
#'   cov_type = "content",
#'   ti_type = "upper",
#'   conf_level = 0.99
#' )
#' }
#'
#' @export
tol_int <- function(
  df,
  analysis_result = "analysis_result",
  distribution = "distribution",
  censored_col = NULL,
  coverage = 0.95,
  cov_type = "content",
  ti_type = "two-sided",
  method = "exact",
  conf_level = 0.95,
  censoring_side = "left"
) {
  required_cols <- c(
    analysis_result,
    distribution,
    censored_col
  )

  missing_cols <- setdiff(
    required_cols,
    names(df)
  )

  if (length(missing_cols) > 0L) {
    rlang::abort(
      paste0(
        "Missing required columns: ",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  if (!is.character(df[[distribution]])) {
    rlang::abort(
      sprintf("`%s` must be a character column.", distribution)
    )
  }

  .validate_interval_distribution(
    df[[distribution]]
  )

  if (
    !is.numeric(coverage) ||
      length(coverage) != 1L ||
      is.na(coverage) ||
      coverage <= 0 ||
      coverage >= 1
  ) {
    rlang::abort("`coverage` must be between 0 and 1.")
  }

  if (
    !is.numeric(conf_level) ||
      length(conf_level) != 1L ||
      is.na(conf_level) ||
      conf_level <= 0 ||
      conf_level >= 1
  ) {
    rlang::abort("`conf_level` must be between 0 and 1.")
  }

  cov_type <- rlang::arg_match0(
    cov_type,
    c("content", "expectation")
  )

  ti_type <- rlang::arg_match0(
    ti_type,
    c("two-sided", "upper", "lower")
  )

  if (!is.null(censored_col)) {
    if (!is.logical(df$data[[1L]][[censored_col]])) {
      rlang::abort(
        sprintf("`%s` must be logical.", censored_col)
      )
    }
  }

  tolerance_results <- purrr::map2(
    df$data,
    df[[distribution]],
    function(data, selected_distribution) {
      values <- data[[analysis_result]]

      censored <- if (is.null(censored_col)) {
        NULL
      } else {
        data[[censored_col]]
      }

      keep <- !is.na(values)

      values <- values[keep]

      if (!is.null(censored)) {
        censored <- censored[keep]

        if (anyNA(censored)) {
          rlang::abort(
            sprintf(
              "`%s` cannot contain missing values.",
              censored_col
            )
          )
        }
      }

      .tol_int_call(
        values = values,
        censored = censored,
        distribution = selected_distribution,
        coverage = coverage,
        cov_type = cov_type,
        ti_type = ti_type,
        method = method,
        conf_level = conf_level,
        censoring_side = censoring_side
      )
    }
  )

  df |>
    dplyr::mutate(
      .tol_int = tolerance_results,
      sample_size = purrr::map_dbl(
        .tol_int,
        ~ .x$sample.size
      ),
      ltl = purrr::map_dbl(
        .tol_int,
        ~ unname(.x$interval$limits["LTL"])
      ),
      utl = purrr::map_dbl(
        .tol_int,
        ~ unname(.x$interval$limits["UTL"])
      ),
      conf_level = purrr::map_dbl(
        .tol_int,
        ~ .x$interval$conf.level
      )
    ) |>
    dplyr::select(
      -data,
      -.tol_int
    )
}

#' Dispatch a tolerance-interval calculation
#'
#' @keywords internal
#' @noRd
.tol_int_call <- function(
  values,
  censored = NULL,
  distribution,
  coverage,
  cov_type,
  ti_type,
  method,
  conf_level,
  censoring_side = "left"
) {
  is_censored <- !is.null(censored) && any(censored)

  if (is_censored) {
    args <- list(
      x = values,
      censored = censored,
      censoring.side = censoring_side,
      coverage = coverage,
      cov.type = cov_type,
      ti.type = ti_type,
      conf.level = conf_level
    )

    fun <- switch(
      distribution,

      Normal = {
        args$method <- method
        EnvStats::tolIntNormCensored
      },

      Lognormal = {
        if (
          !exists(
            "tolIntLnormCensored",
            envir = asNamespace("EnvStats"),
            inherits = FALSE
          )
        ) {
          rlang::abort(
            paste0(
              "`EnvStats::tolIntLnormCensored()` is not available ",
              "in the installed version of EnvStats."
            )
          )
        }

        args$method <- method
        EnvStats::tolIntLnormCensored
      },

      Nonparametric = {
        if (
          !exists(
            "tolIntNparCensored",
            envir = asNamespace("EnvStats"),
            inherits = FALSE
          )
        ) {
          rlang::abort(
            paste0(
              "Censored nonparametric tolerance intervals are not ",
              "available in the installed version of EnvStats."
            )
          )
        }

        EnvStats::tolIntNparCensored
      },

      rlang::abort(
        sprintf(
          "Unsupported tolerance-interval distribution: `%s`.",
          distribution
        )
      )
    )
  } else {
    args <- list(
      x = values,
      coverage = coverage,
      cov.type = cov_type,
      ti.type = ti_type,
      conf.level = conf_level
    )

    fun <- switch(
      distribution,

      Normal = {
        args$method <- method
        EnvStats::tolIntNorm
      },

      Lognormal = {
        args$method <- method
        EnvStats::tolIntLnorm
      },

      Nonparametric = EnvStats::tolIntNpar,

      rlang::abort(
        sprintf(
          "Unsupported tolerance-interval distribution: `%s`.",
          distribution
        )
      )
    )
  }

  do.call(fun, args)
}

#' Function to update background data
#'
#' @param df data frame of groundwater data in tidy format
#' @param well the location to update
#' @param param the constituent to update
#' @param start the start date of the background data. IF not provide the
#' start of the data will be found using @seealso [start_date()]
#' @param n_end the number of samples in current background. The date will be
#' found from this sample.
#' @param n_prop the number of samples to include in the proposed background
#'
#' @export

update_background <- function(
  df,
  well,
  param,
  start = NULL,
  n_end = 8,
  n_prop = 4
) {
  tmp <- df |>
    filter_(~ location_id == well, ~ param_name == param)

  if (is.null(start)) {
    start <- tmp |>
      start_date()
  }

  end <- tmp |>
    nth_date(start = start, n = n_end - 1)

  max <- tmp |>
    select_(~sample_date) |>
    first() |>
    max()

  bkgd <- tmp |>
    set_background(start = start, end = end)

  next_date <- tmp |>
    nth_date(start = end, n = n_prop)

  next_bkgd <- tmp |>
    set_proposed_background(start = end, end = next_date)

  outlier <- EnvStats::rosnerTest(bkgd$analysis_result, k = 1, alpha = 0.01)

  trend <- EnvStats::kendallTrendTest(
    bkgd$analysis_result ~ bkgd$sample_date,
    conf.level = 0.99
  )

  two_sample <- EnvStats::twoSampleLinearRankTest(
    bkgd$analysis_result,
    next_bkgd$analysis_result,
    test = "wilcoxon"
  )

  output <- list(
    rosner_test = outlier,
    kendall_trend = trend,
    two_sample = two_sample
  )

  output
}
