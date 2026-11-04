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
  censored_method = "cenken",
  conf.level = 0.95
) {
  env_schema(
    df,
    location = !!rlang::sym(location_id),
    date = !!rlang::sym(sample_date),
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
  method = "mann_kendall",
  censored_method = "cenken",
  conf.level = 0.95
) {
  validate_env_schema(df)

  alpha <- 1 - conf.level

  date_col <- date_var(df)

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
        date_col = date_col,
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
  date_col,
  method,
  censored_method,
  alpha,
  conf.level
) {
  n <- nrow(df)

  date_values <- stats::na.omit(
    df[[date_col]]
  )

  if (length(date_values) > 0) {
    first_date <- min(date_values)

    last_date <- max(date_values)

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
    df$.analysis_result
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
    date_col = date_col,
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
    censored_method = censored_method,
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
      tr$slope > 0 ~ "Increasing",
      tr$slope < 0 ~ "Decreasing",
      TRUE ~ "Stable"
    )
  )
}

#' @keywords internal
.prepare_trend_data <- function(
  df,
  date_col,
  censored_method = "cenken"
) {
  out <- df |>
    dplyr::filter(
      !is.na(.data[[date_col]]),
      !is.na(.analysis_result)
    ) |>
    dplyr::arrange(
      .data[[date_col]]
    )

  if (nrow(out) < 3) {
    return(NULL)
  }

  if (
    dplyr::n_distinct(
      out$.analysis_result
    ) <
      2
  ) {
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
    date = !!rlang::sym(sample_date),
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
  conf.level = 0.95
) {
  validate_env_schema(df)

  date_col <- date_var(df)

  EnvStats::kendallTrendTest(
    x = df$.analysis_result,
    y = df[[date_col]],
    conf.level = conf.level
  )
}


#' Extract lm stats information
#'
#' Internal helper used by plotting functions.
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

#' Compute Trend Information
#'
#' Internal helper used by plotting functions.
#'
#' @param df Data frame
#' @param method Trend method
#' @param conf.level Confidence level
#'
#' @noRd
.compute_trend <- function(
  df,
  date_col,
  method = "mann_kendall",
  censored_method = "cenken",
  conf.level = 0.95
) {
  df <- .prepare_trend_data(
    df = df,
    date_col = date_col,
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

  censored_method <- rlang::arg_match(
    censored_method,
    c(
      "cenken",
      "as_reported"
    )
  )

  if (method == "none") {
    return(NULL)
  }

  z <- stats::qnorm(
    1 - (1 - conf.level) / 2
  )

  if (method == "lm") {
    df$.date_num <- as.numeric(
      df[[date_col]]
    )

    fit <- stats::lm(
      .analysis_result ~ .date_num,
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
      df[[date_col]]
    )

    fit <- stats::loess(
      .analysis_result ~ .date_num,
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
        model = fit,
        trend_df = trend_df
      )
    )
  }

  if (method == "gam") {
    df$.date_num <- as.numeric(
      df[[date_col]]
    )

    fit <- tryCatch(
      mgcv::gam(
        .analysis_result ~ s(.date_num),
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
      x = df$.analysis_result,
      y = df[[date_col]],
      conf.level = conf.level
    )

    x_num <- as.numeric(
      df[[date_col]]
    )

    x0 <- min(x_num)

    slope <- unname(
      fit$estimate[["slope"]]
    )

    intercept <- stats::median(
      df$.analysis_result -
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
          y = df$.analysis_result,
          ycen = df$.censored,
          x = as.numeric(df[[date_col]])
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
      df[[date_col]]
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
  trend = "none",
  trend_ci = FALSE,
  trend_conf_level = 0.95,
  trend_label = "none",
  trend_label_sig_only = TRUE,
  censored_method = "cenken",
  plot_type = "split"
) {
  censored_method <- rlang::arg_match(
    censored_method,
    c(
      "cenken",
      "as_reported"
    )
  )

  date_col <- date_var(df)

  location_col <- location_var(df)

  analyte_col <- analyte_var(df)

  if (trend == "none") {
    return(p)
  }

  if (trend %in% c("loess", "gam")) {
    trend_label <- "none"
  }

  alpha <- 1 - trend_conf_level

  if (plot_type == "split") {
    tr <- .compute_trend(
      df = df,
      date_col = date_col,
      method = trend,
      censored_method = censored_method,
      conf.level = trend_conf_level
    )

    if (is.null(tr)) {
      return(p)
    }

    if (
      trend_ci &&
        all(c("lwr", "upr") %in% names(tr$trend_df))
    ) {
      p <- p +
        ggplot2::geom_ribbon(
          data = tr$trend_df,
          ggplot2::aes(
            x = .data[[date_col]],
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
        data = tr$trend_df,
        ggplot2::aes(
          x = .data[[date_col]],
          y = fit
        ),
        colour = "red",
        linewidth = 1,
        inherit.aes = FALSE
      )

    if (trend_label != "none") {
      label_df <- tr$trend_df |>
        dplyr::slice_tail(n = 1) |>
        dplyr::mutate(
          .trend_p_value = tr$p_value,
          .trend_slope = tr$slope,
          .trend_conf_low = tr$conf_low,
          .trend_conf_high = tr$conf_high,

          label = .format_trend_label(
            slope = tr$slope,
            p_value = tr$p_value,
            conf_low = tr$conf_low,
            conf_high = tr$conf_high,
            trend_label = trend_label,
            trend = trend
          )
        )

      keep_label <- TRUE

      if (
        trend_label_sig_only &&
          trend == "mann_kendall"
      ) {
        keep_label <-
          !is.na(tr$p_value) &&
          tr$p_value <= alpha
      }

      if (keep_label) {
        p <- p +
          ggplot2::geom_text(
            data = label_df,
            ggplot2::aes(
              x = .data[[date_col]],
              y = fit,
              label = label
            ),
            hjust = -0.05,
            size = 3,
            inherit.aes = FALSE
          )
      }
    }

    return(p)
  }

  trend_df <- .compute_group_trends(
    df = df,
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
          x = .data[[date_col]],
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
        x = .data[[date_col]],
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
      date_col = date_col,
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
#' @noRd
.compute_group_trends <- function(
  df,
  method = "mann_kendall",
  censored_method = "cenken",
  conf.level = 0.95
) {
  date_col <- date_var(df)

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
          date_col = date_col,
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
#'
#' @noRd
.compute_group_labels <- function(
  trend_df,
  trend = "mann_kendall",
  trend_label = "compact",
  date_col,
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
      .data[[date_col]],
      .by_group = TRUE
    ) |>
    dplyr::slice_tail(
      n = 1
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      label_x = .data[[date_col]],

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


#' Dixon test for outliers
#'
#' @param df data frame
#' @param x column of analysis results
#' @param opposite  logical indicating whether you want to check not the value
#' with largest difference from the mean, but opposite
#' (lowest, if most suspicious is highest etc.)
#' @param type an integer specyfying the variant of test to be performed.
#' Possible values are compliant with these given by
#' Dixon (1950): 10, 11, 12, 20, 21. If this value is set to zero, a variant
#' of the test is chosen according to sample size
#' (10 for 3-7, 11 for 8-10, 21 for 11-13, 22 for 14 and more).
#' The lowest or highest value is selected automatically, and can be reversed
#' used opposite parameter.
#' @param two_sided treat test as two-sided. Default is TRUE.
#' @param group_by_location TRUE/FALSE
#' @param keep_data_object TRUE/FALSE to return entire htest object
#'
#' @export

dixon_test <- function(
  df,
  x = "analysis_result",
  type = 0,
  opposite = FALSE,
  two_sided = TRUE,
  group_by_location = FALSE,
  keep_data_object = FALSE
) {
  if (isTRUE(group_by_location)) {
    df <- df %>%
      group_by(param_name, default_unit) %>%
      nest()
  } else {
    df <- df %>%
      group_by(location_id, param_name, default_unit) %>%
      nest()
  }

  df <- df %>%
    mutate(
      dixon = map(
        .x = data,
        ~ outliers::dixon.test(
          .x$analysis_result,
          type = type,
          opposite = opposite,
          two.sided = two_sided
        )
      )
    )

  if (isTRUE(keep_data_object)) {
    outliers <- df %>%
      mutate(
        alternative = map(.x = dixon, ~ .x$alternative),
        p_value = map(.x = dixon, ~ .x$p.value)
      ) %>%
      select(-data)
  } else {
    outliers <- df %>%
      mutate(
        alternative = map(.x = dixon, ~ .x$alternative),
        p_value = map(.x = dixon, ~ .x$p.value)
      ) %>%
      select(-data, -dixon) %>%
      unnest()
  }

  return(outliers)
}

#' Grubb's test for outliers
#'
#' @param df data frame
#' @param x column of analysis results
#' @param type integer value indicating test variant. 10 is a test for one
#' outlier (side is detected automatically and can be reversed by opposite
#' parameter). 11 is a test for two outliers on opposite tails, 20 is test
#' for two outliers in one tail. Default is 10
#' @param opposite a logical indicating whether you want to check not the
#' value with largest difference from the mean, but opposite (lowest, if
#' most suspicious is highest etc.). Default is FALSE
#' @param two_sided Logical value indicating if there is a need to treat
#' this test as two-sided. Default is FALSE
#' @param group_by_location TRUE/FALSE default is FALSE
#' @param keep_data_object TRUE/FALSE default is FALSE
#' @export

grubbs_test <- function(
  df,
  x = "analysis_result",
  type = 10,
  opposite = FALSE,
  two_sided = FALSE,
  group_by_location = FALSE,
  keep_data_object = FALSE
) {
  if (isTRUE(group_by_location)) {
    df <- df %>%
      group_by(param_name, default_unit) %>%
      nest()
  } else {
    df <- df %>%
      group_by(location_id, param_name, default_unit) %>%
      nest()
  }

  df <- df %>%
    mutate(
      grubbs = map(
        .x = data,
        ~ outliers::grubbs.test(
          .x$analysis_result,
          type = type,
          opposite = opposite,
          two.sided = two_sided
        )
      )
    )

  if (isTRUE(keep_data_object)) {
    outliers <- df %>%
      mutate(
        alternative = map(.x = grubbs, ~ .x$alternative),
        p_value = map(.x = grubbs, ~ .x$p.value)
      ) %>%
      select(-data)
  } else {
    outliers <- df %>%
      mutate(
        alternative = map(.x = grubbs, ~ .x$alternative),
        p_value = map(.x = grubbs, ~ .x$p.value)
      ) %>%
      select(-data, -grubbs) %>%
      unnest()
  }

  return(outliers)
}

#' Rosner's test for outliers
#'
#' @param df data.frame of groundwater data
#' @param x column of analysis results
#' @param k number of suspected outliers. Default is 3.
#' @param alpha numerical scalar bewteen 0 and 1 indicating the Type I error
#' @param warn logical scalar indicating whether to issue a warning
#' (warn=TRUE; the default) when the number of non-missing, finite values
#' in x and the value of k are such that the assumed Type I error level might
#' not be maintained.
#' @param group_by_location TRUE/FALSE
#' @param keep_data_object TRUE/FALSE to return entire gofOutlier object
#' @export

rosner_test <- function(
  df,
  x = "analysis_result",
  k = 3,
  alpha = 0.05,
  warn = TRUE,
  group_by_location = FALSE,
  keep_data_object = FALSE
) {
  if (isTRUE(group_by_location)) {
    df <- df %>%
      group_by(param_name, default_unit) %>%
      nest()
  } else {
    df <- df %>%
      group_by(location_id, param_name, default_unit) %>%
      nest()
  }

  df <- df %>%
    mutate(
      rosner = map(
        .x = data,
        ~ rosnerTest(.x$analysis_result, k = k, alpha = alpha, warn = warn)
      )
    )

  if (isTRUE(keep_data_object)) {
    outliers <- df %>%
      mutate(
        value = map(.x = rosner, ~ .x$all.stats[c("Value", "Outlier")])
      ) %>%
      select(-data)
  } else {
    outliers <- df %>%
      mutate(
        value = map(.x = rosner, ~ .x$all.stats[c("Value", "Outlier")])
      ) %>%
      select(-data, -rosner) %>%
      unnest()
  }

  return(outliers)
}

#' Tukey's test for outliers
#'
#' @param df data.frame of groundwater data
#' @param x column of analysis results
#' @param k multiplier for IQR k = 1.5 indicates "outlier", k = 3 indicates
#' "far out"
#' @param na.rm logical. Should missing values be removed?
#' @param type an integer selecting one of the many quantile algorithms.
#' Default is 6 to match Sanitas
#' @param combine_locations TRUE/FALSE
#'
#' @export

tukey_outlier <- function(
  df,
  x = "analysis_result",
  k = 3,
  na.rm = FALSE,
  type = 6,
  combine_locations = FALSE
) {
  if (isTRUE(combine_locations)) {
    df <- df %>%
      group_by(param_name, default_unit) %>%
      nest()
  } else {
    df <- df %>%
      group_by(location_id, param_name, default_unit) %>%
      nest()
  }

  df <- df %>%
    mutate(
      outlier = map(
        .x = data,
        ~ case_when(
          .x$analysis_result >
            .tukey_high_cutoff(
              .x$analysis_result,
              k = k,
              na.rm = na.rm,
              type = type
            ) ~ TRUE,
          .x$analysis_result <
            .tukey_low_cutoff(
              .x$analysis_result,
              k = k,
              na.rm = na.rm,
              type = type
            ) ~ TRUE,
          TRUE ~ FALSE
        )
      )
    ) %>%
    unnest(cols = c(data, outlier))

  return(df)
}

.tukey_low_cutoff <- function(x, k = 3, na.rm = FALSE, type = 6) {
  low <- quantile(x, na.rm = na.rm, type = type)[["25%"]] -
    k * IQR(x, na.rm = na.rm, type = type)
  return(low)
}

.tukey_high_cutoff <- function(x, k = 3, na.rm = FALSE, type = 6, ...) {
  high <- quantile(x, na.rm = na.rm, type = type)[["75%"]] +
    k * IQR(x, na.rm = na.rm, type = type)
  return(high)
}

#' Boxcox transformation
#'
#' @param df data frame
#' @param combine_locations logical
#' @param objective_name character string indicating what objective to use.
#' The possible values are "PPCC" (probability plot correlation coefficient;
#' the default), "Shapiro-Wilk" (the Shapiro-Wilk goodness-of-fit statistic),
#' and "Log-Likelihood" (the log-likelihood function).
#'
#' @export

boxcox_transform <- function(
  df,
  combine_locations = FALSE,
  objective_name = "PPCC"
) {
  if (combine_locations) {
    df <- df %>%
      group_by(param_name, default_unit) %>%
      nest() %>%
      mutate(
        boxcox = map(
          .x = data,
          ~ EnvStats::boxcox(
            .x$analysis_result,
            optimize = TRUE,
            objective.name = objective_name
          )
        )
      ) %>%
      mutate(
        lambda = map(.x = boxcox, ~ .x$lambda)
      ) %>%
      select(-boxcox) %>%
      unnest(lambda) %>%
      unnest(data) %>%
      group_by(param_name, default_unit) %>%
      nest() %>%
      mutate(
        analysis_result = map(
          .x = data,
          ~ boxcoxTransform(.x$analysis_result, lambda = .x$lambda[1])
        )
      ) %>%
      unnest()
  } else {
    df <- df %>%
      group_by(location_id, param_name, default_unit) %>%
      nest() %>%
      mutate(
        boxcox = map(
          .x = data,
          ~ EnvStats::boxcox(
            .x$analysis_result,
            optimize = TRUE,
            objective.name = objective_name
          )
        )
      ) %>%
      mutate(
        lambda = map(.x = boxcox, ~ .x$lambda)
      ) %>%
      select(-boxcox) %>%
      unnest(lambda) %>%
      unnest(data) %>%
      group_by(location_id, param_name, default_unit) %>%
      nest() %>%
      mutate(
        analysis_result = map(
          .x = data,
          ~ boxcoxTransform(.x$analysis_result, lambda = .x$lambda[1])
        )
      ) %>%
      unnest()
  }

  df
}

#' Analysis of Variance
#'
#' @param df data frame
#' @param keep_data_object logical to keep objects from aov and TukeyHSD
#'
#' @examples
#' data(gw_data)
#'
#' wells <- c("MW-1", "MW-2", "MW-3", "MW-4")
#'
#' params <- c("Calcium, dissolved", "Sodium, dissolved", "Sulfate, total")
#'
#' gw_data %>%
#'   filter(location_id %in% wells, param_name %in% params) %>%
#'   anova()
#'
#' anova_object <- gw_data %>%
#'   filter(location_id %in% wells, param_name %in% params) %>%
#'   anova(., keep_data_object = TRUE)
#'
#' plot(anova_object$anova[[1]], 1)
#'
#' @export

anova <- function(df, keep_data_object = FALSE) {
  df <- df %>%
    mutate(location_id = fct_recode(location_id))

  aov_result <- df %>%
    group_by(param_name, default_unit) %>%
    nest() %>%
    mutate(
      anova = map(
        .x = data,
        ~ aov(analysis_result ~ location_id, data = .x)
      ),
      tukey = map(.x = anova, ~ TukeyHSD(.x))
    ) %>%
    ungroup() %>%
    select(-data)

  if (isTRUE(keep_data_object)) {
    aov_result %>%
      mutate(
        pairwise = map(
          .x = tukey,
          ~ rownames_to_column(data.frame(.x$location_id))
        )
      ) %>%
      select(-default_unit)
  } else {
    aov_result %>%
      mutate(
        pairwise = map(
          .x = tukey,
          ~ rownames_to_column(data.frame(.x$location_id))
        )
      ) %>%
      select(-anova, -tukey, -default_unit) %>%
      unnest()
  }
}

#' Estimate statistical distribution
#'
#' Determines whether grouped data are best represented by a
#' normal, lognormal, or other candidate distribution.
#'
#' The returned object retains the original grouped observations in a
#' nested `data` list-column. This allows downstream functions such as
#' prediction interval, tolerance interval, and control chart
#' calculations to access the observations used for distribution
#' estimation.
#'
#' @param df Tidy groundwater data frame. Grouping determines the
#'   level at which distributions are estimated.
#' @param return_result TRUE/FALSE.
#' @param alpha Significance level.
#' @param method Goodness-of-fit method passed to EnvStats::distChoose().
#' @param choices Candidate distributions.
#' @param min_n Minimum sample size required for estimation.
#'
#' @return A tibble with one row per group containing:
#' \describe{
#'   \item{distribution}{Selected distribution.}
#'   \item{n}{Number of non-missing observations used in estimation.}
#'   \item{data}{Nested data frame containing the observations
#'   used for the group.}
#'   \item{result}{EnvStats result object stored as a
#'   list-column when return_result = TRUE.}
#' }
#'
#' @examples
#' data("gw_data")
#'
#' gw_data %>%
#'   dplyr::group_by(
#'     location_id,
#'     param_name,
#'     default_unit
#'   ) %>%
#'   percent_lt() %>%
#'   est_dist()
#'
#' @export

est_dist <- function(
  df,
  return_result = FALSE,
  alpha = 0.05,
  method = "sw",
  choices = c("norm", "lnorm"),
  min_n = 4
) {
  required_cols <- "analysis_result"

  missing_cols <- setdiff(
    required_cols,
    names(df)
  )

  if (length(missing_cols) > 0) {
    rlang::abort(
      paste(
        "Missing required column(s):",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  .validate_est_dist_args(
    alpha = alpha,
    method = method,
    choices = choices,
    return_result = return_result
  )

  .est_dist_worker(
    df = df,
    return_result = return_result,
    min_n = min_n,
    alpha = alpha,
    method = method,
    choices = choices
  )
}

#' Function to return either normal, lognormal, gamma, or non-parametric
#' distribution estimate
#'
#' @param df groundwater data frame in tidy format
#' @param left_censored logical vector.
#' @param alpha alpha
#' @param choices vector of distributions to check. Default is c("norm, "lnorm")
#' @param method default is "sf"
#' @param combine_locations TRUE/FALSE to estimate distribution by individual
#' location, or grouped together. Default is FALSE.
#' @param keep_data_object Default is FALSE
#'
#' @examples
#' data("gw_data")
#' wells <- c("MW-1", "MW-2", "MW-3")
#' params <- c("Fluoride, total",
#'             "Arsenic, dissolved",
#'             "Nickel, dissolved")
#'
#' gw_data <- gw_data %>%
#' to_censored() %>%
#' filter(location_id %in% wells, param_name %in% params) %>%
#' percent_lt() %>%
#' filter(percent_lt >=15, percent_lt <= 50)
#'
#' gw_data %>%
#' group_by(location_id, param_name, default_unit) %>%
#' est_dist_censored()
#'
#'
#' gw_data %>%
#' group_by(param_name, default_unit) %>%
#' est_dist_censored()
#'
#' @export

est_dist_censored <- function(
  df,
  return_result = FALSE,
  alpha = 0.05,
  method = "sf",
  choices = c("norm", "lnorm"),
  min_n = 4
) {
  required_cols <- c(
    "analysis_result",
    "left_censored"
  )

  missing_cols <- setdiff(
    required_cols,
    names(df)
  )

  if (length(missing_cols) > 0) {
    rlang::abort(
      paste(
        "Missing required column(s):",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  .validate_est_dist_args(
    alpha = alpha,
    method = method,
    choices = choices,
    return_result = return_result
  )

  .est_dist_censored_worker(
    df = df,
    return_result = return_result,
    min_n = min_n,
    alpha = alpha,
    method = method,
    choices = choices
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
#' @noRd
.fit_dist <- function(
  x,
  alpha,
  method,
  choices
) {
  EnvStats::distChoose(
    y = x,
    alpha = alpha,
    method = method,
    choices = choices
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
#' @noRd
.fit_dist_censored <- function(
  x,
  censored,
  alpha,
  method,
  choices
) {
  EnvStats::distChooseCensored(
    x = x,
    censored = censored,
    censoring.side = "left",
    alpha = alpha,
    method = method,
    choices = choices
  )
}

#' Estimate distributions by group
#'
#' Internal worker for est_dist().
#'
#' @inheritParams est_dist
#' @param min_n Minimum sample size required for estimation.
#'
#' @return A tibble with one row per group.
#'
#' @noRd
#' @noRd
.est_dist_worker <- function(
  df,
  return_result,
  min_n,
  alpha,
  method,
  choices
) {
  df %>%
    dplyr::group_modify(
      ~ {
        group_data <- .x

        x <- stats::na.omit(
          .x$analysis_result
        )

        n <- length(x)

        if (n < min_n) {
          result <- .make_dist_result(
            distribution = "insufficient_data",
            n = n,
            return_result = return_result
          )

          return(
            .attach_nested_data(
              result = result,
              data = group_data
            )
          )
        }

        fit_result <- purrr::possibly(
          .fit_dist,
          otherwise = NULL
        )(
          x = x,
          alpha = alpha,
          method = method,
          choices = choices
        )

        result <- .handle_dist_result(
          result = fit_result,
          n = n,
          return_result = return_result
        )

        .attach_nested_data(
          result = result,
          data = group_data
        )
      }
    )
}

#' Estimate distributions for censored data by group
#'
#' Internal worker for est_dist_censored().
#'
#' @inheritParams .est_dist_worker
#'
#' @return A tibble with one row per group.
#'
#' @noRd
#' @noRd
.est_dist_censored_worker <- function(
  df,
  return_result,
  min_n,
  alpha,
  method,
  choices
) {
  df %>%
    dplyr::group_modify(
      ~ {
        group_data <- .x

        x <- .x$analysis_result
        censored <- .x$left_censored

        keep <- !is.na(x)

        x <- x[keep]
        censored <- censored[keep]

        n <- length(x)

        if (n < min_n) {
          result <- .make_dist_result(
            distribution = "insufficient_data",
            n = n,
            return_result = return_result
          )

          return(
            .attach_nested_data(
              result = result,
              data = group_data
            )
          )
        }

        if (any(is.na(censored))) {
          rlang::abort(
            "left_censored cannot contain missing values."
          )
        }

        fit_result <- if (!any(censored)) {
          purrr::possibly(
            .fit_dist,
            otherwise = NULL
          )(
            x = x,
            alpha = alpha,
            method = method,
            choices = choices
          )
        } else {
          purrr::possibly(
            .fit_dist_censored,
            otherwise = NULL
          )(
            x = x,
            censored = censored,
            alpha = alpha,
            method = method,
            choices = choices
          )
        }

        result <- .handle_dist_result(
          result = fit_result,
          n = n,
          return_result = return_result
        )

        .attach_nested_data(
          result = result,
          data = group_data
        )
      }
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
#' @noRd
.make_dist_result <- function(
  distribution,
  n,
  result = NULL,
  return_result = FALSE
) {
  out <- tibble::tibble(
    distribution = distribution,
    n = n
  )

  if (return_result) {
    out$result <- list(result)
  }

  out
}

#' Helper function for est_dist() and est_dist_censored()
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
        n,
        return_result = return_result
      )
    )
  }

  .make_dist_result(
    distribution = paste(
      result$decision,
      collapse = ", "
    ),
    n = n,
    result = result,
    return_result = return_result
  )
}

#' Compute prediction intervals
#'
#' @keywords internal
.compute_pred_int <- function(
  df,
  simultaneous = FALSE,
  method = "Bonferroni",
  n_mean = 1,
  k = 1,
  pi_type = "upper",
  conf_level = 0.95
) {
  .validate_est_dist_output(df)

  rlang::arg_match0(
    pi_type,
    c("upper", "lower", "two-sided")
  )

  checkmate::assert_int(n_mean, lower = 1)
  checkmate::assert_int(k, lower = 1)
  checkmate::assert_number(
    conf_level,
    lower = 0,
    upper = 1
  )

  pred_fun <- function(data, distribution) {
    values <- data$analysis_result

    switch(
      distribution,

      "Normal" = {
        if (simultaneous) {
          EnvStats::predIntNormSimultaneous(
            x = values,
            n.mean = n_mean,
            k = k,
            pi.type = pi_type,
            conf.level = conf_level
          )
        } else {
          EnvStats::predIntNorm(
            x = values,
            method = method,
            n.mean = n_mean,
            k = k,
            pi.type = pi_type,
            conf.level = conf_level
          )
        }
      },

      "Lognormal" = {
        if (simultaneous) {
          EnvStats::predIntLnormSimultaneous(
            x = values,
            n.geomean = n_mean,
            k = k,
            pi.type = pi_type,
            conf.level = conf_level
          )
        } else {
          EnvStats::predIntLnorm(
            x = values,
            method = method,
            n.geomean = n_mean,
            k = k,
            pi.type = pi_type,
            conf.level = conf_level
          )
        }
      },

      "Nonparametric" = {
        if (simultaneous) {
          EnvStats::predIntNparSimultaneous(
            x = values,
            pi.type = pi_type
          )
        } else {
          EnvStats::predIntNpar(
            x = values,
            pi.type = pi_type
          )
        }
      }
    )
  }

  df %>%
    dplyr::mutate(
      pred_int = purrr::map2(
        data,
        distribution,
        pred_fun
      )
    ) %>%
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
    ) %>%
    dplyr::select(-data, -pred_int)
}

#' Function to calculate confidence intervals
#'
#' @param df df data frame of groundwater data in tidy format
#' @param analysis_result the analysis result column
#' @param distribution the distribution column
#' @param ci_type character string indicating what kind of confidence interval
#' to compute. The possible values are ci_type="two-sided" (the default),
#' ci_type="lower", and ci_type = "upper
#' @param conf_level a scalar between 0 and 1 indicating the confidence level
#' of the prediction interval. The default value is conf_level = 0.95
#' @param method character string specifying the method of estimation.
#' Possible values are "mvue" (minimum variance unbiased; the default),
#' and "mle/mme" (maximum likelihood/method of moments).
#'
#' @examples
#' data("gw_data")

#' wells <- c("MW-1", "MW-2", "MW-3", "MW-4")

#' params <- c("Sulfate, total",
#'             "Arsenic, dissolved",
#'             "Boron, dissolved")
#'
#' background <- lubridate::ymd(c("2007-12-20", "2012-01-01"), tz = "UTC")

#' # first group data by location, param, and background
#' # estimate percent less than and distribution
#' background_data <- gw_data %>%
#'  filter(location_id %in% wells, param_name %in% params,
#'          sample_date >= background[1] & sample_date <= background[2]) %>%
#'   group_by(location_id, param_name, default_unit) %>%
#'   percent_lt() %>%
#'   est_dist(., keep_data_object = TRUE) %>%
#'   arrange(location_id, param_name)
#'
#' background_data %>%
#' conf_int(., ci_type = "lower", conf_level = 0.99)
#'
#' @export

conf_int <- function(
  df,
  analysis_result = "analysis_result",
  distribution = "distribution",
  method = "mvue",
  ci_type = "two-sided",
  conf_level = 0.95
) {
  conf_int <- df %>%
    mutate(
      conf_int = case_when(
        distribution == "Normal" ~ map(
          .x = data,
          ~ EnvStats::enorm(
            x = .x$analysis_result,
            method = method,
            ci = TRUE,
            ci.type = ci_type,
            conf.level = conf_level,
            ci.param = "mean"
          )
        ),
        distribution == "Lognormal" ~ map(
          .x = data,
          ~ EnvStats::elnormAlt(
            x = .x$analysis_result,
            ci = TRUE,
            ci.type = ci_type,
            ci.method = "land",
            conf.level = conf_level
          )
        ),
        distribution == "Nonparametric" ~ map(
          .x = data,
          ~ EnvStats::eqnpar(
            x = .x$analysis_result,
            ci = TRUE,
            ci.type = ci_type,
            ci.method = "interpolate",
            approx.conf.level = conf_level
          )
        )
      )
    )

  conf_int %>%
    mutate(
      distribution = distribution,
      sample_size = map(.x = conf_int, ~ .x$sample.size),
      lcl = map(.x = conf_int, ~ round(.x$interval$limits["LCL"], 3)),
      ucl = map(.x = conf_int, ~ .x$interval$limits["UCL"]),
      conf_level = map(.x = conf_int, ~ .x$interval$conf.level)
    ) %>%
    select(-data, -conf_int) %>%
    unnest()
}

#' Function to calculate prediction interval
#'
#' @param df df data frame of groundwater data in tidy format
#' @param analysis_result the analysis result column
#' @param distribution the distribution column
#' @param n_mean n.mean positive integer specifying the sample size associated
#' with the future averages.
#' The default value is n.mean=1 (i.e., individual observations).
#' Note that all future averages must be based on the same sample size.
#' @param k k positive integer specifying the number of future observations or
#' averages the prediction interval should contain with confidence level
#' conf.level. The default value is k=1.
#' @param method default is "Bonferroni"
#' @param pi_type character string indicating what kind of prediction interval
#' to compute. The possible values are pi_type="two-sided" (the default),
#' pi_type="upper", and pi_type="lower".
#' @param conf_level a scalar between 0 and 1 indicating the confidence level
#' of the prediction interval. The default value is conf.level=0.95
#'
#' @examples
#' data("gw_data")
#'
#' wells <- c("MW-1", "MW-2", "MW-3", "MW-4")
#'
#' params <- c("Sulfate, total",
#'             "Arsenic, dissolved",
#'             "Boron, dissolved")
#'
#' background <- lubridate::ymd(c("2007-12-20", "2012-01-01"), tz = "UTC")
#'
#' # first group data by location, param, and background
#' # estimate percent less than and distribution
#' background_data <- gw_data %>%
#'  filter(location_id %in% wells, param_name %in% params,
#'          sample_date >= background[1] & sample_date <= background[2]) %>%
#'   group_by(location_id, param_name, default_unit) %>%
#'   percent_lt() %>%
#'   est_dist(., keep_data_object = TRUE) %>%
#'   arrange(location_id, param_name)
#'
#' background_data %>%
#' pred_int(., pi_type = "upper", conf_level = 0.99)
#'
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

#' Function to calculate simultaneous prediction interval
#'
#' @param df df data frame of groundwater data in tidy format
#' @param analysis_result the analysis result column
#' @param distribution the distribution column
#' @param n_mean n.mean positive integer specifying the sample size associated
#' with the future averages.
#' The default value is n.mean=1 (i.e., individual observations).
#' Note that all future averages must be based on the same sample size.
#' @param k k positive integer specifying the number of future observations or
#' averages the prediction interval should contain with confidence level
#' conf.level. The default value is k=1.
#' @param m m if m = 1, then same results as predInt
#' @param r r
#' @param rule rule "k.of.m"
#' @param pi_type character string indicating what kind of prediction interval
#' to compute. The possible values are pi_type="upper" (the default),
#' and pi_type="lower".
#' @param conf_level a scalar between 0 and 1 indicating the confidence level
#' of the prediction interval. The default value is conf.level=0.95
#'
#' @examples
#' data("gw_data")
#'
#' wells <- c("MW-1", "MW-2", "MW-3", "MW-4")
#'
#' params <- c("Sulfate, total",
#'             "Arsenic, dissolved",
#'             "Boron, dissolved")
#'
#' background <- lubridate::ymd(c("2007-12-20", "2012-01-01"), tz = "UTC")
#'
#' # first group data by location, param, and background
#' # estimate percent less than and distribution
#' background_data <- gw_data %>%
#'  filter(location_id %in% wells, param_name %in% params,
#'          sample_date >= background[1] & sample_date <= background[2]) %>%
#'   group_by(location_id, param_name, default_unit) %>%
#'   percent_lt() %>%
#'   est_dist(., keep_data_object = TRUE) %>%
#'   arrange(location_id, param_name)
#'
#' background_data %>%
#' pred_int_sim(., pi_type = "upper", conf_level = 0.99)
#'
#' @export

pred_int_sim <- function(
  df,
  n_mean = 1,
  k = 1,
  pi_type = "upper",
  conf_level = 0.95
) {
  .compute_pred_int(
    df = df,
    simultaneous = TRUE,
    n_mean = n_mean,
    k = k,
    pi_type = pi_type,
    conf_level = conf_level
  )
}

#' Function to calculate tolernace intervals
#'
#' @param df df data frame of groundwater data in tidy format
#' @param analysis_result the analysis result column
#' @param distribution the distribution column
#' @param coverage a scalar between 0 and 1 indicating the desired coverage of
#' the tolerance interval. The default value is coverage=0.95.
#' If cov.type="expectation", this argument is ignored
#' @param cov_type character string specifying the coverage type for the
#' tolerance interval. The possible values are "content"
#' (β-content; the default), and "expectation" (β-expectation).
#' @param method for the case of a two-sided tolerance interval, a character
#' string specifying the method for constructing the tolerance interval.
#' This argument is ignored if ti.type="lower" or ti.type="upper".
#' The possible values are "exact" (the default) and
#' "wald.wolfowitz" (the Wald-Wolfowitz approximation).
#' @param ti_type character string indicating what kind of tolerance interval
#' to compute. The possible values are ti_type="two-sided",
#' ti_type="upper", and ti_type="lower" (the default).
#' @param conf_level a scalar between 0 and 1 indicating the confidence level
#' of the tolerance interval. The default value is conf.level=0.95
#' @param est_method for tolIntLnormAlt, a character string specifying the
#' method of estimating the mean and coefficient of variation.
#' This argument has no effect on the method of constructing the
#' tolerance interval. Possible values are "mvue" (minimum variance unbiased;
#' the default), "qmle" (quasi maximum likelihood), "mle" (maximum likelihood),
#'  "mme" (method of moments), and "mmue" (method of moments based on the
#'  unbiased estimate of variance).
#'
#' @export

tol_int <- function(
  df,
  analysis_result = "analysis_result",
  distribution = "distribution",
  coverage = 0.95,
  cov_type = "content",
  ti_type = "two-sided",
  method = "exact"
) {
  tol_int <- df %>%
    mutate(
      tol_int = case_when(
        distribution == "Normal" ~ map(
          .x = data,
          ~ EnvStats::tolIntNorm(
            x = .x$analysis_result,
            coverage = coverage,
            cov.type = cov_type,
            ti.type = ti_type,
            method = method
          )
        ),
        distribution == "Lognormal" ~ map(
          .x = data,
          ~ EnvStats::tolIntLnorm(
            x = .x$analysis_result,
            coverage = coverage,
            cov.type = cov_type,
            ti.type = ti_type,
            method = method
          )
        ),
        distribution == "Nonparametric" ~ map(
          .x = data,
          ~ EnvStats::tolIntNpar(
            x = .x$analysis_result,
            coverage = coverage,
            cov.type = cov_type,
            ti.type = ti_type
          )
        )
      )
    )

  tol_int %>%
    mutate(
      distribution = distribution,
      sample_size = map(.x = tol_int, ~ .x$sample.size),
      ltl = map(.x = tol_int, ~ .x$interval$limits["LTL"]),
      utl = map(.x = tol_int, ~ .x$interval$limits["UTL"]),
      conf_level = map(.x = tol_int, ~ .x$interval$conf.level)
    ) %>%
    select(-data, -tol_int) %>%
    unnest()
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
  tmp <- df %>%
    filter_(~ location_id == well, ~ param_name == param)

  if (is.null(start)) {
    start <- tmp %>%
      start_date()
  }

  end <- tmp %>%
    nth_date(start = start, n = n_end - 1)

  max <- tmp %>%
    select_(~sample_date) %>%
    first() %>%
    max()

  bkgd <- tmp %>%
    set_background(start = start, end = end)

  next_date <- tmp %>%
    nth_date(start = end, n = n_prop)

  next_bkgd <- tmp %>%
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
