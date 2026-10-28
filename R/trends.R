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
