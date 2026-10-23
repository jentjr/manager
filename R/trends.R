#' Summarize Trends
#'
#' Computes trend statistics for each unique
#' location and analyte combination using the
#' same trend engine used by plotting functions.
#'
#' Date coverage and concentration summary
#' statistics are included to assist with
#' environmental monitoring, reporting,
#' and statistical evaluations.
#'
#' @param df env_schema object.
#' @param method Trend method. One of
#'   "lm", "loess", "gam", or "mann_kendall".
#' @param conf.level Confidence level.
#'
#' @returns A tibble with one row per location/analyte
#' combination containing:
#' \describe{
#'   \item{.location}{Location identifier}
#'   \item{.analyte}{Analyte name}
#'   \item{n}{Number of observations}
#'   \item{first_date}{Earliest sample date}
#'   \item{last_date}{Latest sample date}
#'   \item{date_range_days}{Monitoring period length in days}
#'   \item{date_range_years}{Monitoring period length in years}
#'   \item{sampling_frequency}{Samples per year}
#'   \item{minimum_concentration}{Minimum concentration}
#'   \item{median_concentration}{Median concentration}
#'   \item{mean_concentration}{Mean concentration}
#'   \item{maximum_concentration}{Maximum concentration}
#'   \item{standard_deviation}{Sample standard deviation}
#'   \item{coefficient_of_variation}{Standard deviation divided by mean}
#'   \item{trend_method}{Trend method used}
#'   \item{slope}{Estimated trend slope}
#'   \item{p_value}{Trend p-value}
#'   \item{conf_low}{Lower confidence interval}
#'   \item{conf_high}{Upper confidence interval}
#'   \item{significant}{TRUE if p-value <= alpha}
#'   \item{trend_direction}{Increasing, Decreasing, or Stable}
#' }
#'
#' @export
trend_summary <- function(
  df,
  method = "mann_kendall",
  conf.level = 0.95
) {
  validate_env_schema(df)

  alpha <- 1 - conf.level

  df |>
    dplyr::group_by(
      .location,
      .analyte
    ) |>
    dplyr::group_modify(
      ~ {
        n <- nrow(.x)

        date_values <- stats::na.omit(.x$.date)

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

          date_range_years <-
            date_range_days / 365.25
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

        result_values <- stats::na.omit(
          .x$.analysis_result
        )

        if (length(result_values) > 0) {
          minimum_concentration <- min(result_values)

          median_concentration <- stats::median(
            result_values
          )

          mean_concentration <- mean(
            result_values
          )

          maximum_concentration <- max(
            result_values
          )

          standard_deviation <- stats::sd(
            result_values
          )

          coefficient_of_variation <-
            dplyr::case_when(
              is.na(mean_concentration) ~ NA_real_,
              mean_concentration == 0 ~ NA_real_,
              TRUE ~ standard_deviation /
                mean_concentration
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
          df = .x,
          method = method,
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
              slope = NA_real_,
              p_value = NA_real_,
              conf_low = NA_real_,
              conf_high = NA_real_,
              significant = NA,
              trend_direction = NA_character_
            )
          )
        }

        slope <- if (is.null(tr$slope)) NA_real_ else tr$slope
        p_value <- if (is.null(tr$p_value)) NA_real_ else tr$p_value
        conf_low <- if (is.null(tr$conf_low)) NA_real_ else tr$conf_low
        conf_high <- if (is.null(tr$conf_high)) NA_real_ else tr$conf_high

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
          slope = slope,
          p_value = p_value,
          conf_low = conf_low,
          conf_high = conf_high,
          significant = dplyr::case_when(
            is.na(p_value) ~ NA,
            TRUE ~ p_value <= alpha
          ),
          trend_direction = dplyr::case_when(
            is.na(slope) ~ NA_character_,
            slope > 0 ~ "Increasing",
            slope < 0 ~ "Decreasing",
            TRUE ~ "Stable"
          )
        )
      }
    ) |>
    dplyr::ungroup()
}

#' Kendall Trend Test
#'
#' @param df env_schema object.
#' @param conf.level confidence level
#'
#' @export
kendall_trend <- function(
  df,
  conf.level = 0.95
) {
  validate_env_schema(df)

  EnvStats::kendallTrendTest(
    .analysis_result ~ .date,
    data = df,
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
#' @noRd
.compute_trend <- function(
  df,
  method = "none",
  conf.level = 0.95
) {
  validate_env_schema(df)

  valid_methods <- c(
    "none",
    "lm",
    "loess",
    "gam",
    "mann_kendall"
  )

  if (!method %in% valid_methods) {
    stop(
      "`method` must be one of: ",
      paste(valid_methods, collapse = ", "),
      call. = FALSE
    )
  }

  z <- stats::qnorm(
    1 - (1 - conf.level) / 2
  )

  df <- df |>
    dplyr::filter(
      !is.na(.date),
      !is.na(.analysis_result)
    ) |>
    dplyr::arrange(.date)

  if (nrow(df) < 3) {
    return(NULL)
  }

  if (method == "none") {
    return(NULL)
  }

  if (method == "lm") {
    df$.date_num <- as.numeric(df$.date)

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
    df$.date_num <- as.numeric(df$.date)

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
    df$.date_num <- as.numeric(df$.date)

    fit <- mgcv::gam(
      .analysis_result ~
        s(.date_num),
      data = df
    )

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

  if (method == "mann_kendall") {
    fit <- EnvStats::kendallTrendTest(
      x = df$.analysis_result,
      y = df$.date,
      conf.level = conf.level
    )

    x_num <- as.numeric(df$.date)

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

    return(
      list(
        method = method,
        slope = slope,
        p_value = fit$p.value,
        conf_low = fit$conf.int[1],
        conf_high = fit$conf.int[2],
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
  plot_type = "split"
) {
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
      method = trend,
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
            x = .date,
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
          x = .date,
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

          label = dplyr::case_when(
            trend_label == "compact" &&
              trend == "mann_kendall" ~
              sprintf(
                "%s\np=%.3f",
                dplyr::case_when(
                  tr$slope > 0 ~ "Increasing",
                  tr$slope < 0 ~ "Decreasing",
                  TRUE ~ "Stable"
                ),
                tr$p_value
              ),

            trend_label == "full" &&
              trend == "mann_kendall" ~
              sprintf(
                paste(
                  "Mann-Kendall",
                  "p=%.3f",
                  "Sen=%.3g",
                  "CI=[%.3g, %.3g]",
                  sep = "\n"
                ),
                tr$p_value,
                tr$slope,
                tr$conf_low,
                tr$conf_high
              ),

            trend_label == "compact" ~
              sprintf(
                "%s\np=%.3f",
                dplyr::case_when(
                  tr$slope > 0 ~ "Increasing",
                  tr$slope < 0 ~ "Decreasing",
                  TRUE ~ "Stable"
                ),
                tr$p_value
              ),

            trend_label == "full" ~
              sprintf(
                paste(
                  "Slope=%.3g",
                  "p=%.3f",
                  "CI=[%.3g, %.3g]",
                  sep = "\n"
                ),
                tr$slope,
                tr$p_value,
                tr$conf_low,
                tr$conf_high
              ),

            TRUE ~ ""
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
              x = .date,
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
          x = .date,
          ymin = lwr,
          ymax = upr,
          group = interaction(
            .location,
            .analyte
          ),
          fill = .analyte
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
        x = .date,
        y = fit,
        group = interaction(
          .location,
          .analyte
        ),
        colour = .analyte
      ),
      linewidth = 1,
      inherit.aes = FALSE
    )

  if (trend_label != "none") {
    labels <- .compute_group_labels(
      trend_df = trend_df,
      trend = trend,
      trend_label = trend_label
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
            colour = .analyte
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

#' Compute trend models for well/constituent pairs
#'
#' Internal helper used by plotting functions.
#' Trends are always computed on unique
#' location/analyte combinations.
#' regardless of plot faceting.
#'
#' @param df env_schema object.
#' @param method trend method
#' @param conf.level confidence level
#'
#' @return dataframe containing fitted trend values and
#' associated trend statistics for plotting
#'
#' @noRd
.compute_group_trends <- function(
  df,
  method = "mann_kendall",
  conf.level = 0.95
) {
  validate_env_schema(df)

  df |>
    dplyr::group_by(
      .location,
      .analyte
    ) |>
    dplyr::group_modify(
      ~ {
        tr <- .compute_trend(
          .x,
          method = method,
          conf.level = conf.level
        )

        if (is.null(tr)) {
          return(tibble::tibble())
        }

        tr$trend_df |>
          dplyr::mutate(
            .trend_method = method,
            .trend_slope = if (is.null(tr$slope)) NA_real_ else tr$slope,
            .trend_p_value = if (is.null(tr$p_value)) NA_real_ else tr$p_value,
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

#' Compute trend group labels for series_plot
#'
#' @noRd
.compute_group_labels <- function(
  trend_df,
  trend = "mann_kendall",
  trend_label = "compact"
) {
  trend_df |>
    dplyr::group_by(
      .location,
      .analyte
    ) |>
    dplyr::arrange(.date) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      label_x = .date,

      label_y = fit,

      label = dplyr::case_when(
        is.na(.trend_p_value) ~ "",

        trend_label == "none" ~
          "",

        trend_label == "compact" &
          trend == "mann_kendall" ~
          sprintf(
            "%s\np=%.3f",
            dplyr::case_when(
              .trend_slope > 0 ~ "Increasing",
              .trend_slope < 0 ~ "Decreasing",
              TRUE ~ "Stable"
            ),
            .trend_p_value
          ),

        trend_label == "full" &
          trend == "mann_kendall" ~
          sprintf(
            paste(
              "Mann-Kendall",
              "p=%.3f",
              "Sen=%.3g",
              "CI=[%.3g, %.3g]",
              sep = "\n"
            ),
            .trend_p_value,
            .trend_slope,
            .trend_conf_low,
            .trend_conf_high
          ),

        trend_label == "compact" ~
          sprintf(
            "%s\np=%.3f",
            dplyr::case_when(
              .trend_slope > 0 ~ "Increasing",
              .trend_slope < 0 ~ "Decreasing",
              TRUE ~ "Stable"
            ),
            .trend_p_value
          ),

        trend_label == "full" ~
          sprintf(
            paste(
              "Slope=%.3g",
              "p=%.3f",
              "CI=[%.3g, %.3g]",
              sep = "\n"
            ),
            .trend_slope,
            .trend_p_value,
            .trend_conf_low,
            .trend_conf_high
          ),

        TRUE ~ ""
      )
    )
}
