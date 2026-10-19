#' Summarize Trends for Groundwater Time Series
#'
#' Computes trend statistics for each unique
#' location_id + param_name combination using the same
#' trend engine used by plotting functions.
#'
#' Date coverage and concentration summary statistics
#' are included to assist with groundwater monitoring,
#' CCR reporting, and statistical evaluations.
#'
#' @param df groundwater data in tidy format
#' @param x date column
#' @param y result column
#' @param method trend method. One of:
#' "lm", "loess", "gam", or "mann_kendall"
#' @param conf.level confidence level
#' @param location_id location identifier column
#' @param param_name parameter identifier column
#'
#' @return tibble containing one row per
#' location_id + param_name combination.
#'
#' @return tibble with:
#' \describe{
#'   \item{location_id}{Location identifier}
#'   \item{param_name}{Parameter name}
#'   \item{n}{Number of observations}
#'   \item{first_date}{Earliest sample date}
#'   \item{last_date}{Latest sample date}
#'   \item{date_range_days}{Monitoring period length in days}
#'   \item{date_range_years}{Monitoring period length in years}
#'   \item{sampling_frequency}{Samples per year}
#'   \item{minimum_concentration}{Minimum observed concentration}
#'   \item{median_concentration}{Median observed concentration}
#'   \item{mean_concentration}{Mean observed concentration}
#'   \item{maximum_concentration}{Maximum observed concentration}
#'   \item{standard_deviation}{Sample standard deviation}
#'   \item{coefficient_of_variation}{
#'      Standard deviation divided by mean concentration
#'   }
#'   \item{trend_method}{Trend method used}
#'   \item{slope}{Trend slope estimate}
#'   \item{p_value}{Trend p-value}
#'   \item{conf_low}{Lower confidence interval}
#'   \item{conf_high}{Upper confidence interval}
#'   \item{significant}{TRUE if p-value <= alpha}
#'   \item{trend_direction}{
#'      Increasing,
#'      Decreasing,
#'      Stable
#'   }
#' }
#'
#' @export
trend_summary <- function(
  df,
  x = "sample_date",
  y = "analysis_result",
  method = "mann_kendall",
  conf.level = 0.95,
  location_id = "location_id",
  param_name = "param_name"
) {
  alpha <- 1 - conf.level

  group_cols <- c(
    location_id,
    param_name
  )

  df %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(group_cols)
      )
    ) %>%
    dplyr::group_modify(
      ~ {
        n <- nrow(.x)

        # Date coverage statistics
        date_values <- .x[[x]]
        date_values <- date_values[
          !is.na(date_values)
        ]

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

        # Concentration statistics
        result_values <- .x[[y]]

        result_values <- result_values[
          !is.na(result_values)
        ]

        if (length(result_values) > 0) {
          minimum_concentration <- min(
            result_values
          )

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
          df = .x,
          x = x,
          y = y,
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

        slope <- if (is.null(tr$slope)) {
          NA_real_
        } else {
          tr$slope
        }

        p_value <- if (is.null(tr$p_value)) {
          NA_real_
        } else {
          tr$p_value
        }

        conf_low <- if (is.null(tr$conf_low)) {
          NA_real_
        } else {
          tr$conf_low
        }

        conf_high <- if (is.null(tr$conf_high)) {
          NA_real_
        } else {
          tr$conf_high
        }

        trend_direction <- dplyr::case_when(
          is.na(slope) ~ NA_character_,
          slope > 0 ~ "Increasing",
          slope < 0 ~ "Decreasing",
          TRUE ~ "Stable"
        )

        significant <- dplyr::case_when(
          is.na(p_value) ~ NA,
          TRUE ~ p_value <= alpha
        )

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
          significant = significant,
          trend_direction = trend_direction
        )
      }
    ) %>%
    dplyr::ungroup()
}

#' Extract Theil-Sen trend statistics
#'
#' @param df dataframe
#' @param x response variable
#' @param y time variable
#' @param conf.level confidence level
#' @param ... additional arguments passed to kendallTrendTest
#'
#' @export

get_theilsen <- function(
  df,
  x = "analysis_result",
  y = "sample_date",
  conf.level = 0.95,
  ...
) {
  fit <- EnvStats::kendallTrendTest(
    x = df[[x]],
    y = df[[y]],
    conf.level = conf.level,
    ...
  )

  list(
    slope = unname(fit$estimate),
    p_value = fit$p.value,
    conf_low = fit$conf.int[1],
    conf_high = fit$conf.int[2]
  )
}

#' Kendall Trend Test
#'
#' @param df dataframe
#' @param conf.level confidence level
#'
#' @export

kendall_trend <- function(
  df,
  conf.level = 0.95
) {
  EnvStats::kendallTrendTest(
    analysis_result ~ sample_date,
    data = df,
    conf.level = conf.level
  )
}


#' Compute Trend Information
#'
#' Internal helper used by plotting functions.
#'
#' @noRd

.compute_trend <- function(
  df,
  x = "sample_date",
  y = "analysis_result",
  method = "none",
  conf.level = 0.95
) {
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

  z <- stats::qnorm(1 - (1 - conf.level) / 2)

  df <- df |>
    dplyr::filter(
      !is.na(.data[[x]]),
      !is.na(.data[[y]])
    ) |>
    dplyr::arrange(.data[[x]])

  if (nrow(df) < 3) {
    return(NULL)
  }

  if (method == "none") {
    return(NULL)
  }

  if (method == "lm") {
    fit <- stats::lm(
      stats::as.formula(
        paste(y, "~", x)
      ),
      data = df
    )

    pred <- stats::predict(
      fit,
      interval = "confidence",
      level = conf.level
    )

    slope_ci <- stats::confint(
      fit,
      level = conf.level
    )

    trend_df <- dplyr::bind_cols(
      df,
      as.data.frame(pred)
    ) %>%
      dplyr::arrange(.data[[x]])

    return(
      list(
        method = method,
        model = fit,
        slope = unname(stats::coef(fit)[2]),
        p_value = summary(fit)$coefficients[2, 4],
        conf_low = slope_ci[2, 1],
        conf_high = slope_ci[2, 2],
        trend_df = trend_df
      )
    )
  }

  if (method == "loess") {
    fit <- stats::loess(
      stats::as.formula(
        paste(y, "~", x)
      ),
      data = df
    )

    pred <- stats::predict(
      fit,
      se = TRUE
    )

    trend_df <- df %>%
      dplyr::mutate(
        fit = pred$fit,
        se = pred$se.fit,
        lwr = fit - z * se,
        upr = fit + z * se
      ) %>%
      dplyr::arrange(.data[[x]])

    return(
      list(
        method = method,
        model = fit,
        trend_df = trend_df
      )
    )
  }

  if (method == "gam") {
    fit <- mgcv::gam(
      stats::as.formula(
        paste0(
          y,
          " ~ s(",
          x,
          ")"
        )
      ),
      data = df
    )

    pred <- stats::predict(
      fit,
      se.fit = TRUE
    )

    trend_df <- df %>%
      dplyr::mutate(
        fit = pred$fit,
        se = pred$se.fit,
        lwr = fit - z * se,
        upr = fit + z * se
      ) %>%
      dplyr::arrange(.data[[x]])

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
      x = df[[y]],
      y = df[[x]],
      conf.level = conf.level
    )

    x_num <- as.numeric(df[[x]])

    x0 <- min(x_num)

    slope <- unname(fit$estimate)

    intercept <- stats::median(
      df[[y]] -
        slope * (x_num - x0),
      na.rm = TRUE
    )

    trend_df <- df %>%
      dplyr::mutate(
        fit = intercept +
          slope * (x_num - x0)
      ) %>%
      dplyr::arrange(.data[[x]])

    return(
      list(
        method = method,
        p_value = fit$p.value,
        slope = slope,
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
  x = "sample_date",
  y = "analysis_result",
  trend = "none",
  trend_ci = FALSE,
  trend_conf_level = 0.95,
  trend_label = "none",
  trend_label_sig_only = TRUE,
  plot_type = "split",
  group_var = "param_name",
  location_id = "location_id",
  param_name = "param_name"
) {
  if (trend == "none") {
    return(p)
  }

  if (
    trend %in%
      c(
        "loess",
        "gam"
      )
  ) {
    trend_label <- "none"
  }

  if (plot_type == "split") {
    tr <- .compute_trend(
      df = df,
      x = x,
      y = y,
      method = trend,
      conf.level = trend_conf_level
    )

    if (is.null(tr)) {
      return(p)
    }

    if (
      trend_ci &&
        all(
          c("lwr", "upr") %in%
            names(tr$trend_df)
        )
    ) {
      p <- p +
        ggplot2::geom_ribbon(
          data = tr$trend_df,
          ggplot2::aes(
            x = .data[[x]],
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
          x = .data[[x]],
          y = fit
        ),
        colour = "red",
        linewidth = 1,
        inherit.aes = FALSE
      )
    if (trend_label != "none") {
      alpha <- 1 - trend_conf_level

      label_df <- tr$trend_df %>%
        dplyr::slice_tail(n = 1) %>%
        dplyr::mutate(
          .trend_p_value = tr$p_value,
          .trend_slope = tr$slope,
          .trend_conf_low = tr$conf_low,
          .trend_conf_high = tr$conf_high,

          label = dplyr::case_when(
            trend_label == "compact" &
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
              x = .data[[x]],
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
    x = x,
    y = y,
    method = trend,
    conf.level = trend_conf_level,
    location_id = location_id,
    param_name = param_name
  )

  if (nrow(trend_df) == 0) {
    return(p)
  }

  if (
    trend_ci &&
      all(
        c("lwr", "upr") %in%
          names(trend_df)
      )
  ) {
    p <- p +
      ggplot2::geom_ribbon(
        data = trend_df,
        ggplot2::aes(
          x = .data[[x]],
          ymin = lwr,
          ymax = upr,
          group = interaction(
            .data[[location_id]],
            .data[[param_name]]
          ),
          fill = .data[[group_var]]
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
        x = .data[[x]],
        y = fit,
        group = interaction(
          .data[[location_id]],
          .data[[param_name]]
        ),
        colour = .data[[group_var]]
      ),
      linewidth = 1,
      inherit.aes = FALSE
    )

  labels <- .compute_group_labels(
    trend_df = trend_df,
    x = x,
    location_id = location_id,
    param_name = param_name,
    trend = trend,
    trend_label = trend_label
  )

  if (
    trend_label_sig_only &&
      trend == "mann_kendall"
  ) {
    alpha <- 1 - trend_conf_level

    labels <- labels %>%
      dplyr::filter(
        !is.na(.trend_p_value),
        .trend_p_value <= alpha
      )
  }

  if (
    trend_label != "none" &&
      nrow(labels) > 0
  ) {
    p <- p +
      ggplot2::geom_text(
        data = labels,
        ggplot2::aes(
          x = label_x,
          y = label_y,
          label = label,
          colour = .data[[group_var]]
        ),
        size = 3,
        hjust = -0.05,
        inherit.aes = FALSE,
        show.legend = FALSE
      )
  }

  p
}
#' Compute trend models for well/constituent pairs
#'
#' Internal helper used by plotting functions.
#' Trends are always computed on unique
#' location_id + param_name combinations,
#' regardless of plot faceting.
#'
#' @param df groundwater data in tidy format
#' @param x date column
#' @param y result column
#' @param method trend method
#' @param conf.level confidence level
#' @param location_id location identifier column
#' @param param_name parameter identifier column
#'
#' @return dataframe containing fitted trend values and
#' associated trend statistics for plotting
#'
#' @noRd

.compute_group_trends <- function(
  df,
  x = "sample_date",
  y = "analysis_result",
  method = "mann_kendall",
  conf.level = 0.95,
  location_id = "location_id",
  param_name = "param_name"
) {
  group_cols <- c(
    location_id,
    param_name
  )

  df %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(group_cols)
      )
    ) %>%
    dplyr::group_modify(
      ~ {
        tr <- .compute_trend(
          df = .x,
          x = x,
          y = y,
          method = method,
          conf.level = conf.level
        )

        if (is.null(tr)) {
          return(tibble::tibble())
        }

        trend_df <- tr$trend_df

        trend_df$.trend_method <- method
        trend_df$.trend_slope <- if (is.null(tr$slope)) {
          NA_real_
        } else {
          tr$slope
        }
        trend_df$.trend_p_value <- if (is.null(tr$p_value)) {
          NA_real_
        } else {
          tr$p_value
        }
        trend_df$.trend_conf_low <- if (is.null(tr$conf_low)) {
          NA_real_
        } else {
          tr$conf_low
        }
        trend_df$.trend_conf_high <- if (is.null(tr$conf_high)) {
          NA_real_
        } else {
          tr$conf_high
        }

        trend_df
      }
    ) %>%
    dplyr::ungroup()
}

#' Compute trend group labels for series_plot
#'
#' @noRd
.compute_group_labels <- function(
  trend_df,
  x = "sample_date",
  trend = "mann_kendall",
  trend_label = "compact",
  location_id = "location_id",
  param_name = "param_name"
) {
  trend_df %>%
    dplyr::group_by(
      .data[[location_id]],
      .data[[param_name]]
    ) %>%
    dplyr::arrange(.data[[x]]) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      label_x = .data[[x]],
      label_y = fit,

      label = dplyr::case_when(
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
