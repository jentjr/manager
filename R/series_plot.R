#' Environmental Time Series Plot
#'
#' Creates time-series plots for environmental monitoring data.
#'
#' ...
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional arguments passed to methods.
#'
#' @export
series_plot <- function(df, ...) {
  UseMethod("series_plot")
}

#' @rdname series_plot
#' @param location Location column.
#' @param date Sample date column.
#' @param analyte Analyte column.
#' @param result Analytical result column.
#' @param unit Reporting unit column.
#' @param reporting_flag Reporting qualifier column.
#' @param reporting_limit Reporting limit column.
#' @param detection_limit Detection limit column.
#' @export
series_plot.data.frame <- function(
  df,
  location,
  date,
  analyte,
  result,
  unit = NULL,
  reporting_flag = NULL,
  reporting_limit = NULL,
  detection_limit = NULL,
  ...
) {
  df <- env_schema(
    df,
    location = {{ location }},
    date = {{ date }},
    analyte = {{ analyte }},
    result = {{ result }},
    unit = {{ unit }},
    reporting_flag = {{ reporting_flag }},
    reporting_limit = {{ reporting_limit }},
    detection_limit = {{ detection_limit }}
  )

  series_plot(
    df,
    ...
  )
}

#' @rdname series_plot
#'
#' @section env_schema Method:
#' For `env_schema` objects, `series_plot()` automatically uses
#' the schema mappings and standardized analytical fields:
#'
#' \itemize{
#'   \item `.analysis_result`
#'   \item `.analysis_unit`
#'   \item `.censored`
#'   \item the mapped date variable returned by `date_var()`
#'   \item the mapped location variable returned by `location_var()`
#'   \item the mapped analyte variable returned by `analyte_var()`
#' }
#'
#' This allows plots to be generated consistently regardless of
#' the original source column names or schema mappings used by
#' the input data.
#'
#' @export
series_plot.env_schema <- function(
  df,
  scale = "free",
  scale_y_trans = "identity",
  trend = "none",
  trend_ci = FALSE,
  trend_conf_level = 0.95,
  trend_label = c(
    "none",
    "compact",
    "full"
  ),
  trend_label_sig_only = TRUE,
  background = NULL,
  standards = TRUE,
  short_name = FALSE,
  pnt = 3,
  ncol = NULL,
  plot_type = c(
    "split",
    "combined"
  )
) {
  validate_env_schema(df)

  if (nrow(df) == 0) {
    rlang::abort(
      "No observations available to plot."
    )
  }

  date_col <- date_var(df)

  location_col <- location_var(df)

  analyte_col <- analyte_var(df)

  plot_type <- match.arg(plot_type)

  trend_label <- match.arg(trend_label)

  valid_scales <- c(
    "fixed",
    "free",
    "free_x",
    "free_y"
  )

  .validate_choice(scale, valid_scales, "scale")

  valid_scale_y_trans <- c(
    "asn",
    "atanh",
    "boxcox",
    "exp",
    "identity",
    "log",
    "log10",
    "log1p",
    "log2",
    "logit",
    "probability",
    "probit",
    "reciprocal",
    "reverse",
    "sqrt"
  )

  .validate_choice(scale_y_trans, valid_scale_y_trans, "scale_y_trans")

  valid_trends <- c(
    "none",
    "lm",
    "loess",
    "gam",
    "mann_kendall"
  )

  .validate_choice(trend, valid_trends, "trend")

  if (plot_type == "split") {
    return(
      split(
        df,
        df[[analyte_col]]
      ) |>
        purrr::imap(
          ~ .series_plot(
            .x,
            scale = scale,
            scale_y_trans = scale_y_trans,
            trend = trend,
            trend_ci = trend_ci,
            trend_conf_level = trend_conf_level,
            trend_label = trend_label,
            trend_label_sig_only = trend_label_sig_only,
            background = background,
            standards = standards,
            short_name = short_name,
            pnt = pnt,
            ncol = ncol,
            plot_type = plot_type
          )
        )
    )
  }

  .series_plot(
    df = df,
    scale = scale,
    scale_y_trans = scale_y_trans,
    trend = trend,
    trend_ci = trend_ci,
    trend_conf_level = trend_conf_level,
    trend_label = trend_label,
    trend_label_sig_only = trend_label_sig_only,
    background = background,
    standards = standards,
    short_name = short_name,
    pnt = pnt,
    ncol = ncol,
    plot_type = plot_type
  )
}

#' Helper function for plotting time series
#'
#' @noRd
.series_plot <- function(
  df,
  scale = "free",
  scale_y_trans = "identity",
  trend = "none",
  trend_ci = FALSE,
  trend_conf_level = 0.95,
  trend_label = c(
    "none",
    "compact",
    "full"
  ),
  trend_label_sig_only = TRUE,
  background = NULL,
  standards = TRUE,
  short_name = FALSE,
  pnt = 3,
  ncol = NULL,
  plot_type = "split"
) {
  date_col <- date_var(df)

  location_col <- location_var(df)

  analyte_col <- analyte_var(df)

  trend_label <- match.arg(trend_label)

  if (plot_type == "combined") {
    standards <- FALSE
  }

  df <- df |>
    dplyr::arrange(.data[[date_col]]) |>
    dplyr::mutate(
      non_detect = factor(
        dplyr::if_else(
          .censored %in% TRUE,
          "Non-detect",
          "Detected"
        ),
        levels = c(
          "Detected",
          "Non-detect"
        )
      ),
      .trend_group = interaction(
        .data[[location_col]],
        .data[[analyte_col]],
        drop = TRUE
      )
    )

  units <- unique(
    stats::na.omit(
      analysis_unit(df)
    )
  )

  y_label <-
    if (length(units) == 1) {
      paste0(
        "Analysis Result (",
        units,
        ")"
      )
    } else {
      "Analysis Result"
    }

  if (scale_y_trans != "identity") {
    y_label <- paste(
      y_label,
      paste0(
        "Scale: ",
        scale_y_trans
      ),
      sep = "\n"
    )
  }

  mapping <-
    if (plot_type == "combined") {
      ggplot2::aes(
        x = .data[[date_col]],
        y = .analysis_result,
        colour = .data[[analyte_col]],
        group = .trend_group
      )
    } else {
      ggplot2::aes(
        x = .data[[date_col]],
        y = .analysis_result
      )
    }

  p <- ggplot2::ggplot(
    df,
    mapping
  ) +
    ggplot2::geom_path(
      linewidth = 0.4
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        shape = non_detect
      ),
      size = pnt
    ) +
    ggplot2::scale_shape_manual(
      name = "Detection",
      values = c(
        "Detected" = 16,
        "Non-detect" = 1
      )
    ) +
    ggplot2::scale_x_datetime(
      labels = scales::date_format("%Y"),
      expand = ggplot2::expansion(
        mult = c(0.01, 0.15)
      )
    ) +
    ggplot2::scale_y_continuous(
      trans = scale_y_trans,
      breaks = scales::pretty_breaks(),
      labels = prettyNum
    ) +
    ggplot2::labs(
      x = "Sample Date",
      y = y_label
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.margin = grid::unit(
        c(1, 1, 1, 1),
        "lines"
      ),
      axis.title.x = ggplot2::element_text(
        size = 15,
        vjust = -0.3
      ),
      axis.title.y = ggplot2::element_text(
        size = 15,
        vjust = 0.3
      ),
      plot.title = ggplot2::element_text(
        hjust = 0.5
      )
    )

  if (plot_type == "combined") {
    p <- p +
      ggplot2::guides(
        colour = ggplot2::guide_legend(
          title = "Analyte"
        ),
        shape = ggplot2::guide_legend(
          override.aes = list(
            linetype = 0
          )
        )
      )
  } else {
    p <- p +
      ggplot2::guides(
        colour = "none",
        shape = ggplot2::guide_legend(
          override.aes = list(
            linetype = 0
          )
        )
      )
  }

  p <- .add_trend(
    p = p,
    df = df,
    trend = trend,
    trend_ci = trend_ci,
    trend_conf_level = trend_conf_level,
    trend_label = trend_label,
    trend_label_sig_only = trend_label_sig_only,
    plot_type = plot_type
  )

  p <- p +
    ggplot2::facet_wrap(
      ggplot2::vars(!!rlang::sym(location_col)),
      scales = scale,
      ncol = ncol
    )

  if (plot_type == "split") {
    plot_label <- dplyr::first(
      if (
        short_name &&
          ".short_analyte_label" %in% names(df)
      ) {
        stats::na.omit(df$.short_analyte_label)
      } else {
        stats::na.omit(df$.analyte_label)
      }
    )

    p <- p +
      ggplot2::ggtitle(
        sprintf(
          "Time Series Plot for %s",
          plot_label
        )
      )
  }

  if (!is.null(background)) {
    shaded_dates <- data.frame(
      xmin = background[1],
      xmax = background[2],
      ymin = -Inf,
      ymax = Inf,
      years = "Background"
    )

    p <- p +
      ggplot2::geom_rect(
        shaded_dates,
        mapping = ggplot2::aes(
          xmin = xmin,
          xmax = xmax,
          ymin = ymin,
          ymax = ymax,
          fill = years
        ),
        alpha = 0.3,
        inherit.aes = FALSE,
        fill = "lightblue"
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          title = "Date Ranges",
          override.aes = list(
            linetype = 0
          )
        )
      )
  }

  p <- .add_standards(
    plot = p,
    df = df,
    standards = standards
  )

  p
}
