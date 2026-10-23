#' Plot environmental chemistry time series
#'
#' @param df Environmental data frame or env_schema object.
#' @param location Location column.
#' @param date Date column.
#' @param analyte Analyte column.
#' @param result Result column.
#' @param unit Unit column.
#' @param reporting_flag Reporting qualifier column.
#' @param reporting_limit Reporting limit column.
#' @param detection_limit Detection limit column.
#' @param scale Facet scales.
#' @param scale_y_trans Y-axis transformation.
#' @param trend Trend method.
#' @param background Optional background date range.
#' @param standards Logical.
#' @param short_name Logical.
#' @param pnt Point size.
#' @param ncol Number of facet columns.
#' @param plot_type "split" or "combined".
#'
#' @export
series_plot <- function(
  df,
  location = NULL,
  date = NULL,
  analyte = NULL,
  result = NULL,
  unit = NULL,
  reporting_flag = NULL,
  reporting_limit = NULL,
  detection_limit = NULL,
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
  df <- as_env_schema(
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

  validate_env_schema(df)

  plot_type <- match.arg(plot_type)

  trend_label <- match.arg(trend_label)

  valid_scales <- c(
    "fixed",
    "free",
    "free_x",
    "free_y"
  )

  if (!scale %in% valid_scales) {
    stop(
      "`scale` must be one of: ",
      paste(valid_scales, collapse = ", "),
      call. = FALSE
    )
  }

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

  if (!scale_y_trans %in% valid_scale_y_trans) {
    stop(
      "`scale_y_trans` must be one of: ",
      paste(valid_scale_y_trans, collapse = ", "),
      call. = FALSE
    )
  }

  valid_trends <- c(
    "none",
    "lm",
    "loess",
    "gam",
    "mann_kendall"
  )

  if (!trend %in% valid_trends) {
    stop(
      "`trend` must be one of: ",
      paste(valid_trends, collapse = ", "),
      call. = FALSE
    )
  }

  if (plot_type == "split") {
    return(
      split(
        df,
        df$.analyte
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
  validate_env_schema(df)

  trend_label <- match.arg(trend_label)

  if (plot_type == "combined") {
    standards <- FALSE
  }

  df <- df |>
    dplyr::arrange(.date) |>
    dplyr::mutate(
      non_detect = factor(
        ifelse(
          .censored,
          "Non-detect",
          "Detected"
        ),
        levels = c(
          "Detected",
          "Non-detect"
        )
      ),
      .trend_group = interaction(
        .location,
        .analyte,
        drop = TRUE
      )
    )

  units <- unique(
    stats::na.omit(
      df$.analysis_unit
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
        x = .date,
        y = .analysis_result,
        colour = .analyte,
        group = .trend_group
      )
    } else {
      ggplot2::aes(
        x = .date,
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
      ggplot2::vars(.location),
      scales = scale,
      ncol = ncol
    )

  if (plot_type == "split") {
    plot_label <- if (
      short_name &&
        ".short_analyte_label" %in% names(df)
    ) {
      unique(df$.short_analyte_label)[1]
    } else {
      unique(df$.analyte_label)[1]
    }

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
