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

#' Environmental Boxplots
#'
#' Creates boxplots for environmental monitoring data. Boxplots are
#' generated separately for each analyte and display the distribution
#' of analytical results across sampling locations.
#'
#' The function is implemented as an S3 generic and supports both
#' raw data frames and `env_schema` objects.
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional arguments passed to methods.
#'
#' @section Schema-aware plotting:
#'
#' `boxplot()` is schema-aware and relies on accessor functions rather
#' than hard-coded column names. Users are encouraged to convert raw
#' environmental datasets to `env_schema` objects prior to analysis
#' and visualization.
#'
#' @return
#' A named list of plots, with one plot generated for each analyte.
#' List names correspond to the analyte values in the mapped analyte
#' column.
#'
#' @examples
#' data(gw)
#'
#' plots <- boxplot(gw)
#'
#' names(plots)
#'
#' plots[[1]]
#'
#' data(gw_data)
#'
#' plots <- boxplot(
#'   gw_data,
#'   location = location_id,
#'   analyte = param_name,
#'   result = analysis_result
#' )
#'
#' @export
boxplot <- function(df, ...) {
  UseMethod("boxplot")
}

#' @rdname boxplot
#'
#' @param location Location column.
#' @param analyte Analyte column.
#' @param result Analytical result column.
#' @param date Sample date column.
#' @param reporting_flag Reporting qualifier column.
#' @param unit Reporting unit column.
#'
#' @details
#' For data frames, `boxplot()` first converts the input data to an
#' `env_schema` object using the supplied column mappings and then
#' dispatches to [boxplot.env_schema()].
#'
#' @export
boxplot.data.frame <- function(
  df,
  location,
  analyte,
  result,
  date = NULL,
  reporting_flag = NULL,
  unit = NULL,
  ...
) {
  df <- env_schema(
    df,
    location = {{ location }},
    analyte = {{ analyte }},
    result = {{ result }},
    date = {{ date }},
    reporting_flag = {{ reporting_flag }},
    unit = {{ unit }}
  )

  boxplot(df, ...)
}

.boxplot_build <- function(
  df,
  location,
  analyte,
  result,
  reporting_flag,
  coef = 1.5,
  scale_y_trans = "identity",
  show_points = TRUE,
  fill = NULL,
  notch = TRUE,
  plot_ci = FALSE,
  standards = FALSE,
  pnt = 2,
  short_name = FALSE,
  coord_flip = FALSE,
  legend_title = NULL,
  explanation = NULL,
  add_explanation = FALSE
) {
  if (!is.null(fill) && is.null(legend_title)) {
    legend_title <- fill
  }

  df <- .prepare_boxplot_data(
    df = df,
    reporting_flag = reporting_flag
  )

  if (notch) {
    small_n <- df |>
      dplyr::count(.data[[location]]) |>
      dplyr::filter(n < 5)

    if (nrow(small_n) > 0) {
      rlang::warn(
        paste(
          "Some groups contain fewer than",
          "5 observations. Notched",
          "boxplots may be unreliable."
        )
      )
    }
  }

  title <- .get_plot_title(
    df = df,
    analyte = analyte,
    short_name = short_name
  )

  unit_lbl <- .get_unit_label(df)

  p <- .create_boxplot_base(
    df = df,
    x = location,
    y = result,
    fill = fill,
    scale_y_trans = scale_y_trans,
    unit_lbl = unit_lbl,
    title = title,
    legend_title = legend_title
  )

  p <- .add_boxplot_layers(
    plot = p,
    coef = coef,
    notch = notch,
    plot_ci = plot_ci,
    pnt = pnt
  )

  p <- .add_points(
    plot = p,
    show_points = show_points,
    pnt = pnt
  )

  p <- .add_standards(
    plot = p,
    df = df,
    standards = standards
  )

  if (
    !is.null(fill) &&
      requireNamespace("viridis", quietly = TRUE)
  ) {
    p <- p +
      viridis::scale_fill_viridis(
        discrete = TRUE,
        name = legend_title
      )
  }

  if (coord_flip) {
    p <- p + ggplot2::coord_flip()
  }

  if (!add_explanation) {
    return(
      p +
        ggplot2::theme(
          legend.position = "right"
        )
    )
  }

  legend <- tryCatch(
    cowplot::get_legend(p),
    error = function(e) NULL
  )

  legend_panel <-
    if (is.null(legend)) {
      explanation
    } else {
      cowplot::plot_grid(
        explanation,
        legend,
        ncol = 1,
        rel_heights = c(1.5, 1)
      )
    }

  cowplot::plot_grid(
    p +
      ggplot2::theme(
        legend.position = "none"
      ),
    legend_panel,
    rel_widths = c(4, 1)
  )
}

#' @rdname boxplot
#'
#' @param coef Length of whiskers expressed as a multiple of the
#'   interquartile range.
#' @param scale_y_trans Transformation applied to the y-axis.
#' @param show_points Logical. If `TRUE`, individual observations are
#'   displayed using either a beeswarm or jittered scatter overlay.
#' @param fill Optional column name used to group and color boxplots.
#'   Must be supplied as a character string naming a column in `x`.
#' @param notch Logical. If `TRUE`, notched boxplots are produced.
#' @param plot_ci Logical. If `TRUE`, bootstrap confidence intervals
#'   for the mean are displayed.
#' @param standards Logical. If `TRUE`, applicable standards are
#'   displayed on the plot.
#' @param pnt Point size used for observations and summaries.
#' @param short_name Logical. If `TRUE`, titles use
#'   `short_analyte_label()` values when available; otherwise
#'   `analyte_label()` values are used.
#' @param coord_flip Logical. If `TRUE`, axes are flipped.
#' @param legend_title Optional legend title.
#' @param add_explanation Logical. If `TRUE`, an explanatory panel
#'   describing boxplot components is added.
#'
#' @section env_schema Method:
#'
#' Analyte grouping is determined by the mapped analyte variable
#' returned by `analyte_var()`, not by internal standardized
#' analyte metadata columns.
#'
#' For `env_schema` objects, `boxplot()` automatically uses the
#' schema mappings and standardized analytical fields:
#'
#' \itemize{
#'   \item the mapped location variable returned by `location_var()`
#'   \item the mapped analyte variable returned by `analyte_var()`
#'   \item the mapped result variable returned by `result_var()`
#'   \item the mapped reporting flag variable returned by
#'     `reporting_flag_var()`
#'   \item `analysis_unit()` for axis labeling
#'   \item `analyte_label()` and `short_analyte_label()` for titles
#' }
#'
#' This allows boxplots to be generated consistently regardless of
#' the original source column names used by the underlying dataset.
#'
#' @returns
#' A named list of plot objects. One boxplot is produced for each
#' analyte in the mapped analyte column.
#'
#'
#' @export
boxplot.env_schema <- function(
  x,
  coef = 1.5,
  scale_y_trans = "identity",
  show_points = FALSE,
  fill = NULL,
  notch = FALSE,
  plot_ci = FALSE,
  standards = FALSE,
  pnt = 2,
  short_name = FALSE,
  coord_flip = FALSE,
  legend_title = NULL,
  add_explanation = FALSE,
  ...
) {
  validate_env_schema(x)

  if (nrow(x) == 0) {
    rlang::abort(
      "No observations available to plot."
    )
  }

  .validate_boxplot_args(
    df = x,
    fill = fill,
    scale_y_trans = scale_y_trans,
    coef = coef,
    notch = notch,
    show_points = show_points,
    short_name = short_name,
    coord_flip = coord_flip,
    plot_ci = plot_ci,
    standards = standards,
    legend_title = legend_title,
    add_explanation = add_explanation
  )

  explanation <- NULL

  if (isTRUE(add_explanation)) {
    explanation <- .ggplot_box_legend()
  }

  location <- location_var(x)
  analyte <- analyte_var(x)
  result <- result_var(x)
  reporting_flag <- reporting_flag_var(x)

  split(x, as.character(x[[analyte]])) |>
    purrr::imap(
      ~ .boxplot_build(
        df = .x,
        location = location,
        analyte = analyte,
        result = result,
        reporting_flag = reporting_flag,
        coef = coef,
        scale_y_trans = scale_y_trans,
        show_points = show_points,
        fill = fill,
        notch = notch,
        plot_ci = plot_ci,
        standards = standards,
        pnt = pnt,
        short_name = short_name,
        coord_flip = coord_flip,
        legend_title = legend_title,
        add_explanation = add_explanation,
        explanation = explanation
      )
    )
}

#' Prepare boxplot plotting data
#'
#' Creates the derived `non_detect` variable used to distinguish
#' detected and non-detect observations in point overlays.
#'
#' @param df An env_schema subset.
#' @param reporting_flag Reporting flag column name.
#'
#' @return A data frame with a `non_detect` factor column.
#'
#' @noRd
.prepare_boxplot_data <- function(
  df,
  reporting_flag
) {
  if (
    is.null(reporting_flag) ||
      !reporting_flag %in% names(df)
  ) {
    df$non_detect <- factor(
      rep("Detected", nrow(df)),
      levels = c(
        "Detected",
        "Non-detect"
      )
    )

    return(df)
  }

  df |>
    dplyr::mutate(
      non_detect = factor(
        dplyr::if_else(
          toupper(
            trimws(
              as.character(
                .data[[reporting_flag]]
              )
            )
          ) %in%
            c(
              "<",
              "ND",
              "U"
            ),
          "Non-detect",
          "Detected",
          missing = "Detected"
        ),
        levels = c(
          "Detected",
          "Non-detect"
        )
      )
    )
}


#' Helper function to plot boxplots for groundwater data
#'
#' @noRd

.create_boxplot_base <- function(
  df,
  x,
  y,
  fill,
  scale_y_trans,
  unit_lbl,
  title,
  legend_title
) {
  if (!is.null(fill)) {
    mapping <- ggplot2::aes(
      x = .data[[x]],
      y = .data[[y]],
      fill = .data[[fill]]
    )
  } else {
    mapping <- ggplot2::aes(
      x = .data[[x]],
      y = .data[[y]]
    )
  }

  y_label <- "Analysis Result"

  if (!identical(unit_lbl, "")) {
    y_label <- paste0(
      y_label,
      " (",
      unit_lbl,
      ")"
    )
  }

  if (scale_y_trans != "identity") {
    y_label <- paste0(
      y_label,
      "\nScale: ",
      scale_y_trans
    )
  }

  ggplot2::ggplot(df, mapping) +
    .theme_manager() +
    EnvStats::stat_n_text(vjust = 1.5) +
    ggplot2::labs(
      title = title,
      x = "Location ID",
      y = y_label,
      fill = legend_title
    ) +
    ggplot2::scale_y_continuous(
      trans = scale_y_trans,
      breaks = scales::pretty_breaks(),
      labels = prettyNum,
      sec.axis = ggplot2::dup_axis(name = NULL)
    ) +
    ggplot2::guides(
      shape = ggplot2::guide_legend("Detection"),
      color = ggplot2::guide_legend(
        title = "Standard"
      ),
      linetype = ggplot2::guide_legend(
        title = "Standard"
      )
    )
}

#' Helper function to plot boxplots for groundwater data
#'
#' @noRd

.add_boxplot_layers <- function(
  plot,
  coef,
  notch,
  plot_ci,
  pnt
) {
  if (isTRUE(plot_ci)) {
    boot_ci <- function(x) {
      ci <- Hmisc::smean.cl.boot(x)

      data.frame(
        y = ci[1],
        ymin = ci[2],
        ymax = ci[3]
      )
    }

    plot +
      ggplot2::geom_boxplot(
        coef = coef,
        notch = notch,
        outlier.size = pnt
      ) +
      ggplot2::stat_summary(
        fun.data = boot_ci,
        geom = "errorbar",
        colour = "red",
        width = 0.25
      ) +
      ggplot2::stat_summary(
        fun = mean,
        geom = "point",
        colour = "red",
        size = pnt
      )
  } else {
    plot +
      ggplot2::geom_boxplot(
        coef = coef,
        notch = notch,
        outlier.size = pnt
      )
  }
}

#' Helper function to plot boxplots for groundwater data
#'
#' @noRd

.add_points <- function(
  plot,
  show_points,
  pnt
) {
  if (!isTRUE(show_points)) {
    return(plot)
  }

  if (!"non_detect" %in% names(plot$data)) {
    return(plot)
  }

  if (!any(!is.na(plot$data$non_detect))) {
    return(plot)
  }

  plot <- plot +
    ggplot2::scale_shape_manual(
      values = c(
        "Detected" = 16,
        "Non-detect" = 1
      ),
      drop = FALSE
    ) +
    ggplot2::guides(
      shape = ggplot2::guide_legend("Detection")
    )

  if (requireNamespace("ggbeeswarm", quietly = TRUE)) {
    plot <- plot +
      ggbeeswarm::geom_beeswarm(
        ggplot2::aes(shape = non_detect),
        size = pnt,
        orientation = "x"
      )
  } else {
    plot <- plot +
      ggplot2::geom_jitter(
        ggplot2::aes(shape = non_detect),
        width = 0.20,
        size = pnt
      )
  }

  plot
}

#' Construct boxplot title
#'
#' Uses `short_analyte_label` when available and falls back to `analyte_label`
#' column when metadata are unavailable.
#'
#' @noRd
.get_plot_title <- function(
  df,
  analyte,
  short_name = FALSE
) {
  fallback <- dplyr::first(
    df[[analyte]]
  )

  labels <- if (short_name) {
    short_analyte_label(df)
  } else {
    analyte_label(df)
  }

  labels <- stats::na.omit(labels)

  name <- dplyr::first(
    labels,
    default = fallback
  )

  paste("Boxplot for", name)
}
#' Extract analysis unit label
#'
#' Returns the first non-missing analytical unit associated with the
#' dataset.
#'
#' @noRd
.get_unit_label <- function(df) {
  units <- stats::na.omit(
    analysis_unit(df)
  )

  if (length(units) > 0) {
    return(units[[1]])
  }

  ""
}

#' Helper function to plot boxplots for groundwater data
#'
#' @noRd

.theme_manager <- function(base_family = "sans", ...) {
  ggplot2::theme_bw(base_family = base_family, ...) +
    ggplot2::theme(
      legend.background = ggplot2::element_rect(),
      panel.grid = ggplot2::element_blank(),
      axis.ticks.length = ggplot2::unit(-0.05, "in"),
      plot.margin = ggplot2::margin(
        t = 1,
        r = 1,
        b = 1,
        l = 1,
        unit = "lines"
      ),
      axis.title.x = ggplot2::element_text(
        vjust = -1.5,
        size = 12
      ),
      axis.text.x = ggplot2::element_text(
        angle = 90,
        size = 10,
        margin = ggplot2::margin(
          t = 0.3,
          r = 0.3,
          b = 0.3,
          l = 0.3,
          unit = "cm"
        )
      ),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(vjust = 0.5, size = 12),
      axis.title.y.right = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(
        margin = ggplot2::margin(
          t = 0.3,
          r = 0.3,
          b = 0.3,
          l = 0.3,
          unit = "cm"
        )
      ),
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = 16
      )
    )
}

#' Helper function to plot boxplots for groundwater data
#'
#' @noRd

.ggplot_box_legend <- function(family = "sans") {
  # Create data to use in the boxplot legend:
  set.seed(100)

  sample_df <- data.frame(parameter = "test", values = sample(500))

  # Extend the top whisker a bit:
  sample_df$values[1:100] <- 701:800
  # Make sure there's only 1 lower outlier:
  sample_df$values[1] <- -350

  # Function to calculate important values:
  ggplot2_boxplot <- function(x) {
    quartiles <- as.numeric(quantile(x, probs = c(0.25, 0.5, 0.75)))

    names(quartiles) <- c(
      "25th percentile",
      "50th percentile\n(median)",
      "75th percentile"
    )

    IQR <- diff(quartiles[c(1, 3)])

    upper_whisker <- max(x[x < (quartiles[3] + 1.5 * IQR)])
    lower_whisker <- min(x[x > (quartiles[1] - 1.5 * IQR)])

    upper_dots <- x[x > (quartiles[3] + 1.5 * IQR)]
    lower_dots <- x[x < (quartiles[1] - 1.5 * IQR)]

    return(list(
      "quartiles" = quartiles,
      "25th percentile" = as.numeric(quartiles[1]),
      "50th percentile\n(median)" = as.numeric(quartiles[2]),
      "75th percentile" = as.numeric(quartiles[3]),
      "IQR" = IQR,
      "upper_whisker" = upper_whisker,
      "lower_whisker" = lower_whisker,
      "upper_dots" = upper_dots,
      "lower_dots" = lower_dots
    ))
  }

  # Get those values:
  ggplot_output <- ggplot2_boxplot(sample_df$values)

  # Lots of text in the legend, make it smaller and consistent font:
  ggplot2::update_geom_defaults(
    "text",
    list(size = 3, hjust = 0, family = family)
  )
  # Labels don't inherit text:
  ggplot2::update_geom_defaults(
    "label",
    list(size = 3, hjust = 0, family = family)
  )

  # Create the legend:
  # The main elements of the plot (the boxplot, error bars, and count)
  # are the easy part.
  # The text describing each of those takes a lot of fiddling to
  # get the location and style just right:
  explain_plot <- ggplot2::ggplot() +
    ggplot2::stat_boxplot(
      data = sample_df,
      ggplot2::aes(x = parameter, y = values),
      geom = 'errorbar',
      width = 0.3
    ) +
    ggplot2::geom_boxplot(
      data = sample_df,
      ggplot2::aes(x = parameter, y = values),
      width = 0.3,
      fill = "lightgrey"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = 1, y = 950, label = "500"),
      hjust = 0.5
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = 1.17, y = 950, label = "Number of values"),
      fontface = "bold",
      vjust = 0.4
    ) +
    ggplot2::theme_minimal(base_size = 5, base_family = family) +
    ggplot2::geom_segment(ggplot2::aes(
      x = 2.3,
      xend = 2.3,
      y = ggplot_output[["25th percentile"]],
      yend = ggplot_output[["75th percentile"]]
    )) +
    ggplot2::geom_segment(ggplot2::aes(
      x = 1.2,
      xend = 2.3,
      y = ggplot_output[["25th percentile"]],
      yend = ggplot_output[["25th percentile"]]
    )) +
    ggplot2::geom_segment(ggplot2::aes(
      x = 1.2,
      xend = 2.3,
      y = ggplot_output[["75th percentile"]],
      yend = ggplot_output[["75th percentile"]]
    )) +
    ggplot2::geom_text(
      ggplot2::aes(x = 2.4, y = ggplot_output[["50th percentile\n(median)"]]),
      label = "Interquartile\nrange",
      fontface = "bold",
      vjust = 0.4
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(1.17, 1.17),
        y = c(
          ggplot_output[["upper_whisker"]],
          ggplot_output[["lower_whisker"]]
        ),
        label = c(
          "Largest value within 1.5 times\ninterquartile range above\n75th percentile",
          "Smallest value within 1.5 times\ninterquartile range below\n25th percentile"
        )
      ),
      fontface = "bold",
      vjust = 0.9
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(1.17),
        y = ggplot_output[["lower_dots"]],
        label = "Outside value"
      ),
      vjust = 0.5,
      fontface = "bold"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(1.9),
        y = ggplot_output[["lower_dots"]],
        label = "-Value is >1.5 times and"
      ),
      vjust = 0.5
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = 1.17,
        y = ggplot_output[["lower_dots"]],
        label = "<3 times the interquartile range\nbeyond either end of the box"
      ),
      vjust = 1.5
    ) +
    ggplot2::geom_label(
      ggplot2::aes(
        x = 1.17,
        y = ggplot_output[["quartiles"]],
        label = names(ggplot_output[["quartiles"]])
      ),
      vjust = c(0.4, 0.85, 0.4),
      fill = "white",
      linewidth = 0
    ) +
    ggplot2::ylab("") +
    ggplot2::xlab("") +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      aspect.ratio = 4 / 3,
      plot.title = ggplot2::element_text(hjust = 0.5, size = 10),
      plot.margin = ggplot2::margin(
        t = 1,
        r = 0,
        b = 0,
        l = 0,
        unit = "cm"
      )
    ) +
    ggplot2::coord_cartesian(xlim = c(1.4, 3.1), ylim = c(-600, 900)) +
    ggplot2::labs(title = "EXPLANATION")

  return(explain_plot)
}

#' Validate boxplot arguments
#'
#' @noRd
.validate_boxplot_args <- function(
  df,
  fill,
  scale_y_trans,
  coef,
  notch,
  show_points,
  short_name,
  coord_flip,
  plot_ci,
  standards,
  legend_title,
  add_explanation
) {
  .check_flag(notch, "notch")
  .check_flag(show_points, "show_points")
  .check_flag(short_name, "short_name")
  .check_flag(coord_flip, "coord_flip")
  .check_flag(plot_ci, "plot_ci")
  .check_flag(standards, "standards")
  .check_flag(add_explanation, "add_explanation")

  if (
    !is.null(fill) &&
      (!is.character(fill) || length(fill) != 1)
  ) {
    stop(
      "fill must be NULL or a single column name.",
      call. = FALSE
    )
  }

  if (
    !is.null(fill) &&
      !fill %in% names(df)
  ) {
    stop(
      "Column '",
      fill,
      "' not found.",
      call. = FALSE
    )
  }

  if (
    !is.null(legend_title) &&
      (!is.character(legend_title) ||
        length(legend_title) != 1)
  ) {
    stop(
      "legend_title must be NULL or a single character string.",
      call. = FALSE
    )
  }

  tryCatch(
    scales::as.trans(scale_y_trans),
    error = function(e) {
      stop(
        "Invalid scale_y_trans value.",
        call. = FALSE
      )
    }
  )

  result_col <- result_var(df)

  if (!result_col %in% names(df)) {
    stop(
      "Mapped result column not found.",
      call. = FALSE
    )
  }

  if (!is.numeric(df[[result_col]])) {
    stop(
      result_col,
      " must be numeric.",
      call. = FALSE
    )
  }

  if (
    scale_y_trans %in%
      c("log", "log10", "log2") &&
      any(df[[result_col]] <= 0, na.rm = TRUE)
  ) {
    stop(
      "Log transformations require all values to be positive.",
      call. = FALSE
    )
  }

  if (
    identical(scale_y_trans, "sqrt") &&
      any(df[[result_col]] < 0, na.rm = TRUE)
  ) {
    stop(
      "Sqrt transformation requires non-negative values.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(coef) ||
      length(coef) != 1 ||
      is.na(coef) ||
      coef <= 0
  ) {
    stop(
      "coef must be a single positive number.",
      call. = FALSE
    )
  }

  if (
    isTRUE(short_name) &&
      all(is.na(short_analyte_label(df)))
  ) {
    rlang::warn(
      "short analyte labels not found."
    )
  }

  invisible(TRUE)
}

#' Create a correlation plot
#'
#' @param df dataframe
#' @param location_id column of sample locations
#' @param sample_date column of sample dates
#' @param param_name column of constituents
#' @param analysis_result column of numerical results
#' @param constituents list of consituents to plot e.g. c("Iron, dissolved",
#' "Manganese, dissolved")
#' @param sample_locations list of sample locations to plot e.g.
#' c("MW-1", "MW-2")
#'
#' @export

corr_plot <- function(
  df,
  location_id = "location_id",
  sample_date = "sample_date",
  param_name = "param_name",
  analysis_result = "analysis_result",
  constituents = NULL,
  sample_locations = NULL
) {
  if (!requireNamespace("GGally", quietly = TRUE)) {
    stop(
      "GGally needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  df %>%
    .get_corr_data(
      .,
      constituents = constituents,
      sample_locations = sample_locations
    ) %>%
    GGally::ggpairs(columns = 3:ncol(.), aes(colour = location_id))
}

#' Function to create a correlation matrix plot
#'
#' @noRd

.get_corr_data <- function(
  df,
  location_id = "location_id",
  sample_date = "sample_date",
  param_name = "param_name",
  analysis_result = "analysis_result",
  constituents = NULL,
  sample_locations = NULL
) {
  df <- df %>%
    filter_(
      ~ param_name %in% constituents,
      ~ location_id %in% sample_locations
    ) %>%
    spread_(param_name, analysis_result) %>%
    group_by_(~location_id, ~sample_date) %>%
    summarise_at(vars(constituents), mean, na.rm = TRUE) %>%
    ungroup()

  return(df)
}

#' @export
piper_plot <- function(x, ...) {
  UseMethod("piper_plot")
}

#' @export
piper_plot.data.frame <- function(
  x,
  location,
  analyte,
  result,
  unit,
  date,
  ...
) {
  x <- env_schema(
    x,
    location = {{ location }},
    analyte = {{ analyte }},
    result = {{ result }},
    unit = {{ unit }},
    date = {{ date }}
  )

  piper_plot(x, ...)
}

#' Create a Piper diagram for hydrochemical facies interpretation
#'
#' Creates a Piper diagram from major ion chemistry reported in a tidy
#' groundwater or surface water dataset. Major ions are converted to
#' milliequivalents per liter (meq/L), normalized to relative ionic
#' percentages, and plotted in the cation triangle, anion triangle, and
#' central diamond.
#'
#' Labels accept plotmath expressions.
#'
#' @param df A data frame containing water quality results in tidy format.
#'   Must contain one row per parameter result and include the columns
#'   specified by `location_id`, `sample_date`, `param_name`,
#'   `analysis_result`, and `default_unit`.
#'
#' @param location_id Character string identifying the column containing
#'   sample location identifiers.
#'
#' @param sample_date Character string identifying the column containing
#'   sample dates.
#'
#' @param param_name Character string identifying the column containing
#'   analyte or parameter names.
#'
#' @param analysis_result Character string identifying the column containing
#'   numeric analytical results.
#'
#' @param default_unit Character string identifying the column containing
#'   analytical reporting units.
#'
#' @param analyte_matching Character string specifying how analyte names are
#'   matched. Must be one of `"normalized"`, `"strict"`, or `"exact"`.
#'   The default is `"normalized"`.
#'
#'   \describe{
#'     \item{exact}{
#'       Uses analyte names exactly as reported. For example,
#'       `"Calcium, dissolved"` and `"Calcium, total"` are treated as
#'       separate analytes.
#'     }
#'
#'     \item{strict}{
#'       Applies text normalization (case, punctuation, and whitespace)
#'       while preserving distinctions such as dissolved versus total
#'       fractions. For example, `"Calcium, dissolved"` resolves to
#'       `"calcium dissolved"` and remains distinct from
#'       `"calcium total"`.
#'     }
#'
#'     \item{normalized}{
#'       Applies text normalization and resolves aliases using the package
#'       analyte lookup table. For example,
#'       `"Calcium, dissolved"`, `"Calcium, total"`, and `"Ca"` may all
#'       resolve to `"calcium"`.
#'     }
#'   }
#'
#' @param x_cation Character vector of analytes representing the left corner
#'   cation component of the Piper diagram. Default is
#'   `"Calcium, dissolved"`.
#'
#' @param x_cation_label Label displayed for the left corner cation axis.
#'   Labels are parsed as plotmath expressions.
#'
#' @param y_cation Character vector of analytes representing the upper
#'   cation component of the Piper diagram. Default is
#'   `"Magnesium, dissolved"`.
#'
#' @param y_cation_label Label displayed for the upper cation axis.
#'   Labels are parsed as plotmath expressions.
#'
#' @param z_cation Character vector of analytes representing the right corner
#'   cation component of the Piper diagram. Defaults to sodium plus
#'   potassium:
#'   `c("Sodium, dissolved", "Potassium, dissolved")`.
#'
#' @param z_cation_label Label displayed for the right cation axis.
#'   Labels are parsed as plotmath expressions.
#'
#' @param x_anion Character vector of analytes representing the left corner
#'   anion component of the Piper diagram. Defaults to chloride plus
#'   fluoride:
#'   `c("Chloride, total", "Fluoride, total")`.
#'
#' @param x_anion_label Label displayed for the left anion axis.
#'   Labels are parsed as plotmath expressions.
#'
#' @param y_anion Character vector of analytes representing the lower-right
#'   anion component of the Piper diagram. The default assumes alkalinity is
#'   reported as `"Alkalinity, total (lab)"` in units of mg/L as CaCO3.
#'
#' @param y_anion_label Label displayed for the bicarbonate/carbonate axis.
#'   Labels are parsed as plotmath expressions.
#'
#' @param z_anion Character vector of analytes representing the upper anion
#'   component of the Piper diagram. Default is `"Sulfate, total"`.
#'
#' @param z_anion_label Label displayed for the sulfate axis. Labels are
#'   parsed as plotmath expressions.
#'
#' @param x_z_anion_label Label displayed on the upper-left side of the
#'   central diamond. Labels are parsed as plotmath expressions.
#'
#' @param x_y_cation_label Label displayed on the upper-right side of the
#'   central diamond. Labels are parsed as plotmath expressions.
#'
#' @param scale_analyte Optional analyte name used to scale point
#' size within the central diamond.
#'
#' The analyte must be present in the prepared Piper dataset.
#' If `NULL`, all points are plotted with a constant size.
#'
#' @param group_col Optional column name used to assign point shape.
#'   Each location must correspond to a single grouping value.
#'
#' @param transparency Numeric transparency value passed to
#'   [ggplot2::geom_point()]. Must be between 0 and 1. Default is `0.7`.
#'
#' @param pnt_size Numeric point size used when
#'   `scale_analyte = NULL`. Default is `3`.
#'
#' @param label_size Numeric text size used for Piper axis labels. Default
#'   is `3`.
#'
#' @param title Optional plot title. Defaults to `NULL`.
#'
#' @return
#' An object of class `"ggplot"` containing a Piper diagram.
#'
#' @details
#' Major ion analytes are processed through the package geochemical
#' preparation pipeline before Piper coordinates are calculated.
#'
#' The pipeline performs:
#'
#' \itemize{
#'   \item analyte resolution;
#'   \item analyte validation;
#'   \item unit validation;
#'   \item duplicate detection and averaging;
#'   \item conversion to milliequivalents per liter (meq/L);
#'   \item construction of a wide-format geochemical dataset.
#' }
#'
#' The resulting geochemical dataset is transformed into cation,
#' anion, and central-diamond Piper coordinates for plotting.
#'
#' @section Analyte matching:
#'
#' The same water chemistry dataset may contain multiple reporting
#' conventions for a given constituent (for example `"SO4"` versus
#' `"Sulfate"` or `"Calcium, dissolved"` versus `"Calcium, total"`).
#'
#' The `analyte_matching` argument controls whether these values are
#' treated as distinct analytes or resolved to a common analyte name prior
#' to ion conversion and Piper coordinate calculation.
#'
#' For most hydrochemical applications,
#' `analyte_matching = "normalized"` is recommended.
#'
#' When `analyte_matching = "normalized"`, analyte aliases are resolved
#' using the package analyte lookup table. This allows multiple laboratory
#' reporting conventions to be treated as the same chemical constituent.
#'
#' Examples include:
#'
#' \itemize{
#'   \item `"Ca"` and `"Calcium"`
#'   \item `"SO4"` and `"Sulfate"`
#'   \item `"Alkalinity, total"` and `"Bicarbonate"`
#'   \item `"Calcium, dissolved"` and `"Calcium, total"`
#' }
#'
#' The `"exact"` option preserves laboratory-reported analyte names and may
#' be useful for QA/QC, data auditing, or evaluating dissolved versus total
#' fractions independently. The `"strict"` option provides normalized text
#' matching while preserving those analytical distinctions.
#'
#' @section Validation and error handling:
#'
#' The function performs several validation checks before constructing the
#' Piper diagram.
#'
#' \strong{Required columns}
#'
#' The input data frame must contain the columns identified by
#' `location_id`, `sample_date`, `param_name`, `analysis_result`, and
#' `default_unit`. An error is thrown if any required columns are missing.
#'
#' \strong{Analyte resolution}
#'
#' Specified Piper ions are resolved according to the selected
#' `analyte_matching` strategy. Analytes that cannot be resolved are
#' retained using their normalized names and are not automatically dropped.
#'
#' \strong{Unit validation}
#'
#' Major ion analytes are validated and converted through the package
#' geochemical preparation pipeline. Unsupported units generate an error.
#'
#' \strong{Duplicate analytes}
#'
#' Duplicate analyte measurements within a sample are detected after analyte
#' resolution. A warning is issued when duplicate analyte/sample
#' combinations are found. Duplicate results are averaged before
#' conversion to milliequivalents and Piper coordinate calculation.
#'
#' Under `analyte_matching = "normalized"`, analytes such as
#' `"Calcium, dissolved"` and `"Calcium, total"` may resolve to the same
#' analyte and therefore be treated as duplicates.
#'
#' \strong{Missing major ions}
#'
#' A warning is issued when one or more requested Piper analytes are not
#' present in the input data.
#'
#' Samples with missing major ion concentrations are retained during data
#' preparation but omitted from the corresponding Piper triangle or diamond
#' when coordinates cannot be calculated.
#'
#' Informational messages report the number and percentage of complete
#' Piper analyses available for plotting.
#'
#' \strong{Grouping variables}
#'
#' When `group_col` is supplied, each `location_id` must map to exactly one
#' grouping value. If multiple grouping values are associated with the same
#' location, an error is thrown.
#'
#' @section Warnings:
#'
#' The function may generate warnings when:
#' \itemize{
#'   \item requested Piper ions are not present in the dataset;
#'   \item duplicate analyte/sample combinations are detected;
#'   \item samples are omitted due to incomplete major-ion chemistry.
#' }
#'
#' These warnings are intended to help identify data-quality issues and
#' unexpected analyte resolution behavior.
#'
#' @examples
#' data(gw_data)
#' wells <- c("MW-1", "MW-2", "MW-3", "MW-4", "MW-5")
#'
#' # preserve dissolved and total analytes as separate measurements
#' gw_data %>%
#'   dplyr::filter(location_id %in% wells) %>%
#'   piper_plot(
#'     analyte_matching = "exact"
#'   )
#'
#' # normalize analyte aliases to canonical ion names
#' gw_data %>%
#'   dplyr::filter(location_id %in% wells) %>%
#'   piper_plot(
#'     analyte_matching = "normalized"
#'   )
#'
#' # scaled by analyte
#' gw_data %>%
#' dplyr::filter(location_id %in% wells) %>%
#' piper_plot(
#' scale_analyte = "Total Dissolved Solids",
#' title = "Example Piper Diagram"
#' )
#'
#' # use plotmath expressions for labels
#' gw_data %>%
#' dplyr::filter(location_id %in% wells) %>%
#' piper_plot(x_cation_label = "Ca~phantom()^+2")
#'
#' @export

piper_plot.env_schema <- function(
  df,
  location_id = "location_id",
  sample_date = "sample_date",
  param_name = "param_name",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  analyte_matching = c("normalized", "strict", "exact"),
  x_cation = "Calcium, dissolved",
  x_cation_label = "Ca",
  y_cation = "Magnesium, dissolved",
  y_cation_label = "Mg",
  z_cation = c(
    "Sodium, dissolved",
    "Potassium, dissolved"
  ),
  z_cation_label = "Na + K",
  x_anion = c(
    "Chloride, total",
    "Fluoride, total"
  ),
  x_anion_label = "Cl + F",
  y_anion = "Alkalinity, total (lab)",
  y_anion_label = "HCO[3] + CO[3]",
  z_anion = "Sulfate, total",
  z_anion_label = "SO4",
  x_z_anion_label = "SO4 + Cl + F",
  x_y_cation_label = "Ca + Mg",
  scale_analyte = NULL,
  group_col = NULL,
  transparency = 0.7,
  pnt_size = 3,
  label_size = 3,
  title = NULL
) {
  analyte_matching <- rlang::arg_match(analyte_matching)

  scale_col <- NULL

  if (!is.null(scale_analyte)) {
    scale_col <- .resolve_geochemical_analytes(
      scale_analyte,
      analyte_matching
    )

    if (length(scale_col) == 0) {
      cli::cli_warn(
        c(
          "Unable to resolve {.val {scale_analyte}}.",
          "i" = "Point size scaling will be disabled."
        )
      )

      scale_col <- NULL
    } else {
      scale_col <- scale_col[1]
    }
  }

  required_cols <- c(
    location_id,
    sample_date,
    param_name,
    analysis_result,
    default_unit
  )

  missing_cols <- setdiff(
    required_cols,
    names(df)
  )

  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "Missing required columns.",
        "x" = paste(missing_cols, collapse = ", ")
      )
    )
  }

  piper_df <- df %>%
    .transform_piper_data(
      location_id = location_id,
      sample_date = sample_date,
      param_name = param_name,
      analysis_result = analysis_result,
      default_unit = default_unit,
      analyte_matching = analyte_matching,
      x_cation = x_cation,
      y_cation = y_cation,
      z_cation = z_cation,
      x_anion = x_anion,
      y_anion = y_anion,
      z_anion = z_anion,
      scale_analyte = scale_analyte
    )

  if (!is.null(group_col)) {
    group_df <- df %>%
      dplyr::distinct(
        !!rlang::sym(location_id),
        !!rlang::sym(group_col)
      )
    dup_groups <- group_df %>%
      dplyr::count(!!rlang::sym(location_id)) %>%
      dplyr::filter(n > 1)

    if (nrow(dup_groups) > 0) {
      cli::cli_abort(
        c(
          "{.field {group_col}} must contain only one value per location.",
          "x" = "{nrow(dup_groups)} locations contain multiple grouping values."
        )
      )
    }

    join_by <- stats::setNames(
      location_id,
      location_id
    )

    piper_df <- piper_df %>%
      dplyr::left_join(
        group_df,
        by = join_by
      )
  }

  cation_df <- piper_df %>%
    dplyr::filter(
      !is.na(cation_x),
      !is.na(cation_y)
    )

  anion_df <- piper_df %>%
    dplyr::filter(
      !is.na(anion_x),
      !is.na(anion_y)
    )

  diamond_df <- piper_df %>%
    dplyr::filter(
      !is.na(diamond_x),
      !is.na(diamond_y)
    )

  piper <- .ggplot_piper()

  # cation triangle
  piper <- .add_piper_points(
    plot = piper,
    data = cation_df,
    x = "cation_x",
    y = "cation_y",
    location_id = location_id,
    group_col = group_col,
    alpha = transparency,
    point_size = pnt_size
  )

  # anion triangle
  piper <- .add_piper_points(
    plot = piper,
    data = anion_df,
    x = "anion_x",
    y = "anion_y",
    location_id = location_id,
    group_col = group_col,
    alpha = transparency,
    point_size = pnt_size
  )

  # diamond
  piper <- .add_piper_points(
    plot = piper,
    data = diamond_df,
    x = "diamond_x",
    y = "diamond_y",
    location_id = location_id,
    group_col = group_col,
    scale_col = scale_col,
    alpha = transparency,
    point_size = pnt_size
  )

  piper <- .add_piper_labels(
    plot = piper,
    label_size = label_size,
    x_cation_label = x_cation_label,
    y_cation_label = y_cation_label,
    z_cation_label = z_cation_label,
    x_anion_label = x_anion_label,
    y_anion_label = y_anion_label,
    z_anion_label = z_anion_label,
    x_z_anion_label = x_z_anion_label,
    x_y_cation_label = x_y_cation_label
  )

  if (!is.null(scale_col)) {
    piper <- piper +
      ggplot2::scale_size_area(
        name = tools::toTitleCase(scale_analyte),
        max_size = 8
      )
  }

  if (requireNamespace("viridis", quietly = TRUE)) {
    piper <- piper +
      viridis::scale_colour_viridis(discrete = TRUE)
  } else {
    piper <- piper +
      ggplot2::scale_colour_discrete()
  }

  guide_args <- list(
    colour = ggplot2::guide_legend(location_id),
    alpha = "none"
  )

  if (!is.null(group_col)) {
    guide_args$shape <- ggplot2::guide_legend(group_col)
  }

  piper <- piper +
    do.call(ggplot2::guides, guide_args)

  piper +
    ggplot2::ggtitle(title) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
}

#' Add a Piper point layer
#'
#' @noRd
.add_piper_points <- function(
  plot,
  data,
  x,
  y,
  location_id = "location_id",
  group_col = NULL,
  scale_col = NULL,
  alpha = 0.7,
  point_size = 3
) {
  aes_args <- list(
    x = rlang::sym(x),
    y = rlang::sym(y),
    colour = rlang::sym(location_id)
  )

  if (!is.null(group_col)) {
    aes_args$shape <- rlang::expr(
      factor(!!rlang::sym(group_col), exclude = NULL)
    )
  }

  if (!is.null(scale_col)) {
    if (!scale_col %in% names(data)) {
      cli::cli_warn(
        c(
          "Size scaling analyte not available.",
          "i" = "Using constant point size."
        )
      )

      scale_col <- NULL
    } else {
      aes_args$size <- rlang::sym(scale_col)
    }
  }

  mapping <- do.call(ggplot2::aes, aes_args)

  layer_args <- list(
    data = data,
    mapping = mapping,
    alpha = alpha
  )

  if (is.null(scale_col)) {
    layer_args$size <- point_size
  }

  plot +
    do.call(
      ggplot2::geom_point,
      layer_args
    )
}

#' Add Piper labels
#'
#' @noRd
.add_piper_labels <- function(
  plot,
  label_size,
  x_cation_label,
  y_cation_label,
  z_cation_label,
  x_anion_label,
  y_anion_label,
  z_anion_label,
  x_z_anion_label,
  x_y_cation_label
) {
  label_df <- tibble::tribble(
    ~x    , ~y  , ~label           , ~angle ,
     17   ,  50 , y_cation_label   ,     60 ,
     82   ,  50 , z_cation_label   ,    -60 ,
     50   , -10 , x_cation_label   ,      0 ,
    170   , -10 , x_anion_label    ,      0 ,
    205   ,  50 , z_anion_label    ,    -60 ,
    138.5 ,  50 , y_anion_label    ,     60 ,
     72.5 , 150 , x_z_anion_label  ,     60 ,
    147.5 , 150 , x_y_cation_label ,    -60
  )

  plot +
    ggplot2::geom_text(
      data = label_df,
      ggplot2::aes(
        x = x,
        y = y,
        label = label,
        angle = angle
      ),
      parse = TRUE,
      inherit.aes = FALSE,
      size = label_size
    )
}


#' Function to create an animated Piper plot through time
#' using the animation package
#'
#' @param df data frame of groundwater data transformed using
#' @inheritDotParams piper_plot
#' @export

piper_time_plot <- function(df, ...) {
  if (!requireNamespace("animation", quietly = TRUE)) {
    stop(
      "animation package needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  iter <- unique(df$sample_date)

  for (i in seq_along(iter)) {
    piper_plot(df[df$sample_date == iter[i], ], title = paste(iter[i]), ...)
    animation::ani.pause()
  }
}

#' Function to create an aminated Piper plot and save to html
#'
#' @param df data frame of groundwater data transformed using
#' @inheritDotParams piper_plot
#' @export

piper_time_html <- function(df, ...) {
  if (!requireNamespace("animation", quietly = TRUE)) {
    stop(
      "animation package needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  animation::saveHTML(
    {
      animation::ani.options(
        nmax = length(unique(df$sample_date)),
        outdir = getwd()
      )

      piper_time_plot(df, ...)
    },

    img.name = "PiperPlot",
    htmlfile = "Piper_plot.html",
    autobrowse = TRUE,
    title = "Piper Plot"
  )
}

#' Function to create base Piper plot
#'
#' @noRd

.ggplot_piper <- function() {
  p <- ggplot2::ggplot() +

    ## left hand ternary plot
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 100, yend = 0)) +
    ggplot2::geom_segment(ggplot2::aes(
      x = 0,
      y = 0,
      xend = 50,
      yend = 86.603
    )) +
    ggplot2::geom_segment(ggplot2::aes(
      x = 50,
      y = 86.603,
      xend = 100,
      yend = 0
    )) +

    ## right hand ternary plot
    ## shifted right by 20 points
    ggplot2::geom_segment(ggplot2::aes(x = 120, y = 0, xend = 220, yend = 0)) +
    ggplot2::geom_segment(ggplot2::aes(
      x = 120,
      y = 0,
      xend = 170,
      yend = 86.603
    )) +
    ggplot2::geom_segment(ggplot2::aes(
      x = 170,
      y = 86.603,
      xend = 220,
      yend = 0
    )) +

    ## Upper diamond
    ggplot2::geom_segment(ggplot2::aes(
      x = 110,
      y = 190.5266,
      xend = 60,
      yend = 103.9236
    )) +
    ggplot2::geom_segment(ggplot2::aes(
      x = 110,
      y = 190.5266,
      xend = 160,
      yend = 103.9236
    )) +
    ggplot2::geom_segment(ggplot2::aes(
      x = 110,
      y = 17.3206,
      xend = 160,
      yend = 103.9236
    )) +
    ggplot2::geom_segment(ggplot2::aes(
      x = 110,
      y = 17.3206,
      xend = 60,
      yend = 103.9236
    )) +

    ## Add grid lines to the plots
    ggplot2::geom_segment(
      ggplot2::aes(x = x1, y = y1, yend = y2, xend = x2),
      data = data.frame(
        x1 = c(20, 40, 60, 80),
        x2 = c(10, 20, 30, 40),
        y1 = c(0, 0, 0, 0),
        y2 = c(17.3206, 34.6412, 51.9618, 69.2824)
      ),
      linetype = "dashed",
      linewidth = 0.25,
      colour = "grey50"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x1, y = y1, yend = y2, xend = x2),
      data = data.frame(
        x1 = c(20, 40, 60, 80),
        x2 = c(60, 70, 80, 90),
        y1 = c(0, 0, 0, 0),
        y2 = c(69.2824, 51.9618, 34.6412, 17.3206)
      ),
      linetype = "dashed",
      linewidth = 0.25,
      colour = "grey50"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x1, y = y1, yend = y2, xend = x2),
      data = data.frame(
        x1 = c(10, 20, 30, 40),
        x2 = c(90, 80, 70, 60),
        y1 = c(17.3206, 34.6412, 51.9618, 69.2824),
        y2 = c(17.3206, 34.6412, 51.9618, 69.2824)
      ),
      linetype = "dashed",
      linewidth = 0.25,
      colour = "grey50"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x1, y = y1, yend = y2, xend = x2),
      data = data.frame(
        x1 = c(140, 160, 180, 200),
        x2 = c(130, 140, 150, 160),
        y1 = c(0, 0, 0, 0),
        y2 = c(17.3206, 34.6412, 51.9618, 69.2824)
      ),
      linetype = "dashed",
      linewidth = 0.25,
      colour = "grey50"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x1, y = y1, yend = y2, xend = x2),
      data = data.frame(
        x1 = c(140, 160, 180, 200),
        x2 = c(180, 190, 200, 210),
        y1 = c(0, 0, 0, 0),
        y2 = c(69.2824, 51.9618, 34.6412, 17.3206)
      ),
      linetype = "dashed",
      linewidth = 0.25,
      colour = "grey50"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x1, y = y1, yend = y2, xend = x2),
      data = data.frame(
        x1 = c(130, 140, 150, 160),
        x2 = c(210, 200, 190, 180),
        y1 = c(17.3206, 34.6412, 51.9618, 69.2824),
        y2 = c(17.3206, 34.6412, 51.9618, 69.2824)
      ),
      linetype = "dashed",
      linewidth = 0.25,
      colour = "grey50"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x1, y = y1, yend = y2, xend = x2),
      data = data.frame(
        x1 = c(100, 90, 80, 70),
        y1 = c(34.6412, 51.9618, 69.2824, 86.603),
        x2 = c(150, 140, 130, 120),
        y2 = c(121.2442, 138.5648, 155.8854, 173.2060)
      ),
      linetype = "dashed",
      linewidth = 0.25,
      colour = "grey50"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x1, y = y1, yend = y2, xend = x2),
      data = data.frame(
        x1 = c(70, 80, 90, 100),
        y1 = c(121.2442, 138.5648, 155.8854, 173.2060),
        x2 = c(120, 130, 140, 150),
        y2 = c(34.6412, 51.9618, 69.2824, 86.603)
      ),
      linetype = "dashed",
      linewidth = 0.25,
      colour = "grey50"
    ) +

    # add the grid labels
    # X Cation
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(20, 40, 60, 80),
        y = c(-5, -5, -5, -5),
        label = c(80, 60, 40, 20)
      ),
      size = 2.5
    ) +
    # Y Cation
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(35, 25, 15, 5),
        y = c(69.2824, 51.9618, 34.6412, 17.3206),
        label = c(80, 60, 40, 20)
      ),
      size = 2.5
    ) +
    # Z Cation
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(95, 85, 75, 65),
        y = c(17.3206, 34.6412, 51.9618, 69.2824),
        label = c(80, 60, 40, 20)
      ),
      size = 2.5
    ) +
    ggplot2::coord_equal(ratio = 1) +

    # Y Anion
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(155, 145, 135, 125),
        y = c(69.2824, 51.9618, 34.6412, 17.3206),
        label = c(20, 40, 60, 80)
      ),
      size = 2.5
    ) +
    # Z Anion
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(215, 205, 195, 185),
        y = c(17.3206, 34.6412, 51.9618, 69.2824),
        label = c(20, 40, 60, 80)
      ),
      size = 2.5
    ) +
    # X Anion
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(140, 160, 180, 200),
        y = c(-5, -5, -5, -5),
        label = c(20, 40, 60, 80)
      ),
      size = 2.5
    ) +
    # left lower diamond
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(95, 85, 75, 65),
        y = c(34.6412, 51.9618, 69.2824, 86.603),
        label = c(80, 60, 40, 20)
      ),
      size = 2.5
    ) +
    # right upper diamond
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(155, 145, 135, 125),
        y = c(121.2442, 138.5648, 155.8854, 173.2060),
        label = c(20, 40, 60, 80)
      ),
      size = 2.5
    ) +
    # left upper diamond
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(65, 75, 85, 95),
        y = c(121.2442, 138.5648, 155.8854, 173.2060),
        label = c(20, 40, 60, 80)
      ),
      size = 2.5
    ) +
    # right lower diamond
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(125, 135, 145, 155),
        y = c(34.6412, 51.9618, 69.2824, 86.603),
        label = c(80, 60, 40, 20)
      ),
      size = 2.5
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()
    )

  return(p)
}

#' @export
schoeller_plot <- function(x, ...) {
  UseMethod("schoeller_plot")
}

#' @export
schoeller_plot.data.frame <- function(
  x,
  location,
  analyte,
  result,
  unit,
  date,
  ...
) {
  x <- env_schema(
    x,
    location = {{ location }},
    analyte = {{ analyte }},
    result = {{ result }},
    unit = {{ unit }},
    date = {{ date }}
  )

  schoeller_plot(x, ...)
}


#' Schoeller Diagram
#'
#' Creates a Schoeller diagram for comparing major ion chemistry among
#' groundwater or surface water samples.
#'
#' The function automatically:
#'
#' * Resolves analyte names using the package analyte metadata tables.
#' * Converts analyte concentrations to milliequivalents per liter (meq/L).
#' * Aggregates replicate results within each
#'   location/sample/analyte combination.
#' * Combines sodium and potassium into a single
#'   Na + K Schoeller axis.
#' * Produces a logarithmic concentration profile suitable for
#'   hydrochemical interpretation.
#'
#' Analyte names may be supplied using any supported alias when
#' `analyte_matching = "normalized"` or `"strict"`.
#'
#' @param df Data frame containing environmental analytical results in
#'   tidy format.
#'
#' @param location_id Character. Column containing location identifiers.
#'
#' @param sample_date Character. Column containing sample dates.
#'
#' @param param_name Character. Column containing analyte names.
#'
#' @param analysis_result Character. Column containing analytical results.
#'
#' @param default_unit Character. Column containing analytical units.
#'
#' @param calcium Character. Calcium analyte name or alias.
#'
#' @param magnesium Character. Magnesium analyte name or alias.
#'
#' @param sodium Character. Sodium analyte name or alias.
#'
#' @param potassium Character. Potassium analyte name or alias.
#'
#' @param chloride Character. Chloride analyte name or alias.
#'
#' @param sulfate Character. Sulfate analyte name or alias.
#'
#' @param alkalinity Character. Alkalinity analyte name or alias.
#'
#' @param analyte_matching Character. Method used to match analyte names.
#' One of:
#'
#' * `"normalized"` (default)
#' * `"strict"`
#' * `"exact"`
#'
#' See [resolve_analyte_name()] for details.
#'
#' @param facet_var Character. Variable used for faceting.
#' One of:
#'
#' * `"location_id"`
#' * `"sample_date"`
#'
#' When `"location_id"` is selected, each location receives its own panel
#' and individual sample events are plotted as separate lines.
#'
#' When `"sample_date"` is selected, each sampling event receives its
#' own panel and individual locations are plotted as separate lines.
#'
#' @param title Optional plot title.
#'
#' @param lwt Numeric line width.
#'
#' @return
#' A ggplot object.
#'
#' @details
#' Schoeller diagrams display major ion concentrations on a logarithmic
#' y-axis and are commonly used to:
#'
#' * Compare groundwater chemistry between monitoring wells.
#' * Evaluate temporal geochemical trends.
#' * Identify mixing relationships.
#' * Identify hydrochemical facies.
#' * Screen for anomalous ion signatures.
#'
#' Concentrations are internally converted to milliequivalents per liter
#' (meq/L) using analyte metadata stored in `analyte_tbl`.
#'
#' Missing analytes are permitted and are omitted from the plotted profile.
#'
#' @examples
#'
#' # Default Schoeller diagram
#' schoeller_plot(gw_data)
#'
#' # Facet by sampling event
#' schoeller_plot(
#'   gw_data,
#'   facet_var = "sample_date"
#' )
#'
#' # Custom analyte aliases using normalized matching
#' schoeller_plot(
#'   gw_data,
#'   calcium = "Ca",
#'   magnesium = "Mg",
#'   sodium = "Na",
#'   potassium = "K",
#'   chloride = "Cl",
#'   sulfate = "SO4",
#'   alkalinity = "HCO3",
#'   analyte_matching = "normalized"
#' )
#'
#' # Restrict to selected monitoring wells
#' gw_data %>%
#'   dplyr::filter(
#'     location_id %in% c(
#'       "MW-1",
#'       "MW-2",
#'       "MW-3"
#'     )
#'   ) %>%
#'   schoeller_plot()
#'
#' # Temporal comparison for a single well
#' gw_data %>%
#'   dplyr::filter(
#'     location_id == "MW-1"
#'   ) %>%
#'   schoeller_plot(
#'     facet_var = "location_id"
#'   )
#'
#' # Customized appearance
#' schoeller_plot(
#'   gw_data,
#'   title = "Major Ion Chemistry",
#'   lwt = 1.5
#' )
#'
#' @export
schoeller_plot.env_schmema <- function(
  df,
  location_id = "location_id",
  sample_date = "sample_date",
  param_name = "param_name",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  calcium = "Calcium, dissolved",
  magnesium = "Magnesium, dissolved",
  sodium = "Sodium, dissolved",
  potassium = "Potassium, dissolved",
  chloride = "Chloride, total",
  sulfate = "Sulfate, total",
  alkalinity = "Alkalinity, total (lab)",
  analyte_matching = c(
    "normalized",
    "strict",
    "exact"
  ),
  facet_var = c(
    "location_id",
    "sample_date"
  ),
  title = NULL,
  lwt = 1
) {
  analyte_matching <- rlang::arg_match(
    analyte_matching
  )

  facet_var <- rlang::arg_match(
    facet_var
  )

  chemistry_df <- .prepare_major_ion_chemistry(
    df = df,
    analytes = c(
      calcium,
      magnesium,
      sodium,
      potassium,
      chloride,
      sulfate,
      alkalinity
    ),
    location_id = location_id,
    sample_date = sample_date,
    param_name = param_name,
    analysis_result = analysis_result,
    default_unit = default_unit,
    analyte_matching = analyte_matching
  )

  df_plot <- .transform_schoeller_data(
    chemistry_df = chemistry_df
  )

  p <- ggplot2::ggplot(
    df_plot,
    ggplot2::aes(
      x = schoeller_axis,
      y = analysis_result
    )
  ) +
    ggplot2::scale_y_log10(
      labels = scales::label_number()
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = "Concentration (meq/L)",
      colour = NULL
    ) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        hjust = 0.5
      )
    )

  p <- .add_schoeller_lines(
    p = p,
    facet_var = facet_var,
    lwt = lwt
  )

  if (
    requireNamespace(
      "viridis",
      quietly = TRUE
    )
  ) {
    p <- p +
      viridis::scale_colour_viridis(
        discrete = TRUE
      )
  }

  p
}

#' Add Schoeller lines
#'
#' @noRd
.add_schoeller_lines <- function(
  p,
  facet_var,
  lwt
) {
  if (facet_var == "sample_date") {
    p +
      ggplot2::geom_line(
        ggplot2::aes(
          group = location_id,
          colour = location_id
        ),
        linewidth = lwt
      ) +
      ggplot2::facet_wrap(
        ggplot2::vars(sample_date)
      ) +
      ggplot2::guides(
        colour = ggplot2::guide_legend(
          title = "Location ID"
        )
      )
  } else {
    p +
      ggplot2::geom_line(
        ggplot2::aes(
          group = sample_date,
          colour = factor(sample_date)
        ),
        linewidth = lwt
      ) +
      ggplot2::facet_wrap(
        ggplot2::vars(location_id)
      ) +
      ggplot2::guides(
        colour = ggplot2::guide_legend(
          title = "Sample Date"
        )
      )
  }
}

#' @export
stiff_plot <- function(x, ...) {
  UseMethod("piper_plot")
}

#' @export
stiff_plot.data.frame <- function(
  x,
  location,
  analyte,
  result,
  unit,
  date,
  ...
) {
  x <- env_schema(
    x,
    location = {{ location }},
    analyte = {{ analyte }},
    result = {{ result }},
    unit = {{ unit }},
    date = {{ date }}
  )

  stiff_plot(x, ...)
}

#' @title Stiff Diagram
#'
#' @description plots a stiff diagram
#'
#' @details tidy dataframe of water quality data in mg/L is converted to
#' meq/L and then transformed to coordinates for Stiff diagram.
#'
#' @param data data frame of water quality data
#' @param location_id column for sample locations
#' @param sample_date column for sample date
#' @param param_name column for constituent names
#' @param analysis_result column for analysis result
#' @param default_unit column for units. Assumes all units are in mg/L and
#' converts using \code{\link{convert_ions_to_meq}}
#' @param magnesium name for magnesium
#' @param calcium name for calcium
#' @param sodium name for sodium
#' @param potassium name for potassium
#' @param chloride name for chloride
#' @param sulfate name for sulfate
#' @param alkalinity name for alkalinity
#' @param scale_analyte name of analyte to scale color of plot.
#' Default is NULL
#' @param group_var Character column name used to split the data into
#' separate Stiff diagrams. One ggplot object is returned for each
#' unique value. Default is the value supplied to `location_id`.
#' @param facet_var Character column name used for faceting.
#' Default is the value supplied to `sample_date`.
#' @param lines TRUE/FALSE plots lines
#' @param title Optional plot title. If NULL, the value of the current
#' grouping variable is used.
#' @keywords geochemical plots Stiff Diagram
#'
#' @return
#' A named list of ggplot objects, one element per value of
#' `group_var`.
#'
#' @examples
#' data(gw_data)
#' gw_data %>%
#' filter(location_id == "MW-1", sample_date < lubridate::ymd("2008-01-01")) %>%
#' stiff_plot()
#'
#' gw_data %>%
#' filter(location_id %in% c("MW-1", "MW-2")) %>%
#' stiff_plot(scale_analyte = "Total Dissolved Solids")
#'
#' @export

stiff_plot.env_schema <- function(
  data,
  location_id = "location_id",
  sample_date = "sample_date",
  param_name = "param_name",
  analysis_result = "analysis_result",
  default_unit = "default_unit",
  calcium = "Calcium, dissolved",
  magnesium = "Magnesium, dissolved",
  sodium = "Sodium, dissolved",
  potassium = "Potassium, dissolved",
  chloride = "Chloride, total",
  sulfate = "Sulfate, total",
  alkalinity = "Alkalinity, total (lab)",
  scale_analyte = NULL,
  group_var = location_id,
  facet_var = sample_date,
  stiff_limit = NULL,
  stiff_breaks = 1,
  lines = FALSE,
  title = NULL
) {
  if (!facet_var %in% c(location_id, sample_date)) {
    cli::cli_abort(
      "{.arg facet_var} must be either the location_id or sample_date column."
    )
  }
  if (!group_var %in% c(location_id, sample_date)) {
    cli::cli_abort(
      "{.arg group_var} must be either the location_id or sample_date column."
    )
  }

  required_ions <- c(
    calcium,
    magnesium,
    sodium,
    potassium,
    chloride,
    sulfate,
    alkalinity
  )

  missing <- setdiff(
    required_ions,
    unique(data[[param_name]])
  )

  if (length(missing) == length(required_ions)) {
    cli::cli_abort(
      "No required major ions were found in the data."
    )
  }

  if (length(missing) > 0) {
    cli::cli_warn(
      "Missing major ions: {paste(missing, collapse = ', ')}"
    )
  }

  split(data, data[[group_var]]) %>%
    purrr::imap(
      ~ .stiff_plot(
        data = .x,
        group_name = .y,
        location_id = location_id,
        sample_date = sample_date,
        param_name = param_name,
        analysis_result = analysis_result,
        default_unit = default_unit,
        calcium = calcium,
        magnesium = magnesium,
        sodium = sodium,
        potassium = potassium,
        chloride = chloride,
        sulfate = sulfate,
        alkalinity = alkalinity,
        scale_analyte = scale_analyte,
        facet_var = facet_var,
        group_var = group_var,
        stiff_limit = stiff_limit,
        stiff_breaks = stiff_breaks,
        lines = lines,
        title = title
      )
    )
}

#' Helper function for Stiff Diagrams
#'
#' @noRd
.stiff_plot <- function(
  data,
  group_name,
  location_id,
  sample_date,
  param_name,
  analysis_result,
  default_unit,
  calcium,
  magnesium,
  sodium,
  potassium,
  chloride,
  sulfate,
  alkalinity,
  scale_analyte,
  facet_var,
  group_var,
  lines,
  stiff_limit,
  stiff_breaks,
  title
) {
  chemistry_df <- .prepare_major_ion_chemistry(
    df = data,
    analytes = c(
      calcium,
      magnesium,
      sodium,
      potassium,
      chloride,
      sulfate,
      alkalinity
    ),
    location_id = location_id,
    sample_date = sample_date,
    param_name = param_name,
    analysis_result = analysis_result,
    default_unit = default_unit
  )

  scale_df <- NULL

  if (!is.null(scale_analyte)) {
    scale_df <- .prepare_geochemical_data(
      df = data,
      analytes = scale_analyte,
      location_id = location_id,
      sample_date = sample_date,
      param_name = param_name,
      analysis_result = analysis_result,
      default_unit = default_unit
    )
  }

  stiff_data <- .transform_stiff_data(
    chemistry_df = chemistry_df,
    scale_df = scale_df,
    scale_analyte = scale_analyte
  )

  .build_stiff_plot(
    data = stiff_data,
    location_id = location_id,
    sample_date = sample_date,
    facet_var = facet_var,
    group_var = group_var,
    group_name = group_name,
    lines = lines,
    title = title,
    scale_analyte = scale_analyte,
    stiff_limit = stiff_limit,
    stiff_breaks = stiff_breaks
  )
}

#' Helper function for Stiff Diagrams
#'
#' @noRd

.build_stiff_plot <- function(
  data,
  location_id,
  sample_date,
  facet_var,
  group_var,
  group_name,
  lines,
  title,
  scale_analyte,
  stiff_limit,
  stiff_breaks
) {
  plot_title <- if (is.null(title)) {
    as.character(group_name)
  } else {
    title
  }

  if (is.null(stiff_limit)) {
    stiff_limit <- ceiling(
      max(abs(data$stiff_x), na.rm = TRUE)
    )

    stiff_limit <- max(
      stiff_limit,
      1
    )
  }

  grid_breaks <- setdiff(seq(-stiff_limit, stiff_limit, by = stiff_breaks), 0)

  if (max(abs(data$stiff_x), na.rm = TRUE) > stiff_limit) {
    cli::cli_warn(
      "Some samples exceed {.arg stiff_limit} and will be truncated."
    )
  }

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = stiff_x,
      y = stiff_y,
      group = interaction(
        .data[[location_id]],
        .data[[sample_date]]
      )
    )
  ) +
    ggplot2::geom_vline(
      xintercept = grid_breaks,
      colour = "grey85",
      linewidth = 0.2
    )

  if (!is.null(scale_analyte)) {
    p <- p +
      ggplot2::geom_polygon(
        ggplot2::aes(
          fill = scale_value,
          group = interaction(
            .data[[location_id]],
            .data[[sample_date]]
          )
        ),
        colour = "black"
      ) +
      ggplot2::scale_fill_viridis_c(
        name = paste(scale_analyte)
      )
  } else {
    p <- p +
      ggplot2::geom_polygon(
        ggplot2::aes(
          group = interaction(
            .data[[location_id]],
            .data[[sample_date]]
          )
        ),
        fill = "white",
        colour = "black"
      )
  }

  # draw vertical reference line at 0 meq/L on top.
  p <- p +
    ggplot2::geom_vline(
      xintercept = 0,
      colour = "black",
      linewidth = 0.8
    )

  label_data <- data %>%
    dplyr::distinct(
      .data[[facet_var]],
      ion,
      label_side,
      stiff_y
    )

  label_data <- label_data %>%
    dplyr::mutate(
      label_x = dplyr::if_else(
        label_side == "left",
        -stiff_limit,
        stiff_limit
      ),
      hjust = dplyr::if_else(
        label_side == "left",
        1,
        0
      )
    )

  p <- p +
    ggplot2::geom_text(
      data = label_data,
      ggplot2::aes(
        x = label_x,
        y = stiff_y,
        label = ion,
        hjust = hjust
      ),
      inherit.aes = FALSE,
      parse = TRUE,
      size = 3
    )

  if (isTRUE(lines)) {
    p <- p +
      ggplot2::geom_segment(
        data = label_data,
        ggplot2::aes(
          x = -stiff_limit,
          xend = stiff_limit,
          y = stiff_y,
          yend = stiff_y
        ),
        inherit.aes = FALSE,
        linetype = "dotted",
        colour = "grey70"
      )
  }

  return(
    p +
      ggplot2::facet_wrap(
        stats::reformulate(facet_var),
        scales = "fixed"
      ) +
      ggplot2::coord_equal(
        clip = "off"
      ) +
      ggplot2::scale_x_continuous(
        limits = c(
          -stiff_limit * 1.10,
          stiff_limit * 1.10
        ),
        breaks = seq(
          -stiff_limit,
          stiff_limit,
          by = stiff_breaks
        ),
        labels = abs
      ) +
      ggplot2::labs(
        title = plot_title,
        x = "meq/L",
        y = NULL
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = 0.5
        ),
        plot.margin = ggplot2::margin(
          5.5,
          25,
          5.5,
          25
        ),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank()
      )
  )
}

#' Animate Stiff Diagrams
#'
#' @param data Data frame of water quality data.
#' @param sample_date Sample date column.
#' @param pause Delay between frames in seconds.
#' @param ...
#' Additional arguments passed to `stiff_plot()`.
#'
#' @export

stiff_time_plot <- function(
  data,
  sample_date = "sample_date",
  pause = 1,
  ...
) {
  .animate_geochemical_plots(
    data = data,
    plot_fn = stiff_plot,
    sample_date = sample_date,
    pause = pause,
    ...
  )
}

#' Save animated Stiff Diagram to HTML
#'
#' @param data Data frame of water quality data.
#' @param sample_date Sample date column.
#' @param htmlfile Output HTML file.
#' @param pause Delay between frames in seconds.
#' @param ...
#' Additional arguments passed to `stiff_plot()`.
#'
#' @return Invisibly returns htmlfile.
#'
#' @export

stiff_time_html <- function(
  data,
  sample_date = "sample_date",
  htmlfile = "Stiff_plot.html",
  pause = 1,
  ...
) {
  .save_geochemical_animation(
    data = data,
    plot_fn = stiff_plot,
    sample_date = sample_date,
    htmlfile = htmlfile,
    title = "Stiff Diagram",
    img.name = "StiffPlot",
    pause = pause,
    ...
  )
}

#' @export
durov_plot <- function(x, ...) {
  UseMethod("durov_plot")
}

#' @export
durov_plot.data.frame <- function(
  x,
  location,
  analyte,
  result,
  unit,
  date,
  ...
) {
  x <- env_schema(
    x,
    location = {{ location }},
    analyte = {{ analyte }},
    result = {{ result }},
    unit = {{ unit }},
    date = {{ date }}
  )

  durov_plot(x, ...)
}


#' Durov Diagram
#'
#' Creates a Durov diagram for interpretation of hydrochemical facies,
#' mixing processes, ion exchange, and geochemical evolution.
#'
#' @param df Data frame containing analytical results.
#' @param location_id Location identifier column.
#' @param sample_date Sample date column.
#' @param param_name Analyte column.
#' @param analysis_result Result column.
#' @param default_unit Unit column.
#' @param analyte_matching Analyte matching method.
#' @param facet_var Variable used for faceting.
#' @param title Plot title.
#'
#' @return
#' A ggplot object.
#'
#' @export
durov_plot.env_schema <- function(
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
  title = NULL
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
  )

  plot_df <- .transform_durov_data(
    chemistry_df
  )

  if (
    !all(
      c(
        "location_id",
        "sample_date",
        "durov_x_plot",
        "durov_y_plot"
      ) %in%
        names(plot_df)
    )
  ) {
    rlang::abort(
      "Invalid Durov transform output."
    )
  }

  plot_df <- plot_df %>%
    dplyr::filter(
      !is.na(durov_x_plot),
      !is.na(durov_y_plot)
    )

  framework <- .durov_framework()

  legend_text <- paste(
    c(
      "Durov Field Interpretation",
      "",
      paste0(
        .durov_field_legend()$field,
        ": ",
        .durov_field_legend()$interpretation
      )
    ),
    collapse = "\n"
  )

  ggplot2::ggplot() +

    # -------------------------------------------------
    # CATION TRIANGLE
    # -------------------------------------------------

    ggplot2::geom_path(
      data = framework$cation_triangle,
      ggplot2::aes(
        x,
        y
      )
    ) +

    ggplot2::geom_point(
      data = plot_df,
      ggplot2::aes(
        cation_x,
        cation_y,
        colour = location_id
      ),
      show.legend = FALSE
    ) +
    ggplot2::annotate(
      "text",
      x = 0,
      y = -5,
      label = "Ca"
    ) +

    ggplot2::annotate(
      "text",
      x = 100,
      y = -5,
      label = "Na + K"
    ) +

    ggplot2::annotate(
      "text",
      x = 50,
      y = 90,
      label = "Mg"
    ) +

    # -------------------------------------------------
    # ANION TRIANGLE
    # -------------------------------------------------

    ggplot2::geom_path(
      data = framework$anion_triangle,
      ggplot2::aes(
        x,
        y
      )
    ) +

    ggplot2::geom_point(
      data = plot_df,
      ggplot2::aes(
        anion_x,
        anion_y,
        colour = location_id
      ),
      show.legend = FALSE
    ) +
    ggplot2::annotate(
      "text",
      x = 120,
      y = -5,
      label = expression(
        HCO[3]
      )
    ) +

    ggplot2::annotate(
      "text",
      x = 220,
      y = -5,
      label = "Cl"
    ) +

    ggplot2::annotate(
      "text",
      x = 170,
      y = 90,
      label = expression(
        SO[4]
      )
    ) +

    # -------------------------------------------------
    # DUROV SQUARE
    # -------------------------------------------------

    ggplot2::geom_path(
      data = framework$square,
      ggplot2::aes(
        x,
        y
      )
    ) +

    ggplot2::geom_segment(
      data = framework$square_grid_v,
      ggplot2::aes(
        x = x,
        xend = xend,
        y = y,
        yend = yend
      ),
      linetype = 2,
      linewidth = 0.25
    ) +

    ggplot2::geom_segment(
      data = framework$square_grid_h,
      ggplot2::aes(
        x = x,
        xend = xend,
        y = y,
        yend = yend
      ),
      linetype = 2,
      linewidth = 0.25
    ) +

    ggplot2::geom_point(
      data = plot_df,
      ggplot2::aes(
        durov_x_plot,
        durov_y_plot,
        colour = location_id
      ),
      size = 2
    ) +

    ggplot2::coord_equal(
      xlim = c(-10, 230),
      ylim = c(-10, 210),
      expand = FALSE,
      clip = "off"
    ) +

    ggplot2::theme_bw() +

    ggplot2::annotate(
      "text",
      x = 110,
      y = 95,
      label = "Na + K (% of cations)"
    ) +

    ggplot2::annotate(
      "text",
      x = 50,
      y = 150,
      label = "Cl (% of anions)",
      angle = 90
    ) +

    ggplot2::labs(
      title = title,
      colour = NULL,
      caption = legend_text
    ) +

    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        hjust = 0.5
      ),
      plot.margin = ggplot2::margin(20, 20, 20, 20),
      plot.caption = ggplot2::element_text(
        hjust = 0,
        size = 8,
        margin = ggplot2::margin(
          t = 10
        )
      )
    ) +
    ggplot2::geom_text(
      data = framework$field_labels,
      ggplot2::aes(
        x,
        y,
        label = label
      ),
      size = 3
    )
}

#' Durov framework
#'
#' @noRd
.durov_framework <- function() {
  h <- sqrt(3) / 2 * 100

  list(
    cation_triangle = tibble::tribble(
      ~x  , ~y ,
        0 ,  0 ,
      100 ,  0 ,
       50 , h  ,
        0 ,  0
    ),

    anion_triangle = tibble::tribble(
      ~x  , ~y ,
      120 ,  0 ,
      220 ,  0 ,
      170 , h  ,
      120 ,  0
    ),

    square = tibble::tribble(
      ~x  , ~y      ,
       60 , h       ,
      160 , h       ,
      160 , h + 100 ,
       60 , h + 100 ,
       60 , h
    ),

    square_grid_v = tibble::tribble(
      ~x  , ~xend , ~y , ~yend   ,
       85 ,    85 , h  , h + 100 ,
      110 ,   110 , h  , h + 100 ,
      135 ,   135 , h  , h + 100
    ),

    square_grid_h = tibble::tribble(
      ~x , ~xend , ~y     , ~yend  ,
      60 ,   160 , h + 25 , h + 25 ,
      60 ,   160 , h + 50 , h + 50 ,
      60 ,   160 , h + 75 , h + 75
    ),

    field_labels = tibble::tribble(
      ~label , ~x    , ~y       ,

      "1"    ,  72.5 , h + 87.5 ,
      "2"    ,  97.5 , h + 87.5 ,
      "3"    , 122.5 , h + 87.5 ,

      "4"    ,  72.5 , h + 62.5 ,
      "5"    ,  97.5 , h + 62.5 ,
      "6"    , 122.5 , h + 62.5 ,

      "7"    ,  72.5 , h + 37.5 ,
      "8"    ,  97.5 , h + 37.5 ,
      "9"    , 122.5 , h + 37.5
    )
  )
}


#' Durov field legend
#'
#' @noRd
.durov_field_legend <- function() {
  tibble::tribble(
    ~field , ~interpretation                          ,

         1 , "Ca-HCO3 facies (recharge waters)"       ,
         2 , "Mixed Ca-Mg-HCO3 facies"                ,
         3 , "Na-HCO3 facies"                         ,

         4 , "Ca-Mg-Cl facies"                        ,
         5 , "Mixed water / no dominant facies"       ,
         6 , "Na-Cl facies"                           ,

         7 , "Ca-Cl facies"                           ,
         8 , "Ion exchange influenced waters"         ,
         9 , "Advanced ion exchange or saline waters"
  )
}

#' Add standards to plot
#'
#' @param plot ggplot object.
#' @param df Plotting dataframe.
#' @param standards Logical.
#'
#' @noRd

.add_standards <- function(
  plot,
  df,
  standards = TRUE
) {
  if (!isTRUE(standards)) {
    return(plot)
  }

  std_df <- .prepare_standards_data(df)

  if (nrow(std_df) == 0) {
    return(plot)
  }

  plot +
    ggplot2::geom_hline(
      data = std_df,
      ggplot2::aes(
        yintercept = .standard_value,
        linetype = .standard_type
      ),
      color = "red",
      linewidth = 0.9,
      inherit.aes = FALSE,
      show.legend = TRUE
    ) +
    ggplot2::scale_linetype_discrete(
      name = "Standard"
    )
}

#' Animate geochemical plots by date
#'
#' @param data Data frame.
#' @param plot_fn Plotting function.
#' @param sample_date Sample date column.
#' @param pause Delay between frames in seconds.
#' @param ...
#' Additional arguments passed to `plot_fn`.
#'
#' @return Invisibly returns NULL.
#'
#' @noRd
.animate_geochemical_plots <- function(
  data,
  plot_fn,
  sample_date = "sample_date",
  pause = 1,
  ...
) {
  dates <- sort(unique(data[[sample_date]]))

  plots <- purrr::map(
    dates,
    function(date) {
      plot_fn(
        data = dplyr::filter(
          data,
          .data[[sample_date]] == date
        ),
        sample_date = sample_date,
        ...
      )
    }
  )

  plots <- unlist(
    plots,
    recursive = FALSE
  )

  for (i in seq_along(plots)) {
    print(plots[[i]])

    readline(
      paste(
        "Frame",
        i,
        "of",
        length(plots),
        "- press Enter"
      )
    )
  }

  invisible(plots)
}

#' Save animated geochemical plots to HTML
#'
#' @param data Data frame.
#' @param plot_fn Plotting function.
#' @param sample_date Sample date column.
#' @param htmlfile Output HTML file.
#' @param title Animation title.
#' @param pause Delay between frames in seconds.
#' @param img.name Base image name.
#' @param ...
#' Additional arguments passed to `plot_fn`.
#'
#' @return Invisibly returns htmlfile.
#'
#' @noRd

.save_geochemical_animation <- function(
  data,
  plot_fn,
  sample_date = "sample_date",
  htmlfile,
  title,
  pause = 1,
  img.name = "Animation",
  ...
) {
  animation::saveHTML(
    expr = {
      .animate_geochemical_plots(
        data = data,
        plot_fn = plot_fn,
        sample_date = sample_date,
        pause = pause,
        ...
      )
    },
    img.name = img.name,
    htmlfile = htmlfile,
    autobrowse = TRUE,
    title = title
  )

  invisible(htmlfile)
}

#' Spatial Plot
#' @export
sp_plot <- function(
  df,
  x = "analysis_result",
  color = "location_id",
  sample_interval = "semi-annually",
  group_var = "param_name",
  facet_var = "sample_date"
) {
  df %>%
    group_by_(group_var) %>%
    do(
      plot = .sp_plot(
        .,
        x = x,
        color = color,
        sample_interval = sample_interval,
        group_var = group_var,
        facet_var = facet_var
      )
    )
}

#' Helper function for sp_plot
#'
#' @noRd
.sp_plot <- function(
  df,
  x = "analysis_result",
  color = "location_id",
  sample_interval = "semi-annually",
  group_var = NULL,
  facet_var = NULL
) {
  if (sample_interval == "semi-annually") {
    df$sample_date <- lubridate::semester(df$sample_date, with_year = TRUE)
  }

  if (sample_interval == "quarterly") {
    df$sample_date <- lubridate::quarter(df$sample_date, with_year = TRUE)
  }

  p <- ggplot(data = df, aes_string(size = x, color = color)) +
    geom_sf() +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_size_continuous(name = "Analysis Result") +
    scale_color_viridis_d("Location ID")

  if (!is.null(facet_var)) {
    p <- p +
      facet_wrap(paste(facet_var)) +
      ggtitle(paste("Plot for", df[[paste(group_var)]][1], "\n", sep = " "))
  }

  print(p)
}
