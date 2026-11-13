#' Environmental Time Series Plot
#'
#' Creates time-series plots for environmental monitoring data.
#'
#' `series_plot()` is an S3 generic that supports both raw data
#' frames and `env_schema` objects.
#'
#' For data frames, environmental column mappings must be supplied.
#' For `env_schema` objects, mapped schema fields are used automatically.
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return
#' If `plot_type = "split"`, a named list of ggplot objects,
#' one per analyte.
#'
#' If `plot_type = "combined"`, a single ggplot object.
#'
#' @examples
#' data(gw)
#'
#' plots <- series_plot(gw)
#'
#' names(plots)
#'
#' plots[[1]]
#'
#' @export
series_plot <- function(df, ...) {
  UseMethod("series_plot")
}

#' @rdname series_plot
#'
#' @param location Location identifier column.
#' @param sample_date Sample date column.
#' @param analyte Analyte column.
#' @param result Analytical result column.
#' @param unit Reporting unit column.
#' @param reporting_flag Reporting qualifier column.
#' @param reporting_limit Reporting limit column.
#' @param detection_limit Detection limit column.
#'
#' @details
#' For data frames, `series_plot()` first converts the supplied
#' data to an `env_schema` object using the specified column mappings
#' and then dispatches to [series_plot.env_schema()].
#'
#' @export
series_plot.data.frame <- function(
  df,
  location,
  sample_date,
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
    sample_date = {{ sample_date }},
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
#' @param scale Facet scaling behavior passed to
#' [ggplot2::facet_wrap()]. One of `"fixed"`, `"free"`,
#' `"free_x"`, or `"free_y"`.
#'
#' @param scale_y_trans Transformation applied to the y-axis.
#' One of `"identity"`, `"log"`, `"log10"`, `"log2"`,
#' `"log1p"`, `"sqrt"`, `"reverse"`, `"reciprocal"`,
#' `"exp"`, `"boxcox"`, `"logit"`, `"probit"`,
#' `"probability"`, `"atanh"`, or `"asn"`.
#'
#' @param trend Trend line method. One of
#' `"none"`, `"lm"`, `"loess"`, `"gam"`, or `"mann_kendall"`.
#'
#' @param trend_ci Logical. If `TRUE`, confidence intervals are
#' displayed for supported trend models.
#'
#' @param trend_conf_level Confidence level used when
#' `trend_ci = TRUE`.
#'
#' @param trend_label Trend annotation format. One of
#' `"none"`, `"compact"`, or `"full"`.
#'
#' @param trend_label_sig_only Logical. If `TRUE`, trend labels
#' are displayed only for statistically significant trends.
#'
#' @param background Optional vector containing a start date and
#' end date used to shade a background monitoring period.
#'
#' @param standards Logical. If `TRUE`, applicable analytical
#' standards are displayed.
#'
#' @param short_name Logical. If `TRUE`, analyte titles use
#' short analyte labels when available.
#'
#' @param pnt Point size for plotted observations.
#'
#' @param ncol Number of facet columns.
#'
#' @param plot_type Plot layout. One of `"split"` or `"combined"`.
#'
#' @details
#' `series_plot()` produces time-series visualizations of
#' environmental analytical results.
#'
#' Data are grouped by analyte and location and displayed as
#' chronological sampling records.
#'
#' In `"split"` mode, a separate plot is created for each analyte.
#' In `"combined"` mode, all analytes are displayed on a single plot
#' using colour to distinguish analytes.
#'
#' Non-detect observations are automatically distinguished from
#' detected observations using symbol shape.
#'
#' Optional trend lines, trend annotations, analytical standards,
#' and background periods may be added to aid interpretation.
#'
#' For `env_schema` objects, analyte, result, sample_date, unit,
#' detection status, and location information are obtained
#' directly from schema mappings and standardized analytical
#' fields.
#'
#' @section Plot Types:
#'
#' \describe{
#'   \item{split}{
#'   Produces a separate plot for each analyte.
#'   }
#'
#'   \item{combined}{
#'   Produces a single plot containing all analytes with
#'   analytes distinguished by colour.
#'   }
#' }
#'
#' @section Trend Analysis:
#'
#' Several trend-fitting options are available:
#'
#' \describe{
#'
#'   \item{none}{
#'   No trend line is added.
#'   }
#'
#'   \item{lm}{
#'   Linear regression trend.
#'   }
#'
#'   \item{loess}{
#'   Locally weighted smooth trend.
#'   }
#'
#'   \item{gam}{
#'   Generalized additive model smooth.
#'   }
#'
#'   \item{mann_kendall}{
#'   Mann-Kendall trend test with associated annotation.
#'   }
#' }
#'
#' @section Detection Status:
#'
#' Detected and non-detect observations are displayed using
#' different point symbols. Detection status is obtained from
#' standardized censoring information contained in the
#' `env_schema` object.
#'
#' @section Schema-aware Plotting:
#'
#' For `env_schema` objects, `series_plot()` automatically uses:
#'
#' \itemize{
#'   \item the mapped sample_date field;
#'   \item the mapped location field;
#'   \item the mapped analyte field;
#'   \item standardized analysis results;
#'   \item standardized analysis units;
#'   \item standardized censoring indicators.
#' }
#'
#' This allows plots to be generated consistently regardless of
#' original source column names.
#'
#' @section Validation:
#'
#' Input arguments are validated for:
#'
#' \itemize{
#'   \item supported facet scaling options;
#'   \item supported y-axis transformations;
#'   \item supported trend-fitting methods;
#'   \item presence of observations available for plotting.
#' }
#'
#' @examples
#' data(gw)
#'
#' plots <- series_plot(gw)
#'
#' names(plots)
#'
#' plots[[1]]
#'
#' series_plot(
#'   gw,
#'   trend = "lm"
#' )
#'
#' series_plot(
#'   gw,
#'   trend = "mann_kendall",
#'   trend_label = "compact"
#' )
#'
#' series_plot(
#'   gw,
#'   plot_type = "combined"
#' )
#'
#' series_plot(
#'   gw,
#'   scale_y_trans = "log10"
#' )
#'
#' series_plot(
#'   gw,
#'   background = as.POSIXct(
#'     c(
#'       "2010-01-01",
#'       "2012-12-31"
#'     )
#'   )
#' )
#'
#' series_plot(
#'   gw,
#'   standards = TRUE
#' )
#'
#' @export
series_plot.env_schema <- function(
  df,
  result_col = analysis_result_var(df),
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
  ),
  ...
) {
  validate_env_schema(df)

  if (nrow(df) == 0) {
    rlang::abort(
      "No observations available to plot."
    )
  }

  sample_date_col <- sample_date_var(df)

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
            result_col = result_col,
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
    result_col = result_col,
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
  result_col = analysis_result_var(df),
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
  sample_date_col <- sample_date_var(df)

  location_col <- location_var(df)

  analyte_col <- analyte_var(df)

  trend_label <- match.arg(trend_label)

  if (plot_type == "combined") {
    standards <- FALSE
  }

  df <- df |>
    dplyr::arrange(.data[[sample_date_col]]) |>
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

  if (identical(result_col, analysis_result_var(df))) {
    y_label <- "Analysis Result"

    if (length(units) == 1) {
      y_label <- paste0(
        y_label,
        " (",
        units,
        ")"
      )
    }
  } else {
    y_label <- tools::toTitleCase(
      gsub("^\\.", "", result_col)
    )
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
        x = .data[[sample_date_col]],
        y = .data[[result_col]],
        colour = .data[[analyte_col]],
        group = .trend_group
      )
    } else {
      ggplot2::aes(
        x = .data[[sample_date_col]],
        y = .data[[result_col]]
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
      viridis::scale_colour_viridis(
        discrete = TRUE
      ) +
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
    result_col = result_col,
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
#' @param sample_date Sample date column.
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
  sample_date = NULL,
  reporting_flag = NULL,
  unit = NULL,
  ...
) {
  df <- env_schema(
    df,
    location = {{ location }},
    analyte = {{ analyte }},
    result = {{ result }},
    sample_date = {{ sample_date }},
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
#' Analyte grouping is determined by the mapped analyte variable,
#' not by internal standardized analyte metadata columns.
#'
#' For `env_schema` objects, `boxplot()` automatically uses the
#' schema mappings and standardized analytical fields:
#'
#' \itemize{
#'   \item the mapped location field
#'   \item the mapped analyte field
#'   \item the mapped result field
#'   \item the mapped reporting flag field
#'   \item `analysis_unit()` for axis labeling
#'   \item `analyte_label()` and `short_analyte_label()` for titles
#' }
#'
#' This allows boxplots to be generated consistently regardless of
#' the original source column names used by the underlying dataset.
#'
#' @return
#' A named list of plot objects. One boxplot is produced for each
#' analyte in the mapped analyte column.
#'
#'
#' @export
boxplot.env_schema <- function(
  df,
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
  validate_env_schema(df)

  if (nrow(df) == 0) {
    rlang::abort(
      "No observations available to plot."
    )
  }

  .validate_boxplot_args(
    df = df,
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

  location <- location_var(df)
  analyte <- analyte_var(df)
  result <- analysis_result_var(df)
  reporting_flag <- reporting_flag_var(df)

  split(df, as.character(df[[analyte]])) |>
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

  result_col <- analysis_result_var(df)

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
#' `corr_plot()` is an S3 generic that supports both raw data frames
#' and `env_schema` objects.
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return A `GGally::ggpairs` correlation plot.
#'
#' @export
corr_plot <- function(df, ...) {
  UseMethod("corr_plot")
}

#' @rdname corr_plot
#'
#' @param location Location identifier column.
#' @param analyte Analyte column.
#' @param result Analytical result column.
#' @param unit Reporting unit column.
#' @param sample_date Sample date column.
#'
#' @details
#' For data frames, `corr_plot()` converts the supplied data to an
#' `env_schema` object using the specified column mappings, then
#' dispatches to [corr_plot.env_schema()].
#'
#' @export
corr_plot.data.frame <- function(
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

  corr_plot(df, ...)
}

#' @rdname corr_plot
#'
#' @param constituents Character vector of analytes to include. Defaults to
#'   all analytes in the data.
#' @param sample_locations Character vector of locations to include. Defaults
#'   to all locations in the data.
#' @param viridis_option Viridis palette option. One of `"A"` through `"H"`.
#'
#' @export
corr_plot.env_schema <- function(
  df,
  constituents = NULL,
  sample_locations = NULL,
  viridis_option = "D",
  ...
) {
  validate_env_schema(df)

  if (!requireNamespace("GGally", quietly = TRUE)) {
    cli::cli_abort(
      "{.pkg GGally} is required to create correlation plots."
    )
  }

  location_col <- location_var(df)
  sample_date_col <- sample_date_var(df)
  analyte_col <- analyte_var(df)
  result_col <- analysis_result_var(df)

  corr_data <- .get_corr_data(
    df = df,
    constituents = constituents,
    sample_locations = sample_locations
  )

  correlation_columns <- setdiff(
    names(corr_data),
    c(location_col, sample_date_col)
  )

  if (length(correlation_columns) < 2) {
    cli::cli_abort(
      "At least two analytes are required to create a correlation plot."
    )
  }

  plot <- GGally::ggpairs(
    data = corr_data,
    columns = correlation_columns,
    mapping = ggplot2::aes(
      colour = !!rlang::sym(location_col)
    )
  )

  if (requireNamespace("viridis", quietly = TRUE)) {
    plot <- plot +
      viridis::scale_colour_viridis(
        discrete = TRUE,
        option = viridis_option,
        begin = 0.05,
        end = 0.95,
        name = location_col
      )
  } else {
    plot <- plot +
      ggplot2::scale_colour_discrete(
        name = location_col
      )
  }

  plot
}

#' Prepare correlation data from an env_schema object
#'
#' @noRd
.get_corr_data <- function(
  df,
  constituents = NULL,
  sample_locations = NULL
) {
  location_col <- location_var(df)
  sample_date_col <- sample_date_var(df)
  analyte_col <- analyte_var(df)
  result_col <- analysis_result_var(df)

  if (is.null(constituents)) {
    constituents <- df |>
      dplyr::pull(.data[[analyte_col]]) |>
      unique() |>
      stats::na.omit()
  }

  if (is.null(sample_locations)) {
    sample_locations <- df |>
      dplyr::pull(.data[[location_col]]) |>
      unique() |>
      stats::na.omit()
  }

  if (length(constituents) == 0) {
    cli::cli_abort("No constituents were supplied or found in the data.")
  }

  if (length(sample_locations) == 0) {
    cli::cli_abort("No sample locations were supplied or found in the data.")
  }

  df |>
    dplyr::filter(
      .data[[analyte_col]] %in% constituents,
      .data[[location_col]] %in% sample_locations
    ) |>
    dplyr::group_by(
      .data[[location_col]],
      .data[[sample_date_col]],
      .data[[analyte_col]]
    ) |>
    dplyr::summarise(
      .analysis_result = if (all(is.na(.data[[result_col]]))) {
        NA_real_
      } else {
        mean(.data[[result_col]], na.rm = TRUE)
      },
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(
        c(location_col, sample_date_col)
      ),
      names_from = dplyr::all_of(analyte_col),
      values_from = .analysis_result
    )
}

#' Piper Diagram
#'
#' Creates a Piper diagram for hydrochemical facies interpretation
#' using major-ion chemistry data.
#'
#' `piper_plot()` is an S3 generic that supports both raw data frames
#' and `env_schema` objects.
#'
#' For data frames, environmental column mappings must be supplied.
#' For `env_schema` objects, mapped schema fields are used automatically.
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return
#' A ggplot object containing a Piper diagram.
#'
#' @export
piper_plot <- function(df, ...) {
  UseMethod("piper_plot")
}

#' @rdname piper_plot
#'
#' @param location Location identifier column.
#' @param analyte Analyte column.
#' @param result Analytical result column.
#' @param unit Reporting unit column.
#' @param sample_date Sample date column.
#'
#' @details
#' For data frames, `piper_plot()` first converts the supplied data
#' to an `env_schema` object using the specified column mappings and
#' then dispatches to [piper_plot.env_schema()].
#'
#' @export
piper_plot.data.frame <- function(
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

  piper_plot(df, ...)
}


#' @rdname piper_plot
#'
#' @param analyte_matching Analyte matching strategy used to resolve
#' analyte names. One of `"exact"`, `"strict"`, or `"normalized"`.
#'
#' @param x_cation Character vector identifying analytes representing
#' calcium.
#'
#' @param y_cation Character vector identifying analytes representing
#' magnesium.
#'
#' @param z_cation Character vector identifying analytes representing
#' sodium and potassium.
#'
#' @param x_anion Character vector identifying analytes representing
#' chloride and fluoride.
#'
#' @param y_anion Character vector identifying analytes representing
#' alkalinity.
#'
#' @param z_anion Character vector identifying analytes representing
#' sulfate.
#'
#' @param x_cation_label Label for the calcium axis.
#'
#' @param y_cation_label Label for the magnesium axis.
#'
#' @param z_cation_label Label for the sodium plus potassium axis.
#'
#' @param x_anion_label Label for the chloride plus fluoride axis.
#'
#' @param y_anion_label Label for the bicarbonate/carbonate axis.
#'
#' @param z_anion_label Label for the sulfate axis.
#'
#' @param x_z_anion_label Label displayed on the upper-left side of the
#' Piper diamond.
#'
#' @param x_y_cation_label Label displayed on the upper-right side of
#' the Piper diamond.
#'
#' @param scale_analyte Optional analyte used to scale point size.
#'
#' @param group_col Optional column used to assign point shape.
#'
#' @param transparency Point transparency.
#'
#' @param pnt_size Point size when size scaling is not used.
#'
#' @param label_size Axis label size.
#'
#' @param title Optional plot title.
#'
#' @details
#' Piper diagrams display normalized major-ion chemistry within
#' cation and anion ternary plots and a central hydrochemical
#' facies diamond.
#'
#' Major ions are converted to milliequivalents per liter (meq/L),
#' normalized to ionic percentages, and transformed into Piper
#' coordinates before plotting.
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
#' @section Validation:
#'
#' Input data are validated for:
#'
#' \itemize{
#'   \item required major ions;
#'   \item analyte resolution;
#'   \item supported analytical units;
#'   \item duplicate analyte measurements;
#'   \item completeness of major-ion chemistry.
#' }
#'
#' @examples
#' data(gw)
#'
#' piper_plot(gw)
#'
#' piper_plot(
#'   gw,
#'   analyte_matching = "normalized"
#' )
#'
#' piper_plot(
#'   gw,
#'   scale_analyte = "Total Dissolved Solids"
#' )
#'
#' piper_plot(
#'   gw,
#'   x_cation_label = "Ca~phantom()^+2"
#' )
#'
#' @export
piper_plot.env_schema <- function(
  df,
  analyte_matching = c("strict", "exact", "normalized"),
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
  title = NULL,
  ...
) {
  validate_env_schema(df)

  analyte_matching <- rlang::arg_match(analyte_matching)

  location_col <- location_var(df)
  sample_date_col <- sample_date_var(df)
  analyte_col <- analyte_var(df)
  result_col <- analysis_result_var(df)
  unit_col <- analysis_unit_var(df)

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

  piper_df <- df |>
    .transform_piper_data(
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
    group_df <- df |>
      dplyr::distinct(
        .data[[location_col]],
        .data[[group_col]]
      )
    dup_groups <- group_df |>
      dplyr::count(.data[[location_col]]) |>
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
      location_col,
      location_col
    )

    piper_df <- piper_df |>
      dplyr::left_join(
        group_df,
        by = join_by
      )
  }

  cation_df <- piper_df |>
    dplyr::filter(
      !is.na(cation_x),
      !is.na(cation_y)
    )

  anion_df <- piper_df |>
    dplyr::filter(
      !is.na(anion_x),
      !is.na(anion_y)
    )

  diamond_df <- piper_df |>
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
    location_col = location_col,
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
    location_col = location_col,
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
    location_col = location_col,
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
    colour = ggplot2::guide_legend(
      title = location_col
    ),
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
  location_col,
  group_col = NULL,
  scale_col = NULL,
  alpha = 0.7,
  point_size = 3
) {
  aes_args <- list(
    x = rlang::sym(x),
    y = rlang::sym(y),
    colour = rlang::sym(location_col)
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


#' Animate Piper Diagrams
#'
#' Creates a frame-by-frame Piper diagram animation through time.
#'
#' @param df An `env_schema` object.
#'
#' @inheritDotParams piper_plot
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
piper_time_plot <- function(df, ...) {
  if (!requireNamespace("animation", quietly = TRUE)) {
    stop(
      "animation package needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  sample_date_col <- sample_date_var(df)

  iter <- unique(df[[sample_date_col]])

  for (i in seq_along(iter)) {
    piper_plot(
      df[df[[sample_date_col]] == iter[i], ],
      title = paste(iter[i]),
      ...
    )

    animation::ani.pause()
  }
}

#' Save Animated Piper Diagram
#'
#' Creates a Piper diagram animation through time and saves the
#' animation as an HTML file.
#'
#' @param df An `env_schema` object.
#'
#' @inheritDotParams piper_plot
#'
#' @return Invisibly returns the output HTML filename.
#'
#' @export
piper_time_html <- function(df, ...) {
  if (!requireNamespace("animation", quietly = TRUE)) {
    stop(
      "animation package needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  sample_date_col <- sample_date_var(df)

  animation::saveHTML(
    {
      animation::ani.options(
        nmax = length(unique(df[[sample_date_col]])),
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

#' Schoeller Diagram
#'
#' Creates a Schoeller diagram for comparing major-ion chemistry among
#' groundwater or surface water samples.
#'
#' `schoeller_plot()` is an S3 generic that supports both raw data
#' frames and `env_schema` objects.
#'
#' For data frames, environmental column mappings must be supplied.
#' For `env_schema` objects, mapped schema fields are used automatically.
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return
#' A ggplot object.
#'
#' @export
schoeller_plot <- function(df, ...) {
  UseMethod("schoeller_plot")
}

#' @rdname schoeller_plot
#'
#' @param location Location identifier column.
#' @param analyte Analyte column.
#' @param result Analytical result column.
#' @param unit Reporting unit column.
#' @param sample_date Sample date column.
#'
#' @details
#' For data frames, `schoeller_plot()` first converts the supplied
#' data to an `env_schema` object using the specified column mappings
#' and then dispatches to [schoeller_plot.env_schema()].
#'
#' @export
schoeller_plot.data.frame <- function(
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

  schoeller_plot(df, ...)
}


#' @rdname schoeller_plot
#'
#' @param calcium Calcium analyte.
#'
#' @param magnesium Magnesium analyte.
#'
#' @param sodium Sodium analyte.
#'
#' @param potassium Potassium analyte.
#'
#' @param chloride Chloride analyte.
#'
#' @param sulfate Sulfate analyte.
#'
#' @param alkalinity Alkalinity analyte.
#'
#' @param analyte_matching Analyte matching strategy. One of
#' `"exact"`, `"strict"`, or `"normalized"`.
#'
#' @param facet_var Column used for faceting. Must be either the mapped
#' location field or mapped sample_date field. Defaults to the mapped location field.
#'
#' @param title Optional plot title.
#'
#' @param lwt Line width.
#'
#' @details
#' Schoeller diagrams display major-ion concentrations on a
#' logarithmic scale and are commonly used to compare water
#' chemistry among locations and sampling events.
#'
#' Major-ion concentrations are converted to milliequivalents
#' per liter (meq/L) and plotted as connected profiles across
#' standard hydrochemical ion categories.
#'
#' Sodium and potassium are combined into a single
#' `"Na + K"` axis following conventional Schoeller diagram
#' practice.
#'
#' Missing analytes are permitted and omitted from the
#' resulting profile.
#'
#' For `env_schema` objects, analyte, result, unit, sample_date,
#' and location fields are determined from schema mappings.
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
#' @section Faceting:
#'
#' If `facet_var` is the mapped location field,
#' each location is displayed in a separate panel and
#' sample events are drawn as individual profiles.
#'
#' If `facet_var` is the mapped sample_date field,
#' each sampling event is displayed in a separate panel and
#' locations are drawn as individual profiles.
#'
#' @section Validation:
#'
#' Input data are validated for:
#'
#' \itemize{
#'   \item analyte resolution;
#'   \item supported analytical units;
#'   \item major-ion chemistry availability;
#'   \item successful conversion to milliequivalents.
#' }
#'
#' @examples
#' data(gw)
#'
#' schoeller_plot(gw)
#'
#'
#' schoeller_plot(
#'   gw,
#'   facet_var = sample_date_var(gw)
#' )
#'
#' schoeller_plot(
#'   gw,
#'   analyte_matching = "normalized"
#' )
#'
#' schoeller_plot(
#'   gw,
#'   title = "Major Ion Chemistry",
#'   lwt = 1.5
#' )
#'
#' @export
schoeller_plot.env_schema <- function(
  df,
  calcium = "Calcium, dissolved",
  magnesium = "Magnesium, dissolved",
  sodium = "Sodium, dissolved",
  potassium = "Potassium, dissolved",
  chloride = "Chloride, total",
  sulfate = "Sulfate, total",
  alkalinity = "Alkalinity, total (lab)",
  analyte_matching = c(
    "exact",
    "strict",
    "normalized"
  ),
  facet_var = NULL,
  title = NULL,
  lwt = 1,
  ...
) {
  location_col <- location_var(df)

  sample_date_col <- sample_date_var(df)

  analyte_matching <- rlang::arg_match(
    analyte_matching
  )

  if (is.null(facet_var)) {
    facet_var <- location_col
  }

  if (!facet_var %in% names(df)) {
    cli::cli_abort(
      "{.field {facet_var}} not found in {.arg df}."
    )
  }

  if (!facet_var %in% c(location_col, sample_date_col)) {
    cli::cli_abort(
      c(
        "{.arg facet_var} must be either",
        "{.val {location_col}} or",
        "{.val {sample_date_col}}."
      )
    )
  }
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
    location_col = location_col,
    sample_date_col = sample_date_col,
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
  location_col,
  sample_date_col,
  facet_var,
  lwt
) {
  group_col <- if (identical(facet_var, location_col)) {
    sample_date_col
  } else {
    location_col
  }

  p +
    ggplot2::geom_line(
      ggplot2::aes(
        group = .data[[group_col]],
        colour = factor(.data[[group_col]])
      ),
      linewidth = lwt
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(!!rlang::sym(facet_var))
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        title = tools::toTitleCase(
          gsub("_", " ", group_col)
        )
      )
    )
}

#' Stiff Diagram
#'
#' Creates Stiff diagrams for visualization and comparison of
#' major-ion water chemistry.
#'
#' `stiff_plot()` is an S3 generic that supports both raw data
#' frames and `env_schema` objects.
#'
#' For data frames, environmental column mappings must be supplied.
#' For `env_schema` objects, mapped schema fields are used automatically.
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return
#' A named list of ggplot objects. One Stiff diagram is produced
#' for each value of `group_var`.
#'
#' @export
stiff_plot <- function(df, ...) {
  UseMethod("stiff_plot")
}

#' @rdname stiff_plot
#'
#' @param location Location identifier column.
#' @param analyte Analyte column.
#' @param result Analytical result column.
#' @param unit Reporting unit column.
#' @param sample_date Sample date column.
#'
#' @details
#' For data frames, `stiff_plot()` first converts the supplied
#' data to an `env_schema` object using the specified column mappings
#' and then dispatches to [stiff_plot.env_schema()].
#'
#' @export
stiff_plot.data.frame <- function(
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

  stiff_plot(df, ...)
}

#' @rdname stiff_plot
#'
#' @param analyte_matching Analyte matching strategy. One of
#' `"exact"`, `"strict"`, or `"normalized"`.
#'
#' @param calcium Calcium analyte.
#'
#' @param magnesium Magnesium analyte.
#'
#' @param sodium Sodium analyte.
#'
#' @param potassium Potassium analyte.
#'
#' @param chloride Chloride analyte.
#'
#' @param sulfate Sulfate analyte.
#'
#' @param alkalinity Alkalinity analyte.
#'
#' @param scale_analyte Optional analyte used to scale polygon fill.
#'
#' @param group_var Variable used to split the data into individual
#' Stiff diagrams.
#'
#' @param facet_var Variable used for faceting within each plot.
#'
#' @param stiff_limit Maximum absolute x-axis extent expressed in
#' milliequivalents per liter (meq/L). If `NULL`, the limit is
#' determined automatically from the data.
#'
#' @param stiff_breaks Grid spacing used for x-axis and reference
#' lines.
#'
#' @param lines Logical. If `TRUE`, horizontal reference lines are
#' drawn through each ion category.
#'
#' @param title Optional plot title.
#'
#' @details
#' Stiff diagrams display major-ion chemistry as a polygon whose
#' shape reflects relative ionic composition.
#'
#' Major-ion analytes are converted to milliequivalents per liter
#' (meq/L) and transformed into standard Stiff-diagram coordinates.
#'
#' Cations are plotted on the left side of the diagram and anions
#' are plotted on the right side.
#'
#' Similar polygon shapes generally indicate similar hydrochemical
#' characteristics, making Stiff diagrams useful for evaluating:
#'
#' \itemize{
#'   \item spatial variability;
#'   \item temporal trends;
#'   \item groundwater mixing;
#'   \item hydrochemical facies;
#'   \item geochemical evolution.
#' }
#'
#' For `env_schema` objects, analyte, result, unit, sample_date,
#' and location fields are obtained directly from schema mappings.
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
#' @section Grouping:
#'
#' `group_var` controls how data are divided into separate
#' Stiff diagrams.
#'
#' By default, one plot is generated per location.
#'
#' If `group_var` is set to the mapped sample_date field, one plot is
#' generated per sampling event.
#'
#' @section Faceting:
#'
#' `facet_var` controls the panels displayed within each Stiff
#' diagram plot.
#'
#' By default, individual sampling events are shown as separate
#' facets.
#'
#' @section Scaling:
#'
#' When `scale_analyte` is supplied, polygon fill colour is scaled
#' using values of the selected analyte.
#'
#' This is often useful for displaying Total Dissolved Solids (TDS)
#' or another indicator parameter alongside hydrochemical facies.
#'
#' @examples
#' data(gw)
#'
#' stiff_plot(gw)
#'
#' stiff_plot(
#'   gw,
#'   lines = TRUE
#' )
#'
#' stiff_plot(
#'   gw,
#'   scale_analyte = "Total Dissolved Solids"
#' )
#'
#' stiff_plot(
#'   gw,
#'   group_var = sample_date_var(gw)
#' )
#'
#' @return
#' A named list of ggplot objects. One Stiff diagram is produced
#' for each value of `group_var`.
#'
#' @export

stiff_plot.env_schema <- function(
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
  scale_analyte = NULL,
  group_var = NULL,
  facet_var = NULL,
  stiff_limit = NULL,
  stiff_breaks = 1,
  lines = FALSE,
  title = NULL,
  ...
) {
  location_col <- location_var(df)

  sample_date_col <- sample_date_var(df)

  analyte_col <- analyte_var(df)

  result_col <- analysis_result_var(df)

  unit_col <- analysis_unit_var(df)

  if (is.null(group_var)) {
    group_var <- location_col
  }

  if (is.null(facet_var)) {
    facet_var <- sample_date_col
  }

  if (!facet_var %in% names(df)) {
    cli::cli_abort(
      "{.field {facet_var}} not found in {.arg df}."
    )
  }

  if (!group_var %in% names(df)) {
    cli::cli_abort(
      "{.field {group_var}} not found in {.arg df}."
    )
  }

  if (!facet_var %in% c(location_col, sample_date_col)) {
    cli::cli_abort(
      c(
        "{.arg facet_var} must be either",
        "{.val {location_col}} or",
        "{.val {sample_date_col}}."
      )
    )
  }

  if (!group_var %in% c(location_col, sample_date_col)) {
    cli::cli_abort(
      c(
        "{.arg group_var} must be either",
        "{.val {location_col}} or",
        "{.val {sample_date_col}}."
      )
    )
  }

  split(df, df[[group_var]]) |>
    purrr::imap(
      ~ .stiff_plot(
        df = .x,
        group_name = .y,
        location_col = location_col,
        sample_date_col = sample_date_col,
        analyte_matching = analyte_matching,
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
  df,
  location_col,
  sample_date_col,
  group_name,
  analyte_matching,
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
    analyte_matching = analyte_matching
  )

  scale_df <- NULL

  if (!is.null(scale_analyte)) {
    scale_df <- .prepare_geochemical_data(
      df = df,
      analytes = scale_analyte
    )
  }

  stiff_data <- .transform_stiff_data(
    chemistry_df = chemistry_df,
    scale_df = scale_df,
    scale_analyte = scale_analyte
  )

  if (nrow(stiff_data) == 0) {
    cli::cli_warn(
      "No valid major-ion chemistry available for plotting."
    )

    return(NULL)
  }

  .build_stiff_plot(
    df = stiff_data,
    location_col = location_col,
    sample_date_col = sample_date_col,
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
  df,
  location_col,
  sample_date_col,
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
      max(abs(df$stiff_x), na.rm = TRUE)
    )

    stiff_limit <- max(
      stiff_limit,
      1
    )
  }

  grid_breaks <- setdiff(seq(-stiff_limit, stiff_limit, by = stiff_breaks), 0)

  if (max(abs(df$stiff_x), na.rm = TRUE) > stiff_limit) {
    cli::cli_warn(
      "Some samples exceed {.arg stiff_limit} and will be truncated."
    )
  }

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = stiff_x,
      y = stiff_y,
      group = interaction(
        .data[[location_col]],
        .data[[sample_date_col]]
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
            .data[[location_col]],
            .data[[sample_date_col]]
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
            .data[[location_col]],
            .data[[sample_date_col]]
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

  label_data <- df |>
    dplyr::distinct(
      .data[[facet_var]],
      ion,
      label_side,
      stiff_y
    )

  label_data <- label_data |>
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
#' @param df Data frame of water quality data.
#' @param sample_date Sample date column.
#' @param pause Delay between frames in seconds.
#' @param ...
#' Additional arguments passed to `stiff_plot()`.
#'
#' @export

stiff_time_plot <- function(
  df,
  pause = 1,
  ...
) {
  .animate_geochemical_plots(
    df = df,
    plot_fn = stiff_plot,
    pause = pause,
    ...
  )
}

#' Save animated Stiff Diagram to HTML
#'
#' @param df Data frame of water quality data.
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
  df,
  htmlfile = "Stiff_plot.html",
  pause = 1,
  ...
) {
  .save_geochemical_animation(
    df = df,
    plot_fn = stiff_plot,
    htmlfile = htmlfile,
    title = "Stiff Diagram",
    img.name = "StiffPlot",
    pause = pause,
    ...
  )
}

#' Durov Diagram
#'
#' Creates a Durov diagram for interpretation of hydrochemical
#' facies, mixing processes, ion exchange, and geochemical
#' evolution.
#'
#' `durov_plot()` is an S3 generic that supports both raw data
#' frames and `env_schema` objects.
#'
#' For data frames, environmental column mappings must be supplied.
#' For `env_schema` objects, mapped schema fields are used
#' automatically.
#'
#' @param df A data frame or `env_schema` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return
#' A ggplot object.
#'
#' @export
durov_plot <- function(df, ...) {
  UseMethod("durov_plot")
}

#' @rdname durov_plot
#'
#' @param location Location identifier column.
#' @param analyte Analyte column.
#' @param result Analytical result column.
#' @param unit Reporting unit column.
#' @param sample_date Sample date column.
#'
#' @details
#' For data frames, `durov_plot()` first converts the supplied
#' data to an `env_schema` object using the specified column
#' mappings and then dispatches to [durov_plot.env_schema()].
#'
#' @export
durov_plot.data.frame <- function(
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

  durov_plot(df, ...)
}


#' @rdname durov_plot
#'
#' @param analyte_matching Analyte matching strategy. One of
#' `"exact"`, `"strict"`, or `"normalized"`.
#'
#' @param title Optional plot title.
#'
#' @details
#' Durov diagrams combine cation and anion ternary plots with
#' a central projection field to aid interpretation of
#' hydrochemical facies, mixing trends, ion exchange processes,
#' and geochemical evolution.
#'
#' Major-ion concentrations are converted to milliequivalents
#' per liter (meq/L), normalized to ionic percentages, and
#' transformed into Durov coordinates before plotting.
#'
#' The central Durov field may be used to evaluate:
#'
#' \itemize{
#'   \item groundwater recharge processes;
#'   \item mixing between water types;
#'   \item ion exchange reactions;
#'   \item salinization trends;
#'   \item hydrochemical evolution.
#' }
#'
#' For `env_schema` objects, analyte, result, unit, sample_date,
#' and location fields are obtained directly from schema
#' mappings.
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
#' @section Interpretation:
#'
#' The Durov projection field is divided into nine regions
#' representing characteristic hydrochemical facies and
#' geochemical processes.
#'
#' These fields should be interpreted as general hydrochemical
#' indicators and evaluated alongside site-specific geological,
#' hydrological, and geochemical information.
#'
#' @section Validation:
#'
#' Input data are validated for:
#'
#' \itemize{
#'   \item major-ion chemistry availability;
#'   \item analyte resolution;
#'   \item supported analytical units;
#'   \item successful conversion to milliequivalents;
#'   \item completeness of major-ion chemistry.
#' }
#'
#' @examples
#' data(gw)
#'
#' durov_plot(gw)
#'
#' durov_plot(
#'   gw,
#'   analyte_matching = "normalized"
#' )
#'
#' durov_plot(
#'   gw,
#'   title = "Hydrochemical Facies"
#' )
#'
#' @export
durov_plot.env_schema <- function(
  df,
  analyte_matching = c(
    "normalized",
    "strict",
    "exact"
  ),
  title = NULL,
  ...
) {
  validate_env_schema(df)

  location_col <- location_var(df)

  analyte_matching <- rlang::arg_match(
    analyte_matching
  )

  chemistry_df <- .prepare_major_ion_chemistry(
    df = df,
    analyte_matching = analyte_matching
  )

  plot_df <- .transform_durov_data(
    chemistry_df
  )

  plot_df <- plot_df |>
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
        colour = .data[[location_col]]
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
        colour = .data[[location_col]]
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
        colour = .data[[location_col]]
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
#' @param df Data frame.
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
  df,
  plot_fn,
  pause = 1,
  ...
) {
  sample_date_col <- sample_date_var(df)

  dates <- sort(unique(df[[sample_date_col]]))

  plots <- purrr::map(
    dates,
    function(date) {
      plot_fn(
        dplyr::filter(
          df,
          .data[[sample_date_col]] == date
        ),
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
#' @param df Data frame.
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
  df,
  plot_fn,
  htmlfile,
  title,
  pause = 1,
  img.name = "Animation",
  ...
) {
  animation::saveHTML(
    expr = {
      .animate_geochemical_plots(
        df = df,
        plot_fn = plot_fn,
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
  df |>
    group_by_(group_var) |>
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
