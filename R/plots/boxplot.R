#' Boxplot of environmental data
#'
#' @param x An env_schema object.
#' @param ... Additional arguments passed to methods.
#'
#' @export
boxplot <- function(x, ...) {
  UseMethod("boxplot")
}


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

  split(x, x[[analyte]]) |>
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

#' @export
boxplot.data.frame <- function(
  x,
  location,
  analyte,
  result,
  date = NULL,
  reporting_flag = NULL,
  unit = NULL,
  ...
) {
  x <- env_schema(
    x,
    location = {{ location }},
    analyte = {{ analyte }},
    result = {{ result }},
    date = {{ date }},
    reporting_flag = {{ reporting_flag }},
    unit = {{ unit }}
  )

  boxplot(x, ...)
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

#' Helper function to plot boxplots for groundwater data
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
#' Helper function to plot boxplots for groundwater data
#'
#' @noRd
.get_plot_title <- function(
  df,
  analyte,
  short_name = FALSE
) {
  name <- dplyr::first(df[[analyte]])

  if (
    isTRUE(short_name) &&
      ".short_name" %in% names(df)
  ) {
    name <- dplyr::first(df$.short_name)
  }

  unit <- .get_unit_label(df)

  if (!identical(unit, "")) {
    name <- paste0(name, " (", unit, ")")
  }

  paste("Boxplot for", name)
}

#' Helper function to plot boxplots for groundwater data
#'
#' @noRd
.get_unit_label <- function(df) {
  if (".analysis_unit" %in% names(df) && !all(is.na(df$.analysis_unit))) {
    return(df$.analysis_unit[[1]])
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
      !".short_name" %in% names(df)
  ) {
    rlang::warn(
      "short_name column not found."
    )
  }

  invisible(TRUE)
}
