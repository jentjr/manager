#' This function plots multiple groundwater data time series by location,
#' or constituent
#'
#' @param df df groundwater data in tidy format
#' @param x x column for x variable
#' @param y y column for y variable
#' @param facet_var column to facet wrap plots by, default is by location
#' @param group_var column to group plots by, default is by constituent
#' @param lt_measure column for non-detect symbol.
#' Symbol can be either "<", "ND", or "U"
#' @param scale "fixed" or "free", or in one dimension "free_x", "free_y"
#' @param scale_y_trans type of transformation to use for y scale. Default is
#' "identity".  Built-in transformations include "asn", "atanh", "boxcox",
#' "exp", "identity", "log", "log10", "log1p", "log2", "logit", "probability",
#' "probit", "reciprocal", "reverse" and "sqrt".
#' @param trend trend add trend line to time series plot
#' @param background vector of dates for background start and end dates.
#' @param standards Logical. If TRUE, standards from the internal
#' standards_df lookup table are added as horizontal reference lines.
#' @param short_name If TRUE, the constituent name will be abbreviated
#' @param pnt size of points on time series plots
#' @param ncol number of columns
#'
#' @export

series_plot <- function(
  df,
  x = "sample_date",
  y = "analysis_result",
  facet_var = "location_id",
  group_var = "param_name",
  lt_measure = "lt_measure",
  scale = "free",
  scale_y_trans = "identity",
  trend = "none",
  trend_ci = FALSE,
  trend_conf_level = 0.95,
  trend_label = c("none", "compact", "full"),
  trend_label_sig_only = TRUE,
  background = NULL,
  standards = TRUE,
  short_name = FALSE,
  pnt = 3,
  ncol = NULL,
  plot_type = c("split", "combined")
) {
  plot_type <- match.arg(plot_type)
  trend_label <- match.arg(trend_label)
  valid_scales <- c("fixed", "free", "free_x", "free_y")

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
      split(df, df[[group_var]]) %>%
        purrr::imap(
          ~ .series_plot(
            .x,
            x = x,
            y = y,
            facet_var = facet_var,
            group_var = group_var,
            lt_measure = lt_measure,
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
    df,
    x = x,
    y = y,
    facet_var = facet_var,
    group_var = group_var,
    lt_measure = lt_measure,
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

#' Helper function for plotting time series of groundwater data
#'
#' @noRd

.series_plot <- function(
  df,
  x = "sample_date",
  y = "analysis_result",
  lt_measure = "lt_measure",
  facet_var = NULL,
  group_var = NULL,
  scale = "free",
  scale_y_trans = "identity",
  trend = "none",
  trend_ci = FALSE,
  trend_conf_level = 0.95,
  trend_label = c("none", "compact", "full"),
  trend_label_sig_only = TRUE,
  background = NULL,
  standards = TRUE,
  short_name = FALSE,
  pnt = 3,
  ncol = NULL,
  plot_type = "split"
) {
  x_sym <- rlang::sym(x)
  y_sym <- rlang::sym(y)
  group_sym <- rlang::sym(group_var)
  lt_sym <- rlang::sym(lt_measure)

  trend_label <- match.arg(trend_label)

  df <- df %>%
    dplyr::arrange(!!x_sym) %>%
    dplyr::mutate(
      non_detect = factor(
        dplyr::case_when(
          toupper(trimws(as.character(!!lt_sym))) %in%
            c("<", "ND", "U") ~ "Non-detect",
          TRUE ~ "Detected"
        ),
        levels = c(
          "Detected",
          "Non-detect"
        )
      )
    ) %>%
    name_units(
      short_name = short_name
    )

  df <- df %>%
    dplyr::mutate(
      .trend_group = interaction(
        .data$location_id,
        .data$param_name,
        drop = TRUE
      )
    )

  y_label <- paste0(
    "Analysis Result (",
    df$default_unit[1],
    ")"
  )

  if (scale_y_trans != "identity") {
    y_label <- paste(
      y_label,
      paste0("Scale: ", scale_y_trans),
      sep = "\n"
    )
  }

  mapping <-
    if (plot_type == "combined") {
      ggplot2::aes(
        x = !!x_sym,
        y = !!y_sym,
        colour = !!group_sym,
        group = .trend_group
      )
    } else {
      ggplot2::aes(
        x = !!x_sym,
        y = !!y_sym
      )
    }

  p <- ggplot2::ggplot(
    df,
    mapping
  ) +
    ggplot2::geom_path(linewidth = 0.4) +
    ggplot2::geom_point(
      ggplot2::aes(shape = non_detect),
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
      plot.margin = grid::unit(c(1, 1, 1, 1), "lines"),
      axis.title.x = ggplot2::element_text(size = 15, vjust = -0.3),
      axis.title.y = ggplot2::element_text(size = 15, vjust = 0.3),
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        title = "Standard"
      ),
      shape = ggplot2::guide_legend(
        override.aes = list(
          linetype = 0
        )
      ),
      size = ggplot2::guide_legend(
        "none"
      ),
      linetype = ggplot2::guide_legend(
        title = "Standard"
      )
    )

  p <- .add_trend(
    p = p,
    df = df,
    x = x,
    y = y,
    trend = trend,
    trend_ci = trend_ci,
    trend_conf_level = trend_conf_level,
    trend_label = trend_label,
    trend_label_sig_only = trend_label_sig_only,
    plot_type = plot_type,
    group_var = group_var
  )

  if (!is.null(facet_var)) {
    facet_sym <- rlang::sym(facet_var)

    p <- p +
      ggplot2::facet_wrap(
        ggplot2::vars(!!facet_sym),
        scales = scale,
        ncol = ncol
      )
  }

  if (
    plot_type == "split" &&
      !is.null(group_var)
  ) {
    p <- p +
      ggplot2::ggtitle(
        sprintf(
          "Time Series Plot for %s",
          unique(df[[group_var]])[1]
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
        ggplot2::aes(
          xmin = xmin,
          ymin = ymin,
          xmax = xmax,
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

  # add standards from plotting dataframe
  p <- .add_standards(
    plot = p,
    df = df,
    standards = standards
  )

  p
}
