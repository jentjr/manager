#' This function plots multiple groundwater data time series by location,
#' or constituent.
#'
#' @param df df groundwater data in tidy format
#' @param x x column for x variable
#' @param y y column for y variable
#' @param facet_var column to facet wrap plots by, default is by location
#' @param group_var column to group plots by, default is by constituent
#' @param scale_y_trans type of transformation to use for y scale. Default is
#' "identity".  Built-in transformations include "asn", "atanh", "boxcox",
#' "exp", "identity", "log", "log10", "log1p", "log2", "logit", "probability",
#' "probit", "reciprocal", "reverse" and "sqrt".
#' @param trend trend add trend line to time series plot
#' @param background vector of dates for background start and end dates.
#' @param limit1 horizontal line 1
#' @param limit2 horizontal line 2
#' @param short_name If TRUE, the constituent name will be abbreviated
#' @param pnt size of points on time series plots
#' @param ncol number of columns
#' @param ... parameters passed to get_theilsen
#' @export

ts_plot <- function(df,
                    x = "sample_date",
                    y = "analysis_result",
                    facet_var = "location_id",
                    group_var = "param_name",
                    lt_measure = "lt_measure",
                    scale_y_trans = "identity",
                    trend = NULL,
                    background = NULL,
                    limit1 = NULL,
                    limit2 = NULL,
                    short_name = FALSE,
                    pnt = 3,
                    ncol = NULL
                    ) {

    df %>%
      group_by_(group_var) %>%
      do(plot = .ts_plot(.,
                         x = x,
                         y = y,
                         group_var = group_var,
                         facet_var = facet_var,
                         lt_measure = lt_measure,
                         scale_y_trans = scale_y_trans,
                         trend = trend,
                         background = background,
                         limit1 = limit1,
                         limit2 = limit2,
                         short_name = short_name,
                         pnt = pnt,
                         ncol = ncol
                         ))
}

#' Helper function for plotting time series of groundwater data
#'
#' @param df df groundwater data in tidy format
#' @param x x column for x variable
#' @param y y column for y variable
#' @param facet_var column to facet wrap plots by, default is by location
#' @param group_var column to group plots by, default is by constituent
#' @param trend trend add trend line to time series plot
#' @param background vecotr of dates for background date range
#' @param limit1 horizontal line 1
#' @param limit2 horizontal line 2
#' @param short_name If TRUE, the constituent name will be abbreviated
#' @param pnt size of points on time series plots
#' @param ncol number of columns
#' @param ... parameters passed to get_theilsen

.ts_plot <- function(df,
                     x = "sample_date",
                     y = "analysis_result",
                     lt_measure = "lt_measure",
                     facet_var = NULL,
                     group_var = NULL,
                     scale_y_trans = "identity",
                     trend = NULL,
                     background = NULL,
                     limit1 = NULL,
                     limit2 = NULL,
                     short_name = FALSE,
                     pnt = 3,
                     ncol = NULL
                     ) {

  df <- df %>%
    mutate(non_detect = if_else(lt_measure == "<", 
                                "non-detect", "detected",
                                 missing = "detected"))

  df <- df %>%
   name_units(short_name = short_name)

  # main plot
  p <- ggplot(data = df, aes_string(x = x, y = y)) +
    geom_line(data = df) +
    geom_point(data = df, aes(shape = factor(non_detect, exclude = NULL)),
               size = pnt) +
    ylab(paste("Analysis Result", "\nScale: ",
               scale_y_trans, sep = "")) +
    xlab("Sample Date") +
    scale_x_datetime(labels = scales::date_format("%Y")) +
    scale_y_continuous(trans = scale_y_trans,
                       breaks = scales::pretty_breaks(),
                       labels = prettyNum) +
    theme(plot.margin = grid::unit(c(1, 1, 1, 1), "lines")) +
    theme_bw() +
    theme(axis.title.x = element_text(size = 15, vjust = -0.3)) +
    theme(axis.title.y = element_text(size = 15, vjust = 0.3)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(colour = guide_legend(override.aes = list(linetype = 0)),
           shape = guide_legend("Detection", override.aes = list(linetype = 0)),
           size = guide_legend("none"),
           linetype = guide_legend("Limits")) +
    scale_shape_manual(values = c("non-detect" = 1, "detected" = 16))

  if (!is.null(trend)) {

      p <- p + geom_smooth(method = trend)

  }

  if (!is.null(facet_var)) {

    p <- p + facet_wrap(paste(facet_var), scale = "free", ncol = ncol) +
      ggtitle(paste("Time Series Plots for",
                    df[[paste(group_var)]][1], "\n", sep = " "))

  }

  if (!is.null(background)) {

    shaded_dates <- data.frame(xmin = background[1], xmax = background[2],
                               ymin = -Inf, ymax = Inf, 
                               years = "background")

    p <- p + geom_rect(data = shaded_dates,
                       aes(xmin = xmin, ymin = ymin, xmax = xmax,
                           ymax = ymax, fill = years), inherit.aes = FALSE) +
      scale_fill_viridis(discrete = TRUE, alpha = 0.3) +
      guides(fill = guide_legend(override.aes = list(linetype = 0),
                                 title = "Date Ranges"))

  }

  if (!is.null(limit1)) {

    df$limit1_name <- paste(limit1[[1]])
    p <- p + geom_hline(data = df,
                        aes_string(yintercept = limit1,
                                   linetype = "limit1_name"),
                        show.legend = TRUE)

  }

  if (!is.null(limit2)) {

    df$limit2_name <- paste(limit2[[1]])
    p <- p + geom_hline(data = df,
                        aes_string(yintercept = limit2,
                                   linetype = "limit2_name"),
                        show.legend = TRUE)

  }

  print(p)

}