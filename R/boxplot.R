#' Boxplot function
#'
#' @param df groundwater data in tidy format
#' @param x column to be used for the x axis, default is location
#' @param y column to be used for the y axis, default is analysis result
#' @param lt_measure column for >, or < identifier
#' @param group_var the name of column used for constituents. Default is
#' "param_name"
#' @param coef length of the whiskers as multiple of the IQR. Defualt is 3.
#' @param scale_y_trans type of transformation to use for y scale. Default is
#' "identity".  Built-in transformations include "asn", "atanh", "boxcox",
#' "exp", "identity", "log", "log10", "log1p", "log2", "logit", "probability",
#' "probit", "reciprocal", "reverse" and "sqrt".
#' @param show_points plot the individual points using ggbeeswarm.
#' @param fill column used to fill the variable
#' @param notch TRUE/FALSE plot a notched boxplot. 
#' The notches (if requested) extend to +/-1.58 IQR/sqrt(n)
#' @param plot_ci add boostrapped 95% confidence interval for mean
#' @param limit1 column to be used to draw horizontal line
#' @param limit2 column to be used to draw a second horizontal line
#' @param pnt size of points
#' @param short_name If TRUE the constituent name will be abbreviated
#' @param coord_flip If TRUE the axes are flipped
#' @param legend_title Legend title for fill variable
#' 
#' @examples 
#' data(gw_data)
#' gw_data %>%
#' filter(param_name %in% c("Chloride, total", "Sulfate, total"), 
#'       location_id %in% c("MW-1", "MW-2", "MW-3", "MW-4",
#'       "MW-5", "MW-6", "MW-7", "MW-8")) %>%
#'  mutate(gradient = if_else(location_id %in% c("MW-1", "MW-2", "MW-3", "MW-5"),
#'  "upgradient", "downgradient")) %>% 
#'  boxplot(., fill = "gradient")
#' 
#' @export

boxplot <- function(df,
                    x = "location_id",
                    y = "analysis_result",
                    lt_measure = "lt_measure",
                    group_var = "param_name",
                    coef = 1.5,
                    scale_y_trans = "identity",
                    show_points = FALSE,
                    fill = NULL,
                    notch = FALSE,
                    plot_ci = FALSE,
                    limit1 = NULL,
                    limit2 = NULL,
                    pnt = 2,
                    short_name = FALSE,
                    coord_flip = FALSE,
                    legend_title = NULL){

    df %>%
      group_by(!!!syms(group_var)) %>%
      do(plot = .boxplot(.,
                  x = x,
                  y = y,
                  lt_measure = lt_measure,
                  coef = coef,
                  scale_y_trans = scale_y_trans,
                  show_points = show_points,
                  fill = fill,
                  notch = notch,
                  plot_ci = plot_ci,
                  limit1 = limit1,
                  limit2 = limit2,
                  pnt = pnt,
                  short_name = short_name,
                  coord_flip = coord_flip,
                  legend_title = legend_title
                )
      )
}

#' Helper function to plot boxplots for groundwater data
#' 
#' @noRd

.boxplot <- function(df,
                     x = "location_id",
                     y = "analysis_result",
                     coef = 1.5,
                     lt_measure = "lt_measure",
                     scale_y_trans = "identity",
                     show_points = TRUE,
                     fill = NULL,
                     notch = TRUE,
                     plot_ci = FALSE,
                     limit1 = NULL,
                     limit2 = NULL,
                     pnt = 2,
                     short_name = FALSE,
                     coord_flip = FALSE,
                     legend_title = NULL) {

  if (!is.null(fill) & is.null(legend_title)) {
    legend_title <- fill
  }

  df <- df %>%
    mutate(non_detect = if_else(lt_measure == "<", 
                                "non-detect", "detected",
                                missing = "detected"))

  b <- ggplot(df, aes_string(x = x, y = y, fill = fill)) +
    .theme_manager() +
    ylab(paste("Analysis Result", " (", df$default_unit[1], ")", "\nScale: ",
               scale_y_trans, sep = "")) +
    xlab("Location ID") +
    guides(linetype = guide_legend(title = NULL),
           fill = guide_legend(title = NULL)) +
    stat_n_text(vjust = 1.5) +
    scale_y_continuous(trans = scale_y_trans,
                       breaks = scales::pretty_breaks(),
                       labels = prettyNum,
                       sec.axis = dup_axis(label = NULL, name = NULL)) +
    guides(colour = guide_legend(override.aes = list(linetype = 0)),
           shape = guide_legend("Detection", override.aes = list(linetype = 0)),
           size = guide_legend("none")) +
    scale_shape_manual(values = c("non-detect" = 1, "detected" = 16)) +
    ggtitle(paste("Boxplot for", df$param_name[1], "\n", sep = " "))
  
  if (isTRUE(plot_ci)) {
      b <- b + stat_summary(fun.data = mean_cl_boot,
                            geom = "errorbar", colour = "red") + 
      stat_summary(fun.y = mean, geom = "point", colour = "red")
  } else {
    b <- b + geom_boxplot(coef = coef, outlier.size = pnt, notch = notch)
  }

  if (requireNamespace("viridis", quietly = TRUE)) {
    b <- b + viridis::scale_fill_viridis(discrete = TRUE)
  } 

  if (isTRUE(show_points)) {
    if (requireNamespace("ggbeeswarm", quietly = TRUE)) {
      b <-  b + ggbeeswarm::geom_beeswarm(aes(shape = factor(non_detect, 
                                              exclude = NULL),
                                              size = pnt), groupOnX = TRUE)
    } else {
     b <- b + geom_jitter(aes(shape = factor(non_detect, 
                                             exclude = NULL),
                              size = pnt)) 
    }
  }

  if (!is.null(limit1)) {
    df$limit1_name <- paste(limit1[[1]])
    b <- b + geom_hline(data = df,
                        aes_string(yintercept = limit1,
                                   linetype = "limit1_name"),
                        show.legend = TRUE)
  }

  if (!is.null(limit2)) {
    df$limit2_name <- paste(limit2[[1]])
    b <- b + geom_hline(data = df,
                        aes_string(yintercept = limit2,
                                   linetype = "limit2_name"),
                        show.legend = TRUE)
  }

  if (isTRUE(coord_flip)) {
    b <- b + coord_flip()
  }

  explanation <- .ggplot_box_legend()

  legend <- cowplot::get_legend(b)

  b <- b + theme(legend.position = "none")

  legend_combined <- cowplot::plot_grid(explanation, legend, ncol = 1, axis = "lb",
                                          rel_heights = c(1.5, 1))

  print(cowplot::plot_grid(b, legend_combined, nrow = 1, rel_widths = c(2.75, 1)))

}

.theme_manager <- function(base_family = "sans", ...){
  theme_bw(base_family = base_family, ...) +
  theme(
      legend.background = element_rect(),
      panel.grid = element_blank(),
      axis.ticks.length = unit(-0.05, "in"),
      plot.margin = grid::unit(c(1, 1, 1, 1), "lines"),
      axis.title.x = element_text(vjust = -1.5, size = 12),
      axis.text.x = element_text(angle = 90, size = 10,
                                 margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(vjust = 0.5, size = 12),
      axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
      plot.title = element_text(hjust = 0.5, size = 16)
    )
}

.ggplot_box_legend <- function(family = "sans"){
  
  # Create data to use in the boxplot legend:
  set.seed(100)
  
  sample_df <- data.frame(parameter = "test",
                          values = sample(500))
  
  # Extend the top whisker a bit:
  sample_df$values[1:100] <- 701:800
  # Make sure there's only 1 lower outlier:
  sample_df$values[1] <- -350
  
  # Function to calculate important values:
  ggplot2_boxplot <- function(x){
    
    quartiles <- as.numeric(quantile(x,
                                     probs = c(0.25, 0.5, 0.75)))
    
    names(quartiles) <- c("25th percentile",
                          "50th percentile\n(median)",
                          "75th percentile")
    
    IQR <- diff(quartiles[c(1,3)])
    
    upper_whisker <- max(x[x < (quartiles[3] + 1.5 * IQR)])
    lower_whisker <- min(x[x > (quartiles[1] - 1.5 * IQR)])
    
    upper_dots <- x[x > (quartiles[3] + 1.5*IQR)]
    lower_dots <- x[x < (quartiles[1] - 1.5*IQR)]
    
    return(list("quartiles" = quartiles,
                "25th percentile" = as.numeric(quartiles[1]),
                "50th percentile\n(median)" = as.numeric(quartiles[2]),
                "75th percentile" = as.numeric(quartiles[3]),
                "IQR" = IQR,
                "upper_whisker" = upper_whisker,
                "lower_whisker" = lower_whisker,
                "upper_dots" = upper_dots,
                "lower_dots" = lower_dots))
  }
  
  # Get those values:
  ggplot_output <- ggplot2_boxplot(sample_df$values)
  
  # Lots of text in the legend, make it smaller and consistent font:
  update_geom_defaults("text",
                       list(size = 3,
                            hjust = 0,
                            family = family))
  # Labels don't inherit text:
  update_geom_defaults("label",
                       list(size = 3,
                            hjust = 0,
                            family = family))
  
  # Create the legend:
  # The main elements of the plot (the boxplot, error bars, and count)
  # are the easy part.
  # The text describing each of those takes a lot of fiddling to
  # get the location and style just right:
  explain_plot <- ggplot() +
    stat_boxplot(data = sample_df,
                 aes(x = parameter, y=values),
                 geom ='errorbar', width = 0.3) +
    geom_boxplot(data = sample_df,
                 aes(x = parameter, y=values),
                 width = 0.3, fill = "lightgrey") +
    geom_text(aes(x = 1, y = 950, label = "500"), hjust = 0.5) +
    geom_text(aes(x = 1.17, y = 950,
                  label = "Number of values"),
              fontface = "bold", vjust = 0.4) +
    theme_minimal(base_size = 5, base_family = family) +
    geom_segment(aes(x = 2.3, xend = 2.3,
                     y = ggplot_output[["25th percentile"]],
                     yend = ggplot_output[["75th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.3,
                     y = ggplot_output[["25th percentile"]],
                     yend = ggplot_output[["25th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.3,
                     y = ggplot_output[["75th percentile"]],
                     yend = ggplot_output[["75th percentile"]])) +
    geom_text(aes(x = 2.4, y = ggplot_output[["50th percentile\n(median)"]]),
              label = "Interquartile\nrange", fontface = "bold",
              vjust = 0.4) +
    geom_text(aes(x = c(1.17,1.17),
                  y = c(ggplot_output[["upper_whisker"]],
                        ggplot_output[["lower_whisker"]]),
                  label = c("Largest value within 1.5 times\ninterquartile range above\n75th percentile",
                            "Smallest value within 1.5 times\ninterquartile range below\n25th percentile")),
              fontface = "bold", vjust = 0.9) +
    geom_text(aes(x = c(1.17),
                  y =  ggplot_output[["lower_dots"]],
                  label = "Outside value"),
              vjust = 0.5, fontface = "bold") +
    geom_text(aes(x = c(1.9),
                  y =  ggplot_output[["lower_dots"]],
                  label = "-Value is >1.5 times and"),
              vjust = 0.5) +
    geom_text(aes(x = 1.17,
                  y = ggplot_output[["lower_dots"]],
                  label = "<3 times the interquartile range\nbeyond either end of the box"),
              vjust = 1.5) +
    geom_label(aes(x = 1.17, y = ggplot_output[["quartiles"]],
                   label = names(ggplot_output[["quartiles"]])),
               vjust = c(0.4,0.85,0.4),
               fill = "white", label.size = 0) +
    ylab("") + xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 4/3,
          plot.title = element_text(hjust = 0.5, size = 10),
          plot.margin = unit(c(1, 0, 0, 0), "cm")) +
    coord_cartesian(xlim = c(1.4,3.1), ylim = c(-600, 900)) +
    labs(title = "EXPLANATION")
  
  return(explain_plot)
  
}