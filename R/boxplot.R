#' Boxplot function
#'
#' @param df groundwater data in tidy format
#' @param x column to be used for the x axis, default is location
#' @param y column to be used for the y axis, default is analysis result
#' @param lt_measure column for >, or < identifier
#' @param scale_y_trans type of transformation to use for y scale. Default is
#' "identity".  Built-in transformations include "asn", "atanh", "boxcox",
#' "exp", "identity", "log", "log10", "log1p", "log2", "logit", "probability",
#' "probit", "reciprocal", "reverse" and "sqrt".
#' @param fill column used to fill the variable
#' @param limit1 column to be used to draw horizontal line
#' @param limit2 column to be used to draw a second horizontal line
#' @param short_name If TRUE the constituent name will be abbreviated
#' @param coord_flip If TRUE the axes are flipped
#' @param legend_title Legend title for fill variable
#' @export

boxplot <- function(df,
                    x = "location_id",
                    y = "analysis_result",
                    lt_measure = "lt_measure",
                    group_var = "param_name",
                    scale_y_trans = "identity",
                    fill = NULL,
                    limit1 = NULL,
                    limit2 = NULL,
                    pnt = 2,
                    short_name = FALSE,
                    coord_flip = FALSE,
                    legend_title = NULL){

    df %>%
      group_by_(group_var) %>%
      do(plot = .boxplot(.,
                  x = x,
                  y = y,
                  lt_measure = lt_measure,
                  scale_y_trans = scale_y_trans,
                  fill = fill,
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
#' @param df groundwater data in tidy format
#' @param x column to be used for the x axis, default is location
#' @param y column to be used for the y axis, default is analysis result
#' @param fill column used to fill the variable
#' @param limit1 column to be used for a horizontal line
#' @param limit2 column to be used for a second horizontal line
#' @param short_name If TRUE the constituent name will be abbreviated
#' @param coord_flip If TRUE the axes are flipped
#' @param legend_title Legend title for fill variable

.boxplot <- function(df,
                     x = "location_id",
                     y = "analysis_result",
                     lt_measure = "lt_measure",
                     scale_y_trans = "identity",
                     fill = NULL,
                     limit1 = NULL,
                     limit2 = NULL,
                     pnt = 2,
                     short_name = FALSE,
                     coord_flip = FALSE,
                     legend_title = NULL) {

  if (!is.null(fill) & is.null(legend_title)) {

    legend_title <- fill

  }

  if (isTRUE(short_name)) {

    df$name_units <- paste(df$short_name, " (", df$default_unit, ")", sep = "")

  } else {

    df$name_units <- paste(df$param_name, " (", df$default_unit, ")", sep = "")

  }

  df$non_detect <- if_else(df[, "lt_measure"] == "<",
                           "non-detect", "detected",
                           missing = "detected")

  b <- ggplot(df, aes_string(x = x, y = y, fill = fill)) +
    theme_bw() +
    ylab(paste("Analysis Result", " (", df$default_unit[1], ")", "\nScale: ",
               scale_y_trans, sep = "")) +
    xlab("Location ID") +
    guides(fill = guide_legend(paste0(legend_title)),
           linetype = guide_legend("Limits")) +
    theme(legend.background = element_rect()) +
    theme(plot.margin = grid::unit(c(1, 1, 1, 1), "lines")) +
    theme(axis.title.x = element_text(vjust = -1.5, size = 12)) +
    theme(axis.text.x = element_text(angle = 90, size = 10)) +
    theme(axis.title.y = element_text(vjust = 0.5, size = 12)) +
    theme(axis.text.y = element_text(size = 10)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_boxplot() +
    geom_beeswarm(aes(shape = factor(non_detect, exclude = NULL),
                      size = pnt), groupOnX = TRUE) +
    scale_y_continuous(trans = scale_y_trans,
                       breaks = scales::pretty_breaks(),
                       labels = prettyNum) +
    guides(colour = guide_legend(override.aes = list(linetype = 0)),
           shape = guide_legend("Detection", override.aes = list(linetype = 0)),
           size = guide_legend("none")) +
    scale_shape_manual(values = c("non-detect" = 1, "detected" = 16)) +
    ggtitle(paste("Boxplot for", df$name_units[1], "\n", sep = " "))

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

  print(b)

}