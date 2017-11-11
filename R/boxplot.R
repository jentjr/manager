#' Boxplot function
#'
#' @param df groundwater data in tidy format
#' @param x column to be used for the x axis, default is location
#' @param y column to be used for the y axis, default is analysis result
#' @param lt_measure column for >, or < identifier
#' @param coef length of the whiskers as multiple of the IQR. Defualt is 3.
#' @param scale_y_trans type of transformation to use for y scale. Default is
#' "identity".  Built-in transformations include "asn", "atanh", "boxcox",
#' "exp", "identity", "log", "log10", "log1p", "log2", "logit", "probability",
#' "probit", "reciprocal", "reverse" and "sqrt".
#' @param show_points plot the individual points using ggbeeswarm.
#' @param fill column used to fill the variable
#' @param limit1 column to be used to draw horizontal line
#' @param limit2 column to be used to draw a second horizontal line
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
                    coef = 3,
                    scale_y_trans = "identity",
                    show_points = TRUE,
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
                  coef = coef,
                  scale_y_trans = scale_y_trans,
                  show_points = show_points,
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

.boxplot <- function(df,
                     x = "location_id",
                     y = "analysis_result",
                     coef = 3,
                     lt_measure = "lt_measure",
                     scale_y_trans = "identity",
                     show_points = TRUE,
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

  df <- df %>%
    mutate(non_detect = if_else(lt_measure == "<", 
                                "non-detect", "detected",
                                missing = "detected"))
  
  df <- df %>%
    name_units(short_name = short_name)

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
    geom_boxplot(coef = coef, outlier.colour = "red", outlier.shape = 4,
                 outlier.size = pnt) +
    scale_y_continuous(trans = scale_y_trans,
                       breaks = scales::pretty_breaks(),
                       labels = prettyNum) +
    guides(colour = guide_legend(override.aes = list(linetype = 0)),
           shape = guide_legend("Detection", override.aes = list(linetype = 0)),
           size = guide_legend("none")) +
    viridis::scale_fill_viridis(discrete = TRUE) +
    scale_shape_manual(values = c("non-detect" = 1, "detected" = 16)) +
    ggtitle(paste("Boxplot for", df$param_name[1], "\n", sep = " "))

  if (isTRUE(show_points)) {
    
    if (!requireNamespace("ggbeeswarm", quietly = TRUE)) {
      stop("ggbeeswarm needed for this function to work. Please install it.", 
           call. = FALSE)
    }
    
    b <-  b + ggbeeswarm::geom_beeswarm(aes(shape = factor(non_detect, exclude = NULL),
                          size = pnt), groupOnX = TRUE)
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

  print(b)

}