#' Function to plot boxplots for groundwater data 
#' 
#' @param df groundwater data in tidy, long format
#' @param x column to be used for the x axis, default is location
#' @param y column to be used for the y axis, default is analysis result
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
                    fill = NULL,
                    limit1 = NULL,
                    limit2 = NULL,
                    short_name = FALSE, 
                    coord_flip = FALSE,
                    legend_title = NULL){
  
    df %>% 
      group_by(param_name) %>%
      do(plot = .boxplot(.,
                  x = x,
                  y = y,
                  fill = fill,
                  limit1 = limit1,
                  limit2 = limit2,
                  short_name = short_name,
                  coord_flip = coord_flip,
                  legend_title = legend_title
                )
      )

}

#' Helper function to plot boxplots for groundwater data 
#' 
#' @param df groundwater data in tidy, long format
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
                     fill = NULL,
                     limit1 = NULL,
                     limit2 = NULL,
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
  
  b <- ggplot(df, aes_string(x = x, y = y, fill = fill)) + 
    theme_bw() + 
    ylab(paste("Analysis Result"," (", df$default_unit[1], ")", sep = "")) + 
    xlab("Location ID") +
    guides(fill = guide_legend(paste0(legend_title)), 
           linetype = guide_legend("Limits")) +
    theme(legend.background = element_rect()) + 
    theme(plot.margin = grid::unit(c(1, 1, 1, 1), "lines")) +
    theme(axis.title.x = element_text(vjust = -0.5, size = 15)) +
    theme(axis.text.x = element_text(angle = 90, size = 13)) +
    theme(axis.title.y = element_text(vjust = 0.5, size = 15)) +
    theme(axis.text.y = element_text(size = 13)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_boxplot() + 
    ggtitle(paste("Boxplot for", df$name_units, "\n", sep = " "))
  
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