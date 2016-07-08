#' Function to plot multiple boxplots for groundwater data 
#' 
#' @param df groundwater data
#' @param facet_by parameter to group data by
#' @param name If name = "short" the analyte will be abbreviated
#' @param flip_coords If TRUE the axes are flipped
#' @export

boxplot <- function(df, facet_by = "location_id", short_name = FALSE, 
                    coord_flip = FALSE){
  if (facet_by == "param_name") {
    df %>% 
      group_by(param_name) %>%
      do(plot = .boxplot(., facet_by = "param_name", 
                         short_name = short_name,
                         coord_flip = coord_flip))
  }
  if (facet_by == "location_id") {
    df %>%
      group_by(location_id) %>%
      do(plot = .boxplot(., facet_by = "location_id", 
                         short_name = short_name,
                         coord_flip = coord_flip))
  }
}

#' Function to plot boxplots for groundwater data
#' @param df groundwater data
#' @param facet_by parameter to group data by
#' @param short_name If TRUE, the analyte name will be abbreviated
#' @param flip_coords If TRUE the axes are flipped

.boxplot <- function(df, facet_by = "location_id", short_name = FALSE, 
                       coord_flip = FALSE){

  if (isTRUE(short_name)) {
    df$name_units <- paste(df$short_name, " (", df$default_unit, ")", sep = "")
  } else {
    df$name_units <- paste(df$param_name, " (", df$default_unit, ")", sep = "")
  }
  if (facet_by == "param_name") {
    b <- ggplot(df, aes(location_id, y = analysis_result, fill = location_id)) + 
      theme_bw() + 
      ylab(paste("Analysis Result"," (", df$default_unit[1], ")", sep = "")) + 
      xlab("Location ID") +
      guides(fill = guide_legend("Location ID")) +
      theme(legend.background = element_rect()) + 
      theme(plot.margin = grid::unit(c(1, 1, 1, 1), "lines")) +
      theme(axis.title.x = element_text(vjust = -0.5, size = 15)) +
      theme(axis.text.x = element_text(angle = 90, size = 13)) +
      theme(axis.title.y = element_text(vjust = 0.5, size = 15)) +
      theme(axis.text.y = element_text(size = 13)) +
      geom_boxplot() + 
      ggtitle(paste("Boxplot for", df$name_units, "\n", sep = " "))
  }
  if (facet_by == "location_id") {
    b <- ggplot(df, aes(name_units, y = analysis_result, fill = name_units)) + 
      theme_bw() + ylab("Analysis Result") + 
      xlab("Constituent") +
      guides(fill = guide_legend("Constituent")) +
      theme(legend.background = element_rect()) + 
      theme(plot.margin = grid::unit(c(1, 1, 1, 1), "lines")) +
      theme(axis.title.x = element_text(vjust = -0.5, size = 15)) +
      theme(axis.text.x = element_text(angle = 90, size = 13)) +
      theme(axis.title.y = element_text(vjust = 0.5, size = 15)) +
      theme(axis.text.y = element_text(size = 13)) +
      geom_boxplot() + 
      ggtitle(paste("Boxplot for", df$location_id, "\n", sep = " "))
  }
  if (isTRUE(coord_flip)) {
    b <- b + coord_flip()
  }      
  print(b)
}