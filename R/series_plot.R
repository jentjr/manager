#' Function to plot a series plot of groundwater data
#' 
#' @param df datadframe of groundwater data
#' @param facet_by parameter to facet the plot by
#' @export

series_plot <- function(df, facet_by = NULL) {
  
  p <- ggplot(df, aes(x = LOCATION_ID, y = ANALYSIS_RESULT, group = 1)) + 
    theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank())
  
  if (is.null(facet_by)) {
    
    p <- p + geom_line(aes(colour = PARAM_NAME, group = PARAM_NAME))
    
  }
  
  if (!is.null(facet_by)) {
    
    if (facet_by == "SAMPLE_DATE") {
      
      p <- p + facet_wrap(~SAMPLE_DATE, scale = "free_y") + 
        geom_line(aes(colour = PARAM_NAME, group = PARAM_NAME))
      
    }
    
  }
  
  return(p)
  
}