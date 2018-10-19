#' function to convert data to sf object 
#' 
#' @param df data frame
#' @param crs coordinate reference system to convert the sf object to
#' 
#' @export

to_spatial <- function(df, crs) {
  
  df$coordinate_reference <- as.integer(df$coordinate_reference)
  
  l <- lapply(unique(df$location_id), function(x){
    df <- df[df$location_id == x,]
    epsg <- df$coordinate_reference[1]
    df  <-  sf::st_as_sf(df, coords = c('east_coordinate', 'north_coordinate'), crs = epsg)
  }) 
  
  out <- do.call(rbind, lapply(l, function(x) x <- sf::st_transform(x, crs)))
  
  return(out)
}

sp_plot <- function(df, 
                    x = "analysis_result",
                    color = "location_id",
                    sample_interval = "semi-annually",
                    group_var = "param_name", 
                    facet_var = "sample_date") {
  
  df %>%
    group_by_(group_var) %>%
    do(plot = .sp_plot(., 
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

.sp_plot <- function(df, 
                     x = "analysis_result",
                     color = "location_id",
                     sample_interval = "semi-annually",
                     group_var = NULL,
                     facet_var = NULL) {

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
    
    p <- p + facet_wrap(paste(facet_var)) +
      ggtitle(paste("Plot for",
                    df[[paste(group_var)]][1], "\n", sep = " "))
    
  }

  print(p)

}