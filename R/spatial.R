#' function to convert data to sf object 
#' 
#' @param df data frame
#' @param crs coordinate reference system to convert the sf object to
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

