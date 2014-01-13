# well_data <- read.csv("/Users/justinjent/downloads/well_test.csv", header=TRUE)
# 
# well_data$longitude <- as.numeric(char2dms(as.character(well_data$longitude)))
# well_data$latitude <- as.numeric(char2dms(as.character(well_data$latitude)))
# 
# well_map <- get_map(location = c(lon=mean(well_data$longitude), lat=mean(well_data$latitude)), zoom=12)
# 
# p1 <- ggmap(well_map, extent = "device", maptype = "terrain", color = "color")
# 
# p1 + geom_point(data = well_data, aes(x = longitude, y = latitude), size = 4)


get_center <- function(data, longitude, latitude){
  lat = mean(data[,latitude])
  lng = mean(data[,longitude])
  return(list(lat = lat, lng = lng))
}


library(rCharts)
leaflet_plot <- function(data = sp_data, width = 1600, height = 1000){
  center_ <- get_center(data, "long_pos", "lat_pos")
  L1 <- Leaflet$new()
  L1$set(width = width, height = height)
  L1$setView(c(center_$lat, center_$lng), 14)
  L1$tileLayer(provider = 'Esri.WorldImagery')
  for(i in 1:nrow(sp_data)){
    L1$marker(c(data$lat_pos[i], data$long_pos[i]), bindPopup = paste(data$location_id[i]))
  }
  return(L1)
}

