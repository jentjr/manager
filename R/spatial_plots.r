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

