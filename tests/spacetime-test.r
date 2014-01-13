#' Spatio-temporal
#' 

library(spacetime)
data(gw_data)
data(sp_data)

wells <- c("MW-1", "MW-2", "MW-3", "MW-4", "MW-5", "MW-6", "MW-7", "MW-8")
gw_data <- subset(gw_data, location_id %in% wells)

gw_data$name_units <- paste(gw_data$param_name," (", gw_data$default_unit,")", sep = "")

# cast gw_data to wide format
gw_data <- reshape2::dcast(gw_data, value.var = "analysis_result", location_id + sample_date ~ name_units)

gw_long <- join(gw_data, sp_data, by="location_id")

gw_long <- gw_long[order(gw_long$sample_date, gw_long$location_id),]

gw_long.sp <- unique(gw_long[c("location_id", "long_pos", "lat_pos")])

# create a spatial points data frame
coordinates(gw_long.sp) <- ~long_pos + lat_pos
proj4string(gw_long.sp) <- "+longlat"

time <- gw_long$sample_date

cardSTFDF <- STFDF(gw_long.sp, time, gw_long[order(gw_long$sample_date, gw_long$location_id),])
