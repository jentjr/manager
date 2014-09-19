# # Spatio-temporal test using spacetime 
# library(spacetime)
# library(RColorBrewer)
# data(gw_data)
# data(sp_data)
# 
# wells <- c("MW-1", "MW-2", "MW-3", "MW-4", "MW-5", "MW-6", "MW-7", "MW-8")
# params <- c("Arsenic, dissolved", "Boron, dissolved", "Molybdenum, dissolved")
# gw <- gw_data[gw_data$location_id %in% wells &
#               gw_data$param_name %in% params, ]
# 
# gw_data$name_units <- paste(gw$param_name," (", gw$default_unit,")", 
#                             sep = "")
# 
# # cast gw_data to wide format
# gw_data <- reshape2::dcast(gw, value.var = "analysis_result", 
#                            location_id + sample_date ~ name_units)
# 
# gw_long <- dplyr::inner_join(gw_data, sp_data, by = "location_id")
# gw_long <- gw_long[order(gw_long$sample_date, gw_long$location_id),]
# 
# x = stConstruct(gw_long, c("long_pos", "lat_pos"), "sample_date")
# gwSTSDF = as(x, "STSDF")
# gwSTFDF = as(x, "STFDF")
# 
# stplot(gwSTFDF[ , , "Arsenic, dissolved (ug/L)"], 
#        col.regions = brewer.pal(8, "Greys"), cuts=8, 
#        main = "Arsenic, dissolved (ug/L)")
# 
# stplot(gwSTFDF[ , , "Arsenic, dissolved (ug/L)"], mode = "xt", 
#        col.regions = brewer.pal(8, "Greys"), cuts=8, 
#        main = "Arsenic, dissolved (ug/L)")
# 
# stplot(gwSTFDF[ , , "Arsenic, dissolved (ug/L)"], mode = "ts", 
#        col.regions = brewer.pal(8, "Greys"), cuts=8, 
#        main = "Arsenic, dissolved (ug/L)")
# 
# gw_wide <- dplyr::inner_join(gw, sp_data, by = "location_id")
# 
# gg <- ggplot(gw_wide, aes(long_pos, lat_pos, colour = location_id, 
#                           size = analysis_result, alpha = param_name))
# gg + geom_point() + facet_wrap(~sample_date)
# 
# hoelt <- ggplot(gw_wide[gw_wide$param_name == "Arsenic, dissolved", ], 
#                 aes(location_id, sample_date))
# hoelt + geom_tile(gw_wide[gw_wide$param_name == "Arsenic, dissolved", ], 
#                   aes(fill = analysis_result)) + scale_fill_gradient()
