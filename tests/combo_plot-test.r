# read in test data 
data(gw_data)

# only look at wells 1-8
wells <- c("MW-1", "MW-2")
params <- c("Arsenic, dissolved", "Iron, dissolved")
gw_data <- subset(gw_data, location_id %in% wells & param_name %in% params)

d_ply(gw_data, .(location_id), combo_grid, name="short", .print=TRUE)

combo_plot(gw_data, name = "short", pnt=5)
