# test file for geomchemistry_plots.r

# read in test data 
load("data/gw_data.RData")

# grab the variables needed from the file for the geochemistry plots
plot_data <- get_plot_data(data)

# transform the data for a Piper plot
piper_data <- transform_piper_data(plot_data) 

# Piper plot
# plots all wells for all dates
plot_piper(piper_data)

## run the following to plot all wells by date
d_ply(piper_data[!(piper_data$name %in% c("PZ-1", "PZ-2", "PZ-3", "PZ-4")),], .(date), plot_piper, .print = TRUE)

# animated time plot using animation library
piper_time_plot(piper_data) 

# animated time plot saved to html
piper_time_html(piper_data)

# try to create a animated time plot of piper diagram using googleVis
piper_time <- gvisMotionChart(piper_data, idvar = "name", timevar = "date", xvar = "cation_x", yvar = "cation_y",
                colorvar = "TDS")

plot(piper_time)

# transform data for Stiff Diagram
stiff_data <- transform_stiff_data(plot_data)

# Stiff Diagram
stiff_plot(stiff_data)

# animation of Stiff Diagram
stiff_time_plot(stiff_data)

# animated Stiff diagram saved to html
stiff_time_html(stiff_data)

## run the following to print Stiff diagrams individually by date
d_ply(stiff_data, .(sample_date), stiff_plot, .print = TRUE)

