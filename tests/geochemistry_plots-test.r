# test file for geomchemistry_plots.r

# read in test data 
data(gw_data)

# only look at wells 1-8
wells <- c("MW-1", "MW-2", "MW-3", "MW-4", "MW-5", "MW-6", "MW-7", "MW-8")
gw_data <- subset(gw_data, location_id %in% wells)

# grab the variables needed from the file for the geochemistry plots
ions <- get_major_ions(gw_data)

plot_data <- convert_mgL_to_meqL(ions, Mg=ions$`Magnesium, dissolved`, Ca=ions$`Calcium, dissolved`,
                             Na=ions$`Sodium, dissolved`, K=ions$`Potassium, dissolved`, 
                             Cl=ions$`Chloride, total`, SO4=ions$`Sulfate, total`, 
                             HCO3=ions$`Alkalinity, total (lab)`)

plot_data <- plot_data[complete.cases(plot_data),]

# transform the data for a Piper plot
piper_data <- transform_piper_data(plot_data, TDS=plot_data$`Total Dissolved Solids`) 

# Piper plot
# plots all wells for all dates
plot_piper(piper_data)

# ## run the following to plot all wells by date
# d_ply(piper_data, .(date), plot_piper, .print = TRUE)

# Or, animated time plot using animation library
piper_time_plot(piper_data) 

# animated time plot saved to html
piper_time_html(piper_data)

# transform data for Stiff Diagram
stiff_data <- transform_stiff_data(plot_data)
stiff_data2 <- transform_stiff_data(plot_data, TDS=plot_data$`Total Dissolved Solids`)

one_stiff <- subset(stiff_data2, location_id == "MW-1" & sample_date == as.POSIXct("2007-12-20", format="%Y-%m-%d"))
two_stiff <- subset(stiff_data2, location_id %in% c("MW-1","MW-2") & sample_date == as.POSIXct("2007-12-20", format="%Y-%m-%d"))
# Stiff Diagram
stiff_plot(one_stiff, TDS=TRUE)
stiff_plot(two_stiff, TDS=TRUE)

# animation of Stiff Diagram
stiff_time_plot(stiff_data2, TDS = TRUE)

# animated Stiff diagram saved to html
stiff_time_html(stiff_data)

# ## run the following to print Stiff diagrams individually by date
# d_ply(stiff_data, .(sample_date), stiff_plot, .print = TRUE)

