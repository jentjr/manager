# This function plots groundwater data faceted by parameters with shaded regions
# for background and compliance date ranges. Horizontal lines are plotted for 
# Groundwater Protection Standards. Use d_ply(data, (.location), .print=TRUE) to 
# plot combinations for all wells. See demo

combo_plot <- function(df){ 
  
  limits = df
  
  shaded_dates <- data.frame(xmin = c(as.POSIXct("2004-06-01", format = "%Y-%m-%d"), 
                                      as.POSIXct("2013-10-01", format = "%Y-%m-%d")), 
                             xmax = c(as.POSIXct("2013-10-01", format="%Y-%m-%d"), 
                                      max(df$sample_date)),
                             ymin = c(-Inf, -Inf), 
                             ymax = c(Inf, Inf),
                             years = c("background", "compliance"))
  
  ggplot(data = df, aes(x = sample_date, y = analysis_result)) + 
    geom_point(data = df, aes(colour = default_unit, shape = factor(non_detect))) + 
    geom_line(data = df, aes(colour = default_unit)) +
    facet_wrap(~ param_name, scale="free") + 
    
    # Plot legends, labels, and titles
    ggtitle(paste("Time Series Plots for Monitoring Well", 
                  df$location_id[1], "\n", sep=" ")) + 
    ylab("Analysis Result") +
    xlab("Sample Date") + scale_x_datetime(labels = date_format("%Y")) +
    theme(axis.text.x = element_text(angle = 90)) +
    theme(plot.margin = unit(c(0.75, 0.75, 0.75, 0.75), "in")) + 
    theme_bw() + 
    scale_colour_discrete(name = "Units", guide = "legend") + 
    
    # add rectangles for date ranges
    geom_rect(data = shaded_dates, aes(xmin = xmin, ymin = ymin, xmax = xmax, 
                                       ymax = ymax, fill = years),
              alpha = 0.2, inherit.aes = FALSE) +
    scale_fill_manual(values=c("blue","green")) +
    
    # add horizontal lines for EPA MCL and Upper Prediction Limit
    geom_hline(data = limits, aes(yintercept = GWPS, linetype = "GWPS"), show_guide = TRUE, size = 0.75) +
    geom_hline(data = limits, aes(yintercept = DMR_limit, linetype = "DMR Limit"), show_guide = TRUE, size = 0.75) +
    
    # create custom legend using guide
    theme(axis.title.x = element_text(size = 15, vjust=-.2)) +
    theme(axis.title.y = element_text(size = 15, vjust=0.3)) +
    guides(colour = guide_legend("Units"), fill = guide_legend("Dates"),
           linetype = guide_legend("Limits")) +
    scale_shape_manual(name = "Measure", labels = c("Detected", "Non-Detect"),
                       values = c("1" = 21, "0" = 4))  
    
}