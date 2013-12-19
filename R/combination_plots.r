#' This function prepares plots of groundwater data faceted by parameters with shaded regions
#' for background and compliance date ranges. Horizontal lines are plotted for 
#' Groundwater Protection Standards. 
#' 
#' @param df data frame in long format containing groundwater monitoring data
#' assumes column names of location_id, param_name, analysis_result, default_unit,
#' lt_measure, sample_date. Dates for sample_date column must be in as.POSIXct format
#' @param back_date vector of start and end of background dates. Use as.POSIXct()
#' @param comp_date vector of start and end of compliance dates. Use as.POSIXct()
#' @export

prep_combo_plot <- function(df, back_dates, comp_dates){ 
  
  df$non_detect <- ifelse(df$lt_measure == "<", 0, 1)
  
  shaded_dates <- data.frame(xmin = c(back_dates[1], comp_dates[1]), 
                             xmax = c(back_dates[2], comp_dates[2]),
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
    
    # create custom legend using guide
    theme(axis.title.x = element_text(size = 15, vjust=-.2)) +
    theme(axis.title.y = element_text(size = 15, vjust=0.3)) +
    guides(colour = guide_legend("Units"), fill = guide_legend("Dates"),
           linetype = guide_legend("Limits")) +
    scale_shape_manual(name = "Measure", labels = c("Non-Detect", "Detected"),
                       values = c("0" = 1, "1" = 4)) +
  
    # add horizontal lines for limit1 and limit2
    geom_hline(aes(yintercept = intra_upl, linetype = "Intra-well"), show_guide = TRUE, size = 0.75) +
    geom_hline(aes(yintercept = inter_upl, linetype = "Inter-well"), show_guide = TRUE, size = 0.75)
  
}

#' Function to plot all combinations of wells and varibles
#' @param df data frame of groundwater data
#' @param back_dates vector of start and end of background dates in POSIXct format
#' @param comp_dates vector of start and end of compliance dates in POSIXct format
#' @export

combo_plot <- function(df, back_dates, comp_dates){
  # print all combinations of plots
  d_ply(df, .(location_id), .progress = "text", prep_combo_plot, back_dates, comp_dates, .print = TRUE)
}