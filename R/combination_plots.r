#' This function plots groundwater data faceted by parameters with shaded regions
#' for background and compliance date ranges. Horizontal lines are plotted for 
#' Groundwater Protection Standards. 
#' 
#' @param df data frame in long format containing groundwater monitoring data
#' assumes column names of location_id, param_name, analysis_result, default_unit,
#' lt_measure, sample_date. Dates for sample_date column must be in as.POSIXct format
#' @param back_date vector of start and end of background dates. Use as.POSIXct()
#' @param comp_date vector of start and end of compliance dates. Use as.POSIXct()
#' @param limits column vector of limits e.g. c("EPA_Limits", "DMR_limits")
#' @export


combo_plot <- function(df, back_date = NULL, comp_date = NULL, limits = NULL, ...){
  
  df$non_detect <- ifelse(df$lt_measure == "<", 0, 1)
  df$param_name <- paste(df$param_name, " (", df$default_unit, ")", sep = "")
  
  p <- ggplot(data = df, aes(x = sample_date, y = analysis_result)) + 
    geom_point(data = df, aes(shape = factor(non_detect))) + 
    geom_line(data = df) +
    facet_wrap(~ param_name, scale="free") + 
    
    # Plot legends, labels, and titles
    ggtitle(paste("Time Series Plots for", 
                  df$location_id[1], "\n", sep=" ")) + 
    ylab("Analysis Result") +
    xlab("Sample Date") + scale_x_datetime(labels = date_format("%Y")) +
    theme(plot.margin = unit(c(0.75, 0.75, 0.75, 0.75), "in")) + 
    theme_bw() +  
    
    # create custom legend using guide
    theme(axis.title.x = element_text(size = 15, vjust=-.2)) +
    theme(axis.title.y = element_text(size = 15, vjust=0.3)) +
    guides(colour = guide_legend(override.aes = list(linetype = 0 )), 
           shape = guide_legend(override.aes = list(linetype = 0 )),
           size = guide_legend("none")) +
    scale_shape_manual(name = "Measure", labels = c("Non-Detect", "Detected"),
                       values = c("0" = 1, "1" = 4)) 
  
  # shaded background and compliance date regions
  if(!missing(back_date)){
    # add rectangles for date ranges
    shaded_dates <- data.frame(xmin = c(back_date[1], comp_date[1]), 
                               xmax = c(back_date[2], comp_date[2]),
                               ymin = c(-Inf, -Inf), 
                               ymax = c(Inf, Inf),
                               Years = c("background", "compliance"))
    
    p <- p + geom_rect(data = shaded_dates, aes(xmin = xmin, ymin = ymin, xmax = xmax, 
                                                ymax = ymax, fill = Years),
                       alpha = 0.2, inherit.aes = FALSE) +
      scale_fill_manual(values=c("blue","green")) +
      guides(fill = guide_legend(override.aes = list(linetype = 0)))
  }
  
  # add horizontal lines limits
  if(!missing(limits)){
    limits <- as.quoted(limits)
    for(i in 1:length(limits)){
      df$line.type <- paste(limits[[i]])
      p <- p + geom_hline(data = df, aes_string(yintercept = limits[[i]], linetype = "line.type"), size = 0.75, show_guide = TRUE)
    }
    p <- p + scale_linetype(name = "Limits")
  }    
  print(p)
}