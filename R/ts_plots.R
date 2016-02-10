#' This function plots multiple groundwater data time series by location, 
#' or constituent. 
#' 
#' @param df groundwater data
#' @param facet_by parameter to group plots by
#' @param back_date dates for background date range
#' @param comp_date dates for compliance date range
#' @param limit1 horizontal line 1
#' @param limit2 horizontal line 2
#' @param short_name If TRUE, the analyte name will be abbreviated
#' @param pnt size of points on time series plots
#' @param ncol number of columns
#' @export

ts_plot <- function(df, facet_by = "location_id", ...){
  
  if (facet_by == "param_name") {
    plyr::d_ply(df, .(param_name), .progress = "text", .ts_plot, 
                facet_by = facet_by, ..., .print = TRUE)
  } else{
    plyr::d_ply(df, .(location_id), .progress = "text", .ts_plot, 
                facet_by = facet_by, ..., .print = TRUE)
  }
}

#' Function for plotting time series of groundwater data
#' @param df groundwater data
#' @param facet_by parameter to group plots by
#' @param back_date dates for background date range
#' @param comp_date dates for compliance date range
#' @param limit1 horizontal line 1
#' @param limit2 horizontal line 2
#' @param short_name If TRUE, the analyte name will be abbreviated
#' @param pnt size of points on time series plots
#' @param ncol number of columns

.ts_plot <- function(df, 
                     facet_by = NULL, 
                     trend = FALSE, 
                     back_date = NULL, 
                     comp_date = NULL, 
                     limit1 = NULL, 
                     limit2 = NULL, 
                     short_name = FALSE, 
                     pnt = 3, 
                     ncol = NULL){
  
  df$non_detect <- ifelse(df$lt_measure == "<", "non-detect", "detected")
  
  if (isTRUE(short_name)) {
    df$param_name <- paste(df$short_name, " (", df$default_unit, ")", 
                           sep = "")
  } else {
    df$param_name <- paste(df$param_name, " (", df$default_unit, ")", 
                           sep = "")
  }
  
  # main plot
  p <- ggplot(data = df, aes(x = sample_date, y = analysis_result)) + 
    geom_line(data = df) +
    geom_point(data = df, aes(shape = factor(non_detect)), size = pnt) +
    ylab("Analysis Result") +
    xlab("Sample Date") + 
    scale_x_datetime(labels = scales::date_format("%Y")) +
    theme(plot.margin = grid::unit(c(1, 1, 1, 1), "lines")) + 
    theme_bw() +  
    theme(axis.title.x = element_text(size = 15, vjust = -0.3)) +
    theme(axis.title.y = element_text(size = 15, vjust = 0.3)) +
    guides(colour = guide_legend(override.aes = list(linetype = 0)), 
           shape = guide_legend("Detection", override.aes = list(linetype = 0)),
           size = guide_legend("none"),
           linetype = guide_legend("Limits")) +
    scale_shape_manual(values = c("non-detect" = 1, "detected" = 16)) 
  
  if (isTRUE(trend)) {
    p <- p + geom_smooth(method = "lm")
  }
  
  if (facet_by == "location_id") {
    p <- p + facet_wrap(~param_name, scale = "free", ncol = ncol) + 
      ggtitle(paste("Time Series Plots for", df$location_id[1], "\n", sep = " ")) 
  }
  
  if (facet_by == "param_name") {
    p <- p + facet_wrap(~location_id, scale = "free", ncol = ncol) + 
      ggtitle(paste("Time Series Plots for", df$param_name[1], "\n", sep = " "))
  }
  
  if (!missing(back_date)) {
    shaded_dates <- data.frame(xmin = c(back_date[1], comp_date[1]), 
                               xmax = c(back_date[2], comp_date[2]),
                               ymin = c(-Inf, -Inf), 
                               ymax =  c(Inf, Inf),
                               years = c("background", "compliance"))
    
    p <- p + geom_rect(data = shaded_dates, 
                       aes(xmin = xmin, ymin = ymin, xmax = xmax, 
                           ymax = ymax, fill = years),
                       alpha = 0.2, inherit.aes = FALSE) +
      scale_fill_manual(values = c("blue", "green")) +
      guides(fill = guide_legend(override.aes = list(linetype = 0),
                                 title = "Date Range"))
  }
  
  if (!missing(limit1)) {
    # limit1 <- as.quoted(limit1)
    df$limit1_name <- paste(limit1[[1]])
    p <- p + geom_hline(data = df, 
                        aes_string(yintercept = limit1, 
                                   linetype = "limit1_name"), 
                        show.legend = TRUE)
  }
  
  if (!missing(limit2)) {
    # limit2 <- as.quoted(limit2)
    df$limit2_name <- paste(limit2[[1]])
    p <- p + geom_hline(data = df, 
                        aes_string(yintercept = limit2, 
                                   linetype = "limit2_name"), 
                        show.legend = TRUE)
  }  
  return(p)
}

#' Function to plot multiple time series by parameter
#' 
#' @param df groundwater data
#' @param back_date dates for background date range
#' @param comp_date dates for compliance date range
#' @param limit1 horizontal line 1
#' @param limit2 horizontal line 2
#' @param name If name = "short" the analyte will be abbreviated
#' @param pnt size of points on time series plots
#' @param ncol number of columns

.multi_by_param_grid <- function(df, back_date = NULL, comp_date = NULL, 
                                limit1 = NULL, limit2 = NULL, 
                                name = NULL, pnt = 3){
  
  df$non_detect <- ifelse(df$lt_measure == "<", "non-detect", "detected")
  
  if (isTRUE(name == "short")) {
    df$param_name <- paste(df$short_name, " (", df$default_unit, ")", 
                           sep = "")
  } else {
    df$param_name <- paste(df$param_name, " (", df$default_unit, ")", 
                           sep = "")
  }
  
  # main plot
  p <- ggplot(data = df, aes(x = sample_date, y = analysis_result, 
                             colour = location_id)) + 
    geom_line(data = df) +
    geom_point(data = df, aes(shape = factor(non_detect)), size = pnt) + 
    ggtitle(paste("Time Series Plots for", 
                  df$param_name[1], "\n", sep = " ")) +
    ylab("Analysis Result") +
    xlab("Sample Date") + 
    scale_x_datetime(labels = scales::date_format("%Y")) +
    theme(plot.margin = grid::unit(c(1, 1, 1, 1), "lines")) + 
    theme_bw() +  
    
    # create custom legend using guide
    theme(axis.title.x = element_text(size = 15, vjust = -0.3)) +
    theme(axis.title.y = element_text(size = 15, vjust = 0.3)) +
    guides(colour = guide_legend("Location ID", 
                                 override.aes = list(linetype = 0)), 
           shape = guide_legend("Detection", 
                                override.aes = list(linetype = 0)),
           size = guide_legend("none"),
           linetype = guide_legend("Limits")) +
    scale_shape_manual(values = c("non-detect" = 1, "detected" = 16))
  
  # shaded background and compliance date regions
  if (!missing(back_date)) {
    # add rectangles for date ranges
    shaded_dates <- data.frame(xmin = c(back_date[1], comp_date[1]), 
                               xmax = c(back_date[2], comp_date[2]),
                               ymin = c(-Inf, -Inf), 
                               ymax = c(Inf, Inf),
                               years = c("background", "compliance"))
    
    p <- p + geom_rect(data = shaded_dates, 
                       aes(xmin = xmin, ymin = ymin, xmax = xmax, 
                           ymax = ymax, fill = years),
                       alpha = 0.2, inherit.aes = FALSE) +
      scale_fill_manual(values = c("blue","green")) +
      guides(fill = guide_legend(override.aes = list(linetype = 0),
                                 title = "Date Range"))
  }
  
  # add horizontal line for limit1
  if (!missing(limit1)) {
    limit1 <- as.quoted(limit1)
    df$limit1_name <- paste(limit1[[1]])
    p <- p + geom_hline(data = df, 
                        aes_string(yintercept = limit1, 
                                   linetype = "limit1_name"), 
                        show.legend = TRUE)
  }
  # add horizontal line for limit2
  if (!missing(limit2)) {
    limit2 <- as.quoted(limit2)
    df$limit2_name <- paste(limit2[[1]])
    p <- p + geom_hline(data = df, 
                        aes_string(yintercept = limit2, 
                                   linetype = "limit2_name"), 
                        show.legend = TRUE)
  }  
  return(p)
}

#' This function plots groundwater data faceted by parameters with shaded 
#' regions for background and compliance date ranges. 
#' Horizontal lines are plotted for Groundwater Protection Standards. 
#' 
#' @param df data frame in long format containing groundwater monitoring data
#' assumes column names of location_id, param_name, analysis_result, 
#' default_unit, lt_measure, sample_date. Dates for sample_date column
#'  must be in as.POSIXct format
#' @param facet_col the column to facet the plot by
#' @param back_date vector of start and end of background dates.
#' @param comp_date vector of start and end of compliance dates. 
#' @param limits column vector of limits e.g. c("EPA_Limits", "DMR_limits")

.multi_by_param <- function(df, ...){
  
  plyr::d_ply(df, .(param_name), .progress = "text", .multi_by_param_grid, ...,
              .print = TRUE)
}

#' Function to plot multiple time series by location id
#' 
#' @param df groundwater data
#' @param back_date dates for background date range
#' @param comp_date dates for compliance date range
#' @param limit1 horizontal line 1
#' @param limit2 horizontal line 2
#' @param name If name = "short" the analyte will be abbreviated
#' @param pnt size of points on time series plots
#' @param ncol number of columns

.multi_by_loc_grid <- function(df, back_date = NULL, 
                              comp_date = NULL, limit1 = NULL, limit2 = NULL, 
                              name = NULL, pnt = 3){
  
  df$non_detect <- ifelse(df$lt_measure == "<", "non-detect", "detected")
  
  if (isTRUE(name == "short")) {
    df$param_name <- paste(df$short_name, " (", df$default_unit, ")", 
                           sep = "")
  } else {
    df$param_name <- paste(df$param_name, " (", df$default_unit, ")", 
                           sep = "")
  }
  
  # main plot
  p <- ggplot(data = df, aes(x = sample_date, y = analysis_result, 
                             colour = param_name)) + 
    geom_line(data = df) +
    geom_point(data = df, aes(shape = factor(non_detect)), size = pnt) + 
    ggtitle(paste("Time Series Plots for", 
                  df$location_id[1], "\n", sep = " ")) +
    ylab("Analysis Result") +
    xlab("Sample Date") + 
    scale_x_datetime(labels = scales::date_format("%Y")) +
    theme(plot.margin = grid::unit(c(1, 1, 1, 1), "lines")) + 
    theme_bw() +  
    
    # create custom legend using guide
    theme(axis.title.x = element_text(size = 15, vjust = -0.3)) +
    theme(axis.title.y = element_text(size = 15, vjust = 0.3)) +
    guides(colour = guide_legend("Parameter", 
                                 override.aes = list(linetype = 0)), 
           shape = guide_legend("Detection", 
                                override.aes = list(linetype = 0)),
           size = guide_legend("none"),
           linetype = guide_legend("Limits")) +
    scale_shape_manual(values = c("non-detect" = 1, "detected" = 16))
  
  # shaded background and compliance date regions
  if (!missing(back_date)) {
    # add rectangles for date ranges
    shaded_dates <- data.frame(xmin = c(back_date[1], comp_date[1]), 
                               xmax = c(back_date[2], comp_date[2]),
                               ymin = c(-Inf, -Inf), 
                               ymax = c(Inf, Inf),
                               years = c("background", "compliance"))
    
    p <- p + geom_rect(data = shaded_dates, 
                       aes(xmin = xmin, ymin = ymin, xmax = xmax, 
                           ymax = ymax, fill = years),
                       alpha = 0.2, inherit.aes = FALSE) +
      scale_fill_manual(values = c("blue","green")) +
      guides(fill = guide_legend(override.aes = list(linetype = 0),
                                 title = "Date Range"))
  }
  
  # add horizontal line for limit1
  if (!missing(limit1)) {
    limit1 <- as.quoted(limit1)
    df$limit1_name <- paste(limit1[[1]])
    p <- p + geom_hline(data = df, 
                        aes_string(yintercept = limit1, 
                                   linetype = "limit1_name"), 
                        show.legend = TRUE)
  }
  # add horizontal line for limit2
  if (!missing(limit2)) {
    limit2 <- as.quoted(limit2)
    df$limit2_name <- paste(limit2[[1]])
    p <- p + geom_hline(data = df, 
                        aes_string(yintercept = limit2, 
                                   linetype = "limit2_name"), 
                        show.legend = TRUE)
  }  
  return(p)
}

#' This function plots groundwater data by location with all parameters
#' on one plot. Shaded regions for background and compliance date ranges, 
#' horizontal lines for Groundwater Protection Standards can be passed to the 
#' function as well
#' 
#' @param df data frame in long format containing groundwater monitoring data
#' assumes column names of location_id, param_name, analysis_result, 
#' default_unit, lt_measure, sample_date. Dates for sample_date column
#'  must be in as.POSIXct format
#' @param back_date vector of start and end of background dates. 
#' @param comp_date vector of start and end of compliance dates. 
#' @param limit1 plots a horizontal line for a limit e.g. EPA MCL
#' @param limit2 plots a second limit

.multi_by_loc <- function(df, ...){
  
  plyr::d_ply(df, .(location_id), .progress = "text", .multi_by_loc_grid, ...,
              .print = TRUE)
}
