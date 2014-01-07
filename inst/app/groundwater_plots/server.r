# change options to handle large file size
options(shiny.maxRequestSize=-1)
# force numbers to be decimal instead of scientific
options(scipen=6, digits = 8) 

# load required packages
library(groundwater)
library(plyr)
library(ggmap)

# Define server
shinyServer(function(input, output) {
  # reactive function to upload data
  get_data <- reactive({
    if (!is.null(input$manages_path)){
        switch(input$file_type, 
               ".csv" = from_csv(input$manages_path$datapath),
               ".mdb" = connect_manages(input$manages_path$datapath),
               ".xls" = from_excel(input$manages_path$datapath))
      }      
  })
 
  # reactive function to upload spatial data   
  get_spatial_data <- reactive({
    if (!is.null(input$manages_path)){
      connect_manages_spatial(input$manages_path$datapath)
    }
  })
  
  # return a list of well names
  output$wells <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      well_names <- get_well_names(data)
      selectInput("well", "Monitoring Wells", well_names, multiple = TRUE,
                  selected = well_names[1])
    }
  })
  
  # return a list of constituents
  output$analytes <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      analyte_names <- get_analytes(data)
      selectInput("analyte", "Constituents", analyte_names, multiple = TRUE,
                  selected = analyte_names[1])
    }
  })
  
  # Output a googleTable of the data
  output$well_table <- renderDataTable({
    if (!is.null(input$manages_path)){
      data <- get_data()
      return(data)
    }
   }, options = list(sScrollY = "100%", sScrollX = "100%", 
                     aLengthMenu = c(5, 10, 15, 25, 50, 100), 
                     iDisplayLength = 15)
  )
  
  # googleTable output of a data summary
  output$gw_summary <- renderDataTable({
    if (!is.null(input$manages_path)){
      data <- get_data()
      gw_summary_table <- groundwater_summary(data)
      return(gw_summary_table)
    }
   }, options = list(sScrollY = "100%", sScrollX = "100%", 
                     aLengthMenu = c(5, 10, 15, 25, 50, 100), 
                     iDisplayLength = 15)
  )
  
  # return start and end date of background data
  output$date_ranges <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      tagList(
        dateRangeInput("back_date_range", "Background Date Range", 
                       start = min(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE)),
        dateRangeInput("comp_date_range", "Compliance Date Range", 
                       start = max(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE))  
        )
    }
  })
  
  # time series plot output
  output$time_plot <- renderPlot({
    if (!is.null(input$manages_path)){
      data <- get_data()
      
      data$non_detect <- ifelse(data$lt_measure == "<", 
                                "non-detect", "detected")
#       df$param_name <- paste(df$short_name, " (", 
#                              df$default_unit, ")", sep = "")
      
      data_selected <- subset(data, location_id %in% input$well & 
                                param_name %in% input$analyte)

      t <- ggplot(data_selected, aes(x=sample_date, y=analysis_result, 
                                     colour = param_name)) 
      
      if(input$scale_plot){
        # time series plot of analytes gridded by wells
        t1 <- t + geom_point(aes(colour=param_name, shape=factor(non_detect), 
                                 size=3)) + 
          geom_line() + 
          facet_wrap(~location_id, scales="free") + 
          theme_bw() + 
          xlab("Sample Date") + 
          scale_x_datetime(labels = scales::date_format("%Y")) +
          ylab("Analysis Result") +
          scale_colour_discrete(name = "Parameter") + 
          theme(axis.title.x = element_text(vjust=-0.3)) +
          theme(axis.text.x = element_text(angle=0)) +
          theme(axis.title.y = element_text(vjust=0.3)) +
          theme(legend.background = element_rect()) + 
          guides(colour = guide_legend(override.aes = list(linetype = 0, 
                                                           fill = NA)), 
                 shape = guide_legend("Measure", 
                                      override.aes = list(linetype = 0)),
                 size = guide_legend("none"),
                 linetype = guide_legend("Limits")) +
          scale_shape_manual(values = c("non-detect" = 1, "detected" = 16))
      } else {
        t1 <- t + geom_point(aes(colour=param_name, shape=factor(non_detect), 
                                 size=3)) + geom_line() + 
          facet_wrap(~location_id) + 
          theme_bw() + 
          xlab("Sample Date") + 
          scale_x_datetime(labels = scales::date_format("%Y")) +
          ylab("Analysis Result") + 
          scale_colour_discrete(name = "Parameter") +
          theme(axis.title.x = element_text(vjust=-0.3)) +
          theme(axis.text.x = element_text(angle=0)) +
          theme(axis.title.y = element_text(vjust=0.3)) +
          guides(colour = guide_legend(override.aes = list(linetype = 0, 
                                                           fill = NA)), 
                 shape = guide_legend("Measure", 
                                      override.aes = list(linetype = 0)),
                 size = guide_legend("none"),
                 linetype = guide_legend("Limits")) +
          scale_shape_manual(values = c("non-detect" = 1, "detected" = 16))
      }
      if(input$date_lines){
        # create separate data.frame for geom_rect data
        # change dates to POSIXct which is same as MANAGES database dates
        b1 <- min(as.POSIXct(input$back_date_range, format = "%Y-%m-%d"))
        c1 <- min(as.POSIXct(input$comp_date_range, format = "%Y-%m-%d"))
        b2 <- max(as.POSIXct(input$back_date_range, format = "%Y-%m-%d"))
        c2 <- max(as.POSIXct(input$comp_date_range, format = "%Y-%m-%d"))
        shaded_dates <- data.frame(xmin = c(b1, c1), xmax = c(b2, c2),
                                   ymin = c(-Inf, -Inf), ymax = c(Inf, Inf),
                                   Years = c("background", "compliance"))
        # draw shaded rectangle for background dates and compliance dates
        t1 <- t1 + geom_rect(data = shaded_dates, aes(xmin = xmin, ymin = ymin,
                                                    xmax = xmax, 
                                                    ymax = ymax, fill = Years),
                            alpha = 0.2, inherit.aes = FALSE) +
          scale_fill_manual(values=c("blue","green")) +
          guides(fill = guide_legend(override.aes = list(linetype = 0)))
      }
      if(input$coord_flip){
        t1 <- t1 + coord_flip()
      }
      print(t1)
    }
  })
  
#   # create combinations of all time series plots
#   # use a check box
#   # Insert the right number of plot output objects into the web page
#   output$combo_time_plots <- renderUI({
#     plot_output_list <- lapply(1:input$n, function(i) {
#       plotname <- paste("plot", i, sep="")
#       plotOutput(plotname, height = 280, width = 250)
#     })
#     
#     # Convert the list to a tagList - this is necessary for the list of items
#     # to display properly.
#     do.call(tagList, plot_output_list)
#   })
#   
#   # Call renderPlot for each one. Plots are only actually generated when they
#   # are visible on the web page.
#   for (i in 1:max_plots) {
#     # Need local so that each item gets its own number. Without it, the value
#     # of i in the renderPlot() will be the same across all instances, because
#     # of when the expression is evaluated.
#     local({
#       my_i <- i
#       plotname <- paste("plot", my_i, sep="")
#       
#       output[[plotname]] <- renderPlot({
#         plot(1:my_i, 1:my_i, xlim = c(1, max_plots), ylim = c(1, max_plots), main = paste("1:", my_i, ".  n is ", input$n, sep = ""))
#       })
#     })
#   }
#   
  
 # create boxplots 
  output$box_plot <- renderPlot({
    if (!is.null(input$manages_path)){
      data <- get_data()
      data_selected <- subset(data, location_id %in% input$well & 
                                param_name %in% input$analyte)
      # box plot of analyte
      b <- ggplot(data_selected, aes(location_id, y=analysis_result, 
                                     fill=location_id)) + 
        theme_bw() + ylab("Analysis Result") + xlab("Location ID") +
        guides(fill = guide_legend("Location ID")) +
        theme(legend.background = element_rect()) + 
        theme(axis.title.x = element_text(vjust=-0.5)) +
        theme(axis.text.x = element_text(angle=45)) +
        theme(axis.title.y = element_text(vjust=0.3)) 
      if(input$scale_plot){
        b1 <- b + geom_boxplot()  + facet_wrap(~param_name, scale="free")
      } else{
        b1 <- b + geom_boxplot() + facet_wrap(~param_name)
      }
      if(input$coord_flip){
        b1 <- b1 + coord_flip()
      }
      print(b1)
    }
  })
  
 # saptial plot of wells
  output$well_map <- renderPlot({
    if (!is.null(input$manages_path)){
      # read in spatial data and coerce long and lat to numeric
      sp_data <- get_spatial_data()
      sp_data <- na.omit(sp_data)
      sp_data$long_pos <- as.numeric(as.character(sp_data$long_pos))
      sp_data$lat_pos <- as.numeric(as.character(sp_data$lat_pos))
      
      well_map <- get_map(location = c(lon=mean(sp_data$long_pos), 
                                       lat=mean(sp_data$lat_pos)), zoom=14)
      
      p1 <- ggmap(well_map, extent = "device", maptype = "terrain", 
                  color = "color")
      
      p2 <- p1 + geom_point(data = sp_data, aes(x = long_pos, 
                            y = lat_pos, colour = location_id), size = 2.25)
      
      print(p2)
      
    }
  })
})