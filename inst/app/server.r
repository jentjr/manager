# change options to handle large file size
options(shiny.maxRequestSize=60*1024^2)

# Define server
shinyServer(function(input, output) {
      
  # return a list of well names
  output$wells <- renderUI({
    
    if (is.null(input$manages_path))
      return(NULL)
    
    data <- connect_manages(input$manages_path$datapath)
    
    well_names <- getWellNames(data)

    selectInput("well", "Monitoring Wells", well_names, multiple = TRUE)
  })
  
  # return a list of constituents
  output$analytes <- renderUI({
    
    if (is.null(input$manages_path))
      return(NULL)

    data <- connect_manages(input$manages_path$datapath)
    
    analyte_names <- getAnalytes(data)
    
    selectInput("analyte", "Constituents", analyte_names, multiple = TRUE)
  })
  
  output$well_table <- renderGvis({
    
    if (is.null(input$manages_path))
      return(NULL)
    
    data <- connect_manages(input$manages_path$datapath)
    
#     data_selected <- subset(data, location_id %in% input$well & param_name %in% input$analyte)
    
    gvisTable(data, options=list(page = 'enable', pageSize = 25, width = 825)) 
    
  })
  
  output$gw_summary <- renderGvis({
    
    if (is.null(input$manages_path))
      return(NULL)
    
    data <- connect_manages(input$manages_path$datapath)
    
    gw_summary_table <- groundwater_summary(data)
    
    gvisTable(gw_summary_table, options=list(page = 'enable', pageSize = 25, width = 825))
    
  })
  
  # return start and end date of background data
  output$background_date <- renderUI({
    
    if (is.null(input$manages_path))
      return(NULL)

    data <- connect_manages(input$manages_path$datapath)
    
    
    dateRangeInput("back_date_range", "Background Date Range", start = min(data$sample_date),
                   end = max(data$sample_date) - 365)
    
  })
  
  # return start and end date of compliance period
  output$compliance_date <- renderUI({
    
    if (is.null(input$manages_path))
      return(NULL)
    
    data <- connect_manages(input$manages_path$datapath)
    
    
    dateRangeInput("comp_date_range", "Compliance Date Range", start = max(data$sample_date) - 365,
                   end = max(data$sample_date))
    
  })
  
  # time series plot output
  output$time_plot <- renderPlot({
    
    if (is.null(input$manages_path))
      return(NULL)
    
    data <- connect_manages(input$manages_path$datapath)
    
    data_selected <- subset(data, location_id %in% input$well & param_name %in% input$analyte)
    
    # create separate data.frame for geom_rect data
    # change dates to POSIXct which is same as data.frame dates
    shaded_dates <- data.frame(xmin = c(min(as.numeric(input$back_date_range)), min(as.numeric(input$comp_date_range))), 
                               xmax = c(max(as.numeric(input$back_date_range)), max(as.numeric(input$comp_date_range))),
                               ymin = c(-Inf, -Inf), 
                               ymax = c(Inf, Inf),
                               years = c("background", "compliance"))
    
    
    t <- ggplot(data_selected, aes(x=sample_date, y=analysis_result, colour=param_name)) 
    
    if(input$scale_plot){
      # time series plot of analytes gridded by wells
      t1 <- t + geom_point(aes(shape=param_name), size=3) + geom_line() + 
        facet_wrap(~location_id, scales="free") + theme_bw() + xlab("Sample Date") + ylab("Analysis Result")
    } else {
      t1 <- t + geom_point(aes(shape=param_name), size=3) + geom_line() + 
        facet_wrap(~location_id) + theme_bw() + xlab("Sample Date") + ylab("Analysis Result")
    }
      
    if(input$date_lines){
      # draw shaded rectangle for background date range and compliance date range
      t1 <- t + geom_rect(data = shaded_dates, aes(xmin = xmin, ymin = ymin, xmax = xmax, 
                                         ymax = ymax, fill = years),
                alpha = 0.2, inherit.aes = FALSE) +
        
        scale_fill_manual(values=c("blue","green"))
    }
    
  print(t1)
    
  })
  
  output$box_plot <- renderPlot({
    
    if (is.null(input$manages_path))
      return(NULL)

    data <- connect_manages(input$manages_path$datapath)
    
    data_selected <- subset(data, location_id %in% input$well & param_name %in% input$analyte)
    
    # box plot of analyte
    b <- ggplot(data_selected, aes(location_id, y=analysis_result, colour=param_name)) + theme_bw() + ylab("Analysis Result") + xlab("Location ID")
    
    if(input$scale_plot){
      b1 <- b + geom_boxplot()  + facet_wrap(~param_name, scale="free")
    } else{
      b1 <- b + geom_boxplot() + facet_wrap(~param_name)
    }
    
    print(b1)
    
  })
  
  output$piper_plot <- renderPlot({
    
#     piper_data <- subset(data, location %in% well & analyte %in% c("Magnesium, Mg (dissolved)", "Calcium, Ca (dissolved)", "Chloride, Cl (total)", "Sulfate, SO4, (total)"))
    
    
    
    print(ggplot_piper())
    
  })

  output$well_map <- renderPlot({
    
    sp_data <- connect_manages_spatial(input$manages_path$datapath)
    
    sp_data <- na.omit(sp_data)
    sp_data$long_pos <- as.numeric(as.character(sp_data$long_pos))
    sp_data$lat_pos <- as.numeric(as.character(sp_data$lat_pos))
    
    well_map <- get_map(location = c(lon=mean(sp_data$long_pos), lat=mean(sp_data$lat_pos)), zoom=14)
    
    p1 <- ggmap(well_map, extent = "device", maptype = "terrain", color = "color")
    
    p2 <- p1 + geom_point(data = sp_data, aes(x = long_pos, y = lat_pos, colour = location_id), size = 2.25)
        
    print(p2)
    
  })

})