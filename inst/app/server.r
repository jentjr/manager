# change options to handle large file size
options(shiny.maxRequestSize=-1)

# Define server
shinyServer(function(input, output) {
  # reactive function to upload data
  get_data <- reactive({
    if (!is.null(input$manages_path)){
        switch(input$file_type, 
               ".csv" = read.csv(input$manages_path$datapath, header = TRUE, stringsAsFactors = FALSE),
               ".mdb" = connect_manages(input$manages_path$datapath),
               ".xls" = readWorksheet(loadWorkbook(input$manages_path$datapath), sheet = "Sheet1"))
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
      well_names <- getWellNames(data)
      selectInput("well", "Monitoring Wells", well_names, multiple = TRUE)
    }
  })
  
  # return a list of constituents
  output$analytes <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      analyte_names <- getAnalytes(data)
      selectInput("analyte", "Constituents", analyte_names, multiple = TRUE)
    }
  })
  
  # Output a googleTable of the data
  output$well_table <- renderGvis({
    if (!is.null(input$manages_path)){
      data <- get_data()
      #     data_selected <- subset(data, location_id %in% input$well & param_name %in% input$analyte)
      gvisTable(data, options=list(page = 'enable', pageSize = 25, width = 850, heigth = 900)) 
    }
  })
  
  # googleTable output of a data summary
  output$gw_summary <- renderGvis({
    if (!is.null(input$manages_path)){
      data <- get_data()
      gw_summary_table <- groundwater_summary(data)
      gvisTable(gw_summary_table, options=list(page = 'enable', pageSize = 25, width = 850))
    }
  })
  
  # return start and end date of background data
  output$date_ranges <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      tagList(
        dateRangeInput("back_date_range", "Background Date Range", start = min(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE)),
        dateRangeInput("comp_date_range", "Compliance Date Range", start = max(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE))  
        )
    }
  })
  
  # time series plot output
  output$time_plot <- renderPlot({
    if (!is.null(input$manages_path)){
      data <- get_data()
      data_selected <- subset(data, location_id %in% input$well & param_name %in% input$analyte)
      # create separate data.frame for geom_rect data
      # change dates to POSIXct which is same as data.frame dates
      shaded_dates <- data.frame(xmin = c(min(as.POSIXct(input$back_date_range, format = "%Y-%m-%d")),
                                          min(as.POSIXct(input$comp_date_range, format = "%Y-%m-%d"))), 
                                 xmax = c(max(as.POSIXct(input$back_date_range, format = "%Y-%m-%d")), 
                                          max(as.POSIXct(input$comp_date_range, format = "%Y-%m-%d"))),
                                 ymin = c(-Inf, -Inf), 
                                 ymax = c(Inf, Inf),
                                 years = c("background", "compliance"))

      t <- ggplot(data_selected, aes(x=sample_date, y=analysis_result, colour=param_name)) 
      
      if(input$scale_plot){
        # time series plot of analytes gridded by wells
        t1 <- t + geom_point(aes(shape=param_name), size=3) + geom_line() + 
          facet_wrap(~location_id, scales="free") + theme_bw() + xlab("Sample Date") + 
          ylab("Analysis Result") +
          scale_colour_discrete(name = "Constituent")
      } else {
        t1 <- t + geom_point(aes(shape=param_name), size=3) + geom_line() + 
          facet_wrap(~location_id) + theme_bw() + xlab("Sample Date") + 
          ylab("Analysis Result") + 
          scale_colour_discrete(name = "Constituent")
      }
      if(input$date_lines){
        # draw shaded rectangle for background date range and compliance date range
        t1 <- t1 + geom_rect(data = shaded_dates, aes(xmin = xmin, ymin = ymin, xmax = xmax, 
                                                     ymax = ymax, fill = years),
                            alpha = 0.2, inherit.aes = FALSE) +
          
          scale_fill_discrete(breaks=c("background","compliance"))
      }
      print(t1)
    }
  })
  
 # creat boxplots 
  output$box_plot <- renderPlot({
    if (!is.null(input$manages_path)){
      data <- get_data()
      data_selected <- subset(data, location_id %in% input$well & param_name %in% input$analyte)
      # box plot of analyte
      b <- ggplot(data_selected, aes(location_id, y=analysis_result, colour=param_name)) + theme_bw() + ylab("Analysis Result") + xlab("Location ID")
      if(input$scale_plot){
        b1 <- b + geom_boxplot()  + facet_wrap(~param_name, scale="free")
      } else{
        b1 <- b + geom_boxplot() + facet_wrap(~param_name)
      }
      print(b1)
    }
  })
  
 # piper plot
  output$piper_plot <- renderPlot({
    if (!is.null(input$manages_path)){
      print(ggplot_piper())
    }
  })
  
 # stiff diagram
 output$stiff_diagram <- renderPlot({
   return(NULL)
 })
 
 # saptial plot of wells
  output$well_map <- renderPlot({
    if (!is.null(input$manages_path)){
      # read in spatial data and coerce long and lat to numeric
      sp_data <- get_spatial_data()
      sp_data <- na.omit(sp_data)
      sp_data$long_pos <- as.numeric(as.character(sp_data$long_pos))
      sp_data$lat_pos <- as.numeric(as.character(sp_data$lat_pos))
      # create map using rCharts and Leaflet
      well_map <- Leaflet$new()
      well_map$setView(c(mean(sp_data$long_pos), mean(sp_data$lat_pos)), zoom=13)
      for(i in 1:nrow(sp_data)){
        well_map$marker(sp_data$lat_pos[i], sp_data$long_pos[i], 
            binPopup = paste("<p> Well", sp_data$location_id[i], "</p>", sep = ""))
      }
      print(well_map)
    }
  })
})