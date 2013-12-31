# change options to handle large file size
options(shiny.maxRequestSize=-1)

# Define server
shinyServer(function(input, output) {
  # reactive function to upload data
  get_data <- reactive({
    if (!is.null(input$manages_path)){
        switch(input$file_type, 
               ".csv" = from_csv(input$manages_path$datapath),
               ".mdb" = connect_manages(input$manages_path$datapath),
               ".xls" = readWorksheet(loadWorkbook(input$manages_path$datapath), sheet = "Sheet1"),
               forceConversion = TRUE, dateTimeFormat = "%Y-%m-%d %H:%M:%S")
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
  
 # piper plot
  output$piper_plot <- renderPlot({
    if (!is.null(input$manages_path)){
      data <- get_data()
      # get the major cations/anions
      geochem_plot_data <- get_geochem_plot_data()
      piper_data <- transform_piper_data(geochem_plot_data)
      print(ggplot_piper())
    }
  })
  
 # stiff diagram
 output$stiff_diagram <- renderPlot({
   if (!is.null(input$manages_path)){
     
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