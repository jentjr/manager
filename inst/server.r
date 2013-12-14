options(shiny.maxRequestSize=30*1024^2)


# Define server
shinyServer(function(input, output) {
  
  # get input data from MANAGES database path 
  output$well_table <- renderTable({
  
    if (is.null(input$manages_path))
      return(NULL)
    
    data <- read.csv(input$manages_path$datapath, header=TRUE)
    data$sample_date <- as.Date(data$sample_date, format="%m/%d/%y")
    
    print(data)
    #   connect_manages(path)
  })
  
  
  # return a list of well names
  output$wells <- renderUI({
    
    if (is.null(input$manages_path))
      return(NULL)
    
    data <- read.csv(input$manages_path$datapath, header=TRUE)
    data$sample_date <- as.Date(data$sample_date, format="%m/%d/%y")
    
    well_names <- getWellNames(data)

    selectInput("well", "Monitoring Wells", well_names, multiple = TRUE)
  })
  
  # return a list of constituents
  output$analytes <- renderUI({
    
    if (is.null(input$manages_path))
      return(NULL)
    
    data <- read.csv(input$manages_path$datapath, header=TRUE)
    data$sample_date <- as.Date(data$sample_date, format="%m/%d/%y")
    
    analyte_names <- getAnalytes(data)
    
    selectInput("analyte", "Constituents", analyte_names, multiple = TRUE)
  })
  
  # return start and end date of series selected
  output$background_date <- renderUI({
    
    if (is.null(input$manages_path))
      return(NULL)
    
    data <- read.csv(input$manages_path$datapath, header=TRUE)
    data$sample_date <- as.Date(data$sample_date, format="%m/%d/%y")
    
    dateRangeInput("background", "Background Date Range", min = min(data$sample_date),
                   max = max(data$sample_date))
    
  })
  
  
  # time series plot output
  output$time_plot <- renderPlot({
    
    if (is.null(input$manages_path))
      return(NULL)
    
    data <- read.csv(input$manages_path$datapath, header=TRUE)
    data$sample_date <- as.Date(data$sample_date, format="%m/%d/%y")
    
    data_selected <- subset(data, location_id %in% input$well & param_name %in% input$analyte)
    
    t <- ggplot(data_selected, aes(x=sample_date, y=analysis_result, colour=param_name)) 
    
    if(input$scale_plot){
      # time series plot of analytes gridded by wells
      t1 <- t + geom_point(aes(shape=param_name), size=3) + geom_line() + facet_wrap(~location_id, scales="free") + theme_bw() + xlab("Sample Date") + ylab("Analysis Result")
    } else {
      t1 <- t + geom_point(aes(shape=param_name), size=3) + geom_line() + facet_wrap(~location_id) + theme_bw() + xlab("Sample Date") + ylab("Analysis Result")
    }
      
    if(input$date_lines){
      t1 <- t1 + geom_vline(xintercept=c(min(as.numeric(input$back_date_range)), max(as.numeric(input$back_date_range))))
      t1 <- t1 + geom_vline(xintercept=c(min(as.numeric(input$comp_date_range)), max(as.numeric(input$comp_date_range))), linetype="longdash")
      # change dates to POSIXct which is same as data.frame dates
      # create separate data.frame for geom_rect data
      # draw lines for background date range and compliance date range
#       t1 <- t1 + geom_rect(xmin=min(as.numeric(input$back_date_range)), xmax=max(as.numeric(input$back_date_range)), ymin=-Inf, ymax=Inf, alpha=1/50)
#       t1 <- t1 + geom_rect(xmin=min(as.numeric(input$comp_date_range)), xmax=max(as.numeric(input$comp_date_range)), ymin=-Inf, ymax=Inf, alpha=1/50, fill="red")
    }
    
    print(t1)
    
  })
  
  output$box_plot <- renderPlot({
    
    if (is.null(input$manages_path))
      return(NULL)
    
    data <- read.csv(input$manages_path$datapath, header=TRUE)
    data$sample_date <- as.Date(data$sample_date, format="%m/%d/%y")
    
    data_selected <- subset(data, location_id %in% input$well & param_name %in% input$analyte)
    
    # box plot of analyte
    b <- ggplot(data_selected, aes(location_id, y=analysis_result, colour=param_name)) + theme_bw()
    
    if(input$scale_plot){
      b1 <- b + geom_boxplot()  + facet_wrap(~param_name, scale="free")
    } else{
      b1 <- b + geom_boxplot() + facet_wrap(~param_name)
    }
    
    print(b1)
    
  })
  
  output$piper_plot <- renderPlot({
    
#     piper_data <- subset(data, location %in% well & analyte %in% c("Magnesium, Mg (dissolved)", "Calcium, Ca (dissolved)", "Chloride, Cl (total)", "Sulfate, SO4, (total)"))
    
    
    
#     print(ggplot_piper())
    
  })
  
#   # Show the first "n" observations
#   output$well_table <- renderTable({  
# #     data_selected <- subset(data, location_id %in% input$well & param_name %in% input$analyte)
# #     data_selected$sample_date <- as.Date(data_selected$sample_date, format="%Y-%m-%d")
# #     # out <- subset(out_f, location %in% input$well & analyte %in% input$analyte)
# #     print(data_selected)
#   })
#   
})