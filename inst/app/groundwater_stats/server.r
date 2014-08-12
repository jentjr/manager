# change options to handle large file size
options(shiny.maxRequestSize=-1)
# force numbers to be decimal instead of scientific
options(scipen=6, digits = 8)

library(EnvStats)
library(ggvis)

shinyServer(function(input, output, session) {
  # reactive function to upload data
  get_data <- reactive({
    if (!is.null(input$manages_path)){
      switch(input$file_type, 
             ".csv" = from_csv(input$manages_path$datapath),
             ".mdb" = connect_manages(input$manages_path$datapath),
             ".xls" = from_excel(input$manages_path$datapath))
    }      
  })

  # return a list of well names
  output$wells <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      well_names <- get_well_names(data)
      selectInput("well", "Monitoring Wells", well_names, multiple = FALSE,
                  selected = well_names[1])
    }
  })
  
  # return a list of constituents
  output$analytes <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      analyte_names <- get_analytes(data)
      selectInput("analyte", "Constituents", analyte_names, multiple = FALSE,
                  selected = analyte_names[1])
    }
  })
  
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
  
  # Output a googleTable of the data
  output$well_table <- renderDataTable({
    if (!is.null(input$manages_path)){
      data <- get_data()
      return(data)
    }
  }, options = list(sScrollY = "100%", sScrollX = "100%", 
                    aLengthMenu = c(5, 10, 15, 25, 50, 100), 
                    iDisplayLength = 10)
  )
  
  # ggvis plot for time series
  reactive({
    get_data() %>%
      subset(location_id %in% input$well & 
            param_name %in% input$analyte) %>%
      ggvis(~sample_date, ~analysis_result) %>%
      layer_points() %>%
      layer_lines() 
  }) %>%
  bind_shiny("time_plot", "time_plot_ui")
  
})