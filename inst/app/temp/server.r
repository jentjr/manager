data(gw_data)
library(ggvis)
library(shiny)
shinyServer(function(input, output, session) {
  get_data <- reactive({
    if (!is.null(input$manages_path)){
      switch(input$file_type, 
             ".csv" = from_csv(input$manages_path$datapath),
             ".mdb" = connect_manages(input$manages_path$datapath),
             ".xls" = from_excel(input$manages_path$datapath)) %>%
        arrange(location_id, sample_date, param_name)
    }      
  })
  output$wells <- renderUI({
    well_names <- as.character(get_well_names(get_data()))
    selectInput("well", "Monitoring Wells", well_names, multiple = FALSE,
                selected = well_names[1])
  })
  
  output$analytes <- renderUI({
    analyte_names <- as.character(get_analytes(get_data()))
    selectInput("analyte", "Constituents", analyte_names, multiple = FALSE,
                selected = analyte_names[1])
  })
  # Output a googleTable of the data to be displayed on Data page
  output$well_table <- renderDataTable({
    if (!is.null(input$manages_path)){
      data <- get_data() 
      return(data)
    }
  }, options = list(sScrollY = "100%", sScrollX = "100%", 
                    aLengthMenu = c(5, 10, 15, 25, 50, 100), 
                    iDisplayLength = 10)
  ) 
  
    # ggvis section for time series
    vis <- reactive({
      if (!is.null(input$manages_path)){
        df <- get_data() 
        df <- subset(df, location_id == input$well_time &
              param_name == input$analyte_time)
        df$non_detect <- ifelse(df$lt_measure == "<", 
                                  "non-detect", "detected")
        df %>%
        dplyr::mutate(param = factor(param_name)) %>%
        dplyr::mutate(lt = factor(non_detect)) %>%
        ggvis(x = ~sample_date, y = ~analysis_result, stroke = ~param) %>%
          layer_points(shape = ~lt) %>%
          layer_lines() %>%
          add_axis("x", title = "Sample Date") %>%
          add_axis("y", title = "Analysis Result", title_offset = 55) %>%
          add_legend("shape", "Detected")
      }else{
        df <- data.frame(x = 1:2, y = 1:1,
                         labels = c("enter", "data"))
        df %>% ggvis(~x, ~y, text := ~labels, font = ~labels, fontSize := 40) %>%
          layer_text() 
      }
    })
    
    vis %>% bind_shiny("plot1")
})  