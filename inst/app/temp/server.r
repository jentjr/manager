data(gw_data)
library(ggvis)
library(shiny)
shinyServer(function(input, output, session) {
  
  output$wells <- renderUI({
    well_names <- as.character(get_well_names(gw_data))
    selectInput("well", "Monitoring Wells", well_names, multiple = FALSE,
                selected = well_names[1])
  })
  
  output$analytes <- renderUI({
    analyte_names <- as.character(get_analytes(gw_data))
    selectInput("analyte", "Constituents", analyte_names, multiple = FALSE,
                selected = analyte_names[1])
  })
  
  ts_data <- reactive({
    if (is.null(input$well) | is.null(input$analyte)){
      return(gw_data)
    }
    df <- gw_data %>%
      filter(
        location_id == input$well,
        param_name == input$analyte
      )
    if (nrow(df) == 0){
      return(gw_data)
    }
    df
  })
  
  ts_data %>%
    ggvis(x = ~sample_date, y = ~analysis_result) %>%
    layer_points() %>%
    layer_lines() %>%
    add_axis("x", title = "Sample Date") %>%
    add_axis("y", title = "Analysis Result") %>%
    bind_shiny("plot1")
})  