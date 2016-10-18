wellConstituent <- function(input, output, session, data, multiple) {
  
  output$selectWellConstituent <- renderUI({
    ns <- session$ns
    tagList(
      selectInput(ns("wells"), "Wells", get_wells(data), 
                    multiple = multiple),
      selectInput(ns("constituents"), "Constituents", get_constituents(data),
                    multiple = multiple)
    )
  }) 
  
  return(reactive({
    data %>%
      filter(location_id %in% input$wells &
               param_name %in% input$constituents)
  }))
}