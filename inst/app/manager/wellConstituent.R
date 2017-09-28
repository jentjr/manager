wellConstituent <- function(input, output, session, data, multiple) {
  
  output$selectWellConstituent <- renderUI({
    ns <- session$ns
    tagList(
      selectInput(ns("wells"), "Wells", sample_locations(data), 
                  selected = sample_locations(data)[1], 
                  multiple = multiple),
      selectInput(ns("constituents"), "Constituents", constituents(data),
                  selected = constituents(data)[1],
                  multiple = multiple)
    )
  }) 
  
  return(reactive({
    data %>%
      filter(location_id %in% input$wells &
             param_name %in% input$constituents)
  }))
}