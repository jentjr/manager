boxPlot <- function(input, output, session, data, multiple) {
  
  output$selectBoxplot <- renderUI({
    
    ns <- session$ns
    
    tagList(
      
      selectInput(ns("wells"), "Wells", get_wells(data), 
                  selected = get_wells(data)[1], 
                  multiple = multiple),
      
      selectInput(ns("constituents"), "Constituents", get_constituents(data),
                  selected = get_constituents(data)[1],
                  multiple = multiple),
      
      downloadButton("box_download", "Download Plots")
    ) 
  }) 
  
  return(reactive({
    data %>%
      dplyr::filter(location_id %in% input$wells, 
                    param_name %in% input$constituents)
  }))
}