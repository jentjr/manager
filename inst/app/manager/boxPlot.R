boxPlot <- function(input, output, session, data, multiple) {
  
  output$selectBoxplot <- renderUI({
    
    ns <- session$ns
    
    tagList(
      
      selectInput(ns("upgradient"), "Upgradient Locations", 
                  sample_locations(data),
                  multiple = multiple),
      
      selectInput(ns("downgradient"), "Downgradient Locations", 
                  sample_locations(data), 
                  multiple = multiple),
      
      selectInput(ns("sidegradient"), "Side/Off gradient Locations", 
                  sample_locations(data), 
                  multiple = multiple),
      
      selectInput(ns("leachate"), "Leachate/Source Water", 
                  sample_locations(data), 
                  multiple = multiple),
      
      selectInput(ns("constituents"), "Constituents", 
                  constituents(data),
                  selected = constituents(data)[1],
                  multiple = multiple),
      
      downloadButton("box_download", "Download Plots")
    ) 
  }) 
  
  return(reactive({
    data %>%
      dplyr::filter(location_id %in% 
                      union_all(input$upgradient, 
                                input$downgradient, 
                                input$other), 
                    param_name %in% input$constituents) %>%
      mutate(group = if_else(location_id %in% 
                               input$upgradient, 
                             "upgradient", NA)) %>%
      mutate(group = if_else(location_id %in% 
                               input$downgradient,
                             "downgradient", group)) %>%
      mutate(group = if_else(location_id %in% 
                               input$sidegradient,
                             "side/off gradient", group)) %>%
      mutate(group = if_else(location_id %in%
                               input$leachate, 
                             "Leachate/Source Water", 
                             group))
  }))
}