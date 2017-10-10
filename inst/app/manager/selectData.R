selectData <- function(input, output, session, multiple) {
  
  output$selectDataInput <- renderUI({
    
    ns <- session$ns
    
    tagList(
      
      selectInput(ns("sites"), "Sites", site_list, 
                  selected = site_list[1], 
                  multiple = multiple),
     
      selectInput(ns("locations"), "Locations", location_list,
                  selected = location_list[1],
                  multiple = multiple),
      
      selectInput(ns("constituents"), "Constituents", constituent_list,
                  selected = constituent_list[1],
                  multiple = multiple),
      
      downloadButton(ns("data_table_download"), "Download Data")
    )
  }) 
  
  return(reactive({
    query %>%
      filter(NAME %in% input$sites, 
             LOCATION_ID %in% input$locations,
             PARAM_NAME %in% input$constituents) %>%
      collect()
    
  }))
  
}