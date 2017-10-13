selectData <- function(input, output, session, multiple) {
 
  ns <- session$ns
  
  output$selectSites <- renderUI({
    
    selectInput(ns("sites"), "Sites", site_list, 
                selected = site_list[1],
                multiple = FALSE)
    
  })
 
  get_locations <- reactive({
    query %>%
      filter(NAME %in% input$sites) %>%
      select(LOCATION_ID) %>%
      collect() %>%
      first()
  }) 
  
  output$selectLocations <- renderUI({

    if (is.null(input$sites)) {

      return()

    } else


      selectInput(ns("locations"), "Locations", get_locations(),
                  selected = get_locations()[1],
                  multiple = multiple)

  })

  get_constituents <- reactive({
    query %>%
      filter(NAME %in% input$sites, LOCATION_ID %in% input$locations) %>%
      select(PARAM_NAME) %>%
      collect() %>%
      first()
  })
  
  output$selectConstituents <- renderUI({

    if (is.null(input$sites) || is.null(input$locations)) {

      return()

    } else

    selectInput(ns("constituents"), "Constituents", get_constituents(),
                selected = get_constituents()[1],
                multiple = multiple)

  })


  return(reactive({
    
    query %>%
      filter(NAME %in% input$sites, 
             LOCATION_ID %in% input$locations,
             PARAM_NAME %in% input$constituents) %>%
      collect()

  }))
  
}