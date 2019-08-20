selectData <- function(input, output, session, multiple) {

  ns <- session$ns

  output$selectSites <- renderUI({

    selectInput(ns("sites"), "Sites", site_list,
                selected = site_list[1],
                multiple = TRUE)

  })

  get_locations <- reactive({
 
    query %>%
      filter(name %in% !!input$sites) %>%
      select(location_id) %>%
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
      filter(name %in% !!input$sites, location_id %in% !!input$locations) %>%
      select(param_name) %>%
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

    validate(
      need(input$sites != "", "Please submit a query")
    )

    query %>%
      filter(name %in% !!input$sites,
             location_id %in% !!input$locations,
             param_name %in% !!input$constituents) %>%
      collect() %>%
      replace_missing(value = 0.0)

  }))

}