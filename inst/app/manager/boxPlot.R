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

      checkboxInput("box_short_name", "Abbreviate Constituent Name"),
      
      downloadButton("box_download", "Download Plots"),
      
      checkboxInput(
        inputId = "box_interactive",
        label = "Interactive",
        value = TRUE
      ),
      
      conditionalPanel(
        sprintf("input['%s'] == '0'", ns("box_interactive")),
        actionButton(
          inputId = "box_submit",
          label = "Click to Plot"
        )
      )
    )
  }) 
  
  return(reactive({
    data %>%
      filter(location_id %in% input$wells, 
             param_name %in% input$constituents)
  }))
}