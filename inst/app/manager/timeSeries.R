timeSeries <- function(input, output, session, data, multiple) {
  
  output$time_series <- renderUI({
    ns <- session$ns
    tagList(
      selectInput(ns("wells"), "Wells", get_wells(data), 
                  multiple = multiple),
      selectInput(ns("constituents"), "Constituents", get_constituents(data),
                  multiple = multiple),
      dataRangeInput(ns("background"), "Background", 
                     start = min(data$sample_date, na.rm = TRUE),
                     end = max(data$sample_date, na.rm = TRUE)),
      dateRangeInput(ns("compliance", "Compliance",
                        start = min(data$sample_date, na.rm = TRUE),
                        end = max(data$sample_date, na.rm = TRUE)))
    )
  }) 
  
  return(reactive({
    data %>%
      filter(location_id %in% input$wells &
               param_name %in% input$constituents)
  }))
}