timeSeries <- function(input, output, session, data, multiple) {
  
  output$selectTimeSeries <- renderUI({
    
    ns <- session$ns
    
    tagList(
      
      selectInput(ns("ts_wells"), "Wells", get_wells(data), 
                  selected = get_wells(data)[1], 
                  multiple = multiple),
      
      selectInput(ns("ts_constituents"), "Constituents", get_constituents(data),
                  selected = get_constituents(data)[1],
                  multiple = multiple),
      
      dateRangeInput(ns("ts_back_dates"), "Background Date Range", 
                     start = min(data$sample_date, na.rm = TRUE),
                     end = max(data$sample_date, na.rm = TRUE)),
      
      dateRangeInput(ns("ts_comp_dates"), "Compliance Date Range", 
                     start = max(data$sample_date, na.rm = TRUE),
                     end = max(data$sample_date, na.rm = TRUE)), 
      
      selectInput(ns("ts_facet_by"), "Group plots by:", 
                  c("location_id", "param_name")),
      
      selectInput(ns("ts_trend"), "Add trend line:",
                  c("none" = "None", "lm" = "lm",
                    "glm" = "glm", "gam" = "gam",
                    "loess" = "loess", "theil-sen" = "theil-sen")),
      
      checkboxInput(ns("ts_short_name"), "Abbreviate Constituent Name"),
      
      checkboxInput(ns("ts_date_lines"), "Show Date Ranges"),
      
      numericInput(ns("ts_ncol"), "Number of Columns in Plot", 
                   value = NULL),
      
      downloadButton(ns("ts_download"), "Download Plots"),
      
      checkboxInput(
        inputId = ns("ts_interactive"),
        label = "Interactive",
        value = TRUE
      ),
      
      conditionalPanel(
        sprintf("input['%s'] == '0'", ns("ts_interactive")),
        actionButton(
          inputId = ns("ts_submit"),
          label = "Click to Plot"
        )
      )
    )
  }) 
  
  return(reactive({
    data %>%
      filter(location_id %in% input$wells &
               param_name %in% input$constituents)
  }))
}