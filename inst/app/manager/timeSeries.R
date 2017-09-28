timeSeries <- function(input, output, session, data, multiple) {
  
  output$selectTimeSeries <- renderUI({
    
    ns <- session$ns
    
    tagList(
      
      selectInput(ns("ts_wells"), "Wells", 
                  sample_locations(data, location_id), 
                  selected = sample_locations(data)[1], 
                  multiple = multiple),
      
      selectInput(ns("ts_constituents"), "Constituents", 
                  constituents(data, param_name),
                  selected = constituents(data)[1],
                  multiple = multiple),
      
      selectInput(ns("ts_group_by"), "Group plots by:", 
                  colnames(data)),
      
      selectInput(ns("ts_facet_by"), "Facet Wrap by:",
                  colnames(data)),
      
      selectInput(ns("ts_trend"), "Add trend line:",
                  c("none" = "None", "lm" = "lm",
                    "glm" = "glm", "gam" = "gam",
                    "loess" = "loess", "theil-sen" = "theil-sen")),
      
      checkboxInput(ns("ts_short_name"), "Abbreviate Constituent Name"),
      
      numericInput(ns("ts_ncol"), "Number of Columns in Plot", 
                   value = NULL),
      
      checkboxInput(ns("ts_date_lines"), "Show Date Ranges"),
      
      conditionalPanel(
        sprintf("input['%s'] == '1'", ns("ts_date_lines")),
        
        dateRangeInput(ns("ts_back_dates"), "Background Date Range", 
                       start = min(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE)),
        
        dateRangeInput(ns("ts_comp_dates"), "Compliance Date Range", 
                       start = max(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE))
      ),
      
      downloadButton("ts_download", "Download Plots"),
      
      checkboxInput(
        inputId = "ts_interactive",
        label = "Interactive",
        value = TRUE
      ),
      
      conditionalPanel(
        sprintf("input['%s'] == '0'", ns("ts_interactive")),
        actionButton(
          inputId = "ts_submit",
          label = "Click to Plot"
        )
      )
    )
  }) 
  
  return(reactive({
    data %>%
      filter(location_id %in% input$ts_wells,
             param_name %in% input$ts_constituents)
  }))
}