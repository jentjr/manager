library(shiny)
library(rCharts)

shinyUI(pageWithSidebar(
  
  headerPanel("Groundwater Monitoring"),
  
    sidebarPanel(
      selectInput(inputId = "plant",
                  label = "Select Facility",
                  choices = c("Tanners Creek", "Muskingum River", "Cardinal"),
                  selected = "Cardinal"),
      
      selectInput(inputId = "well",
                  label = "Monitoring Wells",
                  choices = well_names,
                  selected = well_names[1],
                  multiple = TRUE),
      
      dateRangeInput(inputId = "back_date_range",
                  label = "Select Background Date Range \"Solid\"",
                  start = start_date,
                  end = end_date),
      
      dateRangeInput(inputId = "comp_date_range",
                     label = "Select Compliance Date Range \"Dashed\"",
                     start = end_date - 7,
                     end = end_date),
      
      selectInput(inputId = "analyte",
                  label = "Constituents",
                  choices = analyte_names,
                  selected = analyte_names[1],
                  multiple = TRUE),
      
      checkboxInput(inputId = "scale_plot",
                    label = "Scale Free Plot"),
      
      checkboxInput(inputId = "date_lines",
                    label = "Show Date Ranges")
      
      ),

  mainPanel(
    tabsetPanel(
      tabPanel("Table", tableOutput("well_table")),
      tabPanel("Time Series Plot", plotOutput("time_plot" )),
      tabPanel("Boxplot", plotOutput("box_plot")),
      tabPanel("Piper Plot", plotOutput("piper_plot")),
      tabPanel("Stiff Plot", plotOutput("stiff_plot"))
      )
    )

))
        