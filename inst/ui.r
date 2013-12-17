# Define layout of interface
shinyUI(pageWithSidebar(
  
  headerPanel("Groundwater Monitoring"),
  
    sidebarPanel(
      
      fileInput(inputId = "manages_path",
                label = "Enter path to MANAGES database",
                accept = '.mdb'),
      
      conditionalPanel(
        condition = "input.data == TRUE",
        uiOutput("wells")),

      uiOutput("analytes"),
      
      helpText("CTRL- Click to select multiple wells and constituents."),
      
      uiOutput("backgound_date"),
      
#       dateRangeInput(inputId = "back_date_range",
#                   label = "Select Background Date Range",
#                   start = start_date,
#                   end = end_date),
#       
#       dateRangeInput(inputId = "comp_date_range",
#                      label = "Select Compliance Date Range",
#                      start = end_date - 7,
#                      end = end_date),
#       
      
      checkboxInput(inputId = "scale_plot",
                    label = "Scale Free Plot"),
      
      checkboxInput(inputId = "date_lines",
                    label = "Show Date Ranges")
      
      ),

  mainPanel(
    tabsetPanel(
      tabPanel("Data Table",htmlOutput("well_table")),
      tabPanel("Summary Table", htmlOutput("gw_summary")),
      tabPanel("Time Series", plotOutput("time_plot" )),
      tabPanel("Boxplot", plotOutput("box_plot")),
      tabPanel("Piper Plot", plotOutput("piper_plot")),
      tabPanel("Stiff Diagram", plotOutput("stiff_plot")),
      tabPanel("Map", plotOutput("well_map"))
      )
    )

))
        