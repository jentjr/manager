# Define layout of interface
shinyUI(pageWithSidebar(
  headerPanel(title = "", windowTitle = "Groundwater Monitoring"),
  sidebarPanel(
    tags$head(
      tags$style(type="text/css", "select { width: 200px; }"),
      tags$style(type="text/css", "textarea { max-width: 400px; }"),
      tags$style(type="text/css", ".jslider { max-width: 400px; }"),
      tags$style(type='text/css', ".well { max-width: 430px; }"),
      tags$style(type='text/css', ".span4 { max-width: 270px; }")
    ),
    fileInput(inputId = "manages_path",
              label = "Enter path to MANAGES database\n
              .csv file, or excel",
              accept = c(".mdb", ".csv", ".xls")),
    conditionalPanel(
      condition = "input.data == FALSE",
      radioButtons(inputId = "file_type", 
                   label = "File Extension", 
                   choices = c(".mdb", ".csv", ".xls"))),
    conditionalPanel(
      condition = "input.data == TRUE",
      uiOutput("wells")),
    uiOutput("analytes"),
    helpText("CTRL- Click to select multiple wells and constituents."),
    conditionalPanel(condition = "input.data == TRUE", uiOutput("date_ranges")),
    checkboxInput(inputId = "scale_plot", label = "Scale Free Plot"),
    checkboxInput(inputId = "date_lines", label = "Show Date Ranges"),
    checkboxInput(inputId = "coord_flip", label = "Flip Axes")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Data Table", dataTableOutput("well_table")),
      tabPanel("Summary Table", dataTableOutput("gw_summary")),
      tabPanel("Time Series", plotOutput("time_plot")),
#       tabPanel("Combination Time Plots", uiOutput("combo_time_plots")),
      tabPanel("Boxplot", plotOutput("box_plot")),
      tabPanel("Map", plotOutput("well_map"))
#       tabPanel("About", htmlOutput(source("external/about.r")))
    )
  )
))