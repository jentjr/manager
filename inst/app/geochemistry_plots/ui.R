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
                   choices = c("MANAGES Site.mdb", ".csv", ".xls"))),
    conditionalPanel(
      condition = "input.data == TRUE",
      uiOutput("well_list")),
    helpText("CTRL- Click to select multiple wells and constituents."),
    conditionalPanel(
      condition = "input.data == TRUE",
      uiOutput("date_ranges")),
    textInput(inputId = "Mg", label = "Mg", value = "Magnesium, dissolved"),
    textInput(inputId = "Ca", label = "Ca", value = "Calcium, dissolved"),
    textInput(inputId = "K", label = "K", value = "Potassium, dissolved"),
    textInput(inputId = "Na", label = "Na", value = "Sodium, dissolved"),
    textInput(inputId = "Cl", label = "Cl", value = "Chloride, total"),
    textInput(inputId = "SO4", label = "SO4", value = "Sulfate, total"),
    textInput(inputId = "Alk", label = "Alkalinity", 
              value = "Alkalinity, total (lab)"),
    textInput(inputId = "TDS", label = "TDS", value = "Total Dissolved Solids"),
    checkboxInput(inputId = "TDS_plot",
                  label = "Scale by Total Dissolved Solids"),
    checkboxInput("piper_html", "Plot Piper Time Series"),
    checkboxInput("stiff_html", "Plot a Stiff Diagram Time Series")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Data Table", dataTableOutput("well_table")),
      tabPanel("Ion Table", dataTableOutput("ion_summary")),
      tabPanel("Piper Plot Data", dataTableOutput("piper_plot_data")),
      tabPanel("Piper Plot", plotOutput("piper_plot"), height="100%", 
               width="100%"),
      tabPanel("Piper Time Plot", htmlOutput("piper_time_plot")),
      tabPanel("Stiff Plot Data", dataTableOutput("stiff_plot_data")),
      tabPanel("Stiff Diagram", plotOutput("stiff_diagram")),
      tabPanel("Stiff Time Series", htmlOutput("stiff_time_diagram"))
#       tabPanel("Map", mapOutput("well_map"))
    )
  )
))
