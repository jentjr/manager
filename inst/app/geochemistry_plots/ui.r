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
    textInput(inputId = "Mg", label = "Mg", value = "Magnesium, dissolved"),
    textInput(inputId = "Ca", label = "Ca", value = "Calcium, dissolved"),
    textInput(inputId = "K", label = "K", value = "Potassium, dissolved"),
    textInput(inputId = "Na", label = "Na", value = "Sodium, dissolved"),
    textInput(inputId = "Cl", label = "Cl", value = "Chloride, total"),
    textInput(inputId = "SO4", label = "SO4", value = "Sulfate, total"),
    textInput(inputId = "Alk", label = "Alkalinity", value = "Alkalinity, total (lab)"),
    textInput(inputId = "TDS", label = "TDS", value = "Total Dissolved Solids"),
    conditionalPanel(
      condition = "input.data == TRUE",
      uiOutput("date_ranges")),
    checkboxInput(inputId = "TDS_plot",
                  label = "Scale by Total Dissolved Solids")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Data Table", dataTableOutput("well_table")),
      tabPanel("Ion Table", dataTableOutput("ion_summary")),
      tabPanel("Piper Plot", plotOutput("piper_plot")),
      tabPanel("Stiff Diagram", plotOutput("stiff_plot")),
      tabPanel("Map", plotOutput("well_map"))
    )
  )
))