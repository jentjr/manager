shinyUI(navbarPage("Groundwater Monitoring Statistics",
  tabPanel("Data",
    sidebarLayout(
      sidebarPanel(
        fileInput(inputId = "manages_path", 
                  label = "Browse to MANAGES Site.mdb file",
                  accept = c(".mdb", ".csv", ".xls")),
          conditionalPanel(
            condition = "input.data == FALSE",
              radioButtons(inputId = "file_type", 
                           label = "File Extension", 
                           choices = c(".mdb", ".csv", ".xls")))
      ),
      mainPanel(
        dataTableOutput("well_table")
      )
    )          
  ),
  navbarMenu("Plots",
    tabPanel("Boxplots",
      sidebarLayout(
       sidebarPanel(
         uiOutput("wells_box"),
         uiOutput("analytes_box"),
         uiOutput("date_ranges_box"),
         checkboxInput("scale_plot_box", "Scale Free Plot"),
         checkboxInput("coord_flip_box", "Flip Coordinates")
        ),
        mainPanel(
          plotOutput("box_plot")
        )
      )       
    ),
    tabPanel("Time series",
      sidebarLayout(
        sidebarPanel(
          uiOutput("wells_time"),
          uiOutput("analytes_time"),
          uiOutput("date_ranges_time"),
          checkboxInput("scale_plot_time", "Scale Free Plot"),
          checkboxInput("date_lines", "Show Date Ranges")
        ),
        mainPanel(
          plotOutput("time_plot")
        )
      )       
    ),
    tabPanel("Piper Diagram",
     sidebarLayout(
      sidebarPanel(
       uiOutput("wells_piper"),
       uiOutput("date_ranges_piper"),
       textInput(inputId = "Mg", label = "Mg", value = "Magnesium, dissolved"),
       textInput(inputId = "Ca", label = "Ca", value = "Calcium, dissolved"),
       textInput(inputId = "K", label = "K", value = "Potassium, dissolved"),
       textInput(inputId = "Na", label = "Na", value = "Sodium, dissolved"),
       textInput(inputId = "Cl", label = "Cl", value = "Chloride, total"),
       textInput(inputId = "SO4", label = "SO4", value = "Sulfate, total"),
       textInput(inputId = "Alk", label = "Alkalinity", 
                 value = "Alkalinity, total (lab)"),
       textInput(inputId = "TDS", label = "TDS", 
                 value = "Total Dissolved Solids"),
       checkboxInput(inputId = "TDS_plot",
                     label = "Scale by Total Dissolved Solids")
      ),
      mainPanel(
       plotOutput("piper_plot")  
      )
     )
    )
  ),
  navbarMenu("Statistics",
    tabPanel("Prediction Limit",
      sidebarLayout(
       sidebarPanel(
        uiOutput("wells_upl"),
        uiOutput("analytes_upl"),
        uiOutput("date_ranges_upl"),
        numericInput("nc", "Number of Constituents", 1),
        numericInput("nw", "Number of Wells", 1),
        numericInput("swfpr", "Site-Wide False Positive Rate (0-1)", 0.1),
        numericInput("k", "Specify a positive integer specifying the 
                     minimum number of observations (or averages) out of m  
                     observations (or averages) (all obtained on one future
                     sampling “occassion”) the prediction interval should 
                     contain with confidence level conf.level.The default 
                     value is k=1.", 1),
        numericInput("m", "Specify a positive integer specifying the maximum
                     number of future observations (or averages) on one future
                     sampling “occasion”. The default value is m=2", 2),
        numericInput("r", "Sampling frequency", 2, 
                     min=1, max=4),
        radioButtons("int_type", "Type of Prediction Limit", 
                     c("Normal", "Lognormal", "Gamma", 
                       "Non-parametric"), 
                     selected="Normal")
       ),
       mainPanel(
        plotOutput("gof"),
        br(),
        verbatimTextOutput("upl")  
       )
      )       
    ),
    tabPanel("Tolerance Interval")
  )
))