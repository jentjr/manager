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
            tabPanel("Boxplots"),
            tabPanel("Time series",
              sidebarLayout(
                sidebarPanel(
                  uiOutput("wells"),
                  uiOutput("analytes"),
                  uiOutput("date_ranges"),
                  checkboxInput("scale_plot","Scale Free Plot"),
                  checkboxInput("date_lines", "Show Date Ranges")
                ),
                mainPanel(
                 plotOutput("time_plot")
                )
              )       
            ),
            tabPanel("Piper Diagram")
          ),
          navbarMenu("Statistics",
            tabPanel("Prediction Limit"),
            tabPanel("Tolerance Interval")
          )
))