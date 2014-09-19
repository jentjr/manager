shinyUI(navbarPage("GWSTATS",
  tabPanel("Data Input",
    sidebarLayout(
      sidebarPanel(
        fileInput(inputId = "data_path", label = "Browse to file",
                  accept = c(".mdb", ".csv", ".xls", ".xlsx")),
        radioButtons(inputId = "file_type", label = "File Type", 
                    choices = c("MANAGES database", 
                                "csv", 
                                "excel")),
        conditionalPanel(condition = "input.file_type == 'csv'",
                         textInput(inputId = "csv_date_format", 
                                   label = "Date format",
                                   value = "mdy")),
        conditionalPanel(condition = "input.file_type == 'excel'",
                         textInput(inputId = "excel_sheet", 
                                   label = "Sheet name",
                                   value = "Sheet1"))
      ),
      mainPanel(
        dataTableOutput("well_table")
      )
    )
  ),
  tabPanel("Data Summary",
    sidebarLayout(
      sidebarPanel(
       uiOutput("summary_date_ranges")  
      ),
      mainPanel(
        dataTableOutput("summary_table")  
      )
    )         
  ),  
  navbarMenu("Plots",
    tabPanel("Distribution Plots",
      sidebarLayout(
        sidebarPanel(
          uiOutput("dist_wells"),
          uiOutput("dist_analytes"),
          uiOutput("dist_date_ranges"),
          selectInput("dist_type", "Type of Distribution", 
                       row.names(Distribution.df), 
                       selected = "norm")
        ),
        mainPanel(
          plotOutput("gof_plot")  
        )
      )  
    ),
    tabPanel("Time Series",
      sidebarLayout(
        sidebarPanel(
          uiOutput("ts_wells"),
          uiOutput("ts_analytes"),
          uiOutput("ts_date_ranges"),
          selectInput("ts_facet_by", "Group plots by:", 
                      c("location_id", "param_name")),
          checkboxInput("ts_short_name", "Abbreviate Constituent Name"),
          checkboxInput("ts_date_lines", "Show Date Ranges"),
          numericInput("ncol_ts", "Number of Columns in Plot", 
                       value = 1),
          downloadButton("ts_download", "Download Plots")
        ),
        mainPanel(
          uiOutput("ts_out")
        )
      )       
    ),
    tabPanel("Boxplots",
      sidebarLayout(
        sidebarPanel(
          uiOutput("box_wells"),
          uiOutput("box_analytes"),
          selectInput("box_facet_by", "Group plot by:",
                      c("param_name", "location_id")),
          checkboxInput("box_short_name", "Abbreviate Constituent Name"),
          downloadButton("box_download", "Download Plots")
        ),
        mainPanel(
          uiOutput("boxplot_out")  
        )
      )         
    )
  ),
  navbarMenu("Geochemical Plots",
    tabPanel("Piper Diagram",
      sidebarLayout(
        sidebarPanel(
          uiOutput("wells_piper"),
          uiOutput("date_ranges_piper"),
          textInput(inputId = "Mg", label = "Mg", 
                    value = "Magnesium, dissolved"),
          textInput(inputId = "Ca", label = "Ca", 
                    value = "Calcium, dissolved"),
          textInput(inputId = "K", label = "K", 
                    value = "Potassium, dissolved"),
          textInput(inputId = "Na", label = "Na", 
                    value = "Sodium, dissolved"),
          textInput(inputId = "Cl", label = "Cl", 
                    value = "Chloride, total"),
          textInput(inputId = "SO4", label = "SO4", 
                    value = "Sulfate, total"),
          textInput(inputId = "Alk", label = "Alkalinity", 
                    value = "Alkalinity, total (lab)"),
          textInput(inputId = "TDS", label = "TDS", 
                    value = "Total Dissolved Solids"),
          textInput(inputId = "piper_title", label = "Enter Plot Title", 
                    value = "Piper Diagram"),
          checkboxInput(inputId = "TDS_plot",
                        label = "Scale by Total Dissolved Solids"),
          downloadButton("piper_download", "Download Plot")
        ),
        mainPanel(
          plotOutput("piper_plot", height = 700, width = 800)  
        )
      )
    ),
    tabPanel("Stiff Diagram",
      sidebarLayout(
        sidebarPanel(
          uiOutput("wells_stiff"),
          uiOutput("date_ranges_stiff"),
          textInput(inputId = "Mg_stiff", label = "Mg", 
                    value = "Magnesium, dissolved"),
          textInput(inputId = "Ca_stiff", label = "Ca", 
                    value = "Calcium, dissolved"),
          textInput(inputId = "K_stiff", label = "K", 
                    value = "Potassium, dissolved"),
          textInput(inputId = "Na_stiff", label = "Na", 
                    value = "Sodium, dissolved"),
          textInput(inputId = "Cl_stiff", label = "Cl", 
                    value = "Chloride, total"),
          textInput(inputId = "SO4_stiff", label = "SO4", 
                    value = "Sulfate, total"),
          textInput(inputId = "Alk_stiff", label = "Alkalinity", 
                    value = "Alkalinity, total (lab)"),
          textInput(inputId = "TDS_stiff", label = "TDS", 
                    value = "Total Dissolved Solids"),
          checkboxInput(inputId = "TDS_plot_stiff",
                        label = "Scale by Total Dissolved Solids"),
          checkboxInput(inputId = "stiff_lines", label = "Display lines"),
          downloadButton("stiff_download", "Download Plots")
        ),
        mainPanel(
          uiOutput("stiff_diagram_out")  
        )
      )         
    )
  ),
  navbarMenu("Prediction Intervals",
    tabPanel("Intra-well",
      sidebarLayout(
       sidebarPanel(
        uiOutput("wells_intra"),
        uiOutput("analytes_intra"),
        uiOutput("date_ranges_intra"),
        radioButtons(inputId = "pred_int_type", 
                     label = "Type of Prediction Limit",
                     choices = c("Simultaneous", "Regular")),
        conditionalPanel(
            condition = "input.pred_int_type == 'Simultaneous'",
            numericInput("swfpr", "Site-Wide False Positive Rate", 0.1),
            numericInput("k", "Specify a positive integer for the 
                     minimum number of observations (or averages) out of m  
                     observations (or averages) (all obtained on one future
                     sampling “occassion”) the prediction interval should 
                     contain with confidence level.", 1),
            numericInput("m", "Specify a positive integer for the maximum
                     number of future observations (or averages) on one future
                     sampling “occasion”", 2),
            numericInput("r", "Sampling frequency", 2, min=1, max=4)
          ),
        conditionalPanel(
            condition = "input.pred_int_type == 'Regular'",
            numericInput("reg_intra_k", "Specify a positive integer for the number 
                         of future observations or averages the prediction 
                         interval should contain with confidence level",
                         2, min = 0)
          ),
        checkboxInput(inputId = "intra_plot", label = "Plot Time Series"),
        conditionalPanel(
            condition = "input.intra_plot == true",
            selectInput("ts_intra_facet_by", "Group plot by:",
                        c("location_id", "param_name")),
            checkboxInput("ts_intra_short_name", "Abbreviate Constituent Name"),
            checkboxInput("ts_intra_date_lines", "Show Date Ranges"),
            numericInput("ncol_intra_ts", "Number of Columns in Plot", 
                         value = 1)               
         )
       ),
       mainPanel(
        dataTableOutput("intra_limit_out"),
        br(),
        uiOutput("ts_intra_out")
       )
      )       
    ),
    tabPanel("Inter-well")
  ),
  navbarMenu("NADA",
    tabPanel("ROS",
      sidebarLayout(
        sidebarPanel(
          uiOutput("ros_wells"),
          uiOutput("ros_analytes"),
          uiOutput("ros_date_ranges"),
          helpText("Input > 80% censored -- Results are tenuous"),
          helpText("Censored values that exceed max of uncensored values will
                   be dropped.")
        ),
        mainPanel(
          dataTableOutput("ros_summary_table"),
          br(),
          verbatimTextOutput("ros_out"),
          br(),
          plotOutput("ros_plot"),
          br(),
          verbatimTextOutput("ros_out_2")
        )
      )         
    ),
    tabPanel("Kaplan-Meier",
      sidebarLayout(
        sidebarPanel(
          uiOutput("kp_wells"),
          uiOutput("kp_analytes")
        ),
        mainPanel(
          verbatimTextOutput("kp_out")  
        )
      )         
    )
  )
))
