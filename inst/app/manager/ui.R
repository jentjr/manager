shinyUI(navbarPage("MANAGER",
  tabPanel("Data",
    sidebarLayout(
      sidebarPanel(
        selectDataUI("select_data"),
        downloadButton(outputId = "data_table_download", 
                       label = "Download Data")
        ),
      mainPanel(
        dataTableOutput("data_table")
      )
    )
  ),
  tabPanel("Summary",
    sidebarLayout(
      sidebarPanel(
        selectDataUI("summary_data"),
        downloadButton(outputId = "summary_table_download", 
                       label = "Download Data")
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
          selectDataUI("distribution_data"),
          radioButtons(
            inputId = "dist_plot_type", 
            label = "Type of Distribution Plot",
            choices = c("Regular", "Censored"),
            selected = "Regular"
          ),
          conditionalPanel(
              condition = "input.dist_plot_type == 'Censored'",
              selectInput(
                inputId = "cen_dist_side", 
                label = "Censoring Side",
                choices = c("left", "right")
              ),
              selectInput(
                inputId = "cen_dist_test", 
                label = "Select test",
                choices = c("Shapiro-Francia" = "sf", 
                            "Shapiro-Wilk" = "sw", 
                            "Prob-Plot-Corr-Coeff" = "ppcc")
              ),
              selectInput(
                inputId = "cen_dist_dist", 
                label = "Distribution",
                choices = c("Normal" = "norm", "Lognormal" = "lnorm")
              ),
              selectInput(
                inputId = "cen_dist_method", 
                label = "Select method to compute plotting position",
                choices = c("michael-schucany", 
                            "modified kaplan-meier", 
                            "nelson", 
                            "hirsch-stedinger")
              ),
              numericInput(
                inputId = "cen_dist_plot.pos.con", 
                label = "Scalar for plotting position constant",
                value = 0.375, 
                min = 0, 
                max = 1
              )
            ),
          conditionalPanel(
            condition = "input.dist_plot_type == 'Regular'",
            selectInput(
              inputId = "dist_type", 
              label = "Type of Distribution", 
              choices = row.names(Distribution.df), 
              selected = "norm"
            )
          )
        ),
        mainPanel(
          tableOutput("lt_summary"),
          br(),
          plotOutput("gof_plot"),
          br(),
          verbatimTextOutput("gof_test")
        )
      )  
    ),
    tabPanel("Time Series",
      sidebarLayout(
        sidebarPanel(
          selectDataUI("ts_data")
        ),
        mainPanel(
          uiOutput("ts_out")
        )
      )       
    ),
    tabPanel("Boxplots",
      sidebarLayout(
        sidebarPanel(
          selectDataUI("boxplot_data"),
          selectInput(inputId = "box_y_transform", label = "Transform y scale",
                    c("identity", "log", "log10", "sqrt", "boxcox"))
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
    ),
    tabPanel("Schoeller Diagram",
      sidebarLayout(
        sidebarPanel(
          uiOutput("wells_schoeller"),
          uiOutput("date_ranges_schoeller"),
          textInput(inputId = "Mg_schoeller", label = "Mg", 
                    value = "Magnesium, dissolved"),
          textInput(inputId = "Ca_schoeller", label = "Ca", 
                    value = "Calcium, dissolved"),
          textInput(inputId = "K_schoeller", label = "K", 
                    value = "Potassium, dissolved"),
          textInput(inputId = "Na_schoeller", label = "Na", 
                    value = "Sodium, dissolved"),
          textInput(inputId = "Cl_schoeller", label = "Cl", 
                    value = "Chloride, total"),
          textInput(inputId = "SO4_schoeller", label = "SO4", 
                    value = "Sulfate, total"),
          textInput(inputId = "Alk_schoeller", label = "Alkalinity", 
                    value = "Alkalinity, total (lab)"),
          selectInput(inputId = "facet_schoeller", label = "Facet plot by:",
                       c("location_id", "sample_date")),
#           conditionalPanel(
#             condition = "input.schoeller_type == 'separate'",
#             selectInput(inputId = "group_schoeller", 
#                         label = "Group plot by:",
#                         c("location_id", "sample_date"))
#             ),
#           conditionalPanel(
#             condition = "input.schoeller_type == 'group'",
#             selectInput(inputId = "facet_schoeller_by", 
#                         label = "Facet plots by:",
#                         c("location_id", "sample_date"))
#             ),
          downloadButton("schoeller_download", "Download Plots")
        ),
        mainPanel(
          plotOutput("schoeller_diagram_out")  
        )
      )  
    )
  ),
tabPanel("Outliers",
         sidebarLayout(
           sidebarPanel(
             uiOutput("outlier_wells"),
             uiOutput("outlier_analytes"),
             uiOutput("outlier_date_ranges"),
             selectInput(
               inputId = "outlier_test_name", 
               label = "Outlier Test",
               choices = c("Rosner", "Grubb", "Dixon")
             ),
             conditionalPanel(
               condition = "input.outlier_test_name == 'Rosner'",
               numericInput(
                 inputId = "rosnerN", 
                 label = "Number of suspected outliers",
                 value = 2, min = 0
               ),
               numericInput(
                 inputId = "rosnerAlpha",
                 label = "alpha",
                 value = 0.01, min = 0, max = 1
               )
             ),
             conditionalPanel(
               condition = "input.outlier_test_name == 'Grubb'",
               selectInput(
                 inputId = "grubbType",
                 label = "Type of Test",
                 choices = c("10" = 10, "11" = 11, "20" = 20)
               ),
               selectInput(
                 inputId = "grubbOpposite",
                 label = "Choose Opposite",
                 choices = c("FALSE" = 0, "TRUE" = 1)
               ),
               selectInput(
                 inputId = "grubbSide",
                 label = "Treat as two-sided",
                 choices = c("FALSE" = 0, "TRUE" = 1)
               )
             ),
             conditionalPanel(
               condition = "input.outlier_test_name == 'Dixon'",
               selectInput(
                 inputId = "dixonType",
                 label = "Type of Test",
                 choices = c("0" = 0, "10" = 10, "11" = 11, 
                             "12" = 12, "20" = 20, "21" = 21)
               ),
               selectInput(
                 inputId = "dixonOpposite",
                 label = "Choose Opposite",
                 choices = c("FALSE" = 0, "TRUE" = 1)
               ),
               selectInput(
                 inputId = "dixonSide",
                 label = "Treat as two-sided",
                 choices = c("TRUE" = 1, "FALSE" = 0)
               )
             )
           ),
           mainPanel(
             verbatimTextOutput("outlier_test"),
             br(),
             dataTableOutput("outlier_table")
           )
         )
),
tabPanel("Trends",
         sidebarLayout(
           sidebarPanel(
             uiOutput("trend_wells"),
             uiOutput("trend_analytes"),
             uiOutput("trend_date_ranges")
           ),
           mainPanel(
             verbatimTextOutput("trend_test")
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
        
         numericInput(inputId = "sim_intra_n.mean", label = "Specify
                      positive integer for the sample size associated
                      with the future averages. The default value is
                      n.mean=1 (i.e., individual observations).
                      Note that all future averages must be based on the
                      same sample size", value = 1, min = 0),
        
        numericInput(inputId = "sim_intra_k", label = "Specify an integer k 
                     in the k-of-m rule for the minimum number of 
                     observations (or averages) out of m observations 
                     (or averages) (all obtained on one future 
                     sampling “occassion”) the prediction interval 
                     should contain with the specified confidence level.",
                     value = 1, min = 0),
        
        numericInput(inputId = "sim_intra_m", label = "Specify a positive
                     integer, m, for the maximum number of future 
                     observations (or averages) on one future sampling
                     'occasion'", value = 2, min = 0, max = 4),
         
        numericInput(inputId = "sim_intra_r", label = "Sampling frequency
                     annually = 1, semi-annually = 2, etc.", value = 2, 
                     min = 1, max = 4),
        
        selectInput(inputId = "sim_intra_rule", label = "Character string
                  specifying which rule to use. The possible values are 
                  'k.of.m', the default, 'CA' (California rule), and
                  'Modified.CA' (modified California rule).", 
                  choices = c("k.of.m" = "k.of.m", 
                             "CA" = "CA", 
                             "Modified.CA" = "Modified.CA")),
         
        textInput(inputId = "sim_intra_pi.type", label = "Specify what kind 
                  of prediction interval to compute. The possible values 
                  are 'upper' (the default), and 'lower'", value = "upper"),
         
        numericInput(inputId = "sim_intra_swfpr", label = "Site-Wide False
                    Positive Rate", value = 0.1, min = 0, max = 1)
       
        ),
       
       mainPanel(
        
         verbatimTextOutput("intra_limit_out")
       
      )
    )       
  ),
  
  tabPanel("Inter-well")
 
  )

))
