shinyUI(navbarPage("MANAGER",
  tabPanel("Data",
    fluidPage(
      fluidRow(
        column(2, 
          selectDataUI("select_data"),
          downloadButton(outputId = "data_table_download", 
                         label = "Download Data")
        ),
        column(10, 
          dataTableOutput("data_table")
        )
      )
    )
  ),
  tabPanel("Summary",
    fluidPage(
      fluidRow(
        column(12, dataTableOutput("summary_table"))
      )
    )
  ),
  navbarMenu("Plots",
    tabPanel("Distribution Plots",
      fluidPage(
        fluidRow(
          column(2,
            uiOutput("select_distribution_wells"),
            uiOutput("select_distribution_params"),
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
          column(10,
            plotOutput("gof_plot")
            # br(),
            # verbatimTextOutput("gof_test")     
          )
        )
      )       
    ),
    tabPanel("Correlation",
      fluidPage(
        fluidRow(
          column(2, 
            uiOutput("select_corr_wells"),
            uiOutput("select_corr_params")     
          ),
          column(10, 
            plotOutput("corr_plot", height = 700, width = "100%")       
          )
        )
      )       
    ),
    tabPanel("Time Series",
      fluidRow(
        column(12, uiOutput("ts_out"))       
      )
    ),
    tabPanel("Boxplots",
      fluidRow(
        column(2, 
          selectInput(inputId = "box_y_transform", label = "Transform y scale",
                      c("identity", "log", "log10", "sqrt", "boxcox")),
          numericInput(inputId = "box_iqr_mult", label = "IQR Multiplier",
                       value = 3, min = 0, max = 3),
          numericInput(inputId = "box_pnt_size", label = "Point size",
                       value = 2, min = 1),
          checkboxInput(inputId = "box_points", label = "Show points", FALSE)
          ),
        column(10, 
          uiOutput("boxplot_out")     
        )
      )
    )
  ),

# Geochemical Plots Navigation Bar ---------------------------------------------  
  
  navbarMenu("Geochemical Plots",
    tabPanel("Piper Diagram",
      fluidPage(
        fluidRow(
          column(2,
            selectInput(inputId = "x_cation", label = "x-cation", 
                        choices = c("Calcium, dissolved", "Calcium, total"),
                        selected = "Calcium, dissolved"),
            
            selectInput(inputId = "y_cation", label = "y-cation", 
                        choices = c("Magnesium, dissolved", "Magnesium, total")),
            
            selectInput(inputId = "z_cation", label = "z-cation", 
                        multiple = TRUE,
                        choices = c("Potassium, dissolved", "Potassium, total", 
                                   "Sodium, dissolved", "Sodium, total"),
                        selected = c("Potassium, dissolved", "Sodium, dissolved")),
            
            selectInput(inputId = "x_anion", label = "x-anion", multiple = TRUE,
                        choices = c("Chloride, total", "Chloride, dissolved", 
                                    "Fluoride, total", "Fluoride, dissolved"),
                        selected = c("Chloride, total", "Fluoride, total")),
            
            selectInput(inputId = "y_anion", label = "y-anion", 
                        choices = c("Alkalinity, total (lab)")),
            
            selectInput(inputId = "z_anion", label = "z-anion", 
                        choices = c("Sulfate, total", "Sulfate, dissolved"),
                        selected = "Sulfate, total"),
            
            selectInput(inputId = "TDS", label = "TDS", 
                       choices = c("Total Dissolved Solids")),
            
            textInput(inputId = "piper_title", label = "Enter Plot Title", 
                      value = "Piper Diagram"),
            
            checkboxInput(inputId = "TDS_plot",
                          label = "Scale by Total Dissolved Solids"),
           
            downloadButton("piper_download", "Download Plot")    
          ),
          column(10, 
            plotOutput("piper_plot", height = 700, width = "100%")    
          )
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
      fluidPage(
        fluidRow(
          column(2,
            uiOutput("select_schoeller_wells"),
            uiOutput("select_schoeller_dates"),
            
            selectInput(inputId = "Mg_schoeller", label = "Mg", 
                        choices = c("Magnesium, dissolved", 
                                    "Magnesium, total")),
            
            selectInput(inputId = "Ca_schoeller", label = "Ca", 
                      choices = c("Calcium, dissolved",
                                  "Calcium, total")),
            
            selectInput(inputId = "K_schoeller", label = "K", 
                        choices = c("Potassium, dissolved",
                                    "Potassium, total")),
            
            selectInput(inputId = "Na_schoeller", label = "Na", 
                        choices = c("Sodium, dissolved",
                                    "Sodium, total")),
            
            selectInput(inputId = "Cl_schoeller", label = "Cl", 
                        choices = c("Chloride, total", 
                                    "Chloride, dissolved")),
            
            selectInput(inputId = "SO4_schoeller", label = "SO4", 
                        choices = c("Sulfate, total",
                                    "Sulfate, dissolved")),
            
            selectInput(inputId = "Alk_schoeller", label = "Alkalinity", 
                      choices = c("Alkalinity, total (lab)")),
            
            selectInput(inputId = "facet_schoeller", label = "Facet plot by:",
                        c("location_id", "sample_date")),
            
            textInput(inputId = "schoeller_title", label = "Enter Plot Title", 
                      value = "Schoeller Diagram"),
            
            downloadButton("schoeller_download", "Download Plots")    
            ),
          column(10, 
            plotOutput("schoeller_diagram_out")     
          )
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
      verbatimTextOutput("outlier_test")
             # br(),
             # dataTableOutput("outlier_table")
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
  tabPanel("Inter-well",
    fluidPage(
      fluidRow(
        column(2,
          uiOutput("select_wells_inter"),
          uiOutput("select_analyte_inter"),
          uiOutput("select_date_ranges_inter")
        ),
        column(10, 
          verbatimTextOutput("inter_limit_out")
        )
      )
    )
  )
  )
))
