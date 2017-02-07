library(EnvStats)
library(manager)
shinyUI(navbarPage("MANAGER",
  tabPanel("Data",
    sidebarLayout(
      sidebarPanel(
        userFileInput("datafile", "Select file") 
        ),
      mainPanel(
        dataTableOutput("table")
      )
    )
  ),
  tabPanel("Summary",
    sidebarLayout(
      sidebarPanel(
        wellConstituentInput("summary")
     ),
     mainPanel(
      verbatimTextOutput("summary_table")
      )
    )
  ),
  navbarMenu("Plots",
    tabPanel("Distribution Plots",
      sidebarLayout(
        sidebarPanel(
          wellConstituentInput("dist"),
          uiOutput("dist_date_ranges"),
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
          wellConstituentInput("tsplot"),
          uiOutput("ts_date_ranges"),
          selectInput("ts_facet_by", "Group plots by:", 
                      c("location_id", "param_name")),
          selectInput("ts_trend", "Add trend line:",
                      c("none" = "None", "lm" = "lm",
                        "glm" = "glm", "gam" = "gam",
                        "loess" = "loess", "theil-sen" = "theil-sen")),
          checkboxInput("ts_short_name", "Abbreviate Constituent Name"),
          checkboxInput("ts_date_lines", "Show Date Ranges"),
          numericInput("ncol_ts", "Number of Columns in Plot", 
                       value = NULL),
          downloadButton("ts_download", "Download Plots"),
          checkboxInput(
            inputId = "ts_interactive",
            label = "Interactive",
            value = TRUE
          ),
          conditionalPanel(
            condition = "input.ts_interactive == '0'",
            actionButton(
              inputId = "ts_submit",
              label = "Click to Plot"
            )
          )
        ),
        mainPanel(
          uiOutput("ts_out")
        )
      )       
    ),
    tabPanel("Boxplots",
      sidebarLayout(
        sidebarPanel(
          wellConstituentInput("boxplot"),
          selectInput("box_facet_by", "Group plot by:",
                      c("param_name", "location_id")),
          checkboxInput("box_short_name", "Abbreviate Constituent Name"),
          downloadButton("box_download", "Download Plots"),
          checkboxInput(
            inputId = "box_interactive",
            label = "Interactive",
            value = TRUE
          ),
          conditionalPanel(
            condition = "input.box_interactive == '0'",
            actionButton(
              inputId = "box_submit",
              label = "Click to Plot"
            )
          )
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
        radioButtons(inputId = "pred_int_type", 
                     label = "Type of Prediction Limit",
                     choices = c("Simultaneous", "Regular")),
        conditionalPanel(
            condition = "input.pred_int_type == 'Simultaneous'",
#             numericInput(inputId = "sim_intra_n.mean", label = "Specify 
#                          positive integer for the sample size associated 
#                          with the future averages. The default value is 
#                          n.mean=1 (i.e., individual observations). 
#                          Note that all future averages must be based on the 
#                          same sample size", value = 1, min = 0),
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
            textInput(inputId = "sim_intra_rule", label = "Character string
                      specifying which rule to use. The possible values are 
                      'k.of.m', the default, 'CA' (California rule), and
                      'Modified.CA' (modified California rule).", 
                      value = "k.of.m"),
            textInput(inputId = "sim_intra_pi.type", label = "Specify what kind 
                      of prediction interval to compute. The possible values 
                      are 'upper' (the default), and 'lower'", value = "upper"),
            numericInput(inputId = "sim_intra_swfpr", label = "Site-Wide False
                        Positive Rate", value = 0.1, min = 0, max = 1)
          ),
        conditionalPanel(
            condition = "input.pred_int_type == 'Regular'",
            numericInput("reg_intra_mean", "Specify a positive integer for the
                         sample size associated with the k future averages 
                         (i.e., individual observations). Note that all future
                         averages must be based on the same sample size.", 
                         1, min = 0),
            numericInput("reg_intra_k", "Specify a positive integer for the
                         number of future observations or averages the 
                         prediction interval should contain with 
                         confidence level", 2, min = 0),
            textInput(inputId = "reg_intra_method", label = "Character string
                      specifying the method to use if the number of future
                      observations (k) is greater than 1. The possible values 
                      are 'Bonferroni'(approximate method based on 
                      Bonferonni inequality; the default), 'exact'
                      (exact method due to Dunnett, 1955", 
                      value = "Bonferroni"),
            textInput(inputId = "reg_intra_pi.type", 
                      label = "Character string indicating what kind of 
                      prediction interval to compute. The possible 
                      values are 'two-sided' (the default), 'lower', and 
                      'upper'.", value = "two-sided"),
            numericInput(inputId = "reg_intra_conf.level", label = "Scalar
                         between 0 and 1 indicating the confidence level of 
                         the prediction interval", value = 0.95, min = 0, 
                         max = 1)
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
  )
))