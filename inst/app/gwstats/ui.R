library(EnvStats)
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
  navbarMenu("Plots",
    tabPanel("Distribution Plots",
      sidebarLayout(
        sidebarPanel(
          uiOutput("dist_wells"),
          uiOutput("dist_analytes"),
          uiOutput("dist_date_ranges"),
          radioButtons(inputId = "dist_plot_type", 
                       label = "Type of Distribution Plot",
                       choices = c("Regular", "Censored"),
                       selected = "Regular"),
          conditionalPanel(
              condition = "input.dist_plot_type == 'Censored'",
              selectInput(inputId = "cen_dist_side", label = "Censoring Side",
                        c("left", "right")),
              selectInput(inputId = "cen_dist_test", label = "Select test",
                          c("Shapiro-Francia" = "sf", "Shapiro-Wilk" = "sw", 
                            "Prob-Plot-Corr-Coeff" = "ppcc")),
              selectInput(inputId = "cen_dist_dist", label = "Distribution",
                          c("Normal" = "norm", "Lognormal" = "lnorm")),
              selectInput(inputId = "cen_dist_method", 
                          label = "Select method to compute plotting position",
                          c("michael-schucany", "modified kaplan-meier", 
                            "nelson", "hirsch-stedinger")),
              numericInput(inputId = "cen_dist_plot.pos.con", 
                           label = "Scalar for plotting position constant",
                           value = 0.375, min = 0, max = 1)
            ),
          conditionalPanel(
            condition = "input.dist_plot_type == 'Regular'",
            selectInput("dist_type", "Type of Distribution", 
                        row.names(Distribution.df), 
                        selected = "norm")
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
