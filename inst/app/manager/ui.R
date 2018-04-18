shinyUI(
navbarPage("MANAGER",
 tabPanel("Data", 
  fluidPage(
    fluidRow(
      column(2, 
        selectDataUI("select_data"),
        actionButton("run_query", "Submit Query")
      ),
      column(10, 
        tabsetPanel(
          tabPanel("Data", 
            dataTableOutput("data_table"),
            downloadButton(outputId = "data_table_download", 
                           label = "Download Data")
          ),
          tabPanel("Summary",
            dataTableOutput("summary_table"),
            downloadButton(outputId = "summary_table_download",
                           label = "Download Data")
          ),
          tabPanel("Wide Table", 
            dataTableOutput("wide_table")
          )
        )
      )
   )
  )
),
tabPanel("Map",
  fluidPage(
    mapviewOutput("mapplot", height = 725)
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
        column(2, 
          uiOutput("select_ts_wells"),
          uiOutput("select_ts_params"),
          downloadButton(outputId = "ts_download",
                         label = "Download Plots")
          ),
        column(10, 
          uiOutput("ts_out"))
      )
    ),
    tabPanel("Boxplots",
      fluidRow(
        column(2, 
          uiOutput("select_boxplot_wells"),
          uiOutput("select_boxplot_params"),
          selectInput(inputId = "boxplot_fill", label = "Fill Variable",
                      choices = c("location_id", "location_class", "water_class"),
                      selected = "location_class"),
          selectInput(inputId = "box_y_transform", label = "Transform y scale",
                      c("identity", "log", "log10", "sqrt", "boxcox")),
          numericInput(inputId = "box_iqr_mult", label = "IQR Multiplier",
                       value = 3, min = 0, max = 3),
          numericInput(inputId = "box_pnt_size", label = "Point size",
                       value = 2, min = 1),
          checkboxInput(inputId = "box_points", label = "Show points", FALSE),
          downloadButton("box_download", "Download Plots")
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
          uiOutput("select_piper_wells"),
          uiOutput("select_piper_x_cation"),
          textInput(inputId = "x_cation_label", label = "X Cation Label",
                    value = "Ca"),
          uiOutput("select_piper_y_cation"),
          textInput(inputId = "y_cation_label", label = "Y Cation Label",
                    value = "Mg"),
          uiOutput("select_piper_z_cation"),
          textInput(inputId = "z_cation_label", label = "Z Cation Label",
                    value = "Na + K"),
          uiOutput("select_piper_x_anion"),
          textInput(inputId = "x_anion_label", label = "X Anion Label",
                    value = "Cl + F"),
          uiOutput("select_piper_y_anion"),
          textInput(inputId = "y_anion_label", label = "Y Anion Label",
                    value = "HCO3 + CO2"),
          uiOutput("select_piper_z_anion"),
          textInput(inputId = "z_anion_label", label = "Z Anion Label",
                    value = "SO4"),
          textInput(inputId = "x_z_anion_label",label = "X-Z Anion Label", 
                    value = "SO4 + Cl + F"),
          textInput(inputId = "x_y_cation_label", label = "X Y Cation Label",
                    value = "Ca + Mg"),
          checkboxInput(inputId = "TDS_plot",
                        label = "Scale by Total Dissolved Solids"),
          uiOutput("select_piper_tds"),
          textInput(inputId = "piper_title", label = "Enter Plot Title", 
                    value = "Piper Diagram"),
          downloadButton("piper_download", "Download Plot")
        ),
        column(10, 
          plotOutput("piper_plot", height = 700, width = "100%")
        )
      )
    )
  ),
  tabPanel("Stiff Diagram",
    fluidPage(
      fluidRow(
        column(2, 
          uiOutput("select_stiff_wells"),
          uiOutput("select_stiff_dates"),
          selectInput(inputId = "Mg_stiff", label = "Magnesium",
                      choices = c("Magnesium, dissolved",
                                  "Magnesium, total"),
                      selected = "Magnesium, total"),
          selectInput(inputId = "Ca_stiff", label = "Calcium",
                      choices = c("Calcium, dissolved",
                                  "Calcium, total"),
                      selected = "Calcium, total"),
          selectInput(inputId = "K_stiff", label = "Potassium", 
                      choices = c("Potassium, dissolved",
                                  "Potassium, total"),
                      selected = "Potassium, total"),
          selectInput(inputId = "Na_stiff", label = "Sodium", 
                      choices = c("Sodium, dissolved",
                                  "Sodium, total"),
                      selected = "Sodium, total"),
          selectInput(inputId = "Cl_stiff", label = "Chloride", 
                      choices = c("Chloride, dissolved",
                                  "Chloride, total"),
                      selected = "Chloride, total"),
          selectInput(inputId = "SO4_stiff", label = "Sulfate", 
                      choices = c("Sulfate, dissolved",
                                  "Sulfate, total"),
                      selected = "Sulfate, total"),
          selectInput(inputId = "Alk_stiff", label = "Alkalinity", 
                      choices = c("Alkalinity, total (lab)"),
                      selected = "Alkalinity, total (lab)"),
          checkboxInput(inputId = "TDS_stiff",
                        label = "Scale by Total Dissolved Solids"),
          uiOutput("select_stiff_tds"),
          checkboxInput(inputId = "stiff_lines", label = "Display lines"),
          selectInput(inputId = "stiff_group", label = "Grouping Variable",
                      choices = c("location_id", "sample_date"),
                      selected = "location_id"),
          downloadButton("stiff_download", "Download Plots")
        ),
        column(10,
          uiOutput("stiff_diagram") 
          )
        )
      )
    ),
    tabPanel("Schoeller Diagram",
      fluidPage(
        fluidRow(
          column(2,
            uiOutput("select_schoeller_wells"),
            uiOutput("select_schoeller_dates"),
            selectInput(inputId = "Mg_schoeller", label = "Magnesium",
                        choices = c("Magnesium, dissolved", 
                                    "Magnesium, total"),
                        selected = "Magnesium, total"),
            selectInput(inputId = "Ca_schoeller", label = "Calcium",
                      choices = c("Calcium, dissolved",
                                  "Calcium, total"),
                      selected = "Calcium, total"),
            selectInput(inputId = "K_schoeller", label = "Potassium",
                        choices = c("Potassium, dissolved",
                                    "Potassium, total"),
                        selected = "Potassium, total"),
            selectInput(inputId = "Na_schoeller", label = "Sodium",
                        choices = c("Sodium, dissolved",
                                    "Sodium, total"),
                        selected = "Sodium, total"),
            selectInput(inputId = "Cl_schoeller", label = "Chloride",
                        choices = c("Chloride, total",
                                    "Chloride, dissolved"),
                        selected = "Chloride, total"),
            selectInput(inputId = "SO4_schoeller", label = "Sulfate",
                        choices = c("Sulfate, total",
                                    "Sulfate, dissolved"),
                        selected = "Sulfate, total"),
            selectInput(inputId = "Alk_schoeller", label = "Alkalinity",
                      choices = c("Alkalinity, total (lab)"),
                      selected = "Alkalinity, total (lab)"),
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
navbarMenu("Statistical Intervals", 
  tabPanel("Confidence Intervals",
    fluidPage(
      fluidRow(
        column(2, 
          uiOutput("select_conf_int_wells"),
          uiOutput("select_conf_int_analytes"),
          uiOutput("select_conf_int_date_range")
        ),
        column(10, 
          dataTableOutput("conf_int_out")
        )
      )
    )
  ),
  tabPanel("Tolerance Intervals",
    fluidPage(
      fluidRow(
        column(2, 
          uiOutput("select_tol_int_wells"),
          uiOutput("select_tol_int_analytes"),
          uiOutput("select_tol_int_date_range")
        ),
        column(10, 
          dataTableOutput("tol_int_out")
        )
      )
    )
  ),
  tabPanel("Prediction Intervals",
    tabsetPanel(
      tabPanel("Intrawell",
        fluidPage(
          fluidRow(
            column(2,
              uiOutput("wells_intra"),
              uiOutput("analytes_intra"),
              uiOutput("date_ranges_intra"),
              numericInput(inputId = "intra_n.mean", label = "Specify a
                           positive integer for the sample size associated
                           with the future averages. The default value is
                           n.mean=1 (i.e., individual observations).
                           Note that all future averages must be based on the
                           same sample size", value = 1, min = 0),
              numericInput(inputId = "intra_k", label = "Specify a positive
                           integer for the k-of-m rule (rule='k.of.m'), a
                           positive integer specifying the minimum number of
                           observations (or averages) out of m observations
                           (or averages) (all obtained on one future sampling
                           “occassion”) the prediction interval should contain
                           with confidence level conf.level. The default value
                           is k=1. This argument is ignored when the argument
                           rule is not equal to 'k.of.m'.", value = 1, min = 0),
              numericInput(inputId = "intra_m", label = "positive integer
                           specifying the maximum number of future observations
                           (or averages) on one future sampling “occasion”. The
                           default value is m=2, except when rule='Modified.CA',
                           in which case this argument is ignored and m is
                           automatically set equal to 4.",
                           value = 2, min = 0),
              numericInput(inputId = "intra_r", label = "positive integer
                           specifying the number of future sampling “occasions”.
                           The default value is r=1.", value = 1, min = 0),
              selectInput(inputId = "intra_rule", label = "character string
                          specifying which rule to use. The possible values are
                          'k.of.m' (k-of-m rule; the default), 'CA' (California
                          rule), and 'Modified.CA' (modified California rule)",
                          choices = c("k.of.m", "CA", "Modified.CA"),
                          selected = "k.of.m"),
              selectInput(inputId = "intra_pi.type", label = "Specify what kind 
                          of prediction interval to compute. The possible values 
                          are 'upper' (the default), and 'lower'", 
                          choices = c("two-sided", "upper", "lower"),
                          selected = "upper"),
              numericInput(inputId = "intra_conf", label = "Enter a value
                           between 0 and 1 indicating the confidence level of the
                           prediction interval", 
                           value = 0.95, min = 0, max = 1)
              ),
              column(10, 
                dataTableOutput("intra_limit_out"),
                br(),
                uiOutput("ts_intra_out")
              )
            )
          )
        ),
  tabPanel("Interwell",
    fluidPage(
      fluidRow(
        column(2,
          uiOutput("select_wells_inter"),
          uiOutput("select_analyte_inter"),
          uiOutput("select_date_ranges_inter"),
          numericInput(inputId = "inter_n.mean", label = "Specify a positive
                       integer for the sample size associated with the future
                       averages. The default value is n.mean=1 (i.e.,
                       individual observations). Note that all future averages
                       must be based on the same sample size",
                       value = 1, min = 0),
          numericInput(inputId = "inter_k", label = "Specify a positive integer
                       for the k-of-m rule (rule='k.of.m'), a positive integer 
                       specifying the minimum number of observations (or averages)
                       out of m observations (or averages)
                       (all obtained on one future sampling “occassion”)
                       the prediction interval should contain with confidence
                       level conf.level. The default value is k=1.
                       This argument is ignored when the argument rule is not
                       equal to 'k.of.m'.", value = 1, min = 0),
          numericInput(inputId = "inter_m", label = "positive integer specifying
                       the maximum number of future observations (or averages) on
                       one future sampling “occasion”. The default value is m=2,
                       except when rule='Modified.CA', in which case this argument
                       is ignored and m is automatically set equal to 4.",
                       value = 2, min = 0),
          numericInput(inputId = "inter_r", label = "positive integer specifying
                       the number of future sampling “occasions”. The default
                       value is r=1.", value = 1, min = 0),
          selectInput(inputId = "inter_rule", label = "character string specifying
                      which rule to use. The possible values are 'k.of.m'
                      (k-of-m rule; the default), 'CA' (California rule), and
                      'Modified.CA' (modified California rule)",
                      choices = c("k.of.m", "CA", "Modified.CA"),
                      selected = "k.of.m"),
          selectInput(inputId = "inter_pi.type", label = "Specify what kind 
                      of prediction interval to compute. The possible values 
                      are 'upper' (the default), and 'lower'", 
                      choices = c("two-sided", "upper", "lower"),
                      selected = "upper"),
          numericInput(inputId = "inter_conf", label = "Enter a value
                       between 0 and 1 indicating the confidence level of the
                       prediction interval", 
                       value = 0.95, min = 0, max = 1)
        ),
        column(10, 
          dataTableOutput("inter_limit_out")
        )
      )
    )
  ),
  tabPanel("Test Power",
   fluidPage(
     fluidRow(
       column(2, 
         numericInput(inputId = "power_n", 
                      label = "Number of background samples",
                      value = 8, min = 1),
         numericInput(inputId = "power_n_mean", label = "Sample size for 
                      future averages",
                      value = 1, min = 1),
         numericInput(inputId = "power_k", label = "k in k.of.m",
                      value = 1, min = 1),
         numericInput(inputId = "power_m", label = "m",
                      value = 2, min = 1),
         numericInput(inputId = "power_r", label = "r",
                      value = 1, min = 1),
         numericInput(inputId = "conf_power", label = "conf.level",
                      value = 0.95)
      ),
      column(10,
        plotOutput("power_plot"),
        br(),
        helpText("The Unified Guidance recommends that when background is
                 approximately normal, any statistical test should be able to
                 detect a 3 standard deviation increase at least 55-60% of the
                 time and a 4 standard deviation increase with at least 80-85%
                 probability.")
      )
    )
  )
 ) # End Test Power tabPanel
) # End tabsetPanel for Inter and Intra Intervals
) # End Prediction Intervals
), # End of Statisitcal Intervals

navbarMenu("Clustering",
    tabPanel("Hierarchical",
    fluidPage(
      fluidRow(
        column(2,
               uiOutput("select_wells_hca"),
               uiOutput("select_analyte_hca"),
               uiOutput("select_date_ranges_hca"),
               selectInput("clust_dist_method", 
                           label = "Select Distance Method",
                           choices = c("euclidean",
                                       "maximum",
                                       "manhattan",
                                       "canberra",
                                       "binary",
                                       "minkowski"),
                           selected = "euclidean"),
               selectInput("clust_agg_method",
                           label = "Cluster Agglomeration Method",
                           choices = c("ward.D",
                                       "ward.D2",
                                       "single",
                                       "complete",
                                       "average",
                                       "mcquitty",
                                       "median",
                                       "centroid"),
                           selected = "average")
        ),
        column(10, 
          plotOutput("hca_out")
        )
      )
    )
  ),
  tabPanel("k-means",
    fluidPage(
      fluidRow(
        column(2,
               uiOutput("select_wells_kmeans"),
               uiOutput("select_analyte_kmeans"),
               uiOutput("select_date_ranges_kmeans"),
               selectInput("kmeans_dist_method", 
                           label = "Select Distance Method",
                           choices = c("euclidean",
                                       "maximum",
                                       "manhattan",
                                       "canberra",
                                       "binary",
                                       "minkowski"),
                           selected = "euclidean"),
               numericInput("kmeans_centers",
                            label = "Number of clusters",
                            value = 1,
                            min = 1),
               selectInput("kmeans_algorithm",
                           label = "K-Means Algorithm",
                           choices = c("Hartigan-Wong",
                                       "Lloyd",
                                       "Forgy",
                                       "MacQueen"),
                           selected = "Hartigan-Wong")
               ),
        column(10, 
               plotOutput("kmeans_out")
        )
    )
  )
 )
)
))
