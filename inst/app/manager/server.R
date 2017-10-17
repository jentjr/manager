shinyServer(function(input, output, session) {
  
  # Data Table -----------------------------------------------------------------
  select_data <- callModule(selectData, "select_data", multiple = TRUE)
  
  output$data_table <- renderDataTable({
    
    data <- select_data()
    
    data
    
  }, options = list(scrollY = "100%", scrollX = "100%",
                    lengthMenu = c(5, 10, 15, 25, 50, 100),
                    pageLength = 10)
  )
  
  output$data_table_download <- downloadHandler(
    
    filename = function() {
      
      paste("MANAGER_EXPORT_", Sys.Date(), "_.csv", sep = "")
      
    },
    
    content = function(file) {
      
      write.csv(select_data(), file, row.names = FALSE)
      
    }
    
  )
  # End Data Table -------------------------------------------------------------
  
  # Summary table --------------------------------------------------------------
  output$summary_table <- renderDataTable({

    manager::summary(select_data())

  }, options = list(scrollY = "100%", scrollX = "100%",
                    lengthMenu = c(5, 10, 15, 25, 50, 100),
                    pageLength = 10)
  )
  
  output$summary_table_download <- downloadHandler(
    
    filename = function() {
      
      paste("MANAGER_EXPORT_", Sys.Date(), "_.csv", sep = "")
      
    },
    
    content = function(file) {
      
      write.csv(summary(summary_data()), file, row.names = FALSE)
      
    }
    
  )
  # End Summary table ----------------------------------------------------------
  
  # Begin Distribution Plots ---------------------------------------------------
  output$gof_test <- renderPrint({
    
    df <- select_data()

    if (isTRUE(input$dist_plot_type == "Censored")) {
      df$CENSORED <- ifelse(df$LT_MEASURE == "<", TRUE, FALSE)
      out <- EnvStats::gofTestCensored(
        x = df$ANALYSIS_RESULT, censored = df$CENSORED,
        censoring.side = input$cen_dist_side,
        test = input$cen_dist_test,
        distribution = input$cen_dist_dist,
        prob.method = input$cen_dist_method,
        plot.pos.con =  input$cen_dist_plot.pos.con
        )
      out["data.name"] <- paste(df$LOCATION_ID,
                                df$PARAM_NAME,
                                sep = " ")
    } else {
      out <- EnvStats::gofTest(
        df$ANALYSIS_RESULT, distribution = input$dist_type
      )
      out["data.name"] <- paste(df$LOCATION_ID,
                                df$PARAM_NAME,
                                sep = " ")
    }
    out
  })

  output$gof_plot <- renderPlot({
    
    df <- select_data()
    
    if (isTRUE(input$dist_plot_type == "Censored")) {
      
      df$CENSORED <- ifelse(df$LT_MEASURE == "<", TRUE, FALSE)
      
      out <- EnvStats::gofTestCensored(
        
        x = df$ANALYSIS_RESULT, censored = df$CENSORED,
        censoring.side = input$cen_dist_side,
        test = input$cen_dist_test,
        distribution = input$cen_dist_dist,
        prob.method = input$cen_dist_method,
        plot.pos.con =  input$cen_dist_plot.pos.con
      )
      out["data.name"] <- paste(df$LOCATION_ID,
                                df$PARAM_NAME,
                                sep = " ")
    } else {
      
      out <- EnvStats::gofTest(
        df$ANALYSIS_RESULT, distribution = input$dist_type
      )
      out["data.name"] <- paste(df$LOCATION_ID,
                                df$PARAM_NAME,
                                sep = " ")
    }
    plot(out)
  })
  # End Distribution Plots -----------------------------------------------------
   
  # Begin Boxplot Page----------------------------------------------------------
  
  boxplot <- reactive({

      box_data <- select_data()
      box_wells <- sample_locations(box_data)
      box_params <- constituents(box_data)

      box_list <- lapply(seq_along(box_params), function(i) {
        box_name <- paste("box_plot", i, sep = "")
        plotOutput(box_name)
      })

      for (i in seq_along(box_params)) {
        local({
          box_i <- i
          box_name <- paste("box_plot", box_i, sep = "")
          output[[box_name]] <- renderPlot({
            box <- manager::boxplot(
              box_data[box_data$PARAM_NAME ==
                         box_params[box_i], ],
              x = "LOCATION_ID",
              y = "ANALYSIS_RESULT",
              fill = "LOCATION_CLASS",
              scale_y_trans = input$box_y_transform
            )
            box
          })
        })
      }
    do.call(tagList, box_list)
  })

  output$boxplot_out <- renderUI({
      boxplot()
  })

  # Begin Boxplot Download Page-------------------------------------------------
  get_box_data <- reactive({

    box_data <- select_data()
    box_wells <- sample_locations(box_data)
    box_params <- constituents(box_data)

  })

  output$box_download <- downloadHandler(
    filename = function() {
      paste("boxplot_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file = file, width = 17, height = 11)
      manager::boxplot(get_box_data())
      dev.off()
    }
  )

  # End Boxplot Page------------------------------------------------------------

  # Time Series Page -----------------------------------------------------------

  ts_plot <- reactive({

    ts_data <- select_data()
    ts_wells <- sample_locations(ts_data)
    ts_params <- constituents(ts_data)

    # Need to inlcude group_var option 
    # Using PARAM_NAME for now
    
    ts_list <- lapply(seq_along(ts_params), function(i) {
      ts_name <- paste("ts_plot", i, sep = "")
      plotOutput(ts_name)
    })

    for (i in seq_along(ts_params)) {
      local({
        ts_i <- i
        ts_name <- paste("ts_plot", ts_i, sep = "")
        output[[ts_name]] <- renderPlot({

          ts <- manager::ts_plot(
            ts_data[ts_data$PARAM_NAME == ts_params[ts_i], ]
            )

            # if (input$ts_date_lines) {
            #   b1 <- min(lubridate::ymd(input$ts_back_dates, tz = Sys.timezone()))
            #   c1 <- min(lubridate::ymd(input$ts_comp_dates, tz = Sys.timezone()))
            #   b2 <- max(lubridate::ymd(input$ts_back_dates, tz = Sys.timezone()))
            #   c2 <- max(lubridate::ymd(input$ts_comp_dates, tz = Sys.timezone()))
            # 
            #   ts <- manager::ts_plot(
            #     ts_data[ts_data$LOCATION_ID ==
            #                    ts_well[ts_i], ],
            #     back_date = c(b1, b2),
            #     comp_date = c(c1, c2)
            #   )
            # }
            ts
          })
        })
      }
    do.call(tagList, ts_list)
  })

  output$ts_out <- renderUI({
      ts_plot()
  })
  # Begin Time Series Download Page --------------------------------------------
  get_ts_data <- reactive({
    validate(
      need(input$data_path != "", "Please upload a data set")
    )
    data <- get_data()
    ts_well <- input$ts_well
    ts_analyte <- input$ts_analyte
    ts_data <- data[data$LOCATION_ID %in% ts_well &
                           data$PARAM_NAME %in% ts_analyte, ]
    ts_data
  })

  output$ts_download <- downloadHandler(
    filename = function() {
      paste("ts_plot_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if (input$ts_date_lines) {
        b1 <- min(lubridate::ymd(input$ts_back_dates, tz = Sys.timezone()))
        c1 <- min(lubridate::ymd(input$ts_comp_dates, tz = Sys.timezone()))
        b2 <- max(lubridate::ymd(input$ts_back_dates, tz = Sys.timezone()))
        c2 <- max(lubridate::ymd(input$ts_comp_dates, tz = Sys.timezone()))

        pdf(file = file, width = 17, height = 11)
        manager::ts_plot(get_ts_data(), back_date = c(b1, b2),
                         facet_by = input$ts_facet_by,
                         trend = input$ts_trend,
                         short_name = input$ts_short_name,
                         comp_date = c(c1, c2), ncol = input$ts_ncol)
        dev.off()
      } else {
        pdf(file = file, width = 17, height = 11)
        manager::ts_plot(get_ts_data(), facet_by = input$ts_facet_by,
                         short_name = input$ts_short_name,
                         trend = input$ts_trend,
                         ncol = input$ts_ncol)
        dev.off()
      }
    }
  )
  # End Time Series Download Page ----------------------------------------------
  # End Time Series Page--------------------------------------------------------
  
  # Begin Piper Diagram Page----------------------------------------------------

  plot_piper <- reactive({
    
    data <- select_data()

    piper_plot(data,
               x_cation = paste(input$x_cation),
               y_cation = paste(input$y_cation),
               z_cation = paste(input$z_cation),
               x_anion = paste(input$x_anion),
               y_anion = paste(input$y_anion),
               z_anion = paste(input$z_anion)
               # TDS = paste(input$TDS)
               )
    
  })
  
  output$piper_plot <- renderPlot({
    plot_piper()
  })
  
  output$piper_download <- downloadHandler(
    filename = function() {
      paste("piper_plot_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file = file, width = 17, height = 11)
      print(piper_plot(df = get_piper_data(), TDS = input$TDS_plot,
                       title = input$piper_title))
      dev.off()
    }
  )
  # End Piper Diagram Page------------------------------------------------------

  # Begin Stiff Diagram Page ---------------------------------------------------
  output$wells_stiff <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      well_names <- as.character(get_wells(data))
      selectInput("well_stiff", "Monitoring Wells", well_names, 
                  multiple = TRUE, selected = well_names[1])
  })
  
  output$date_ranges_stiff <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      dateRangeInput("date_range_stiff", "Date Range", 
                     start = min(data$sample_date, na.rm = TRUE), 
                     end = max(data$sample_date, na.rm = TRUE))
  })

  get_stiff_data <- reactive({
    validate(
      need(input$data_path != "", "Please upload a data set")
    )
    
    data <- get_data()
    start <- min(lubridate::ymd(input$date_range_stiff, tz = Sys.timezone()))
    end <- max(lubridate::ymd(input$date_range_stiff, tz = Sys.timezone()))
    
    data_selected <- data %>%
      filter(LOCATION_ID %in% input$well_stiff &
             sample_date >= start & 
             sample_date <= end)
    
    Mg = paste(input$Mg_stiff)
    Ca = paste(input$Ca_stiff)
    Na = paste(input$Na_stiff)
    K = paste(input$K_stiff)
    Cl = paste(input$Cl_stiff)
    SO4 = paste(input$SO4_stiff)
    Alk = paste(input$Alk_stiff)
    TDS = paste(input$TDS_stiff)
    
    ions <- get_major_ions(data_selected, Mg = Mg, Ca = Ca, Na = Na, 
                           K = K, Cl = Cl, SO4 = SO4, Alk = Alk, 
                           TDS = TDS)
    
    ions <- ions[complete.cases(ions), ]
    
    plot_data <- conc_to_meq(ions, Mg = Mg, Ca = Ca, Na = Na, 
                             K = K, Cl = Cl, SO4 = SO4, total_alk = Alk)
    
    stiff_data <- transform_stiff_data(plot_data, Mg = Mg, Ca = Ca, Na = Na, 
                                       K = K, Cl = Cl, SO4 = SO4, Alk = Alk, 
                                       TDS = TDS)
    
    stiff_data
  })
  
  stiff_diagram <- reactive({
      validate(
        need(input$data_path != "", "")
      )
      data <- get_stiff_data()
      wells <- unique(data$LOCATION_ID)
      
      stiff_list <- lapply(1:length(wells), function(i) {
        name_stiff <- paste("stiff_plot", i, sep = "")
        plotOutput(name_stiff)
      })
      
      for (i in 1:length(wells)) {
        local({
          stiff_i <- i
          name_stiff <- paste("stiff_plot", stiff_i, sep = "")
          output[[name_stiff]] <- renderPlot({
            stiff_plot(
              data[data$LOCATION_ID == wells[stiff_i], ], 
              TDS = input$TDS_plot_stiff,
              lines = input$stiff_lines
            )
          })
        })
      }
      do.call(tagList, stiff_list)
  })
  
  output$stiff_diagram_out <- renderUI({
    stiff_diagram()
  })
  
  output$stiff_download <- downloadHandler(
    filename = function() {
      paste("stiff_plot_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file = file, width = 17, height = 11)
      stiff_by_loc(df = get_stiff_data(), TDS = input$TDS_plot_stiff,
            lines = input$stiff_lines)
      dev.off()
    }
  )
  # End Stiff Diagram Page------------------------------------------------------
  
  # Begin Schoeller Diagram Page------------------------------------------------
  output$wells_schoeller <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
    data <- get_data()
    well_names <- as.character(get_wells(data))
    selectInput("well_schoeller", "Monitoring Wells", well_names, 
                multiple = TRUE, selected = well_names[1])
  })
  
  output$date_ranges_schoeller <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
    data <- get_data()
    dateRangeInput("date_range_schoeller", "Date Range", 
                   start = min(data$sample_date, na.rm = TRUE), 
                   end = max(data$sample_date, na.rm = TRUE))
  })
  
  get_schoeller_data <- reactive({
    validate(
      need(input$data_path != "", "Please upload a data set")
    )
    data <- get_data()
    start <- min(lubridate::ymd(input$date_range_schoeller, tz = Sys.timezone()))
    end <- max(lubridate::ymd(input$date_range_schoeller, tz = Sys.timezone()))
    
    data_selected <- data %>%
      filter(LOCATION_ID %in% input$well_schoeller &
             sample_date >= start & 
             sample_date <= end)
    
    Mg = paste(input$Mg_schoeller)
    Ca = paste(input$Ca_schoeller)
    Na = paste(input$Na_schoeller)
    K = paste(input$K_schoeller)
    Cl = paste(input$Cl_schoeller)
    SO4 = paste(input$SO4_schoeller)
    Alk = paste(input$Alk_schoeller)
    
    ions <- get_major_ions(data_selected, Mg = Mg, Ca = Ca, Na = Na, 
                           K = K, Cl = Cl, SO4 = SO4, Alk = Alk)
    
    ions <- ions[complete.cases(ions), ]
    
    plot_data <- conc_to_meq(ions, Mg = Mg, Ca = Ca, Na = Na, K = K, 
                             Cl = Cl, SO4 = SO4, HCO3 = Alk)
    
    schoeller_data <- transform_schoeller(plot_data, Mg = Mg, 
                                       Ca = Ca, 
                                       Na = Na, 
                                       K = K, 
                                       Cl = Cl,
                                       SO4 = SO4, 
                                       HCO3 = Alk)
    schoeller_data
  })
  
  schoeller_diagram <- reactive({
    validate(
      need(input$data_path != "", "")
    )
    
    data <- get_schoeller_data()
    
    schoeller(data, facet_by = input$facet_schoeller)
  })
  
  output$schoeller_diagram_out <- renderPlot({
    schoeller_diagram()
  })
  
  output$schoeller_download <- downloadHandler(
    filename = function() {
      paste("schoeller_plot_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file = file, width = 17, height = 11)
      schoeller_diagram()
      dev.off()
    }
  )
  # End Schoeller Diagram Page--------------------------------------------------
  
  # Begin outlier detecion -----------------------------------------------------
  output$outlier_wells <- renderUI({
    
    data <- select_data()
    
    well_names <- as.character(sample_locations(data))
    
    selectInput("outlier_well", "Monitoring Well", well_names,
                multiple = FALSE,
                selected = well_names[1])
    
  })
  
  output$outlier_analytes <- renderUI({
    
    data <- select_data()
    
    analyte_names <- as.character(constituents(data))
    
    selectInput("outlier_analyte", "Constituent", analyte_names, 
                multiple = FALSE,
                selected = analyte_names[1])
    
  })
  
  output$outlier_date_ranges <- renderUI({
    
    data <- select_data()
    
    tagList(
      
      dateRangeInput("outlier_date_range", "Date Range", 
                     start = min(data$SAMPLE_DATE, na.rm = TRUE),
                     end = max(data$SAMPLE_DATE, na.rm = TRUE))
      
    )
    
  })
  
  get_outlier_data <- reactive({
    
    df <- select_data()
    
    start <- min(lubridate::ymd(input$outlier_date_range, tz = Sys.timezone()),
                 na.rm = TRUE)
    
    end <- max(lubridate::ymd(input$outlier_date_range, tz = Sys.timezone()), 
               na.rm = TRUE)
    
    data_selected <- df %>%
      filter(LOCATION_ID %in% input$outlier_well,
             PARAM_NAME %in% input$outlier_analyte,
             SAMPLE_DATE >= start & 
               SAMPLE_DATE <= end)
    
    data_selected
    
  })
  
  output$outlier_test <- renderPrint({
    
    df <- get_outlier_data()
    
    validate(
      need(length(unique(df$ANALYSIS_RESULT)) > 2, "")
    )
    
    if (input$outlier_test_name == "Rosner") {
      
      out <- EnvStats::rosnerTest(df$ANALYSIS_RESULT, 
                k = input$rosnerN, 
                alpha = input$rosnerAlpha
             )
    } 
    
    if (input$outlier_test_name == "Grubb") {
      
      out <- outliers::grubbs.test(df$ANALYSIS_RESULT, 
                type = input$grubbType,
                opposite = as.integer(input$grubbOpposite),
                two.sided = as.integer(input$grubbSide)
             )
    } 
    
    if (input$outlier_test_name == "Dixon") {
      
      out <- outliers::dixon.test(df$ANALYSIS_RESULT, 
                type = input$dixonType, 
                opposite = as.integer(input$dixonOpposite),
                two.sided = as.integer(input$dixonSide)
             )
    }
    
    out
    
  })
  
  output$outlier_table <- renderDataTable({
    
    data <- get_outlier_data()
    
    data
    
  }, options = list(scrollY = "100%", scrollX = "100%", 
                    lengthMenu = c(5, 10, 15, 25, 50, 100), 
                    pageLength = 10)
  )
  # End outlier detection ------------------------------------------------------
  
  # Begin trend analysis -------------------------------------------------------
  output$trend_wells <- renderUI({

    data <- select_data()
    well_names <- as.character(sample_locations(data))
    selectInput("trend_well", "Monitoring Well", well_names,
                multiple = FALSE,
                selected = well_names[1])
  })
  
  output$trend_analytes <- renderUI({
    
    data <- select_data()
    analyte_names <- as.character(constituents(data))
    selectInput("trend_analyte", "Constituent", analyte_names, 
                multiple = FALSE,
                selected = analyte_names[1])
  })
  
  output$trend_date_ranges <- renderUI({
    
    data <- select_data()
    
    tagList(
      
      dateRangeInput("trend_date_range", "Date Range", 
                     start = min(data$SAMPLE_DATE, na.rm = TRUE),
                     end = max(data$SAMPLE_DATE, na.rm = TRUE))
    )
  })
  
  get_trend_data <- reactive({

    df <- select_data()
    
    start <- min(lubridate::ymd(input$trend_date_range, tz = Sys.timezone()), 
                 na.rm = TRUE)
    
    end <- max(lubridate::ymd(input$trend_date_range, tz = Sys.timezone()), 
               na.rm = TRUE)
    
    data_selected <- df %>%
      filter(LOCATION_ID %in% input$trend_well,
             PARAM_NAME %in% input$trend_analyte,
             SAMPLE_DATE >= start & 
               SAMPLE_DATE <= end)
    
    data_selected
    
  })
  
  output$trend_test <- renderPrint({
    
    df <- get_trend_data()
    
    validate(
      need(length(unique(df$ANALYSIS_RESULT)) > 2, "")
    )
    
    out <- EnvStats::kendallTrendTest(ANALYSIS_RESULT ~ SAMPLE_DATE, data  = df)
    
    out
    
  })
  
  # End trend analysis ---------------------------------------------------------
  
  # Begin Intrawell Prediction Limits-------------------------------------------
  output$wells_intra <- renderUI({

      data <- select_data()
      well_names <- as.character(sample_locations(data))
      selectInput("well_intra", "Monitoring Wells", well_names, 
                  multiple = FALSE,
                  selected = well_names[1])
  })

  output$analytes_intra <- renderUI({
      data <- select_data()
      analyte_names <- as.character(constituents(data))
      selectInput("analyte_intra", "Constituents", analyte_names, 
                  multiple = FALSE,
                  selected = analyte_names[1])
  })

  output$date_ranges_intra <- renderUI({
      data <- select_data()
      tagList(
        dateRangeInput("back_dates_intra", "Background Date Range", 
                       start = min(data$SAMPLE_DATE, na.rm = TRUE),
                       end = max(data$SAMPLE_DATE, na.rm = TRUE)),
        dateRangeInput("comp_dates_intra", "Compliance Date Range", 
                       start = min(data$SAMPLE_DATE, na.rm = TRUE),
                       end = max(data$SAMPLE_DATE, na.rm = TRUE))
      )
  })

  intra_limit <- reactive({
    df <- select_data()
    df <- df %>% 
      filter(LOCATION_ID %in% input$well_intra,
             PARAM_NAME %in% input$analyte_intra)

    bkgd_start <- min(lubridate::ymd(input$back_dates_intra, tz = Sys.timezone()))
    bkgd_end <- max(lubridate::ymd(input$back_dates_intra, tz = Sys.timezone()))
    bkgd <- c(bkgd_start, bkgd_end)
    
    comp_start <- min(lubridate::ymd(input$comp_dates_intra, tz = Sys.timezone()))
    comp_end <- max(lubridate::ymd(input$comp_dates_intra, tz = Sys.timezone()))
    comp <- c(comp_start, comp_end)
  

    out <- EnvStats::predIntNormSimultaneous(
              df$ANALYSIS_RESULT,
              k = input$sim_intra_k,
              m = input$sim_intra_m,
              r = input$sim_intra_r, 
              rule = input$sim_intra_rule,
              pi.type = input$sim_intra_pi.type,
              conf.level = input$sim_intra_swfpr
          )
    out
  })
  
  output$intra_limit_out <- renderPrint({
    intra_limit()
  })
  
  # End Prediction Limits ------------------------------------------------------
})
