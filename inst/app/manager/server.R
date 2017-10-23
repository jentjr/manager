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
  output$select_distribution_wells <- renderUI({

    data <- select_data()
    well_names <- as.character(sample_locations(data))
    selectInput("dist_well", "Monitoring Wells", well_names,
                multiple = FALSE,
                selected = well_names[1])
  })

  output$select_distribution_params <- renderUI({
    data <- select_data()
    analyte_names <- as.character(constituents(data))
    selectInput("dist_param", "Constituents", analyte_names,
                multiple = FALSE,
                selected = analyte_names[1])
  })


  get_distribution_data <- reactive({

    df <- select_data()

    df <- df %>%
      filter(location_id %in% input$dist_well,
             param_name %in% input$dist_param)

    df

  })

  # output$gof_test <- renderPrint({
  #
  #   df <- get_distribution_data()
  #
  #   if (isTRUE(input$dist_plot_type == "Censored")) {
  #     df$CENSORED <- ifelse(df$lt_measure == "<", TRUE, FALSE)
  #     out <- EnvStats::gofTestCensored(
  #       x = df$analysis_result, censored = df$CENSORED,
  #       censoring.side = input$cen_dist_side,
  #       test = input$cen_dist_test,
  #       distribution = input$cen_dist_dist,
  #       prob.method = input$cen_dist_method,
  #       plot.pos.con =  input$cen_dist_plot.pos.con
  #       )
  #     out["data.name"] <- paste(df$location_id,
  #                               df$param_name,
  #                               sep = " ")
  #   } else {
  #     out <- EnvStats::gofTest(
  #       df$analysis_result, distribution = input$dist_type
  #     )
  #     out["data.name"] <- paste(df$location_id,
  #                               df$param_name,
  #                               sep = " ")
  #   }
  #   out
  # })

  output$gof_plot <- renderPlot({

    df <- get_distribution_data()

    if (isTRUE(input$dist_plot_type == "Censored")) {

      df$censored <- ifelse(df$lt_measure == "<", TRUE, FALSE)

      out <- EnvStats::gofTestCensored(

        x = df$analysis_result, censored = df$censored,
        censoring.side = input$cen_dist_side,
        test = input$cen_dist_test,
        distribution = input$cen_dist_dist,
        prob.method = input$cen_dist_method,
        plot.pos.con =  input$cen_dist_plot.pos.con
      )
      out["data.name"] <- paste(df$location_id,
                                df$param_name,
                                sep = " ")
    } else {

      out <- EnvStats::gofTest(
        df$analysis_result, distribution = input$dist_type
      )
      out["data.name"] <- paste(df$location_id,
                                df$param_name,
                                sep = " ")
    }
    plot(out)
  })
  # End Distribution Plots -----------------------------------------------------

  # Begin Correlation Plots ----------------------------------------------------
  output$select_corr_wells <- renderUI({

    data <- select_data()

    well_names <- as.character(sample_locations(data))

    selectInput("corr_wells", "Monitoring Wells", well_names,
                multiple = TRUE, selected = well_names[1])
  })

  output$select_corr_params <- renderUI({

    data <- select_data()

    param_names <- as.character(constituents(data))

    selectInput("corr_params", "Constituents", param_names,
                multiple = TRUE, selected = param_names[1])
  })

  output$corr_plot <- renderPlot({

    df <- select_data()

    df %>%
      corr_plot(., sample_locations = c(input$corr_wells),
                constituents = c(input$corr_params))

  })

  # End Correlation Plots ------------------------------------------------------

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
              box_data[box_data$param_name ==
                         box_params[box_i], ],
              x = "location_id",
              y = "analysis_result",
              fill = "location_class",
              scale_y_trans = input$box_y_transform,
              coef = input$box_iqr_mult,
              show_points = input$box_points,
              pnt = input$box_pnt_size
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
      df <- get_box_data() %>%
        boxplot(., x = "location_id",
                y = "analysis_result",
                fill = "location_class",
                scale_y_trans = input$box_y_transform,
                coef = input$box_iqr_mult,
                show_points = input$box_points,
                pnt = input$box_pnt_size)
      dev.off()
    }
  )

  # End Boxplot Page------------------------------------------------------------

  # Time Series Page -----------------------------------------------------------

  ts_plot <- reactive({

    ts_data <- select_data()
    ts_wells <- sample_locations(ts_data)
    ts_params <- constituents(ts_data)

    # Need to inlcude group_var option, using param_name for now

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
            ts_data[ts_data$param_name == ts_params[ts_i], ]
            )

            # if (input$ts_date_lines) {
            #   b1 <- min(lubridate::ymd(input$ts_back_dates, tz = Sys.timezone()))
            #   c1 <- min(lubridate::ymd(input$ts_comp_dates, tz = Sys.timezone()))
            #   b2 <- max(lubridate::ymd(input$ts_back_dates, tz = Sys.timezone()))
            #   c2 <- max(lubridate::ymd(input$ts_comp_dates, tz = Sys.timezone()))
            # 
            #   ts <- manager::ts_plot(
            #     ts_data[ts_data$location_id ==
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
    ts_data <- data[data$location_id %in% ts_well &
                           data$param_name %in% ts_analyte, ]
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

  output$select_piper_tds <- renderUI({

    if (isTRUE(input$TDS_plot)) {

      selectInput(inputId = "piper_tds",
                  label = "Total Dissolved Solids", 
                  choices = c("Total Dissolved Solids"))

    }

  })

  plot_piper <- reactive({

    data <- select_data()

    if (isTRUE(input$TDS_plot)) {
     
     piper_plot(data,
                x_cation = paste(input$x_cation),
                y_cation = paste(input$y_cation),
                z_cation = paste(input$z_cation),
                x_anion = paste(input$x_anion),
                y_anion = paste(input$y_anion),
                z_anion = paste(input$z_anion),
                total_dissolved_solids = paste(input$piper_tds),
                title = input$piper_title
                )

     } else { 
     
     piper_plot(data,
                x_cation = paste(input$x_cation),
                y_cation = paste(input$y_cation),
                z_cation = paste(input$z_cation),
                x_anion = paste(input$x_anion),
                y_anion = paste(input$y_anion),
                z_anion = paste(input$z_anion),
                title = input$piper_title
                )
     }
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

  stiff_diagram <- reactive({
    
    data <- select_data()
    
    data %>%
      stiff_plot(., magnesium = paste(input$Mg_stiff), 
                calcium = paste(input$Ca_stiff), 
                sodium = paste(input$Na_stiff), 
                potassium = paste(input$K_stiff), 
                chloride = paste(input$Cl_stiff), 
                sulfate = paste(input$SO4_stiff), 
                alkalinity = paste(input$Alk_stiff), 
                group_var = "location_id",
                facet_var = "sample_date"
                )

  })

  output$stiff_diagram <- renderPlot({

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
  output$select_schoeller_wells <- renderUI({
    
    data <- select_data()
    well_names <- as.character(sample_locations(data))
    selectInput("well_schoeller", "Monitoring Wells", well_names, 
                multiple = TRUE, selected = well_names[1])
  })
  
  output$select_schoeller_dates <- renderUI({
    
    data <- select_data()
    dateRangeInput("date_range_schoeller", "Date Range", 
                   start = min(data$sample_date, na.rm = TRUE), 
                   end = max(data$sample_date, na.rm = TRUE))
  })
  
  get_schoeller_data <- reactive({
    
    data <- select_data()
    
    start <- min(lubridate::ymd(input$date_range_schoeller, tz = Sys.timezone()))
    end <- max(lubridate::ymd(input$date_range_schoeller, tz = Sys.timezone()))
    
    data_selected <- data %>%
      filter(location_id %in% input$well_schoeller &
             sample_date >= start & 
             sample_date <= end)
    
    data_selected

  })
  
  schoeller_diagram <- reactive({
    
    data <- get_schoeller_data()
    
    data %>%
      schoeller_plot(magnesium = paste(input$Mg_schoeller),
                     calcium = paste(input$Ca_schoeller),
                     sodium = paste(input$Na_schoeller),
                     potassium = paste(input$K_schoeller),
                     chloride = paste(input$Cl_schoeller),
                     sulfate = paste(input$SO4_schoeller),
                     alkalinity = paste(input$Alk_schoeller), 
                     facet_by = input$facet_schoeller,
                     title = input$schoeller_title)
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
                     start = min(data$sample_date, na.rm = TRUE),
                     end = max(data$sample_date, na.rm = TRUE))
      
    )
    
  })
  
  get_outlier_data <- reactive({
    
    df <- select_data()
    
    start <- min(lubridate::ymd(input$outlier_date_range, tz = Sys.timezone()),
                 na.rm = TRUE)
    
    end <- max(lubridate::ymd(input$outlier_date_range, tz = Sys.timezone()), 
               na.rm = TRUE)
    
    data_selected <- df %>%
      filter(location_id %in% input$outlier_well,
             param_name %in% input$outlier_analyte,
             sample_date >= start & 
               sample_date <= end)
    
    data_selected
    
  })
  
  output$outlier_test <- renderPrint({
    
    df <- get_outlier_data()
    
    validate(
      need(length(unique(df$analysis_result)) > 2, "")
    )
    
    if (input$outlier_test_name == "Rosner") {
      
      out <- EnvStats::rosnerTest(df$analysis_result, 
                k = input$rosnerN, 
                alpha = input$rosnerAlpha
             )
    } 
    
    if (input$outlier_test_name == "Grubb") {
      
      out <- outliers::grubbs.test(df$analysis_result, 
                type = input$grubbType,
                opposite = as.integer(input$grubbOpposite),
                two.sided = as.integer(input$grubbSide)
             )
    } 
    
    if (input$outlier_test_name == "Dixon") {
      
      out <- outliers::dixon.test(df$analysis_result, 
                type = input$dixonType, 
                opposite = as.integer(input$dixonOpposite),
                two.sided = as.integer(input$dixonSide)
             )
    }
    
    out
    
  })
  # 
  # output$outlier_table <- renderDataTable({
  #   
  #   data <- get_outlier_data()
  #   
  #   data
  #   
  # }, options = list(scrollY = "100%", scrollX = "100%", 
  #                   lengthMenu = c(5, 10, 15, 25, 50, 100), 
  #                   pageLength = 10)
  # )
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
                     start = min(data$sample_date, na.rm = TRUE),
                     end = max(data$sample_date, na.rm = TRUE))
    )
  })
  
  get_trend_data <- reactive({

    df <- select_data()
    
    start <- min(lubridate::ymd(input$trend_date_range, tz = Sys.timezone()), 
                 na.rm = TRUE)
    
    end <- max(lubridate::ymd(input$trend_date_range, tz = Sys.timezone()), 
               na.rm = TRUE)
    
    data_selected <- df %>%
      filter(location_id %in% input$trend_well,
             param_name %in% input$trend_analyte,
             sample_date >= start & 
               sample_date <= end)
    
    data_selected
    
  })
  
  output$trend_test <- renderPrint({
    
    df <- get_trend_data()
    
    validate(
      need(length(unique(df$analysis_result)) > 2, "")
    )
    
    out <- EnvStats::kendallTrendTest(analysis_result ~ sample_date, data  = df)
    
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
                       start = min(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE)),
        dateRangeInput("comp_dates_intra", "Compliance Date Range", 
                       start = min(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE))
      )
  })

  intra_limit <- reactive({
    df <- select_data()
    df <- df %>%
      filter(location_id %in% input$well_intra,
             param_name %in% input$analyte_intra)

    bkgd_start <- min(lubridate::ymd(input$back_dates_intra, tz = Sys.timezone()))
    bkgd_end <- max(lubridate::ymd(input$back_dates_intra, tz = Sys.timezone()))
    bkgd <- c(bkgd_start, bkgd_end)

    comp_start <- min(lubridate::ymd(input$comp_dates_intra, tz = Sys.timezone()))
    comp_end <- max(lubridate::ymd(input$comp_dates_intra, tz = Sys.timezone()))
    comp <- c(comp_start, comp_end)

    out <- EnvStats::predIntNormSimultaneous(
              df$analysis_result,
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

  # Begin Inter-well prediction intervals --------------------------------------
  output$select_wells_inter <- renderUI({
    
    data <- select_data()
    well_names <- as.character(sample_locations(data))
    selectInput("wells_inter", "Monitoring Wells", well_names, 
                multiple = TRUE,
                selected = well_names[1])
  })
  
  output$select_analyte_inter <- renderUI({
    data <- select_data()
    analyte_names <- as.character(constituents(data))
    selectInput("analyte_inter", "Constituents", analyte_names, 
                multiple = FALSE,
                selected = analyte_names[1])
  })
  
  output$select_date_ranges_inter <- renderUI({
    data <- select_data()
    tagList(
      dateRangeInput("background_inter", "Background Date Range",
                     start = min(data$sample_date, na.rm = TRUE),
                     end = max(data$sample_date, na.rm = TRUE)),
      dateRangeInput("compliance_inter", "Compliance Date Range",
                     start = min(data$sample_date, na.rm = TRUE),
                     end = max(data$sample_date, na.rm = TRUE))
    )
  })

  inter_limit <- reactive({

    df <- select_data()

    df <- df %>%
      filter(location_id %in% input$wells_inter,
             param_name %in% input$analyte_inter)

    bkgd_start <- min(lubridate::ymd(input$background_inter,
                                     tz = Sys.timezone()))
    bkgd_end <- max(lubridate::ymd(input$background_inter,
                                   tz = Sys.timezone()))
    bkgd <- c(bkgd_start, bkgd_end)
    
    comp_start <- min(lubridate::ymd(input$compliance_inter,
                                     tz = Sys.timezone()))
    comp_end <- max(lubridate::ymd(input$compliance_inter,
                                   tz = Sys.timezone()))
    comp <- c(comp_start, comp_end)
    
    out <- EnvStats::predIntNormSimultaneous(
      df$analysis_result
    )
    out
  })

  output$inter_limit_out <- renderPrint({
    inter_limit()
  })
  # End Inter-well Prediction Limits -------------------------------------------
  # End Prediction Limits ------------------------------------------------------
})