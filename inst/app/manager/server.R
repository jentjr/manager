# Server File for use with MANAGES Database
library(manager)

# change options to handle large file size
options(shiny.maxRequestSize = -1)
# force numbers to be decimal instead of scientific
options(scipen = 6, digits = 8)

shinyServer(function(input, output, session) {
  
  # Data Entry -----------------------------------------------------------------
  datafile <- callModule(userFile, "datafile")

  output$table <- renderDataTable({
    datafile()
  }, options = list(scrollY = "100%", scrollX = "100%",
                    lengthMenu = c(5, 10, 15, 25, 50, 100),
                    pageLength = 10)
  )
  # End Data Entry -------------------------------------------------------------
  
  # Summary table --------------------------------------------------------------
  # summaryfile <- callModule(wellConstituent, "summary", datafile(), 
  #                             multiple = FALSE)
  # 
  # output$summary_table <- renderPrint({
  #   EnvStats::summaryFull(summaryfile()$analysis_result)
  # })
  # End Summary table ----------------------------------------------------------
  
  # Begin Distribution Plots ---------------------------------------------------
  distfile <- callModule(wellConstituent, "dist", datafile(),
                         multiple = FALSE)
  
  output$gof_test <- renderPrint({
    df <- distfile()

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
    out
  })
  
  output$gof_plot <- renderPlot({
    df <- distfile()

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
  
  # Begin Boxplot Page----------------------------------------------------------
  boxplotfile <- callModule(boxPlot, "boxplot", datafile(),
                         multiple = TRUE)
  
  boxplot <- reactive({
      
      box_data <- boxplotfile()
      box_wells <- unique(box_data$location_id)
      box_analytes <- unique(box_data$param_name)
    
      box_list <- lapply(1:length(box_analytes), function(i) {
        box_name <- paste("box_plot", i, sep = "")
        plotOutput(box_name)
      })
      
      for (i in 1:length(box_analytes)) {
        local({
          box_i <- i
          box_name <- paste("box_plot", box_i, sep = "")
          output[[box_name]] <- renderPlot({
            box <- manager::boxplot(
              box_data[box_data$param_name == 
                         box_analytes[box_i], ],
              x = paste(input$box_x),
              y = paste(input$box_y),
              fill = paste(input$box_group),
              short_name = input$box_short_name,
              coord_flip = input$coord_flip_box
            )
            box
          })
        })
      }
    do.call(tagList, box_list)
  })
  
  boxplot_react <- eventReactive(input$box_submit, {
    boxplot()
  })
  
  output$boxplot_out <- renderUI({
    if (input$box_interactive == TRUE) {
      boxplot()
    } else {
      boxplot_react()
    }
  })
  
  # Begin Boxplot Download Page-------------------------------------------------
  get_box_data <- reactive({
    validate(
      need(input$data_path != "", "")
    )
    box_data <- boxplotfile()
    box_wells <- unique(box_data$location_id)
    box_analytes <- unique(box_data$param_name)
  })
  
  output$box_download <- downloadHandler(
    filename = function() {
      paste("boxplot_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file = file, width = 17, height = 11)
      manager::boxplot(get_box_data(), facet_by = input$box_facet_by, 
                       short_name = input$box_short_name, 
                       coord_flip = input$box_coord_flip)
      dev.off()
    }
  )
  
  # End Boxplot Page------------------------------------------------------------
  
  # Time Series Page -----------------------------------------------------------
  tsplotfile <- callModule(timeSeries, "tsplot", datafile(),
                            multiple = TRUE)
  
  # time series plot output
  ts_plot <- reactive({

    ts_data <- tsplotfile()
    ts_wells <- unique(ts_data$location_id)
    ts_analytes <- unique(ts_data$param_name)
    
    # num_plots <- length(ts_data[[paste(input$ts_group_by)]])  
   
    manager::ts_plot(ts_data, 
                     x = "sample_date",
                     y = "analysis_result",
                     facet_var = "location_id",
                     group_var = "param_name",
                     trend = NULL, 
                     back_date = NULL, 
                     comp_date = NULL, 
                     limit1 = NULL, 
                     limit2 = NULL, 
                     short_name = FALSE, 
                     pnt = 3, 
                     ncol = NULL
                     ) 
    # ts_list <- lapply(1:num_plots, function(i) {
    #   ts_name <- paste("ts_plot", i, sep = "")
    #   plotOutput(ts_name)
    # })
    #   
    # for (i in 1:num_plots) {
    #   local({
    #     ts_i <- i
    #     ts_name <- paste("ts_plot", ts_i, sep = "")
    #     output[[ts_name]] <- renderPlot({
    #         
    #       ts <- manager::ts_plot(
    #         ts_data[ts_data$location_id == ts_well[ts_i], ],
    #         group_var = input$ts_group_by,
    #         facet_var = input$ts_facet_by,
    #         trend = input$ts_trend,
    #         short_name = input$ts_short_name, 
    #         ncol = input$ts_ncol)
    #         
    #         # if (input$ts_date_lines) {
    #         #   b1 <- min(lubridate::ymd(input$ts_back_dates, tz = Sys.timezone()))
    #         #   c1 <- min(lubridate::ymd(input$ts_comp_dates, tz = Sys.timezone()))
    #         #   b2 <- max(lubridate::ymd(input$ts_back_dates, tz = Sys.timezone()))
    #         #   c2 <- max(lubridate::ymd(input$ts_comp_dates, tz = Sys.timezone()))
    #         #   
    #         #   ts <- manager::ts_plot(
    #         #     ts_data[ts_data$location_id == 
    #         #                    ts_well[ts_i], ], 
    #         #     facet_by = "location_id",
    #         #     trend = input$ts_trend,
    #         #     short_name = input$ts_short_name,
    #         #     back_date = c(b1, b2), 
    #         #     comp_date = c(c1, c2),
    #         #     ncol = input$ts_ncol
    #         #   )
    #         # }
    #         ts
    #       })
    #     })
    #   }
    # do.call(tagList, ts_list)
  })
  
  # plot time series when actioButton is clicked
  ts_plot_react <- eventReactive(input$ts_submit, {
    ts_plot()
  })
  
  output$ts_out <- renderUI({
    if (input$ts_interactive == TRUE) {
      ts_plot()
    } else {
      ts_plot_react()
    }
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
  output$wells_piper <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      well_names <- as.character(get_wells(data))
      selectInput("well_piper", "Monitoring Wells", well_names, 
                  multiple = TRUE, selected = well_names[1])
  })
  
  output$date_ranges_piper <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      dateRangeInput("date_range_piper", "Date Range", 
                     start = min(data$sample_date, na.rm = TRUE), 
                     end = max(data$sample_date, na.rm = TRUE))
  })
  
  get_piper_data <- reactive({
    validate(
      need(input$data_path != "", "Please upload a data set")
    )
    
    data <- get_data()
    start <- min(lubridate::ymd(input$date_range_piper, tz = Sys.timezone()))
    end <- max(lubridate::ymd(input$date_range_piper, tz = Sys.timezone()))
    wells <- input$well_piper
    Mg = paste(input$Mg)
    Ca = paste(input$Ca)
    Na = paste(input$Na)
    K = paste(input$K)
    Cl = paste(input$Cl)
    SO4 = paste(input$SO4)
    Alk = paste(input$Alk)
    TDS = paste(input$TDS)

    data_selected <- data %>%
      filter(location_id %in% wells, 
             sample_date >= start &
               sample_date <= end)
    
    ions <- get_major_ions(data_selected, Mg = Mg, Ca = Ca, Na = Na, K = K, 
                           Cl = Cl, SO4 = SO4, Alk = Alk, TDS = TDS)
    
    piper_data <- transform_piper_data(ions, Mg = Mg, Ca = Ca, Na = Na, K = K,
                                       Cl = Cl, SO4 = SO4, Alk = Alk, TDS = TDS)
    piper_data
  })
  
  plot_piper <- reactive({
    piper_plot(df = get_piper_data(), TDS = input$TDS_plot, 
               title = input$piper_title)
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
      filter(location_id %in% input$well_stiff &
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
      wells <- unique(data$location_id)
      
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
              data[data$location_id == wells[stiff_i], ], 
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
      filter(location_id %in% input$well_schoeller &
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
    validate(
      need(input$data_path != "", "")
    )
    data <- datafile()
    well_names <- as.character(get_wells(data))
    selectInput("outlier_well", "Monitoring Well", well_names,
                multiple = FALSE,
                selected = well_names[1])
  })
  
  output$outlier_analytes <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
    data <- datafile()
    analyte_names <- as.character(get_analytes(data))
    selectInput("outlier_analyte", "Constituent", analyte_names, 
                multiple = FALSE,
                selected = analyte_names[1])
  })
  
  output$outlier_date_ranges <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
    data <- datafile()
    tagList(
      dateRangeInput("outlier_date_range", "Date Range", 
                     start = min(data$sample_date, na.rm = TRUE),
                     end = max(data$sample_date, na.rm = TRUE))
    )
  })
  
  get_outlier_data <- reactive({
    validate(
      need(input$data_path != "", "")
    )
    df <- datafile()
    start <- min(lubridate::ymd(input$outlier_date_range, tz = Sys.timezone()),
                 na.rm = TRUE)
    end <- max(lubridate::ymd(input$outlier_date_range, tz = Sys.timezone()), 
               na.rm = TRUE)
    
    data_selected <- df %>%
      filter(location_id %in% input$outlier_well,
             param_name %in% input$outlier_analyte,
             sample_date >= start & 
               df$sample_date <= end)
    
    data_selected
  })
  
  output$outlier_test <- renderPrint({
    validate(
      need(input$data_path != "", "Please upload a data set")
    )
    
    df <- get_outlier_data()
    validate(
      need(length(unique(df$analysis_result)) > 2, "")
    )
    
    if (input$outlier_test_name == "Rosner") {
      out <- EnvStats::rosnerTest(df$analysis_result, 
                                  k = input$rosnerN, 
                                  alpha = input$rosnerAlpha)
    } 
    
    if (input$outlier_test_name == "Grubb") {
      out <- outliers::grubbs.test(df$analysis_result, 
                                   type = input$grubbType,
                                   opposite = as.integer(input$grubbOpposite),
                                   two.sided = as.integer(input$grubbSide))
    } 
    
    if (input$outlier_test_name == "Dixon") {
      out <- outliers::dixon.test(df$analysis_result, 
                                  type = input$dixonType, 
                                  opposite = as.integer(input$dixonOpposite),
                                  two.sided = as.integer(input$dixonSide))
    }
    out
  })
  
  output$outlier_table <- renderDataTable({
    validate(
      need(input$data_path != "", "Please upload a data set")
    )
    data <- get_outlier_data()
    data
  }, options = list(scrollY = "100%", scrollX = "100%", 
                    lengthMenu = c(5, 10, 15, 25, 50, 100), 
                    pageLength = 10)
  )
  
  # End outlier detection ------------------------------------------------------
  
  # Begin trend analysis -------------------------------------------------------
  output$trend_wells <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
    data <- get_data()
    well_names <- as.character(get_wells(data))
    selectInput("trend_well", "Monitoring Well", well_names,
                multiple = FALSE,
                selected = well_names[1])
  })
  
  output$trend_analytes <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
    data <- get_data()
    analyte_names <- as.character(get_analytes(data))
    selectInput("trend_analyte", "Constituent", analyte_names, 
                multiple = FALSE,
                selected = analyte_names[1])
  })
  
  output$trend_date_ranges <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
    data <- get_data()
    tagList(
      dateRangeInput("trend_date_range", "Date Range", 
                     start = min(data$sample_date, na.rm = TRUE),
                     end = max(data$sample_date, na.rm = TRUE))
    )
  })
  
  get_trend_data <- reactive({
    validate(
      need(input$data_path != "", "")
    )
    df <- get_data()
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
    validate(
      need(input$data_path != "", "Please upload a data set")
    )
    
    df <- get_trend_data()
    validate(
      need(length(unique(df$analysis_result)) > 2, "")
    )
    
    out <- EnvStats::kendallTrendTest(analysis_result ~ sample_date, 
                                      data  = df)
    out
  })
  
  # End trend analysis ---------------------------------------------------------
  
  # Begin Intrawell Prediction Limits-------------------------------------------
  output$wells_intra <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      well_names <- as.character(get_wells(data))
      selectInput("well_intra", "Monitoring Wells", well_names, 
                  multiple = TRUE,
                  selected = well_names[1])
  })

  output$analytes_intra <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      analyte_names <- as.character(get_analytes(data))
      selectInput("analyte_intra", "Constituents", analyte_names, 
                  multiple = TRUE,
                  selected = analyte_names[1])
  })

  output$date_ranges_intra <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
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
    validate(
      need(input$data_path != "", "")
    )
    
    df <- get_data()
    
    df <- to_censored(df)
    
    df <- df %>%
      filter(
        location_id %in% input$well_intra,
        param_name %in% input$analyte_intra
      )
    
    bkgd_start <- min(lubridate::ymd(input$back_dates_intra, tz = Sys.timezone()))
    bkgd_end <- max(lubridate::ymd(input$back_dates_intra, tz = Sys.timezone()))
    bkgd <- c(bkgd_start, bkgd_end)
    
    comp_start <- min(lubridate::ymd(input$comp_dates_intra, tz = Sys.timezone()))
    comp_end <- max(lubridate::ymd(input$comp_dates_intra, tz = Sys.timezone()))
    comp <- c(comp_start, comp_end)
  
    validate(
      need(
        length(unique(df$analysis_result)) > 2, 
           "One of the input variables has fewer than 2 unique data points."
        )
      )
    if (isTRUE(input$pred_int_type == "Simultaneous")) {
      out <- intra_pred_int(df, analysis_result, input$well_intra, 
                            input$analyte_intra,
                            bkgd, comp, 
                            k = input$sim_intra_k, m = input$sim_intra_m,
                            r = input$sim_intra_r, 
                            rule = input$sim_intra_rule,
                            pi.type = input$sim_intra_pi.type,
                            SWFPR = input$sim_intra_swfpr)
    }
    if (isTRUE(input$pred_int_type == "Regular")) {
      out <- intra_pred_int(df, analysis_result, input$well_intra, 
                            input$analyte_intra, bkgd, comp,
                            k = input$reg_intra_k,
                            method = input$reg_intra_method,
                            pi.type = input$reg_intra_pi.type,
                            intra.conf.level = input$reg_intra_conf.level,
                            simultaneous = FALSE 
                            )
    }
    out
  })
  
  output$intra_limit_out <- renderDataTable({
    intra_limit()
  }, options = list(scrollY = "100%", scrollX = "100%", 
                    lengthMenu = c(5, 10, 15, 25, 50, 100), 
                    pageLength = 10)
  )
  
  ts_intra_plot <- reactive({
    validate(
      need(input$data_path != "", "Please upload a data set")
    )
    
    ts_intra_well <- input$well_intra
    ts_intra_analyte <- input$analyte_intra
    ts_intra_data <- intra_limit()
    
    if (input$ts_intra_facet_by == "location_id") {
      
      ts_intra_list <- lapply(1:length(ts_intra_well), function(i) {
        ts_intra_name <- paste("ts_intra_plot", i, sep = "")
        plotOutput(ts_intra_name)
      })
      
      for (i in 1:length(ts_intra_well)) {
        local({
          ts_intra_i <- i
          ts_intra_name <- paste("ts_intra_plot", ts_intra_i, sep = "")
          output[[ts_intra_name]] <- renderPlot({
            
            ts <- manager::ts_plot(
              ts_intra_data[ts_intra_data$location_id == 
              ts_intra_well[ts_intra_i], ],
              facet_by = "location_id", 
              short_name = input$ts_intra_short_name, 
              ncol = input$ncol_intra_ts,
              limit1 = "lower_limit",
              limit2 = "upper_limit"
              )
            
            if (input$ts_intra_date_lines) {
              b1 <- min(lubridate::ymd(input$back_dates_intra, tz = Sys.timezone()))
              c1 <- min(lubridate::ymd(input$comp_dates_intra, tz = Sys.timezone()))
              b2 <- max(lubridate::ymd(input$back_dates_intra, tz = Sys.timezone()))
              c2 <- max(lubridate::ymd(input$comp_dates_intra, tz = Sys.timezone()))
              
              ts <- manager::ts_plot(
                ts_intra_data[ts_intra_data$location_id == 
                          ts_intra_well[ts_intra_i], ], 
                facet_by = "location_id",
                short_name = input$ts_intra_short_name,
                back_date = c(b1, b2), 
                comp_date = c(c1, c2),
                ncol = input$ncol_intra_ts,
                limit1 = "lower_limit",
                limit2 = "upper_limit"
              )
            }
            ts
          })
        })
      }
    }
    
    if (input$ts_intra_facet_by == "param_name") {
      ts_intra_list <- lapply(1:length(ts_intra_analyte), function(i) {
        ts_intra_name <- paste("ts_intra_plot", i, sep = "")
        plotOutput(ts_intra_name)
      })
      
      for (i in 1:length(ts_intra_analyte)) {
        local({
          ts_intra_i <- i
          ts_intra_name <- paste("ts_intra_plot", ts_intra_i, sep = "")
          output[[ts_intra_name]] <- renderPlot({
            
            ts <- manager::ts_plot(
              ts_intra_data[ts_intra_data$param_name == 
              ts_intra_analyte[ts_intra_i], ],
              facet_by = "param_name", 
              short_name = input$ts_intra_short_name,
              limit1 = "lower_limit",
              limit2 = "upper_limit",
              ncol = input$ncol_intra_ts
              )
            
            if (input$ts_intra_date_lines) {
              b1 <- min(lubridate::ymd(input$back_dates_intra, tz = Sys.timezone()))
              c1 <- min(lubridate::ymd(input$comp_dates_intra, tz = Sys.timezone()))
              b2 <- max(lubridate::ymd(input$back_dates_intra, tz = Sys.timezone()))
              c2 <- max(lubridate::ymd(input$comp_dates_intra, tz = Sys.timezone()))
              
              ts <- manager::ts_plot(
                ts_intra_data[ts_intra_data$param_name == 
                ts_intra_analyte[ts_intra_i], ], 
                facet_by = "param_name",
                short_name = input$ts_intra_short_name,
                back_date = c(b1, b2), 
                comp_date = c(c1, c2),
                limit1 = "lower_limit",
                limit2 = "upper_limit",
                ncol = input$ncol_intra_ts
              )
            }
            ts
          })
        })
      }
    }
    do.call(tagList, ts_intra_list)
  })
  
  output$ts_intra_out <- renderUI({
    if (input$intra_plot) {
      ts_intra_plot()
    }
  })
  # End Prediction Limits ------------------------------------------------------
})
