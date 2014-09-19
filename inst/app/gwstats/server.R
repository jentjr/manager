library(gwstats)
library(EnvStats)
# change options to handle large file size
options(shiny.maxRequestSize=-1)
# force numbers to be decimal instead of scientific
options(scipen=6, digits = 8)

shinyServer(function(input, output, session) {
  get_data <- reactive({
    validate(
      need(input$data_path != "", "Please upload a data set")
      )
      switch(input$file_type, 
             "csv" = from_csv(path = input$data_path$datapath, 
                               date_format = input$csv_date_format),
             "MANAGES database" = connect_manages(input$data_path$datapath),
             "excel" = from_excel(path = input$data_path$datapath, 
                                 sheet = input$excel_sheet)) %>%
        arrange(location_id, param_name, sample_date)    
  })
  
  output$well_table <- renderDataTable({
    validate(
      need(input$data_path != "", "Please upload a data set. The data should be in the following form: \n\n 

location_id | sample_date | param_name | lt_measure | analysis_result | default_unit  
-------------- | ---------------- | ----------------- | -------------- | ------------------ | --------------
MW-1         | 2008-01-01  | Arsenic, diss  |      <            |     0.01             |       ug/L
-------------- | ---------------- | ----------------- | -------------- | ------------------ | --------------
MW-1         | 2008-01-01  | Boron, diss     |                   |     0.24             |       mg/L ")
    
    )
      data <- get_data() 
      return(data)
  }, options = list(scrollY = "100%", scrollX = "100%", 
                    lengthMenu = c(5, 10, 15, 25, 50, 100), 
                    pageLength = 10)
  ) 
  # Begin Summary Table page ---------------------------------------------------  
  output$summary_date_ranges <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
    data <- get_data()
    tagList(
      dateRangeInput("background_date_range", "Background Date Range", 
                      start = min(data$sample_date, na.rm = TRUE),
                      end = max(data$sample_date, na.rm = TRUE)) 
    )
  })
  output$summary_table <- renderDataTable({
    validate(
      need(input$data_path != "", "Please upload a data set")
    )
    data <- get_data()
    start <- min(lubridate::ymd(input$background_date_range))
    end <- max(lubridate::ymd(input$background_date_range))
    gw_summary(data, start, end)
  }, options = list(scrollY = "100%", scrollX = "100%", 
                    lengthMenu = c(5, 10, 15, 25, 50, 100), 
                    pageLength = 10)
  )
  # End Summary Table page -----------------------------------------------------

  # Begin Distribution Plots ---------------------------------------------------
  output$dist_wells <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
    data <- get_data()
    well_names <- as.character(get_wells(data))
    selectInput("dist_well", "Monitoring Wells", well_names, 
                multiple = FALSE,
                selected = well_names[1])
  })
  
  output$dist_analytes <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
    data <- get_data()
    analyte_names <- as.character(get_analytes(data))
    selectInput("dist_analyte", "Constituents", analyte_names, 
                multiple = FALSE,
                selected = analyte_names[1])
  })
  
  output$dist_date_ranges <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
    data <- get_data()
    tagList(
      dateRangeInput("dist_back_dates", "Background Date Range", 
                     start = min(data$sample_date, na.rm = TRUE),
                     end = max(data$sample_date, na.rm = TRUE))
    )
  })
  
  get_dist_data <- reactive({
    validate(
      need(input$data_path != "", "")
    )
    df <- get_data()
    start <- min(lubridate::ymd(input$dist_back_dates))
    end <- max(lubridate::ymd(input$dist_back_dates))
    data_selected <- df[df$location_id %in% input$dist_well &
                              df$param_name %in% input$dist_analyte &
                              df$sample_date >= start & df$sample_date <= end, ]
    data_selected
  })
  
  output$gof_plot <- renderPlot({
    validate(
      need(input$data_path != "", "Please upload a data set")
    )
    df <- get_dist_data()
    validate(
      need(length(unique(df$analysis_result)) > 2, "")
    )
    out <- EnvStats::gofTest(df$analysis_result, distribution = input$dist_type)
    out["data.name"] <- paste(input$dist_well, input$dist_analyte, sep=" ")
    plot(out)
  })
  # End Distribution Plots -----------------------------------------------------
  
  # Begin Boxplot Page----------------------------------------------------------
  output$box_wells <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      well_names <- as.character(get_wells(data))
      selectInput("box_well", "Monitoring Wells", well_names, 
                  multiple = TRUE, selected = well_names[1])
  })
  
  output$box_analytes <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      analyte_names <- as.character(get_analytes(data))
      selectInput("box_analyte", "Constituents", analyte_names, 
                  multiple = TRUE, selected = analyte_names[1])
  })
  
  boxplot <- reactive({
    validate(
      need(input$data_path != "", "Please upload a data set")
    )
      data <- get_data()
      box_wells <- input$box_well
      box_analytes <- input$box_analyte
      box_data <- data[data$location_id %in% box_wells & 
                             data$param_name %in% box_analytes, ]
    if (input$box_facet_by == "param_name") {
      box_list <- lapply(1:length(box_analytes), function(i) {
        box_name <- paste("box_plot", i, sep="")
        plotOutput(box_name)
      })
      
      for (i in 1:length(box_analytes)){
        local({
          box_i <- i
          box_name <- paste("box_plot", box_i, sep="")
          output[[box_name]] <- renderPlot({
            box <- gw_boxplot(
              box_data[box_data$param_name == 
                         box_analytes[box_i], ], 
              facet_by = input$box_facet_by,
              short_name = input$box_short_name,
              coord_flip = input$coord_flip_box
            )
            box
          })
        })
      }
    }
    if (input$box_facet_by == "location_id") {
      box_list <- lapply(1:length(box_wells), function(i) {
        box_name <- paste("box_plot", i, sep="")
        plotOutput(box_name)
      })
      
      for (i in 1:length(box_wells)){
        local({
          box_i <- i
          box_name <- paste("box_plot", box_i, sep="")
          output[[box_name]] <- renderPlot({
            box <- gw_boxplot(
              box_data[box_data$location_id == 
                         box_wells[box_i], ], 
              facet_by = input$box_facet_by,
              short_name = input$box_short_name,
              coord_flip = input$coord_flip_box
            )
            box
          })
        })
      }
    }
    do.call(tagList, box_list)
  })
  
  output$boxplot_out <- renderUI({
    boxplot()
  })
  
  # Begin Boxplot Download Page-------------------------------------------------
  get_box_data <- reactive({
    validate(
      need(input$data_path != "", "")
    )
    data <- get_data()
    box_wells <- input$box_well
    box_analytes <- input$box_analyte
    box_data <- data[data$location_id %in% box_wells & 
                             data$param_name %in% box_analytes, ]
    box_data
  })
  
  output$box_download <- downloadHandler(
    filename = function() {
      paste("boxplot_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file = file, width = 17, height = 11)
      multi_gw_boxplot(get_box_data(), facet_by = input$box_facet_by, 
                       short_name = input$box_short_name, 
                       coord_flip = input$box_coord_flip)
      dev.off()
    }
  )
  
  # End Boxplot Page------------------------------------------------------------
  
  # Time Series Page -----------------------------------------------------------
  output$ts_wells <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      well_names <- as.character(get_wells(data))
      selectInput("ts_well", "Monitoring Wells", well_names, 
                  multiple = TRUE,
                  selected = well_names[1])
  })
  
  # return a list of constituents for time series page
  output$ts_analytes <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      analyte_names <- as.character(get_analytes(data))
      selectInput("ts_analyte", "Constituents", analyte_names, 
                  multiple = TRUE,
                  selected = analyte_names[1])
  })
  
  # return start and end date of background data for time series page
  output$ts_date_ranges <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
    data <- get_data()
    tagList(
      dateRangeInput("ts_back_dates", "Background Date Range", 
                     start = min(data$sample_date, na.rm = TRUE),
                     end = max(data$sample_date, na.rm = TRUE)),
      dateRangeInput("ts_comp_dates", "Compliance Date Range", 
                     start = max(data$sample_date, na.rm = TRUE),
                     end = max(data$sample_date, na.rm = TRUE))  
    )
  })
  
  # time series plot output
  ts_plot <- reactive({
    validate(
      need(input$data_path != "", "Please upload a data set")
    )
      data <- get_data()
      ts_well <- input$ts_well
      ts_analyte <- input$ts_analyte
      ts_data <- data[data$location_id %in% ts_well &
                           data$param_name %in% ts_analyte, ]
    
    if (input$ts_facet_by == "location_id") {
      
      ts_list <- lapply(1:length(ts_well), function(i) {
        ts_name <- paste("ts_plot", i, sep="")
        plotOutput(ts_name)
      })
      
      for (i in 1:length(ts_well)){
        local({
          ts_i <- i
          ts_name <- paste("ts_plot", ts_i, sep="")
          output[[ts_name]] <- renderPlot({
            
            ts <- gw_ts_plot(ts_data[ts_data$location_id == ts_well[ts_i], ],
                             facet_by = "location_id", 
                             short_name = input$ts_short_name, 
                             ncol = input$ncol_ts)
            
            if (input$ts_date_lines){
              b1 <- min(lubridate::ymd(input$ts_back_dates))
              c1 <- min(lubridate::ymd(input$ts_comp_dates))
              b2 <- max(lubridate::ymd(input$ts_back_dates))
              c2 <- max(lubridate::ymd(input$ts_comp_dates))
              
              ts <- gw_ts_plot(
                ts_data[ts_data$location_id == 
                               ts_well[ts_i], ], 
                facet_by = "location_id",
                short_name = input$ts_short_name,
                back_date = c(b1, b2), 
                comp_date = c(c1, c2),
                ncol = input$ncol_ts
              )
            }
            ts
          })
        })
      }
    }
    
    if (input$ts_facet_by == "param_name") {
      ts_list <- lapply(1:length(ts_analyte), function(i) {
        ts_name <- paste("ts_plot", i, sep="")
        plotOutput(ts_name)
      })
      
      for (i in 1:length(ts_analyte)){
        local({
          ts_i <- i
          ts_name <- paste("ts_plot", ts_i, sep="")
          output[[ts_name]] <- renderPlot({
            
            ts <- gw_ts_plot(ts_data[ts_data$param_name == ts_analyte[ts_i], ],
                             facet_by = "param_name", 
                             short_name = input$ts_short_name,
                             ncol = input$ncol_ts)
            
            if (input$ts_date_lines){
              b1 <- min(lubridate::ymd(input$ts_back_dates))
              c1 <- min(lubridate::ymd(input$ts_comp_dates))
              b2 <- max(lubridate::ymd(input$ts_back_dates))
              c2 <- max(lubridate::ymd(input$ts_comp_dates))
              
              ts <- gw_ts_plot(
                ts_data[ts_data$param_name == 
                          ts_analyte[ts_i], ], 
                facet_by = "param_name",
                short_name = input$ts_short_name,
                back_date = c(b1, b2), 
                comp_date = c(c1, c2),
                ncol = input$ncol_ts
              )
            }
            ts
          })
        })
      }
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
      if (input$ts_date_lines){
        b1 <- min(lubridate::ymd(input$ts_back_dates))
        c1 <- min(lubridate::ymd(input$ts_comp_dates))
        b2 <- max(lubridate::ymd(input$ts_back_dates))
        c2 <- max(lubridate::ymd(input$ts_comp_dates))
        
        pdf(file = file, width = 17, height = 11)
        multi_gw_ts_plot(get_ts_data(), back_date = c(b1, b2), 
                         facet_by = input$ts_facet_by,
                         short_name = input$ts_short_name,
                         comp_date = c(c1, c2), ncol = input$ncol_ts)
        dev.off()
      } else {
        pdf(file = file, width = 17, height = 11)
        multi_gw_ts_plot(get_ts_data(), facet_by = input$ts_facet_by,
                         short_name = input$ts_short_name,
                         ncol = input$ncol_ts)
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
    start <- min(lubridate::ymd(input$date_range_piper))
    end <- max(lubridate::ymd(input$date_range_piper))
    wells <- input$well_piper
    Mg = paste(input$Mg)
    Ca = paste(input$Ca)
    Na = paste(input$Na)
    K = paste(input$K)
    Cl = paste(input$Cl)
    SO4 = paste(input$SO4)
    Alk = paste(input$Alk)
    TDS = paste(input$TDS)
    
    data_selected <- data[data$location_id %in% wells &
                            data$sample_date >= start & 
                            data$sample_date <= end, ]  
    
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
      start <- min(lubridate::ymd(input$date_range_stiff))
      end <- max(lubridate::ymd(input$date_range_stiff))
      data_selected <- data[data$location_id %in% input$well_stiff &
                              data$sample_date >= start & 
                              data$sample_date <= end, ]
      ions <- get_major_ions(data_selected, Mg = input$Mg_stiff, 
                             Ca = input$Ca_stiff, Na = input$Na_stiff, 
                             K = input$K_stiff, Cl = input$Cl_stiff, 
                             SO4 = input$SO4_stiff, Alk = input$Alk_stiff, 
                             TDS = input$TDS_stiff)
      ions <- ions[complete.cases(ions), ]
      plot_data <- conc_to_meq(ions, Mg = input$Mg_stiff, 
                               Ca = input$Ca_stiff, Na = input$Na_stiff, 
                               K = input$K_stiff, Cl = input$Cl_stiff, 
                               SO4 = input$SO4_stiff, 
                               HCO3 = input$Alk_stiff)
      stiff_data <- transform_stiff_data(plot_data, Mg = input$Mg_stiff, 
                                         Ca = input$Ca_stiff, 
                                         Na = input$Na_stiff, 
                                         K = input$K_stiff, 
                                         Cl = input$Cl_stiff,
                                         SO4 = input$SO4_stiff, 
                                         HCO3 = input$Alk_stiff, 
                                         TDS = input$TDS_stiff)
      stiff_data
  })
  
  stiff_diagram <- reactive({
      validate(
        need(input$data_path != "", "")
      )
      data <- get_stiff_data()
      wells <- unique(data$location_id)
      
      stiff_list <- lapply(1:length(wells), function(i) {
        name_stiff <- paste("stiff_plot", i, sep="")
        plotOutput(name_stiff)
      })
      
      for (i in 1:length(wells)){
        local({
          stiff_i <- i
          name_stiff <- paste("stiff_plot", stiff_i, sep="")
          output[[name_stiff]] <- renderPlot({
            stiff_plot(
              data[data$location_id == wells[stiff_i], ], 
              TDS=input$TDS_plot_stiff,
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
      stiff_by_loc(df = get_stiff_data(), TDS=input$TDS_plot_stiff,
            lines = input$stiff_lines)
      dev.off()
    }
  )
  # End Stiff Diagram Page -----------------------------------------------------
  
  # Begin Intrawell Prediction Limits ----------------------------------------------------
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
    
    df <- df %>%
      filter(
        location_id %in% input$well_intra,
        param_name %in% input$analyte_intra
      )
    
    bkgd_start <- min(lubridate::ymd(input$back_dates_intra))
    bkgd_end <- max(lubridate::ymd(input$back_dates_intra))
    bkgd <- c(bkgd_start, bkgd_end)
    
    comp_start <- min(lubridate::ymd(input$comp_dates_intra))
    comp_end <- max(lubridate::ymd(input$comp_dates_intra))
    comp <- c(comp_start, comp_end)
  
    validate(
      need(
        length(unique(df$analysis_result)) > 2, 
           "One of the input variables has fewer than 2 unique data points."
        )
      )
    
    out <- intra_pred_int(df, analysis_result, input$well_intra, 
                          input$analyte_intra,
                          bkgd, comp, SWFPR = input$swfpr, 
                          k = input$k, m = input$m,
                          r = input$r)
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
        ts_intra_name <- paste("ts_intra_plot", i, sep="")
        plotOutput(ts_intra_name)
      })
      
      for (i in 1:length(ts_intra_well)){
        local({
          ts_intra_i <- i
          ts_intra_name <- paste("ts_intra_plot", ts_intra_i, sep="")
          output[[ts_intra_name]] <- renderPlot({
            
            ts <- gw_ts_plot(
              ts_intra_data[ts_intra_data$location_id == 
              ts_intra_well[ts_intra_i], ],
              facet_by = "location_id", 
              short_name = input$ts_intra_short_name, 
              ncol = input$ncol_intra_ts,
              limit1 = "lower_limit",
              limit2 = "upper_limit"
              )
            
            if (input$ts_intra_date_lines){
              b1 <- min(lubridate::ymd(input$back_dates_intra))
              c1 <- min(lubridate::ymd(input$comp_dates_intra))
              b2 <- max(lubridate::ymd(input$back_dates_intra))
              c2 <- max(lubridate::ymd(input$comp_dates_intra))
              
              ts <- gw_ts_plot(
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
        ts_intra_name <- paste("ts_intra_plot", i, sep="")
        plotOutput(ts_intra_name)
      })
      
      for (i in 1:length(ts_intra_analyte)){
        local({
          ts_intra_i <- i
          ts_intra_name <- paste("ts_intra_plot", ts_intra_i, sep="")
          output[[ts_intra_name]] <- renderPlot({
            
            ts <- gw_ts_plot(
              ts_intra_data[ts_intra_data$param_name == 
              ts_intra_analyte[ts_intra_i], ],
              facet_by = "param_name", 
              short_name = input$ts_intra_short_name,
              limit1 = "lower_limit",
              limit2 = "upper_limit",
              ncol = input$ncol_intra_ts,
              )
            
            if (input$ts_intra_date_lines){
              b1 <- min(lubridate::ymd(input$back_dates_intra))
              c1 <- min(lubridate::ymd(input$comp_dates_intra))
              b2 <- max(lubridate::ymd(input$back_dates_intra))
              c2 <- max(lubridate::ymd(input$comp_dates_intra))
              
              ts <- gw_ts_plot(
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
    if(input$intra_plot){
      ts_intra_plot()
    }
  })
  # End Prediction Limits ------------------------------------------------------

  # Begin NADA Page ------------------------------------------------------------

  # ROS
  output$ros_wells <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      well_names <- as.character(get_wells(data))
      selectInput("ros_well", "Monitoring Wells", well_names, 
                  multiple = FALSE,
                  selected = well_names[1])
  })

  output$ros_analytes <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      analyte_names <- as.character(get_analytes(data))
      selectInput("ros_analyte", "Constituents", analyte_names, 
                  multiple = FALSE,
                  selected = analyte_names[1])
  })

  output$ros_date_ranges <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      tagList(
        dateRangeInput("bkgd_date_range_ros", "Background Date Range", 
                       start = min(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE)) 
      )
  })
  output$ros_summary_table <- renderDataTable({
    validate(
      need(input$data_path != "", "Please upload a data set")
    )
      data <- get_data()
      wells <- input$ros_well
      const <- input$ros_analyte
      data <- data[data$location_id %in% wells &
                  data$param_name %in% const, ]
      start <- min(lubridate::ymd(input$bkgd_date_range_ros))
      end <- max(lubridate::ymd(input$bkgd_date_range_ros))
      gw_summary(data, start, end)
  }, options = list(scrollY = "100%", scrollX = "100%", 
                    lengthMenu = c(5, 10, 15, 25, 50, 100), 
                    pageLength = 10)
  )

  output$ros_out <- renderPrint({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      wells <- input$ros_well
      const <- input$ros_analyte
      start <- min(lubridate::ymd(input$bkgd_date_range_ros))
      end <- max(lubridate::ymd(input$bkgd_date_range_ros))
      data <- data[data$location_id == wells &
                     data$param_name == const &
                     data$sample_date >= start &
                     data$sample_date <= end, ]
      data$censored <- ifelse(data$lt_measure == "<", TRUE, FALSE)
      data <- as.data.frame(data)
      ros <- NADA::ros(data$analysis_result, data$censored)
      NADA::print(ros)
  })
  
    output$ros_out_2 <- renderPrint({
      validate(
        need(input$data_path != "", "")
      )
      data <- get_data()
      wells <- input$ros_well
      const <- input$ros_analyte
      start <- min(lubridate::ymd(input$bkgd_date_range_ros))
      end <- max(lubridate::ymd(input$bkgd_date_range_ros))
      data <- data[data$location_id == wells &
                   data$param_name == const &
                   data$sample_date >= start &
                   data$sample_date <= end, ]
      data$censored <- ifelse(data$lt_measure == "<", TRUE, FALSE)
      data <- as.data.frame(data)
      ros <- NADA::ros(data$analysis_result, data$censored)
      summary(ros)
  })
  
  output$ros_plot <- renderPlot({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      wells <- input$ros_well
      const <- input$ros_analyte
      start <- min(lubridate::ymd(input$bkgd_date_range_ros))
      end <- max(lubridate::ymd(input$bkgd_date_range_ros))
      data <- data[data$location_id %in% wells &
                     data$param_name %in% const &
                     data$sample_date >= start &
                     data$sample_date <= end, ]
      data$censored <- ifelse(data$lt_measure == "<", TRUE, FALSE)
      data <- as.data.frame(data)
      ros <- NADA::ros(data$analysis_result, data$censored)
      NADA::plot(ros)
})
  # End ROS
  
  # begin Kaplan-Meier
  output$kp_wells <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      well_names <- as.character(get_wells(data))
      selectInput("kp_well", "Monitoring Wells", well_names, 
                  multiple = FALSE,
                  selected = well_names[1])
  })

  output$kp_analytes <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      analyte_names <- as.character(get_analytes(data))
      selectInput("kp_analyte", "Constituents", analyte_names, 
                  multiple = FALSE,
                  selected = analyte_names[1])
  })
  # End Kaplan-Meier
  # End NADA Page -------------------------------------------------------------

})
