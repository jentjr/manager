library(gwstats)
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
             ".csv" = from_csv(input$data_path$datapath),
             ".mdb" = connect_manages(input$data_path$datapath),
             ".xls" = from_excel(input$data_path$datapath)) %>%
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
  
  # Begin Boxplot by constituent page-------------------------------------------
  output$wells_box_const <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      well_names <- as.character(get_wells(data))
      selectInput("well_box_const", "Monitoring Wells", well_names, 
                  multiple = TRUE, selected = well_names[1])
  })
  
  output$analytes_box_const <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      analyte_names <- as.character(get_analytes(data))
      selectInput("analyte_box_const", "Constituents", analyte_names, 
                  multiple = TRUE, selected = analyte_names[1])
  })
  
  box_const <- reactive({
    validate(
      need(input$data_path != "", "Please upload a data set")
    )
      data <- get_data()
      w_box_const <- input$well_box_const
      a_box_const <- input$analyte_box_const
      box_const_data <- data[data$location_id %in% w_box_const & 
                             data$param_name %in% a_box_const, ]
      
      box_list_const <- lapply(1:length(a_box_const), function(i) {
        box_const_name <- paste("box_list_plot", i, sep="")
        plotOutput(box_const_name, height = 675, width = 825)
      })
      
      for (i in 1:length(a_box_const)){
        local({
          box_const_i <- i
          box_const_name <- paste("box_list_plot", box_const_i, sep="")
          output[[box_const_name]] <- renderPlot({
            box_const <- boxplot_by_param_grid(
              box_const_data[box_const_data$param_name == 
                             a_box_const[box_const_i], ],
                             coord_flip = input$coord_flip_box_const
              )
            if (input$short_name_box_const){
              box_const <- boxplot_by_param_grid(
                box_const_data[box_const_data$param_name == 
                               a_box_const[box_const_i], ], 
                               name = "short",
                               coord_flip = input$coord_flip_box_const
                )
            }
            box_const
          })
        })
      }
      do.call(tagList, box_list_const)
  })
  
  output$box_out_const <- renderUI({
    box_const()
  })
  
  get_box_const_data <- reactive({
    validate(
      need(input$data_path != "", "")
    )
    data <- get_data()
    w_box_const <- input$well_box_const
    a_box_const <- input$analyte_box_const
    box_const_data <- data[data$location_id %in% w_box_const & 
                             data$param_name %in% a_box_const, ]
    box_const_data
  })
  
  output$box_const_download <- downloadHandler(
    filename = function() {
      paste("box_const_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file = file, width = 17, height = 11)
      boxplot_by_param(get_box_const_data())
      dev.off()
    }
  )
  
  # End Boxplot by constituent Page---------------------------------------------
   
  # Begin Boxplot by well page -------------------------------------------------
  output$wells_box_well <- renderUI({
    validate(
      need(input$data_path != "", "")
    )  
    data <- get_data()
    well_names <- as.character(get_wells(data))
    selectInput("well_box_well", "Monitoring Wells", well_names, 
                 multiple = TRUE, selected = well_names[1])
  })
  
  output$analytes_box_well <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
    data <- get_data()
    analyte_names <- as.character(get_analytes(data))
    selectInput("analyte_box_well", "Constituents", analyte_names, 
                 multiple = TRUE, selected = analyte_names[1])
  })
  
  box_well <- reactive({
    validate(
      need(input$data_path != "", "Please upload a data set")
    )
      data <- get_data()
      w_box_well <- input$well_box_well
      a_box_well <- input$analyte_box_well
      data_box_well <- data[data$location_id %in% w_box_well & 
                            data$param_name %in% a_box_well, ]
      
      box_list_well <- lapply(1:length(w_box_well), function(i) {
        box_well_name <- paste("box_well_plot", i, sep="")
        plotOutput(box_well_name, height = 675, width = 825)
      })
      
      for (i in 1:length(w_box_well)){
        local({
          box_well_i <- i
          box_well_name <- paste("box_well_plot", box_well_i, sep="")
          output[[box_well_name]] <- renderPlot({
            box_well <- boxplot_by_well_grid(
              data_box_well[data_box_well$location_id == 
                            w_box_well[box_well_i], ],
                            coord_flip = input$coord_flip_box_well
              )
            if (input$short_name_box_well){
              box_well <- boxplot_by_well_grid(
                data_box_well[data_box_well$location_id == 
                              w_box_well[box_well_i], ], 
                              name = "short",
                              coord_flip = input$coord_flip_box_well
                )
            }
            box_well
          })
        })
      }
      do.call(tagList, box_list_well)
  })
  
  output$box_out_well <- renderUI({
    box_well()
  })
  
  get_box_well_data <- reactive({
    validate(
      need(input$data_path != "", "Please upload a data set")
    )
    data <- get_data()
    w_box_well <- input$well_box_well
    a_box_well <- input$analyte_box_well
    data_box_well <- data[data$location_id %in% w_box_well & 
                            data$param_name %in% a_box_well, ]
    data_box_well
  })
  
  output$box_well_download <- downloadHandler(
    filename = function() {
      paste("box_well_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file = file, width = 17, height = 11)
      boxplot_by_well(get_box_well_data())
      dev.off()
    }
  )
  # End Boxplot by well page ---------------------------------------------------
  
  # Time Series by well Page ---------------------------------------------------
  # return a list of well names for time series page
  output$wells_time_well <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      well_names <- as.character(get_wells(data))
      selectInput("well_time_well", "Monitoring Wells", well_names, 
                  multiple = TRUE,
                  selected = well_names[1])
  })
  
  # return a list of constituents for time series page
  output$analytes_time_well <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      analyte_names <- as.character(get_analytes(data))
      selectInput("analyte_time_well", "Constituents", analyte_names, 
                  multiple = TRUE,
                  selected = analyte_names[1])
  })
  
  # return start and end date of background data for time series page
  output$date_ranges_time_well <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
    data <- get_data()
    tagList(
      dateRangeInput("back_date_range_time_well", "Background Date Range", 
                     start = min(data$sample_date, na.rm = TRUE),
                     end = max(data$sample_date, na.rm = TRUE)),
      dateRangeInput("comp_date_range_time_well", "Compliance Date Range", 
                     start = max(data$sample_date, na.rm = TRUE),
                     end = max(data$sample_date, na.rm = TRUE))  
    )
  })
  
  # time series plot output
  ts_by_well <- reactive({
    validate(
      need(input$data_path != "", "Please upload a data set")
    )
      data <- get_data()
      w_ts_well <- input$well_time_well
      a_ts_well <- input$analyte_time_well
      data_ts_well <- data[data$location_id %in% w_ts_well &
                           data$param_name %in% a_ts_well, ]
      
      ts_well_list <- lapply(1:length(w_ts_well), function(i) {
        ts_well_name <- paste("ts_well_plot", i, sep="")
        plotOutput(ts_well_name, height = 675, width = 825)
      })
      
      for (i in 1:length(w_ts_well)){
        local({
          ts_well_i <- i
          ts_well_name <- paste("ts_well_plot", ts_well_i, sep="")
          output[[ts_well_name]] <- renderPlot({
            ts_well <- ind_by_loc_grid(
              data_ts_well[data_ts_well$location_id == 
                           w_ts_well[ts_well_i], ], 
                           ncol = input$ncol_ts_well
              )
            
            if (input$date_lines_well){
              b1 <- min(lubridate::ymd(input$back_date_range_time_well))
              c1 <- min(lubridate::ymd(input$comp_date_range_time_well))
              b2 <- max(lubridate::ymd(input$back_date_range_time_well))
              c2 <- max(lubridate::ymd(input$comp_date_range_time_well))
              
              ts_well <- ind_by_loc_grid(
                data_ts_well[data_ts_well$location_id == 
                             w_ts_well[ts_well_i], ], 
                             back_date = c(b1, b2), 
                             comp_date = c(c1, c2),
                             ncol = input$ncol_ts_well
                )
            }
            if (input$short_name_well){
              ts_well <- ind_by_loc_grid(
                data_ts_well[data_ts_well$location_id == 
                             w_ts_well[ts_well_i], ], 
                             name="short",
                             ncol = input$ncol_ts_well
                )
            }
            if (input$short_name_well & input$date_lines_well){
              b1 <- min(lubridate::ymd(input$back_date_range_time_well))
              c1 <- min(lubridate::ymd(input$comp_date_range_time_well))
              b2 <- max(lubridate::ymd(input$back_date_range_time_well))
              c2 <- max(lubridate::ymd(input$comp_date_range_time_well))
              
              ts_well <- ind_by_loc_grid(
                data_ts_well[data_ts_well$location_id == 
                             w_ts_well[ts_well_i], ], 
                             back_date = c(b1, b2), 
                             comp_date = c(c1, c2), 
                             name = "short",
                             ncol = input$ncol_ts_well
                )
            }
            ts_well
          })
        })
      }
      do.call(tagList, ts_well_list)
  })
  
  output$ts_by_well_out <- renderUI({
    ts_by_well()
  })

  # Begin Time Series by Well Download Page ------------------------------------
  get_ts_well_data <- reactive({
    validate(
      need(input$data_path != "", "Please upload a data set")
    )
    data <- get_data()
    w_ts_well <- input$well_time_well
    a_ts_well <- input$analyte_time_well
    data_ts_well <- data[data$location_id %in% w_ts_well &
                           data$param_name %in% a_ts_well, ]
    data_ts_well
  })
  
  output$ts_well_download <- downloadHandler(
    filename = function() {
      paste("ts_well_plot_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if (input$date_lines_well){
        b1 <- min(lubridate::ymd(input$back_date_range_time_well))
        c1 <- min(lubridate::ymd(input$comp_date_range_time_well))
        b2 <- max(lubridate::ymd(input$back_date_range_time_well))
        c2 <- max(lubridate::ymd(input$comp_date_range_time_well))
        
        pdf(file = file, width = 17, height = 11)
        ind_by_loc(get_ts_well_data(), back_date = c(b1, b2), 
                   comp_date = c(c1, c2), ncol = input$ncol_ts_well)
        dev.off()
      }
      if (input$short_name_well){
        pdf(file = file, width = 17, height = 11)
        ind_by_loc(get_ts_well_data(), name = "short", 
                   ncol = input$ncol_ts_well)
        dev.off()
      }
      if (input$short_name_well & input$date_lines_well){
        b1 <- min(lubridate::ymd(input$back_date_range_time_well))
        c1 <- min(lubridate::ymd(input$comp_date_range_time_well))
        b2 <- max(lubridate::ymd(input$back_date_range_time_well))
        c2 <- max(lubridate::ymd(input$comp_date_range_time_well))
        
        pdf(file = file, width = 17, height = 11)
        ind_by_loc(get_ts_well_data(), back_date = c(b1, b2), 
                   comp_date = c(c1, c2), name = "short",
                   ncol = input$ncol_ts_well)
        dev.off()
      } else {
        pdf(file = file, width = 17, height = 11)
        ind_by_loc(get_ts_well_data(), ncol = input$ncol_ts_well)
        dev.off()
      }
    }
  )
  # End Time Series by Well Download Page --------------------------------------
  # End Time Series  by well page-----------------------------------------------
  
  # Begin Time Series by constituent page --------------------------------------
  output$wells_time_const <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      well_names <- as.character(get_wells(data))
      selectInput("well_time_const", "Monitoring Wells", well_names, 
                  multiple = TRUE,
                  selected = well_names[1])
  })
  
  output$analytes_time_const <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      analyte_names <- as.character(get_analytes(data))
      selectInput("analyte_time_const", "Constituents", analyte_names, 
                  multiple = TRUE,
                  selected = analyte_names[1])
  })
  
  output$date_ranges_time_const <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      tagList(
        dateRangeInput("back_date_range_time_const", "Background Date Range", 
                       start = min(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE)),
        dateRangeInput("comp_date_range_time_const", "Compliance Date Range", 
                       start = max(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE))  
      )
  })
  
  ts_by_const <- reactive({
    validate(
      need(input$data_path != "", "Please upload a data set")
    )
      data <- get_data()
      w_ts_const <- input$well_time_const
      a_ts_const <- input$analyte_time_const
      data_ts_const <- data[data$location_id %in% w_ts_const &
                            data$param_name %in% a_ts_const, ]
      
      ts_const_list <- lapply(1:length(a_ts_const), function(i) {
        name_ts_const <- paste("ts_const_plot", i, sep="")
        plotOutput(name_ts_const, height = 675, width = 825)
      })
      
      for (i in 1:length(a_ts_const)){
        local({
          ts_const_i <- i
          name_ts_const <- paste("ts_const_plot", ts_const_i, sep="")
          output[[name_ts_const]] <- renderPlot({
            ts_const <- ind_by_param_grid(
              data_ts_const[data_ts_const$param_name == 
                            a_ts_const[ts_const_i], ], 
                            ncol = input$ncol_ts_const
              )
            
            if (input$date_lines_const){
              b1 <- min(lubridate::ymd(input$back_date_range_time_const))
              c1 <- min(lubridate::ymd(input$comp_date_range_time_const))
              b2 <- max(lubridate::ymd(input$back_date_range_time_const))
              c2 <- max(lubridate::ymd(input$comp_date_range_time_const))
              
              ts_const <- ind_by_param_grid(
                data_ts_const[data_ts_const$param_name == 
                              a_ts_const[ts_const_i], ], 
                              back_date = c(b1, b2), 
                              comp_date = c(c1, c2),
                              ncol = input$ncol_ts_const
                )
            }
            if (input$short_name_const){
              ts_const <- ind_by_param_grid(
                data_ts_const[data_ts_const$param_name == 
                              a_ts_const[ts_const_i], ], 
                              name="short",
                              ncol = input$ncol_ts_const
                )
            }
            if (input$short_name_const & input$date_lines_const){
              b1 <- min(lubridate::ymd(input$back_date_range_time_const))
              c1 <- min(lubridate::ymd(input$comp_date_range_time_const))
              b2 <- max(lubridate::ymd(input$back_date_range_time_const))
              c2 <- max(lubridate::ymd(input$comp_date_range_time_const))
              
              ts_const <- ind_by_param_grid(
                data_ts_const[data_ts_const$param_name == 
                              a_ts_const[ts_const_i], ], 
                              back_date = c(b1, b2), 
                              comp_date = c(c1, c2), 
                              name = "short",
                              ncol = input$ncol_ts_const
                )
            } 
            ts_const
          })
        })
      }
      do.call(tagList, ts_const_list)
  })
  
  output$ts_by_const_out <- renderUI({
    ts_by_const()
  })
  
  # Begin Time Series by Constituent Download Page -----------------------------
  get_ts_const_data <- reactive({
    validate(
      need(input$data_path != "", "Please upload a data set")
    )
    data <- get_data()
    w_ts_const <- input$well_time_const
    a_ts_const <- input$analyte_time_const
    data_ts_const <- data[data$location_id %in% w_ts_const &
                            data$param_name %in% a_ts_const, ]
    data_ts_const
  })
  
  output$ts_const_download <- downloadHandler(
    filename = function() {
      paste("ts_const_plot_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if (input$short_name_const){
        pdf(file = file, width = 17, height = 11)
        ind_by_param(get_ts_const_data(), name = "short", 
                     ncol = input$ncol_ts_const)
        dev.off()
      }
      if (input$date_lines_const){
        b1 <- min(lubridate::ymd(input$back_date_range_time_const))
        c1 <- min(lubridate::ymd(input$comp_date_range_time_const))
        b2 <- max(lubridate::ymd(input$back_date_range_time_const))
        c2 <- max(lubridate::ymd(input$comp_date_range_time_const))
        
        pdf(file = file, width = 17, height = 11)
        ind_by_param(get_ts_const_data(), back_date = c(b1, b2), 
                     comp_date = c(c1, c2), ncol = input$ncol_ts_const)
        dev.off()
      } 
      if (input$date_lines_const & input$short_name_const){
        b1 <- min(lubridate::ymd(input$back_date_range_time_const))
        c1 <- min(lubridate::ymd(input$comp_date_range_time_const))
        b2 <- max(lubridate::ymd(input$back_date_range_time_const))
        c2 <- max(lubridate::ymd(input$comp_date_range_time_const))
        
        pdf(file = file, width = 17, height = 11)
        ind_by_param(get_ts_const_data(), back_date = c(b1, b2), 
                     comp_date = c(c1, c2), name = "short",
                     ncol = input$ncol_ts_const)
        dev.off()
      } else {
        pdf(file = file, width = 17, height = 11)
        ind_by_param(get_ts_const_data(), ncol = input$ncol_ts_const)
        dev.off()
      }
    }
  )
  # End Time Series by Constituent Download Page -------------------------------
  # End Time Series by constituent page ----------------------------------------
  
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
    data_selected <- data[data$location_id %in% wells &
                            data$sample_date >= start & 
                            data$sample_date <= end, ]  
    ions <- get_major_ions(data_selected, Mg=input$Mg, Ca=input$Ca, 
                           Na=input$Na, K=input$K, Cl=input$Cl, 
                           SO4=input$SO4, Alk=input$Alk, TDS=input$TDS)
    piper_data <- transform_piper_data(ions, Mg=input$Mg, Ca=input$Ca, 
                                       Na=input$Na, K=input$K, Cl=input$Cl, 
                                       SO4=input$SO4, Alk=input$Alk, 
                                       TDS=input$TDS)
    piper_data
  })
  
  plot_piper <- reactive({
    piper_plot(get_piper_data(), TDS=input$TDS_plot)
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
      print(piper_plot(df = get_piper_data(), TDS=input$TDS_plot))
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
                  multiple = FALSE, selected = well_names[1])
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

  output$stiff_diagram <- renderPlot({
    validate(
      need(input$data_path != "", "Please upload a data set")
    )
      data <- get_data()
      start <- min(lubridate::ymd(input$date_range_stiff))
      end <- max(lubridate::ymd(input$date_range_stiff))
      data_selected <- data[data$location_id %in% input$well_stiff &
                              data$sample_date >= start & 
                              data$sample_date <= end, ]
      ions <- get_major_ions(data_selected, Mg=input$Mg_stiff, 
                             Ca=input$Ca_stiff, Na=input$Na_stiff, 
                             K=input$K_stiff, Cl=input$Cl_stiff, 
                             SO4=input$SO4_stiff, Alk=input$Alk_stiff, 
                             TDS=input$TDS_stiff)
      ions <- ions[complete.cases(ions), ]
      plot_data <- conc_to_meq(ions, Mg=input$Mg_stiff, 
                                       Ca=input$Ca_stiff, Na=input$Na_stiff, 
                                       K=input$K_stiff, Cl=input$Cl_stiff, 
                                       SO4=input$SO4_stiff, 
                                       HCO3=input$Alk_stiff)
      stiff_data <- transform_stiff_data(plot_data, Mg=input$Mg_stiff, 
                                         Ca=input$Ca_stiff, Na=input$Na_stiff, 
                                         K=input$K_stiff, Cl=input$Cl_stiff,
                                         SO4=input$SO4_stiff, 
                                         HCO3=input$Alk_stiff, 
                                         TDS=input$TDS_stiff)
      stiff_plot(stiff_data, TDS=input$TDS_plot_stiff)
  })
  # End Stiff Diagram Page -----------------------------------------------------
  
  # Begin Prediction Limits ----------------------------------------------------
  output$wells_upl <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      well_names <- as.character(get_wells(data))
      selectInput("well_upl", "Monitoring Wells", well_names, 
                  multiple = FALSE,
                  selected = well_names[1])
  })

  # return a list of constituents for time series page
  output$analytes_upl <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      analyte_names <- as.character(get_analytes(data))
      selectInput("analyte_upl", "Constituents", analyte_names, 
                  multiple = FALSE,
                  selected = analyte_names[1])
  })

  # return start and end date of background data for time series page
  output$date_ranges_upl <- renderUI({
    validate(
      need(input$data_path != "", "")
    )
      data <- get_data()
      tagList(
        dateRangeInput("back_date_range_upl", "Background Date Range", 
                       start = min(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE))
      )
  })

  bkgd_data <- reactive({
    validate(
      need(input$data_path != "", "")
    )
      df <- get_data()
      start <- min(lubridate::ymd(input$back_date_range_upl))
      end <- max(lubridate::ymd(input$back_date_range_upl))
      data_selected <- subset(df, location_id %in% input$well_upl &
                                param_name %in% input$analyte_upl &
                                sample_date >= start & sample_date <= end)
      data_selected
  })

  output$gof <- renderPlot({
    validate(
      need(input$data_path != "", "Please upload a data set")
    )
      df <- bkgd_data()
    validate(
      need(length(unique(df$analysis_result)) > 2, "")
    )
      if (input$int_type == "Normal" || input$int_type == "Non-parametric"){
        out <- EnvStats::gofTest(df$analysis_result)
        out["data.name"] <- paste(input$well_upl, input$analyte_upl, sep=" ")
      }
      if (input$int_type == "Lognormal"){
        out <- EnvStats::gofTest(df$analysis_result, distribution = "lnorm")
        out["data.name"] <- paste(input$well_upl, input$analyte_upl, sep=" ")
      }
      if (input$int_type == "Gamma"){
        out <- EnvStats::gofTest(df$analysis_result, distribution = "gamma")
        out["data.name"] <- paste(input$well_upl, input$analyte_upl, sep=" ")
      }
      plot(out)
  })

  output$upl <- renderPrint({
    validate(
      need(input$data_path != "", "")
    )
      
      df <- bkgd_data()
      nw <- input$nw
      nc <- input$nc
      swfpr <- input$swfpr
      conf.level = (1 - swfpr)^(1/(nc*nw))
    validate(
      need(length(unique(df$analysis_result)) > 2, 
           "One of the input variables has fewer than 2 unique data points.")
      )  
      if (input$int_type == "Normal"){
        out <- EnvStats::predIntNormSimultaneous(df$analysis_result, 
                                                 k = input$k, m = input$m, 
                                                 r = input$r, 
                                                 conf.level = conf.level)
        out["data.name"] <- paste(input$well_upl, input$analyte_upl, sep=" ") 
      }
      if (input$int_type == "Lognormal"){
        out <- EnvStats::predIntLnormSimultaneous(df$analysis_result, 
                                                  k = input$k, m = input$m, 
                                                  r = input$r, 
                                                  conf.level = conf.level)
        out["data.name"] <- paste(input$well_upl, input$analyte_upl, sep=" ")
      }
      if (input$int_type == "Gamma"){
        out <- EnvStats::predIntGammaSimultaneous(df$analysis_result, 
                                                  k = input$k, m = input$m, 
                                                  r = input$r,
                                                  conf.level = conf.level)
      }
      if (input$int_type == "Non-parametric"){
        out <- EnvStats::predIntNparSimultaneous(df$analysis_result, 
                                                 k = input$k, m = input$m, 
                                                 r = input$r)  
        out["data.name"] <- paste(input$well_upl, input$analyte_upl, sep=" ")
      }
      out
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
  }, options = list(sScrollY = "100%", sScrollX = "100%", 
                    aLengthMenu = c(5, 10, 15, 25, 50, 100), 
                    iDisplayLength = 10)
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
