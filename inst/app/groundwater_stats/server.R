# change options to handle large file size
options(shiny.maxRequestSize=-1)
# force numbers to be decimal instead of scientific
options(scipen=6, digits = 8)

shinyServer(function(input, output, session) {
  get_data <- reactive({
    if (!is.null(input$manages_path)){
      switch(input$file_type, 
             ".csv" = from_csv(input$manages_path$datapath),
             ".mdb" = connect_manages(input$manages_path$datapath),
             ".xls" = from_excel(input$manages_path$datapath)) %>%
        dplyr::arrange(location_id, sample_date, param_name)
    }      
  })
  
  output$well_table <- renderDataTable({
    if (!is.null(input$manages_path)){
      data <- get_data() 
      return(data)
    }
  }, options = list(sScrollY = "100%", sScrollX = "100%", 
                    aLengthMenu = c(5, 10, 15, 25, 50, 100), 
                    iDisplayLength = 10)
  ) 
  # Begin Summary Table page --------------------------------------------------  
  output$summary_date_ranges <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      tagList(
        dateRangeInput("background_date_range", "Background Date Range", 
                       start = min(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE)) 
      )
    }
  })
  output$summary_table <- renderDataTable({
    if (!is.null(input$manages_path)){
      data <- get_data()
      start <- min(lubridate::ymd(input$background_date_range))
      end <- max(lubridate::ymd(input$background_date_range))
      gw_summary(data, start, end)
    }
  }, options = list(sScrollY = "100%", sScrollX = "100%", 
                    aLengthMenu = c(5, 10, 15, 25, 50, 100), 
                    iDisplayLength = 10)
  )
  # End Summary Table page ----------------------------------------------------
  
  # Begin Boxplot by constituent page------------------------------------------
  output$wells_box_const <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      well_names <- as.character(get_wells(data))
      selectInput("well_box_const", "Monitoring Wells", well_names, 
                  multiple = TRUE, selected = well_names[1])
    }
  })
  
  output$analytes_box_const <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      analyte_names <- as.character(get_analytes(data))
      selectInput("analyte_box_const", "Constituents", analyte_names, 
                  multiple = FALSE, selected = analyte_names[1])
    }
  })
  
  # create boxplots by constituent 
  box_out_const <- reactive({
    if (!is.null(input$manages_path)){
      data <- get_data()
      wells <- input$well_box_const
      analytes <- input$analyte_box_const
      data_selected <- data[data$location_id %in% wells & 
                            data$param_name %in% analytes, ]
      
      if (input$short_name_box_const){
        boxplot_by_param_grid(data_selected, name = "short")
      }
      if (input$short_name_box_const & input$coord_flip_box_const){
          boxplot_by_param_grid(data_selected, name = "short", coord_flip = TRUE)
      } else {
          boxplot_by_param_grid(data_selected)
      }
    }
  })
  
  # boxplot output to ui
  output$box_plot_const <- renderPlot({
    box_out_const()
  })
  # End Boxplot by constituent Page--------------------------------------------
   
  # Begin Boxplot by well page ------------------------------------------------
  output$wells_box_well <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      well_names <- as.character(get_wells(data))
      selectInput("well_box_well", "Monitoring Wells", well_names, 
                  multiple = FALSE, selected = well_names[1])
    }
  })
  
  output$analytes_box_well <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      analyte_names <- as.character(get_analytes(data))
      selectInput("analyte_box_well", "Constituents", analyte_names, 
                  multiple = TRUE, selected = analyte_names[1])
    }
  })
  
  # create boxplots by constituent 
  box_out_well <- reactive({
    if (!is.null(input$manages_path)){
      data <- get_data()
      wells <- input$well_box_well
      analytes <- input$analyte_box_well
      data_selected <- data[data$location_id %in% wells & 
                              data$param_name %in% analytes, ]
      
      if (input$short_name_box_well){
        boxplot_by_well_grid(data_selected, name = "short")
      }
      if (input$short_name_box_well & input$coord_flip_box_well){
        boxplot_by_well_grid(data_selected, name = "short", coord_flip = TRUE)
      } else {
          boxplot_by_well_grid(data_selected)
      }            
    }
  })
  
  # boxplot output to ui
  output$box_plot_well <- renderPlot({
    box_out_well()
  })
  # End Boxplot by well page --------------------------------------------------
  
  # Time Series by well Page --------------------------------------------------
  # return a list of well names for time series page
  output$wells_time_well <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      well_names <- as.character(get_wells(data))
      selectInput("well_time_well", "Monitoring Wells", well_names, 
                  multiple = FALSE,
                  selected = well_names[1])
    }
  })
  
  # return a list of constituents for time series page
  output$analytes_time_well <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      analyte_names <- as.character(get_analytes(data))
      selectInput("analyte_time_well", "Constituents", analyte_names, 
                  multiple = TRUE,
                  selected = analyte_names[1])
    }
  })
  
  # return start and end date of background data for time series page
  output$date_ranges_time_well <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      tagList(
        dateRangeInput("back_date_range_time_well", "Background Date Range", 
                       start = min(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE)),
        dateRangeInput("comp_date_range_time_well", "Compliance Date Range", 
                       start = max(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE))  
      )
    }
  })
  
  # time series plot output
  ts_by_well_out <- reactive({
    if (!is.null(input$manages_path)){
      
      data <- get_data()
      wells <- input$well_time_well
      analytes <- input$analyte_time_well
      data_selected <- data[data$location_id %in% wells &
                              data$param_name %in% analytes, ]
      
      t1 <- ind_by_loc_grid(data_selected)
              
      if (input$date_lines_well){
        # create separate data.frame for geom_rect data
        # change dates to POSIXct which is same as MANAGES database dates
        b1 <- min(as.POSIXct(input$back_date_range_time_well, 
                             format = "%Y-%m-%d"))
        c1 <- min(as.POSIXct(input$comp_date_range_time_well, 
                             format = "%Y-%m-%d"))
        b2 <- max(as.POSIXct(input$back_date_range_time_well, 
                             format = "%Y-%m-%d"))
        c2 <- max(as.POSIXct(input$comp_date_range_time_well, 
                             format = "%Y-%m-%d"))
        
        t1 <- ind_by_loc_grid(data_selected, back_date = c(b1, b2), 
                              comp_date = c(c1, c2))
      }
      if (input$short_name_well){
        t1 <- ind_by_loc_grid(data_selected, name="short")
      }
      if (input$short_name_well & input$date_lines_well){
        # create separate data.frame for geom_rect data
        # change dates to POSIXct which is same as MANAGES database dates
        b1 <- min(as.POSIXct(input$back_date_range_time_well, 
                             format = "%Y-%m-%d"))
        c1 <- min(as.POSIXct(input$comp_date_range_time_well, 
                             format = "%Y-%m-%d"))
        b2 <- max(as.POSIXct(input$back_date_range_time_well,
                             format = "%Y-%m-%d"))
        c2 <- max(as.POSIXct(input$comp_date_range_time_well, 
                             format = "%Y-%m-%d"))
        
        t1 <- ind_by_loc_grid(data_selected, back_date = c(b1, b2), 
                              comp_date = c(c1, c2), name = "short")
      }
      t1
    }
  })
  
  output$ts_by_well <- renderPlot({
    ts_by_well_out()
  })
  
  # End Time Series  by well page-----------------------------------------------
  
  # Begin Time Series by constituent page -------------------------------------
  output$wells_time_const <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      well_names <- as.character(get_wells(data))
      selectInput("well_time_const", "Monitoring Wells", well_names, 
                  multiple = TRUE,
                  selected = well_names[1])
    }
  })
  
  output$analytes_time_const <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      analyte_names <- as.character(get_analytes(data))
      selectInput("analyte_time_const", "Constituents", analyte_names, 
                  multiple = FALSE,
                  selected = analyte_names[1])
    }
  })
  
  output$date_ranges_time_const <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      tagList(
        dateRangeInput("back_date_range_time_const", "Background Date Range", 
                       start = min(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE)),
        dateRangeInput("comp_date_range_time_const", "Compliance Date Range", 
                       start = max(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE))  
      )
    }
  })
  
  ts_by_const_out <- reactive({
    if (!is.null(input$manages_path)){
      
      data <- get_data()
      wells <- input$well_time_const
      analytes <- input$analyte_time_const
      data_selected <- data[data$location_id %in% wells &
                              data$param_name %in% analytes, ]
      
      t1 <- ind_by_param_grid(data_selected)
      
      if (input$date_lines_const){
        # create separate data.frame for geom_rect data
        # change dates to POSIXct which is same as MANAGES database dates
        b1 <- min(as.POSIXct(input$back_date_range_time_const, 
                             format = "%Y-%m-%d"))
        c1 <- min(as.POSIXct(input$comp_date_range_time_const, 
                             format = "%Y-%m-%d"))
        b2 <- max(as.POSIXct(input$back_date_range_time_const, 
                             format = "%Y-%m-%d"))
        c2 <- max(as.POSIXct(input$comp_date_range_time_const, 
                             format = "%Y-%m-%d"))
        
        t1 <- ind_by_param_grid(data_selected, back_date = c(b1, b2), 
                                comp_date = c(c1, c2))
      }
      if (input$short_name_const){
        t1 <- ind_by_param_grid(data_selected, name="short")
      }
      if (input$short_name_const & input$date_lines_const){
        # create separate data.frame for geom_rect data
        # change dates to POSIXct which is same as MANAGES database dates
        b1 <- min(as.POSIXct(input$back_date_range_time_const, 
                             format = "%Y-%m-%d"))
        c1 <- min(as.POSIXct(input$comp_date_range_time_const, 
                             format = "%Y-%m-%d"))
        b2 <- max(as.POSIXct(input$back_date_range_time_const,
                             format = "%Y-%m-%d"))
        c2 <- max(as.POSIXct(input$comp_date_range_time_const, 
                             format = "%Y-%m-%d"))
        
        t1 <- ind_by_param_grid(data_selected, back_date = c(b1, b2), 
                                comp_date = c(c1, c2), name = "short")
      }
      t1
    }
  })
  
  output$ts_by_const <- renderPlot({
    ts_by_const_out()
  })
  
  # End Time Series by constituent page ---------------------------------------
  
  # Begin Piper Diagram Page---------------------------------------------------
  output$wells_piper <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      well_names <- as.character(get_wells(data))
      selectInput("well_piper", "Monitoring Wells", well_names, 
                  multiple = TRUE, selected = well_names[1])
    }
  })
  
  output$date_ranges_piper <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      dateRangeInput("date_range_piper", "Date Range", 
                     start = min(data$sample_date, na.rm = TRUE), 
                     end = max(data$sample_date, na.rm = TRUE))
    }
  })
  
#   # gather major ions and conver for Piper diagram
#   output$piper_data <- reactive({
#     if (!is.null(input$manages_path)){
#       data <- get_data()
#       # get the major cations/anions
#       start <- min(as.POSIXct(input$date_range_piper, format = "%Y-%m-%d"))
#       end <- max(as.POSIXct(input$date_range_piper, format = "%Y-%m-%d"))
#       wells <- input$well_piper
#       data_selected <- data[data$location_id %in% wells &
#                             data$sample_date >= start & 
#                             data$sample_date <= end, ]      
#       ions <- get_major_ions(data_selected, Mg=input$Mg, Ca=input$Ca,
#                              Na=input$Na, K=input$K, Cl=input$Cl, 
#                              SO4=input$SO4, Alk=input$Alk, TDS=input$TDS)
#       piper_data <- transform_piper_data(ions, Mg=input$Mg, Ca=input$Ca,
#                                          Na=input$Na, K=input$K, Cl=input$Cl, 
#                                          SO4=input$SO4, Alk=input$Alk, 
#                                          TDS=input$TDS)
#       return(piper_data)
#     } 
#   })

  output$piper_plot <- renderPlot({
   if (!is.null(input$manages_path)){
    data <- get_data()
    start <- min(as.POSIXct(input$date_range_piper, format = "%Y-%m-%d"))
    end <- max(as.POSIXct(input$date_range_piper, format = "%Y-%m-%d"))
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

    piper_plot(piper_data, TDS=input$TDS_plot)
    }
  })
  # End Piper Diagram Page-----------------------------------------------------

  # Begin Stiff Diagram Page --------------------------------------------------
  output$wells_stiff <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      well_names <- as.character(get_wells(data))
      selectInput("well_stiff", "Monitoring Wells", well_names, 
                  multiple = FALSE, selected = well_names[1])
    }
  })
  
  output$date_ranges_stiff <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      dateRangeInput("date_range_stiff", "Date Range", 
                     start = min(data$sample_date, na.rm = TRUE), 
                     end = max(data$sample_date, na.rm = TRUE))
    }
  })

  output$stiff_diagram <- renderPlot({
    if (!is.null(input$manages_path)){
      data <- get_data()
      start <- min(as.POSIXct(input$date_range_stiff, format = "%Y-%m-%d"))
      end <- max(as.POSIXct(input$date_range_stiff, format = "%Y-%m-%d"))
      data_selected <- data[data$location_id %in% input$well_stiff &
                              data$sample_date >= start & 
                              data$sample_date <= end, ]
      ions <- get_major_ions(data_selected, Mg=input$Mg_stiff, 
                             Ca=input$Ca_stiff, Na=input$Na_stiff, 
                             K=input$K_stiff, Cl=input$Cl_stiff, 
                             SO4=input$SO4_stiff, Alk=input$Alk_stiff, 
                             TDS=input$TDS_stiff)
      ions <- ions[complete.cases(ions), ]
      plot_data <- convert_mgL_to_meqL(ions, Mg=input$Mg_stiff, 
                                       Ca=input$Ca_stiff, Na=input$Na_stiff, 
                                       K=input$K_stiff, Cl=input$Cl_stiff, 
                                       SO4=input$SO4_stiff, HCO3=input$Alk_stiff)
      stiff_data <- transform_stiff_data(plot_data, Mg=input$Mg_stiff, 
                                         Ca=input$Ca_stiff, Na=input$Na_stiff, 
                                         K=input$K_stiff, Cl=input$Cl_stiff,
                                         SO4=input$SO4_stiff, 
                                         HCO3=input$Alk_stiff, 
                                         TDS=input$TDS_stiff)
      stiff_plot(stiff_data, TDS=input$TDS_plot_stiff)
    } 
  })
  # End Stiff Diagram Page ----------------------------------------------------
  
  # Begin Prediction Limits ---------------------------------------------------
  output$wells_upl <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      well_names <- as.character(get_wells(data))
      selectInput("well_upl", "Monitoring Wells", well_names, 
                  multiple = FALSE,
                  selected = well_names[1])
    }
  })

  # return a list of constituents for time series page
  output$analytes_upl <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      analyte_names <- as.character(get_analytes(data))
      selectInput("analyte_upl", "Constituents", analyte_names, 
                  multiple = FALSE,
                  selected = analyte_names[1])
    }
  })

  # return start and end date of background data for time series page
  output$date_ranges_upl <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      tagList(
        dateRangeInput("back_date_range_upl", "Background Date Range", 
                       start = min(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE))
      )
    }
  })

  bkgd_data <- reactive({
    if (!is.null(input$manages_path)){
      df <- get_data()
      start <- min(as.POSIXct(input$back_date_range_upl, format = "%Y-%m-%d"))
      end <- max(as.POSIXct(input$back_date_range_upl, format = "%Y-%m-%d"))
      data_selected <- subset(df, location_id %in% input$well_upl &
                                param_name %in% input$analyte_upl &
                                sample_date >= start & sample_date <= end)
      data_selected
    }
  })

  output$gof <- renderPlot({
    if (!is.null(input$manages_path)){
      df <- bkgd_data()
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
    }
  })

  output$upl <- renderPrint({
    if (!is.null(input$manages_path)){
      
      df <- bkgd_data()
      nw <- input$nw
      nc <- input$nc
      swfpr <- input$swfpr
      conf.level = (1 - swfpr)^(1/(nc*nw))
      
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
    }
  })
  # End Prediction Limits -----------------------------------------------------

  # Begin NADA Page -----------------------------------------------------------

  # ROS
  output$ros_wells <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      well_names <- as.character(get_wells(data))
      selectInput("ros_well", "Monitoring Wells", well_names, 
                  multiple = FALSE,
                  selected = well_names[1])
    }
  })

  output$ros_analytes <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      analyte_names <- as.character(get_analytes(data))
      selectInput("ros_analyte", "Constituents", analyte_names, 
                  multiple = FALSE,
                  selected = analyte_names[1])
    }
  })

  output$ros_date_ranges <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      tagList(
        dateRangeInput("bkgd_date_range_ros", "Background Date Range", 
                       start = min(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE)) 
      )
    }
  })
  output$ros_summary_table <- renderDataTable({
    if (!is.null(input$manages_path)){
      data <- get_data()
      wells <- input$ros_well
      const <- input$ros_analyte
      data <- data[data$location_id %in% wells &
                  data$param_name %in% const, ]
      start <- min(lubridate::ymd(input$bkgd_date_range_ros))
      end <- max(lubridate::ymd(input$bkgd_date_range_ros))
      gw_summary(data, start, end)
    }
  }, options = list(sScrollY = "100%", sScrollX = "100%", 
                    aLengthMenu = c(5, 10, 15, 25, 50, 100), 
                    iDisplayLength = 10)
  )

  output$ros_out <- renderPrint({
    if (!is.null(input$manages_path)){
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
    }
  })
  
    output$ros_out_2 <- renderPrint({
    if (!is.null(input$manages_path)){
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
    }
  })
  
  output$ros_plot <- renderPlot({
    if (!is.null(input$manages_path)){
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
  }
})
  # End ROS
  
  # begin Kaplan-Meier
  output$kp_wells <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      well_names <- as.character(get_wells(data))
      selectInput("kp_well", "Monitoring Wells", well_names, 
                  multiple = FALSE,
                  selected = well_names[1])
    }
  })

  output$kp_analytes <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      analyte_names <- as.character(get_analytes(data))
      selectInput("kp_analyte", "Constituents", analyte_names, 
                  multiple = FALSE,
                  selected = analyte_names[1])
    }
  })
  # End Kaplan-Meier
  # End NADA Page -------------------------------------------------------------

})
