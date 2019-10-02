shinyServer(function(input, output, session) {

  # Data Table -----------------------------------------------------------------
  select_data <- callModule(selectData, "select_data", multiple = TRUE)

  get_data <- reactive({
    input$run_query
    data <- isolate(select_data())
    return(data)
  })

  output$data_table <- renderDataTable({

    data <- get_data()

    data

  }, options = list(scrollY = "100%", scrollX = "100%",
                    lengthMenu = c(5, 10, 15, 25, 50, 100),
                    pageLength = 10)
  )

  output$data_table_download <- downloadHandler(
    filename = function() {
      paste0("manager_export_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(get_data(), file, row.names = FALSE)
    }
  )
  # End Data Table -------------------------------------------------------------

  # Summary table --------------------------------------------------------------
  output$summary_table <- renderDataTable({
    manager::summary(get_data())
  }, options = list(scrollY = "100%", scrollX = "100%",
                    lengthMenu = c(5, 10, 15, 25, 50, 100),
                    pageLength = 10)
  )

  output$summary_table_download <- downloadHandler(
    filename =  paste0("manager_export_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(summary(get_data()), file, row.names = FALSE)
    }
  )
  # End Summary table ----------------------------------------------------------

  # Cast to wide data table ----------------------------------------------------
  output$wide_table <- renderDataTable({
    to_wide(get_data(), lab_id = TRUE)
  })

  # Map ------------------------------------------------------------------------
  output$mapplot <- renderLeaflet({
    
    map_data <- to_spatial(get_data(), crs = 4326)
    
    m <- mapview(map_data, map.types = c("Esri.WorldImagery", "OpenTopoMap",
                                         "OpenStreetMap"))
    
    m@map
    
  })

  # End Map --------------------------------------------------------------------

  # Begin Distribution Plots ---------------------------------------------------
  output$select_distribution_wells <- renderUI({

    data <- get_data()
    well_names <- as.character(sample_locations(data))
    selectInput("dist_well", "Monitoring Wells", well_names,
                multiple = TRUE,
                selected = well_names[1])
  })

  output$select_distribution_params <- renderUI({

    data <- get_data()
    analyte_names <- as.character(constituents(data))
    selectInput("dist_param", "Constituents", analyte_names,
                multiple = FALSE,
                selected = analyte_names[1])
  })

  get_distribution_data <- reactive({

    df <- get_data()

    df %>%
      filter(location_id %in% input$dist_well,
             param_name %in% input$dist_param)

  })

  output$gof_plot <- renderPlot({

    df <- get_distribution_data()

    if (isTRUE(input$dist_plot_type == "Censored")) {

      df <- df %>% to_censored()
      
      out <- EnvStats::gofTestCensored(

        x = df$analysis_result, censored = df$left_censored,
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

    data <- get_data()

    well_names <- as.character(sample_locations(data))
    selectInput("corr_wells", "Monitoring Wells", well_names,
                multiple = TRUE, selected = well_names[1])
  })

  output$select_corr_params <- renderUI({

    data <- get_data()

    param_names <- as.character(constituents(data))
    selectInput("corr_params", "Constituents", param_names,
                multiple = TRUE, selected = param_names[1])
  })

  output$corr_plot <- renderPlot({

    df <- get_data()

    df %>%
      corr_plot(., sample_locations = c(input$corr_wells),
                constituents = c(input$corr_params))

  })

  # End Correlation Plots ------------------------------------------------------

  # Begin Boxplot Page----------------------------------------------------------
  output$select_boxplot_wells <- renderUI({
    
    data <- get_data()
    well_names <- as.character(sample_locations(data))
    selectInput("boxplot_well", "Monitoring Wells", well_names,
                multiple = TRUE,
                selected = well_names)
  })

  output$select_boxplot_params <- renderUI({
    
    data <- get_data()
    analyte_names <- as.character(constituents(data))
    selectInput("boxplot_param", "Constituents", analyte_names,
                multiple = TRUE,
                selected = analyte_names)
  })

  boxplot_react <- reactive({

      box_data <- get_data()
      
      box_data <- box_data %>%
        filter(location_id %in% input$boxplot_well,
               param_name %in% input$boxplot_param)
      
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
              fill = input$boxplot_fill,
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
      boxplot_react()
  })

  # Begin Boxplot Download Page-------------------------------------------------
  download_boxplot <- reactive({

    box_data <- get_data()
    
    box_data <- box_data %>%
      filter(location_id %in% input$boxplot_well,
             param_name %in% input$boxplot_param)

    boxplot(box_data, x = "location_id",
            y = "analysis_result",
            fill = input$boxplot_fill,
            scale_y_trans = input$box_y_transform,
            coef = input$box_iqr_mult,
            show_points = input$box_points,
            pnt = input$box_pnt_size)

  })

  output$box_download <- downloadHandler(
    filename = function() {
      paste0("boxplot_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      pdf(file = file, width = 17, height = 11)
      download_boxplot()
      dev.off()
    }
  )
  # End Boxplot Page------------------------------------------------------------

  # Time Series Page -----------------------------------------------------------
  output$select_ts_wells <- renderUI({
    
    data <- get_data()
    well_names <- as.character(sample_locations(data))
    selectInput("ts_well", "Monitoring Wells", well_names,
                multiple = TRUE,
                selected = well_names)
  })

  output$select_ts_params <- renderUI({

    data <- get_data()
    analyte_names <- as.character(constituents(data))
    selectInput("ts_param", "Constituents", analyte_names,
                multiple = TRUE,
                selected = analyte_names)
  })

  ts_plot_react <- reactive({

    ts_data <- get_data()

    ts_data <- ts_data %>%
      filter(location_id %in% input$ts_well,
             param_name %in% input$ts_param)

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
            ts
          })
        })
      }
    do.call(tagList, ts_list)
  })

  output$ts_out <- renderUI({
      ts_plot_react()
  })
  # Begin Time Series Download Page --------------------------------------------
  get_ts_data <- reactive({

    ts_data <- get_data()
    
    ts_data <- ts_data %>%
      filter(location_id %in% input$ts_well,
             param_name %in% input$ts_param)

  })

  output$ts_download <- downloadHandler(
    filename = function() {
      paste0("ts_plot_", Sys.Date(), ".pdf")
      },
    content = function(file) {
        pdf(file = file, width = 17, height = 11)
        ts_plot(get_ts_data())
        dev.off()

    }
  )
  # End Time Series Download Page ----------------------------------------------
  # End Time Series Page--------------------------------------------------------

  # Begin Piper Diagram Page----------------------------------------------------
  output$select_piper_wells <- renderUI({

    data <- get_data()
    well_names <- as.character(sample_locations(data))
    selectInput("piper_well", "Monitoring Wells", choices = well_names,
                selected = well_names, multiple = TRUE)
  })

  output$select_piper_x_cation <- renderUI({
    data <- get_data()
    x_cation_list <- data %>%
      slice(grep("Calcium", param_name)) %>%
      constituents()
    selectInput("x_cation", "Select X Cation", choices = x_cation_list,
                selected = x_cation_list, multiple = TRUE)
  })

  output$select_piper_y_cation <- renderUI({
    data <- get_data()
    y_cation_list <- data %>%
      slice(grep("Magnesium", param_name)) %>%
      constituents()
    selectInput("y_cation", "Select Y Cation", choices = y_cation_list,
                selected = y_cation_list, multiple = TRUE)
  })

  output$select_piper_z_cation <- renderUI({
    data <- get_data()
    z_cation_list <- data %>%
      slice(grep("Potassium|Sodium", param_name)) %>%
      constituents()
    selectInput("z_cation", "Select Z Cations", choices = z_cation_list,
                selected = z_cation_list, multiple = TRUE)
  })

  output$select_piper_x_anion <- renderUI({
    data <- get_data()
    x_anion_list <- data %>%
      slice(grep("Chloride|Fluoride", param_name)) %>%
      constituents()
    selectInput("x_anion", "Select X Anions", choices = x_anion_list,
                selected = x_anion_list, multiple = TRUE)
  })

  output$select_piper_y_anion <- renderUI({
    data <- get_data()
    y_anion_list <- data %>%
      slice(grep("Alkalinity|Carbonate|Bicarbonate", param_name)) %>%
      constituents()
    selectInput("y_anion", "Select Y Anion", choices = y_anion_list,
                selected = y_anion_list, multiple = TRUE)
  })

  output$select_piper_z_anion <- renderUI({
    data <- get_data()
    z_anion_list <- data %>%
      slice(grep("Sulfate", param_name)) %>%
      constituents()
    selectInput("z_anion", "Select Z Anion", choices = z_anion_list,
                selected = z_anion_list, multiple = TRUE)
  })

  output$select_piper_tds <- renderUI({
    data <- get_data()
    tds_list <- data %>%
      slice(grep("TDS|Total Dissolved Solids", param_name)) %>%
      constituents()
    if (isTRUE(input$TDS_plot)) {
      selectInput("piper_tds", "Total Dissolved Solids", choices = tds_list,
                  multiple = TRUE)
    }

  })

  piper_plot_react <- reactive({

    data <- get_data()
    
    data <- data %>%
      filter(location_id %in% input$piper_well)

    if (isTRUE(input$TDS_plot)) {

     piper_plot(data,
                x_cation = paste(input$x_cation),
                x_cation_label = input$x_cation_label,
                y_cation = paste(input$y_cation),
                y_cation_label = paste(input$y_cation_label),
                z_cation = paste(input$z_cation),
                z_cation_label = paste(input$z_cation_label),
                x_anion = paste(input$x_anion),
                x_anion_label = paste(input$x_anion_label),
                y_anion = paste(input$y_anion),
                y_anion_label = paste(input$y_anion_label),
                z_anion = paste(input$z_anion),
                z_anion_label = paste(input$z_anion_label),
                x_y_cation_label = paste(input$x_y_cation_label),
                x_z_anion_label = paste(input$x_z_anion_label),
                total_dissolved_solids = paste(input$piper_tds),
                title = input$piper_title
                )

     } else { 

     piper_plot(data,
                x_cation = paste(input$x_cation),
                x_cation_label = input$x_cation_label,
                y_cation = paste(input$y_cation),
                y_cation_label = paste(input$y_cation_label),
                z_cation = paste(input$z_cation),
                z_cation_label = paste(input$z_cation_label),
                x_anion = paste(input$x_anion),
                x_anion_label = paste(input$x_anion_label),
                y_anion = paste(input$y_anion),
                y_anion_label = paste(input$y_anion_label),
                z_anion = paste(input$z_anion),
                z_anion_label = paste(input$z_anion_label),
                x_y_cation_label = paste(input$x_y_cation_label),
                x_z_anion_label = paste(input$x_z_anion_label),
                title = input$piper_title
                )
     }
  })

  output$piper_plot <- renderPlot({
    piper_plot_react()
  })

  output$piper_download <- downloadHandler(
    filename = function() {
      paste("piper_plot_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file = file, width = 17, height = 11)
      print(piper_plot_react())
      dev.off()
    }
  )
  # End Piper Diagram Page------------------------------------------------------

  # Begin Stiff Diagram Page ---------------------------------------------------
  output$select_stiff_wells <- renderUI({

    data <- get_data()
    well_names <- as.character(sample_locations(data))
    selectInput("well_stiff", "Monitoring Wells", well_names, 
                multiple = TRUE, selected = well_names[1])
  })

  output$select_stiff_dates <- renderUI({
    
    data <- get_data()
    dateRangeInput("date_range_stiff", "Date Range", 
                   start = min(data$sample_date, na.rm = TRUE), 
                   end = max(data$sample_date, na.rm = TRUE))
  })
  
  output$select_stiff_calcium <- renderUI({
    data <- get_data()
    stiff_calcium_list <- data %>%
      slice(grep("Calcium", param_name)) %>%
      constituents()
    selectInput("calcium_stiff", "Select Calcium", choices = stiff_calcium_list,
                selected = stiff_calcium_list, multiple = TRUE)
  })
  
  output$select_stiff_magnesium <- renderUI({
    data <- get_data()
    stiff_magnesium_list <- data %>%
      slice(grep("Magnesium", param_name)) %>%
      constituents()
    selectInput("magnesium_stiff", "Select Magnesium", choices = stiff_magnesium_list,
                selected = stiff_magnesium_list, multiple = TRUE)
  })
  
  output$select_stiff_potassium <- renderUI({
    data <- get_data()
    stiff_potassium_list <- data %>%
      slice(grep("Potassium", param_name)) %>%
      constituents()
    selectInput("potassium_stiff", "Select Potassium", choices = stiff_potassium_list,
                selected = stiff_potassium_list, multiple = TRUE)
  })
  
  output$select_stiff_sodium <- renderUI({
    data <- get_data()
    stiff_sodium_list <- data %>%
      slice(grep("Sodium", param_name)) %>%
      constituents()
    selectInput("sodium_stiff", "Select Sodium", choices = stiff_sodium_list,
                selected = stiff_sodium_list, multiple = TRUE)
  })
  
  output$select_stiff_chloride <- renderUI({
    data <- get_data()
    stiff_chloride_list <- data %>%
      slice(grep("Chloride", param_name)) %>%
      constituents()
    selectInput("chloride_stiff", "Select Chloride", choices = stiff_chloride_list,
                selected = stiff_chloride_list, multiple = TRUE)
  })
  
  output$select_stiff_sulfate <- renderUI({
    data <- get_data()
    stiff_sulfate_list <- data %>%
      slice(grep("Sulfate", param_name)) %>%
      constituents()
    selectInput("sulfate_stiff", "Select Sulfate", choices = stiff_sulfate_list,
                selected = stiff_sulfate_list, multiple = TRUE)
  })
  
  output$select_stiff_alkalinity <- renderUI({
    data <- get_data()
    stiff_alkalinity_list <- data %>%
      slice(grep("Alkalinity", param_name)) %>%
      constituents()
    selectInput("alkalinity_stiff", "Select Alkalinity", choices = stiff_alkalinity_list,
                selected = stiff_alkalinity_list, multiple = TRUE)
  })
  
  get_stiff_data <- reactive({

    stiff_data <- get_data()

    ions <- c(input$magnesium_stiff, input$calcium_stiff,
              input$sodium_stiff, input$potassium_stiff,
              input$chloride_stiff, input$sulfate_stiff,
              input$alkalinity_stiff, input$tds_stiff)

    start <- min(as.Date(input$date_range_stiff, format = "%Y/%m/%d",
                         tz = "UTC"), na.rm = TRUE)

    end <- max(as.Date(input$date_range_stiff, format = "%Y/%m/%d",
                       tz = "UTC"), na.rm = TRUE)

    stiff_data <- stiff_data %>%
      filter(param_name %in% ions, location_id %in% input$well_stiff,
             sample_date >= start &
               sample_date <= end)

  })

  output$select_stiff_tds <- renderUI({

    if (isTRUE(input$tds_stiff)) {

      selectInput(inputId = "tds_stiff",
                  label = "Total Dissolved Solids", 
                  choices = c("Total Dissolved Solids"))

    }
    
  })

  stiff_diagram <- reactive({
    
    stiff_data <- get_stiff_data()
    
    stiff_locations <- sample_locations(stiff_data)
    stiff_dates <- unique(stiff_data$sample_date)
    
    if (input$stiff_group == 'location_id') {
      
      stiff_list <- lapply(seq_along(stiff_locations), function(i) {
        stiff_name <- paste("stiff_plot", i, sep = "")
        plotOutput(stiff_name)
      })

      for (i in seq_along(stiff_locations)) {
        local({
          stiff_i <- i
          stiff_name <- paste("stiff_plot", stiff_i, sep = "")
          output[[stiff_name]] <- renderPlot({

            if (isTRUE(input$TDS_stiff)) {

              stiff <- stiff_plot(
                stiff_data[stiff_data$location_id == 
                             stiff_locations[stiff_i], ],
                magnesium = paste(input$magnesium_stiff),
                calcium = paste(input$calcium_stiff),
                sodium = paste(input$sodium_stiff),
                potassium = paste(input$potassium_stiff),
                chloride = paste(input$chloride_stiff),
                sulfate = paste(input$sulfate_stiff),
                alkalinity = paste(input$alkalinity_stiff),
                total_dissolved_solids = paste(input$tds_stiff),
                group_var = "location_id",
                facet_var = "sample_date",
                lines = input$stiff_lines
              )
            } else {
              stiff <- stiff_plot(
                stiff_data[stiff_data$location_id ==
                             stiff_locations[stiff_i], ],
                magnesium = paste(input$magnesium_stiff),
                calcium = paste(input$calcium_stiff),
                sodium = paste(input$sodium_stiff),
                potassium = paste(input$potassium_stiff),
                chloride = paste(input$chloride_stiff),
                sulfate = paste(input$sulfate_stiff),
                alkalinity = paste(input$alkalinity_stiff),
                group_var = "location_id",
                facet_var = "sample_date",
                lines = input$stiff_lines
              )
            }
            stiff
          })
        })
      }
    }

    if (input$stiff_group == 'sample_date') {
      stiff_list <- lapply(seq_along(stiff_dates), function(i) {
        stiff_name <- paste("stiff_plot", i, sep = "")
        plotOutput(stiff_name)
      })

      for (i in seq_along(stiff_dates)) {
        local({
          stiff_i <- i
          stiff_name <- paste("stiff_plot", stiff_i, sep = "")
          output[[stiff_name]] <- renderPlot({

            if (isTRUE(input$tds_stiff)) {

              stiff <- stiff_plot(
                stiff_data[stiff_data$sample_date == 
                             stiff_dates[stiff_i], ],
                magnesium = paste(input$magnesium_stiff),
                calcium = paste(input$calcium_stiff),
                sodium = paste(input$sodium_stiff),
                potassium = paste(input$potassium_stiff),
                chloride = paste(input$chloride_stiff),
                sulfate = paste(input$sulfate_stiff),
                alkalinity = paste(input$alkalinity_stiff),
                total_dissolved_solids = paste(input$tds_stiff),
                group_var = "sample_date",
                facet_var = "location_id",
                lines = input$stiff_lines
              )
            } else {
              stiff <- stiff_plot(
                stiff_data[stiff_data$sample_date ==
                             stiff_dates[stiff_i], ],
                magnesium = paste(input$magnesium_stiff),
                calcium = paste(input$calcium_stiff),
                sodium = paste(input$sodium_stiff),
                potassium = paste(input$potassium_stiff),
                chloride = paste(input$chloride_stiff),
                sulfate = paste(input$sulfate_stiff),
                alkalinity = paste(input$alkalinity_stiff),
                group_var = "sample_date",
                facet_var = "location_id",
                lines = input$stiff_lines
              )
            }
            stiff
          })
        })
      }
    }
    do.call(tagList, stiff_list)
  })

  output$stiff_diagram <- renderUI({
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

    data <- get_data()
    well_names <- as.character(sample_locations(data))
    selectInput("well_schoeller", "Monitoring Wells", well_names, 
                multiple = TRUE, selected = well_names[1])
  })

  output$select_schoeller_dates <- renderUI({
    
    data <- get_data()
    dateRangeInput("date_range_schoeller", "Date Range", 
                   start = min(data$sample_date, na.rm = TRUE), 
                   end = max(data$sample_date, na.rm = TRUE))
  })

  output$select_schoeller_calcium <- renderUI({
    data <- get_data()
    calcium_list <- data %>%
      slice(grep("Calcium", param_name)) %>%
      constituents()
    selectInput("schoeller_calcium", "Select Calcium", choices = calcium_list,
                selected = calcium_list, multiple = TRUE)
  })
  
  output$select_schoeller_magnesium <- renderUI({
    data <- get_data()
    magnesium_list <- data %>%
      slice(grep("Magnesium", param_name)) %>%
      constituents()
    selectInput("schoeller_magnesium", "Select Magnesium", choices = magnesium_list,
                selected = magnesium_list, multiple = TRUE)
  })
  
  output$select_schoeller_potassium <- renderUI({
    data <- get_data()
    potassium_list <- data %>%
      slice(grep("Potassium", param_name)) %>%
      constituents()
    selectInput("schoeller_potassium", "Select Potassium", choices = potassium_list,
                selected = potassium_list, multiple = TRUE)
  })
  
  output$select_schoeller_sodium <- renderUI({
    data <- get_data()
    sodium_list <- data %>%
      slice(grep("Sodium", param_name)) %>%
      constituents()
    selectInput("schoeller_sodium", "Select Sodium", choices = sodium_list,
                selected = sodium_list, multiple = TRUE)
  })
  
  output$select_schoeller_chloride <- renderUI({
    data <- get_data()
    chloride_list <- data %>%
      slice(grep("Chloride", param_name)) %>%
      constituents()
    selectInput("schoeller_chloride", "Select Chloride", choices = chloride_list,
                selected = chloride_list, multiple = TRUE)
  })
  
  output$select_schoeller_sulfate <- renderUI({
    data <- get_data()
    sulfate_list <- data %>%
      slice(grep("Sulfate", param_name)) %>%
      constituents()
    selectInput("schoeller_sulfate", "Select Sulfate", choices = sulfate_list,
                selected = sulfate_list, multiple = TRUE)
  })
  
  output$select_schoeller_alkalinity <- renderUI({
    data <- get_data()
    alkalinity_list <- data %>%
      slice(grep("Alkalinity", param_name)) %>%
      constituents()
    selectInput("schoeller_alkalinity", "Select Alkalinity", choices = alkalinity_list,
                selected = alkalinity_list, multiple = TRUE)
  })
  
  get_schoeller_data <- reactive({

    data <- get_data()

    start <- min(as.Date(input$date_range_schoeller, format = "%Y/%m/%d",
                         tz = "UTC"))
    end <- max(as.Date(input$date_range_schoeller, format = "%Y/%m/%d",
                       tz = "UTC"))

    data_selected <- data %>%
      filter(location_id %in% input$well_schoeller &
             sample_date >= start & 
             sample_date <= end)

    data_selected

  })

  schoeller_plot_react <- reactive({

    data <- get_schoeller_data()

    data %>%
      schoeller_plot(magnesium = paste(input$magnesium_schoeller),
                     calcium = paste(input$schoeller_calcium),
                     sodium = paste(input$sodium_schoeller),
                     potassium = paste(input$potassium_schoeller),
                     chloride = paste(input$chloride_schoeller),
                     sulfate = paste(input$sulfate_schoeller),
                     alkalinity = paste(input$alkalinity_schoeller), 
                     facet_var = input$facet_schoeller,
                     title = input$schoeller_title)
  })

  output$schoeller_diagram_out <- renderPlot({
    schoeller_plot_react()
  })

  output$schoeller_download <- downloadHandler(
    filename = function() {
      paste("schoeller_plot_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file = file, width = 17, height = 11)
      schoeller_plot_react()
      dev.off()
    }
  )
  # End Schoeller Diagram Page--------------------------------------------------

  # Begin outlier detecion -----------------------------------------------------
  output$outlier_wells <- renderUI({

    data <- get_data()

    well_names <- as.character(sample_locations(data))

    selectInput("outlier_well", "Monitoring Well", well_names,
                multiple = FALSE,
                selected = well_names[1])

  })

  output$outlier_analytes <- renderUI({

    data <- get_data()

    analyte_names <- as.character(constituents(data))

    selectInput("outlier_analyte", "Constituent", analyte_names, 
                multiple = FALSE,
                selected = analyte_names[1])

  })
  
  output$outlier_date_ranges <- renderUI({

    data <- get_data()

    tagList(

      dateRangeInput("outlier_date_range", "Date Range", 
                     start = min(data$sample_date, na.rm = TRUE),
                     end = max(data$sample_date, na.rm = TRUE))

    )

  })

  get_outlier_data <- reactive({

    df <- get_data()

    start <- min(as.Date(input$outlier_date_range, format = "%Y/%m/%d",
                         tz = "UTC"), na.rm = TRUE)

    end <- max(as.Date(input$outlier_date_range, format = "%Y/%m/%d",
                       tz = "UTC"), na.rm = TRUE)

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
  # End outlier detection ------------------------------------------------------

  # Begin trend analysis -------------------------------------------------------
  output$trend_wells <- renderUI({

    data <- get_data()
    well_names <- as.character(sample_locations(data))
    selectInput("trend_well", "Monitoring Well", well_names,
                multiple = FALSE,
                selected = well_names[1])
  })

  output$trend_analytes <- renderUI({

    data <- get_data()
    analyte_names <- as.character(constituents(data))
    selectInput("trend_analyte", "Constituent", analyte_names,
                multiple = FALSE,
                selected = analyte_names[1])
  })

  output$trend_date_ranges <- renderUI({

    data <- get_data()

    tagList(

      dateRangeInput("trend_date_range", "Date Range",
                     start = min(data$sample_date, na.rm = TRUE),
                     end = max(data$sample_date, na.rm = TRUE))
    )
  })

  get_trend_data <- reactive({

    df <- get_data()

    start <- min(as.Date(input$trend_date_range, format = "%Y/%m/%d",
                         tz = "UTC"), na.rm = TRUE)

    end <- max(as.Date(input$trend_date_range,format = "%Y/%m/%d",
                       tz = "UTC"), na.rm = TRUE)

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

  # Begin Confidence Intervals -------------------------------------------------
  output$select_conf_int_wells <- renderUI({

    data <- get_data()
    well_names <- as.character(sample_locations(data))
    selectInput("conf_int_wells", "Monitoring Wells", well_names, 
                multiple = TRUE,
                selected = well_names[1])
  })

  output$select_conf_int_analytes <- renderUI({
    data <- get_data()
    analyte_names <- as.character(constituents(data))
    selectInput("conf_int_analytes", "Constituents", analyte_names, 
                multiple = TRUE,
                selected = analyte_names[1])
  })

  output$select_conf_int_date_range <- renderUI({
    data <- get_data()
    tagList(
      dateRangeInput("conf_int_dates", "Background Date Range", 
                     start = min(data$sample_date, na.rm = TRUE),
                     end = max(data$sample_date, na.rm = TRUE))
    )
  })

  conf_int <- reactive({

    df <- get_data()
    
    df <- df %>%
      filter(location_id %in% input$conf_int_wells,
             param_name %in% input$conf_int_analytes)

    start <- min(as.Date(input$conf_int_dates, format = "%Y/%m/%d", tz = "UTC"))
    end <- max(as.Date(input$conf_int_dates, format = "%Y/%m/%d",tz = "UTC"))

    df <- df %>%
      filter(sample_date >= start & sample_date <= end)

    # first group data by location, param, and background
    # estimate percent less than
    df <- df %>%
      group_by(location_id, param_name, default_unit) %>%
      percent_lt() %>%
      est_dist(., keep_data_object = TRUE) %>%
      arrange(location_id, param_name)

    conf_int <- df %>%
      mutate(conf_int = case_when(
        distribution == "Normal" ~ map(.x=data,
                                       ~enorm(
                                         x = .x$analysis_result,
                                         ci = TRUE, 
                                         ci.type = input$conf_int_type,
                                         conf.level = input$conf_int_conf,
                                         ci.param = "mean")
                                       ),
        distribution == "Lognormal" ~ map(.x = data,
                                          ~elnormAlt(
                                            x = .x$analysis_result,
                                            ci = TRUE,
                                            ci.type = input$conf_int_type,
                                            ci.method = "land",
                                            conf.level = input$conf_int_conf)
                                          ),
        distribution == "Nonparametric" ~ map(.x = data,
                                              ~eqnpar(
                                                x = .x$analysis_result,
                                                ci = TRUE,
                                                ci.type = input$conf_int_type,
                                                ci.method = "interpolate",
                                                approx.conf.level = input$conf_int_conf)
                                              )
      )
      )

    conf_int %>%
      mutate(distribution = distribution,
             sample_size = map(.x = conf_int, ~ .x$sample.size),
             lcl = map(.x = conf_int, ~ round(.x$interval$limits["LCL"], 3)),
             ucl = map(.x = conf_int, ~ .x$interval$limits["UCL"]),
             conf_level = map(.x = conf_int, ~ .x$interval$conf.level)) %>%
      select(-data, -conf_int) %>%
      unnest()

  })

  output$conf_int_out <- renderDataTable({
    
    conf_int()

  })
  # End Confidence Intervals ---------------------------------------------------
  
  # Begin Tolerance Intervals --------------------------------------------------
  output$select_tol_int_wells <- renderUI({

    data <- get_data()
    well_names <- as.character(sample_locations(data))
    selectInput("tol_int_wells", "Monitoring Wells", well_names, 
                multiple = TRUE,
                selected = well_names[1])
  })

  output$select_tol_int_analytes <- renderUI({
    data <- get_data()
    analyte_names <- as.character(constituents(data))
    selectInput("tol_int_analytes", "Constituents", analyte_names, 
                multiple = TRUE,
                selected = analyte_names[1])
  })

  output$select_tol_int_date_range <- renderUI({
    data <- get_data()
    tagList(
      dateRangeInput("tol_int_dates", "Background Date Range", 
                     start = min(data$sample_date, na.rm = TRUE),
                     end = max(data$sample_date, na.rm = TRUE))
    )
  })

  tol_int <- reactive({

    df <- get_data()

    df <- df %>%
      filter(location_id %in% input$tol_int_wells,
             param_name %in% input$tol_int_analytes)

    start <- min(as.Date(input$tol_int_dates, format = "%Y/%m/%d", tz = "UTC"))
    end <- max(as.Date(input$tol_int_dates, format = "%Y/%m/%d", tz = "UTC"))

    df <- df %>%
      filter(sample_date >= start & sample_date <= end)

    # first group data by location, param, and background
    # estimate percent less than
    df <- df %>%
      group_by(param_name, default_unit) %>%
      est_dist(., keep_data_object = TRUE, group_by_location = TRUE)

    tol_int <- df %>%
      filter(param_name != "pH (field)") %>%
      mutate(tol_int = case_when(
        distribution == "Normal" ~ map(.x=data,
                                       ~tolIntNorm(
                                         x = .x$analysis_result,
                                         coverage = 0.99,
                                         cov.type = "content",
                                         method = "exact",
                                         ti.type = "upper",
                                         conf.level = 0.95)
        ),
        distribution == "Lognormal"  ~ map(.x = data,
                                           ~tolIntLnormAlt(
                                             x = .x$analysis_result,
                                             coverage = 0.99,
                                             cov.type = "content",
                                             ti.type = "upper",
                                             conf.level = 0.95,
                                             method = "exact",
                                             est.method = "mvue"
                                             )
        ),
        distribution == "Nonparametric" ~ map(.x = data,
                                              ~tolIntNpar(
                                                x = .x$analysis_result,
                                                cov.type = "content",
                                                coverage = 0.99,
                                                ti.type = "upper")
        )
      )
      )

    tol_int_pH <- df %>%
      filter(param_name == "pH (field)") %>%
      mutate(tol_int = case_when(
        distribution == "Normal"  ~ map(.x=data,
                                        ~tolIntNorm(
                                          x = .x$analysis_result,
                                          coverage = 0.99,
                                          cov.type = "content",
                                          method = "exact",
                                          ti.type = "two-sided",
                                          conf.level = 0.95)
        ),
        distribution == "Lognormal" ~ map(.x=data,
                                          ~tolIntLnormAlt(
                                            x = .x$analysis_result,
                                            coverage = 0.99,
                                            ti.type = "two-sided",
                                            cov.type = "content",
                                            method = "exact",
                                            est.method = "mvue")
        ),
        distribution == "Nonparametric" ~ map(.x=data,
                                              ~tolIntNpar(
                                                x = .x$analysis_result,
                                                cov.type = "content",
                                                coverage = 0.99,
                                                ti.type = "two-sided")
        )
      )
      )

    tol_int <- rbind(tol_int, tol_int_pH)

    tol_int %>%
      mutate(distribution = distribution,
             sample_size = map(.x = tol_int, ~ .x$sample.size),
             method = map(.x = tol_int,  ~ tolower(.x$interval$method)),
             ltl = map(.x = tol_int, ~ .x$interval$limits["LTL"]),
             utl = map(.x = tol_int, ~ round(.x$interval$limits["UTL"], 3)),
             conf_level = map(.x = tol_int, ~ .x$interval$conf.level*100)) %>%
      select(-data, -tol_int) %>%
      unnest() %>%
      arrange(param_name)

  })

  output$tol_int_out <- renderDataTable({

    tol_int()

  })
  # End Tolerance Intervals ----------------------------------------------------

  # Begin Intrawell Prediction Limits-------------------------------------------
  output$wells_intra <- renderUI({

      data <- get_data()
      well_names <- as.character(sample_locations(data))
      selectInput("well_intra", "Monitoring Wells", well_names, 
                  multiple = TRUE,
                  selected = well_names[1])
  })

  output$analytes_intra <- renderUI({
      data <- get_data()
      analyte_names <- as.character(constituents(data))
      selectInput("analyte_intra", "Constituents", analyte_names, 
                  multiple = TRUE,
                  selected = analyte_names[1])
  })

  output$date_ranges_intra <- renderUI({
      data <- get_data()
      tagList(
        dateRangeInput("back_dates_intra", "Background Date Range", 
                       start = min(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE))
      )
  })

  intra_limit <- reactive({

    df <- get_data()

    df <- df %>%
      filter(location_id %in% input$well_intra,
             param_name %in% input$analyte_intra)

    start <- min(as.Date(input$back_dates_intra, format = "%Y/%m/%d", tz = "UTC"))
    end <- max(as.Date(input$back_dates_intra, format = "%Y/%m/%d", tz = "UTC"))

    df <- df %>%
      filter(sample_date >= start & sample_date <= end)

    # first group data by location, param, and background
    # estimate percent less than
    df <- df %>%
      group_by(location_id, param_name, default_unit) %>%
      percent_lt() %>%
      est_dist(., keep_data_object = TRUE) %>%
      arrange(location_id, param_name)

    pred_int <- df %>%
      filter(param_name != "pH (field)") %>%
      mutate(pred_int = case_when(
        distribution == "Normal" ~ map(.x=data,
                                       ~predIntNormSimultaneous(
                                         x = .x$analysis_result,
                                         n.mean = input$intra_n.mean,
                                         k = input$intra_k,
                                         m = input$intra_m,
                                         r = input$intra_r,
                                         rule = input$intra_rule,
                                         pi.type = input$intra_pi.type,
                                         conf.level = input$intra_conf
                                         )
        ),
        distribution == "Lognormal"  ~ map(.x = data,
                                           ~predIntLnormAltSimultaneous(
                                             x = .x$analysis_result,
                                             n.geomean = input$intra_n.mean,
                                             k = input$intra_k,
                                             m = input$intra_m,
                                             r = input$intra_r,
                                             rule = input$intra_rule,
                                             pi.type = input$intra_pi.type,
                                             conf.level = input$intra_conf
                                             )
        ),
        distribution == "Nonparametric" ~ map(.x = data,
                                              ~predIntNpar(
                                                x = .x$analysis_result,
                                                pi.type = input$intra_pi.type
                                                )
        )
      )
      )

    pred_int_pH <- df %>%
      filter(param_name == "pH (field)") %>%
      mutate(pred_int = case_when(
        distribution == "Normal"  ~ map(.x=data,
                                        ~predIntNormSimultaneous(
                                          x = .x$analysis_result,
                                          n.mean = input$intra_n.mean,
                                          k = input$intra_k,
                                          m = input$intra_m,
                                          r = input$intra_r,
                                          rule = input$intra_rule,
                                          pi.type = "two-sided",
                                          conf.level = input$intra_conf
                                          )
                                        ),
        distribution == "Lognormal" ~ map(.x=data,
                                          ~predIntLnormAltSimultaneous(
                                            x = .x$analysis_result,
                                            n.geomean = input$intra_n.mean,
                                            k = input$intra_k,
                                            m = input$intra_m,
                                            r = input$intra_r,
                                            rule = input$intra_rule,
                                            pi.type = "two-sided",
                                            conf.level = input$intra_conf
                                            )
                                          ),
        distribution == "Nonparametric" ~ map(.x=data,
                                              ~predIntNpar(
                                                x = .x$analysis_result,
                                                pi.type = "two-sided")
                                              )
      )
      )

    pred_int <- rbind(pred_int, pred_int_pH)

    pred_int <- pred_int %>%
      mutate(distribution = distribution,
             sample_size = map(.x = pred_int, ~ .x$sample.size),
             method = map(.x = pred_int,  ~ tolower(.x$interval$method)),
             lpl = map(.x = pred_int, ~ .x$interval$limits["LPL"]),
             upl = map(.x = pred_int, ~ round(.x$interval$limits["UPL"], 3)),
             conf_level = map(.x = pred_int, ~ .x$interval$conf.level*100)) %>%
      select(-data, -pred_int) %>%
      unnest() %>%
      arrange(location_id, param_name)

    pred_int <- pred_int %>%
      mutate(lpl = if_else(lpl == 0, -Inf, lpl, missing = lpl))

    return(pred_int)

  })

  output$intra_limit_out <- renderDataTable({

    intra_limit()

  })

  # Begin Intrawell prediction interval time series plots ----------------------
  ts_intra_plot <- reactive({

    df <- get_data()

    ts_data <- df %>%
      filter(location_id %in% input$well_intra,
             param_name %in% input$analyte_intra)

    ts_params <- constituents(ts_data)
    ts_wells <- sample_locations(ts_data)

    start <- min(as.Date(input$back_dates_intra, format = "%Y/%m/%d", tz = "UTC"))
    end <- max(as.Date(input$back_dates_intra, format = "%Y/%m/%d", tz = "UTC"))

    intra_limit_data <- intra_limit()

    ts_data <- ts_data %>%
      left_join(intra_limit_data,
                by = c("location_id", "param_name", "default_unit"))
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
            ts_data[ts_data$param_name == ts_params[ts_i], ],
            background = c(start, end),
            limit1 = "lpl",
            limit2 = "upl"
          )
          ts
        })
      })
    }
    do.call(tagList, ts_list)
  })

  output$ts_intra_out <- renderUI({

    ts_intra_plot()

  })
  # End Intrawell time series plots --------------------------------------------

  # Begin Prediction Interval Power Test ---------------------------------------
  output$power_plot <- renderPlot({
    
    plotPredIntNormSimultaneousTestPowerCurve(n = input$power_n,
                                         n.mean = input$power_n_mean,
                                              k = input$power_k,
                                              m = input$power_m,
                                              r = input$power_r,
                                              conf.level = input$conf_power
                                             )

  })

  # End Prediction Interval Power Test -----------------------------------------

  # End Intrawell Prediction Intervals -----------------------------------------

  # Begin Interwell Prediction Intervals ---------------------------------------
  output$select_wells_inter <- renderUI({

    data <- get_data()
    well_names <- as.character(sample_locations(data))
    selectInput("well_inter", "Monitoring Wells", well_names, 
                multiple = TRUE,
                selected = well_names[1])
  })

  output$select_analyte_inter <- renderUI({
    data <- get_data()
    analyte_names <- as.character(constituents(data))
    selectInput("analyte_inter", "Constituents", analyte_names, 
                multiple = TRUE,
                selected = analyte_names[1])
  })

  output$select_date_ranges_inter <- renderUI({
    data <- get_data()
    tagList(
      dateRangeInput("background_inter", "Background Date Range",
                     start = min(data$sample_date, na.rm = TRUE),
                     end = max(data$sample_date, na.rm = TRUE))
    )
  })

  inter_limit <- reactive({

    df <- get_data()

    df <- df %>%
      filter(location_id %in% input$well_inter,
             param_name %in% input$analyte_inter)

    start <- min(as.Date(input$background_inter, format = "%Y/%m/%d", tz = "UTC"))
    end <- max(as.Date(input$background_inter, format = "%Y/%m/%d", tz = "UTC"))

    df <- df %>%
      filter(sample_date >= start, sample_date <= end)

    # first group data by location, param, and background
    # estimate percent less than
    df <- df %>%
      group_by(param_name, default_unit) %>%
      est_dist(., keep_data_object = TRUE, group_by_location = TRUE)


    pred_int <- df %>%
      filter(param_name != "pH (field)") %>%
      mutate(pred_int = case_when(
        distribution == "Normal" ~ map(.x=data,
                                       ~predIntNormSimultaneous(
                                         x = .x$analysis_result,
                                         n.mean = input$inter_n.mean,
                                         k = input$inter_k,
                                         m = input$inter_m,
                                         r = input$inter_r,
                                         rule = input$inter_rule,
                                         pi.type = input$inter_pi.type,
                                         conf.level = input$inter_conf
                                         )
                                       ),
        distribution == "Lognormal"  ~ map(.x = data,
                                           ~predIntLnormAltSimultaneous(
                                             x = .x$analysis_result,
                                             n.geomean = input$inter_n.mean,
                                             k = input$inter_k,
                                             m = input$inter_m,
                                             r = input$inter_r,
                                             rule = input$inter_rule,
                                             pi.type = input$inter_pi.type,
                                             conf.level = input$inter_conf
                                             )
                                           ),
        distribution == "Nonparametric" ~ map(.x = data,
                                              ~predIntNpar(
                                                x = .x$analysis_result,
                                                pi.type = input$inter_pi.type
                                                )
                                              )
      )
    )

    pred_int_pH <- df %>%
      filter(param_name == "pH (field)") %>%
      mutate(pred_int = case_when(
        distribution == "Normal"  ~ map(.x=data,
                                        ~predIntNormSimultaneous(
                                          x = .x$analysis_result,
                                          n.mean = input$inter_n.mean,
                                          k = input$inter_k,
                                          m = input$inter_m,
                                          r = input$inter_r,
                                          rule = input$inter_rule,
                                          pi.type = "two-sided",
                                          conf.level = input$inter_conf
                                          )
                                        ),
        distribution == "Lognormal" ~ map(.x=data,
                                          ~predIntLnormAltSimultaneous(
                                            x = .x$analysis_result,
                                            n.geomean = input$inter_n.mean,
                                            k = input$inter_k,
                                            m = input$inter_m,
                                            r = input$inter_r,
                                            rule = input$inter_rule,
                                            pi.type = "two-sided",
                                            conf.level = input$inter_conf
                                            )
                                          ),
        distribution == "Nonparametric" ~ map(.x=data,
                                              ~predIntNpar(
                                                x = .x$analysis_result,
                                                pi.type = "two-sided"
                                                )
                                              )
      )
    )

    pred_int <- rbind(pred_int, pred_int_pH)

    pred_int %>%
      mutate(distribution = distribution,
             sample_size = map(.x = pred_int, ~ .x$sample.size),
             method = map(.x = pred_int,  ~ tolower(.x$interval$method)),
             lpl = map(.x = pred_int, ~ .x$interval$limits["LPL"]),
             upl = map(.x = pred_int, ~ round(.x$interval$limits["UPL"], 3)),
             conf_level = map(.x = pred_int, ~ .x$interval$conf.level*100)) %>%
      select(-data, -pred_int) %>%
      unnest() %>%
      arrange(param_name)
  })

  output$inter_limit_out <- renderDataTable({

    inter_limit()

  })
  # End Interwell Prediction Limits --------------------------------------------
  # End Prediction Limits ------------------------------------------------------

  # Begin Clustering -----------------------------------------------------------
  output$select_wells_hca <- renderUI({

    data <- get_data()
    well_names <- as.character(sample_locations(data))
    selectInput("well_hca", "Monitoring Wells", well_names, 
                multiple = TRUE,
                selected = well_names)
  })

  output$select_analyte_hca <- renderUI({
    data <- get_data()
    analyte_names <- as.character(constituents(data))
    selectInput("analyte_hca", "Constituents", analyte_names, 
                multiple = TRUE,
                selected = analyte_names)
  })

  output$select_date_ranges_hca <- renderUI({
    data <- get_data()
    tagList(
      dateRangeInput("dates_hca", "Background Date Range",
                     start = min(data$sample_date, na.rm = TRUE),
                     end = max(data$sample_date, na.rm = TRUE))
    )
  })

  hc_plot <- reactive({

    df <- get_data()
    df <- df %>%
      filter(location_id %in% input$well_hca,
             param_name %in% input$analyte_hca)

    start <- min(as.Date(input$dates_hca, format = "%Y/%m/%d", tz = "UTC"))
    end <- max(as.Date(input$dates_hca, format = "%Y/%m/%d", tz = "UTC"))

    df <- df %>%
      filter(sample_date >= start, sample_date <= end)

    df <- df %>%
      name_units() %>%
      group_by(location_id, param_name) %>%
      summarise(analysis_mean = mean(analysis_result, na.rm = TRUE)) %>%
      spread(param_name, analysis_mean) %>%
      na.omit()

    d <- dist(scale(df[, -1]), method = input$clust_dist_method)

    hc_result <- hclust(d, method = "complete")

    dend <- as.dendrogram(hc_result)

    labels(dend) <- df[order.dendrogram(dend), 1][[1]]

    return(dend)

  })

  output$hca_out <- renderPlot({

    fviz_dend(hc_plot(), k = input$hca_colors, horiz = input$hca_horiz) +
      theme(plot.title = element_text(hjust = 0.5))

  })

  # Begin K-means --------------------------------------------------------------
  output$select_wells_kmeans <- renderUI({

    data <- get_data()
    well_names <- as.character(sample_locations(data))
    selectInput("well_kmeans", "Monitoring Wells", well_names, 
                multiple = TRUE,
                selected = well_names)
  })

  output$select_analyte_kmeans <- renderUI({
    data <- get_data()
    analyte_names <- as.character(constituents(data))
    selectInput("analyte_kmeans", "Constituents", analyte_names, 
                multiple = TRUE,
                selected = analyte_names)
  })

  output$select_date_ranges_kmeans <- renderUI({
    data <- get_data()
    tagList(
      dateRangeInput("dates_kmeans", "Background Date Range",
                     start = min(data$sample_date, na.rm = TRUE),
                     end = max(data$sample_date, na.rm = TRUE))
    )
  })

  kmeans_plot <- reactive({
    
    df <- get_data()
    df <- df %>%
      filter(location_id %in% input$well_kmeans,
             param_name %in% input$analyte_kmeans)

    start <- min(as.Date(input$dates_kmeans, format = "%Y/%m/%d", tz = "UTC"))
    end <- max(as.Date(input$dates_kmeans, format = "%Y/%m/%d", tz = "UTC"))

    df <- df %>%
      filter(sample_date >= start, sample_date <= end)

    df <- df %>%
      name_units() %>%
      group_by(location_id, param_name) %>%
      summarise(analysis_mean = mean(analysis_result, na.rm = TRUE)) %>%
      spread(param_name, analysis_mean) %>%
      na.omit()

    df <- as.data.frame(df)
    rownames(df) <- df[, 1]
    df <- df[, -1]

    d <- dist(scale(df), method = input$kmeans_dist_method)

    kmeans_result <- kmeans(d, centers = input$kmeans_centers,
                            algorithm = input$kmeans_algorithm)

    fviz_cluster(kmeans_result, data = df,
                 ellipse.type = "norm", ggtheme = theme_bw()) + 
      theme(plot.title = element_text(hjust = 0.5))

  })

  output$kmeans_out <- renderPlot({

    kmeans_plot()

  })
  # End K-means ----------------------------------------------------------------
  # End Clustering -------------------------------------------------------------
})