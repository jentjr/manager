# change options to handle large file size
options(shiny.maxRequestSize=-1)
# force numbers to be decimal instead of scientific
options(scipen=6, digits = 8)

library(EnvStats)
library(ggvis)

shinyServer(function(input, output, session) {
  # reactive function to upload data
  get_data <- reactive({
    if (!is.null(input$manages_path)){
      switch(input$file_type, 
             ".csv" = from_csv(input$manages_path$datapath),
             ".mdb" = connect_manages(input$manages_path$datapath),
             ".xls" = from_excel(input$manages_path$datapath)) %>%
        arrange(location_id, sample_date, param_name)
    }      
  })
  
  # Output a googleTable of the data to be displayed on Data page
  output$well_table <- renderDataTable({
    if (!is.null(input$manages_path)){
      data <- get_data() 
      return(data)
    }
  }, options = list(sScrollY = "100%", sScrollX = "100%", 
                    aLengthMenu = c(5, 10, 15, 25, 50, 100), 
                    iDisplayLength = 10)
  ) 
  
  # Begin Boxplot Page
  # return a list of well names for boxplot page
  output$wells_box <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      well_names <- as.character(get_well_names(data))
      selectInput("well_box", "Monitoring Wells", well_names, multiple = TRUE,
                  selected = well_names[1])
    }
  })
  
  # return a list of constituents for boxplot page
  output$analytes_box <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      analyte_names <- as.character(get_analytes(data))
      selectInput("analyte_box", "Constituents", analyte_names, multiple = TRUE,
                  selected = analyte_names[1])
    }
  })
  
#   # return start and end date of background data for boxplot page
#   output$date_ranges_box <- renderUI({
#     if (!is.null(input$manages_path)){
#       data <- get_data()
#       tagList(
#         dateRangeInput("back_date_range_box", "Background Date Range", 
#                        start = min(data$sample_date, na.rm = TRUE),
#                        end = max(data$sample_date, na.rm = TRUE)),
#         dateRangeInput("comp_date_range_box", "Compliance Date Range", 
#                        start = max(data$sample_date, na.rm = TRUE),
#                        end = max(data$sample_date, na.rm = TRUE))  
#       )
#     }
#   })
  
  # create boxplots 
  box_out <- reactive({
    if (!is.null(input$manages_path)){
      data <- get_data()
      data_selected <- subset(data, location_id %in% input$well_box & 
                                param_name %in% input$analyte_box)
      data_selected$name_units <- paste(data_selected$param_name,
                              " (", data_selected$default_unit, ")", sep = "")
      
      # box plot of analyte
      b <- ggplot(data_selected, aes(location_id, y=analysis_result, 
                                     fill=location_id)) + 
        theme_bw() + ylab("Analysis Result") + xlab("Location ID") +
        guides(fill = guide_legend("Location ID")) +
        theme(legend.background = element_rect()) + 
        theme(axis.title.x = element_text(vjust=-0.5)) +
        theme(axis.text.x = element_text(angle=90)) +
        theme(axis.title.y = element_text(vjust=0.3)) +
        theme(plot.margin = grid::unit(c(0.75, 0.75, 0.75, 0.75), "in"))
      if(input$scale_plot_box){
        b1 <- b + geom_boxplot()  + facet_wrap(~name_units, scale="free")
      } else{
        b1 <- b + geom_boxplot() + facet_wrap(~name_units)
      }
      if(input$coord_flip_box){
        b1 <- b1 + coord_flip()
      }
      print(b1)
    }
  })
  
  # boxplot output to ui
  output$box_plot <- renderPlot({
    box_out()
  })
  # End Boxplot Page
  
  # Time Series Page
  # return a list of well names for time series page
  output$wells_time <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      well_names <- as.character(get_well_names(data))
      selectInput("well_time", "Monitoring Wells", well_names, 
                  multiple = TRUE,
                  selected = well_names[1])
    }
  })
  
  # return a list of constituents for time series page
  output$analytes_time <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      analyte_names <- as.character(get_analytes(data))
      selectInput("analyte_time", "Constituents", analyte_names, 
                  multiple = TRUE,
                  selected = analyte_names[1])
    }
  })
  
  # return start and end date of background data for time series page
  output$date_ranges_time <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      tagList(
        dateRangeInput("back_date_range_time", "Background Date Range", 
                       start = min(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE)),
        dateRangeInput("comp_date_range_time", "Compliance Date Range", 
                       start = max(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE))  
      )
    }
  })
  
#   # ggvis section for time series
#   vis <- reactive({
#     if (!is.null(input$manages_path)){
#       df <- get_data() 
#       df <- subset(df, location_id == input$well_time &
#             param_name == input$analyte_time)
#       df$non_detect <- ifelse(df$lt_measure == "<", 
#                                 "non-detect", "detected")
#       df %>%
#       dplyr::mutate(param = factor(param_name)) %>%
#       dplyr::mutate(lt = factor(non_detect)) %>%
#       ggvis(x = ~sample_date, y = ~analysis_result, stroke = ~param) %>%
#         layer_points(shape = ~lt) %>%
#         layer_lines() %>%
#         add_axis("x", title = "Sample Date") %>%
#         add_axis("y", title = "Analysis Result", title_offset = 55) %>%
#         add_legend("shape", "Detected")
#     }else{
#       df <- data.frame(x = 1:2, y = 1:1,
#                        labels = c("enter", "data"))
#       df %>% ggvis(~x, ~y, text := ~labels, font = ~labels, fontSize := 40) %>%
#         layer_text() 
#     }
#   })
#   
#   vis %>% bind_shiny("plot1")
   
  # end ggvis time series section
  
  # time series plot output
  ts_out <- reactive({
    if (!is.null(input$manages_path)){
      data <- get_data()
                  
      data_selected <- subset(data, location_id %in% input$well_time & 
                                param_name %in% input$analyte_time)
      
      t1 <- ind_by_loc_grid(data_selected)
              
      if (input$date_lines){
        # create separate data.frame for geom_rect data
        # change dates to POSIXct which is same as MANAGES database dates
        b1 <- min(as.POSIXct(input$back_date_range_time, format = "%Y-%m-%d"))
        c1 <- min(as.POSIXct(input$comp_date_range_time, format = "%Y-%m-%d"))
        b2 <- max(as.POSIXct(input$back_date_range_time, format = "%Y-%m-%d"))
        c2 <- max(as.POSIXct(input$comp_date_range_time, format = "%Y-%m-%d"))
        
        t1 <- ind_by_loc_grid(data_selected, back_date = c(b1, b2), 
                              comp_date = c(c1, c2))
      }
      if (input$short_name){
        t1 <- ind_by_loc_grid(data_selected, name="short")
      }
      if (input$short_name & input$date_lines){
        # create separate data.frame for geom_rect data
        # change dates to POSIXct which is same as MANAGES database dates
        b1 <- min(as.POSIXct(input$back_date_range_time, format = "%Y-%m-%d"))
        c1 <- min(as.POSIXct(input$comp_date_range_time, format = "%Y-%m-%d"))
        b2 <- max(as.POSIXct(input$back_date_range_time, format = "%Y-%m-%d"))
        c2 <- max(as.POSIXct(input$comp_date_range_time, format = "%Y-%m-%d"))
        
        t1 <- ind_by_loc_grid(data_selected, back_date = c(b1, b2), 
                              comp_date = c(c1, c2), name = "short")
      }
      t1
    }
  })
  # output ts plot to ui
  output$time_plot <- renderPlot({
    ts_out()
  })
  # End Time Series page
  
  # Begin Piper Diagram Page
  # return a list of well names
  output$wells_piper <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      well_names <- as.character(get_well_names(data))
      selectInput("well_piper", "Monitoring Wells", well_names, 
                  multiple = TRUE, selected = well_names[1])
    }
  })
  
  # return start and end dates of data
  output$date_ranges_piper <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      dateRangeInput("date_range_piper", "Date Range", 
                     start = min(data$sample_date, na.rm = TRUE), 
                     end = max(data$sample_date, na.rm = TRUE))
    }
  })
  
  # gather major ions and conver for Piper diagram
  output$piper_data <- reactive({
    if (!is.null(input$manages_path)){
      data <- get_data()
      # get the major cations/anions
      start <- min(as.POSIXct(input$date_range_piper, format = "%Y-%m-%d"))
      end <- max(as.POSIXct(input$date_range_piper, format = "%Y-%m-%d"))
      data_selected <- subset(data, location_id %in% input$well_piper &
                                sample_date >= start & sample_date <= end)      
      ions <- get_major_ions(data_selected, Mg=input$Mg, Ca=input$Ca,
                             Na=input$Na, K=input$K, Cl=input$Cl, 
                             SO4=input$SO4, Alk=input$Alk, TDS=input$TDS)
      piper_data <- transform_piper_data(ions, Mg=input$Mg, Ca=input$Ca,
                                         Na=input$Na, K=input$K, Cl=input$Cl, 
                                         SO4=input$SO4, Alk=input$Alk, 
                                         TDS=input$TDS)
      return(piper_data)
    } 
  })

  # piper plot
  output$piper_plot <- renderPlot({
   if (!is.null(input$manages_path)){
    data <- get_data()
    # get the major cations/anions
    start <- min(as.POSIXct(input$date_range_piper, format = "%Y-%m-%d"))
    end <- max(as.POSIXct(input$date_range_piper, format = "%Y-%m-%d"))
    data_selected <- subset(data, location_id %in% input$well_piper &
                              sample_date >= start & sample_date <= end)
    
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
  # End Piper Diagram Page

  # Begin Prediction Limits
  output$wells_upl <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      well_names <- as.character(get_well_names(data))
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
        out <- gofTest(df$analysis_result)
        out["data.name"] <- paste(input$well_upl, input$analyte_upl, sep=" ")
      }
      if (input$int_type == "Lognormal"){
        out <- gofTest(df$analysis_result, distribution = "lnorm")
        out["data.name"] <- paste(input$well_upl, input$analyte_upl, sep=" ")
      }
      if (input$int_type == "Gamma"){
        out <- gofTest(df$analysis_result, distribution = "gamma")
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
        out <- predIntNormSimultaneous(df$analysis_result, k = input$k, 
                                     m = input$m, r = input$r, 
                                     conf.level = conf.level)
        out["data.name"] <- paste(input$well_upl, input$analyte_upl, sep=" ") 
      }
      if (input$int_type == "Lognormal"){
        out <- predIntLnormSimultaneous(df$analysis_result, k = input$k,
                                        m = input$m, r = input$r, 
                                        conf.level = conf.level)
        out["data.name"] <- paste(input$well_upl, input$analyte_upl, sep=" ")
      }
      if (input$int_type == "Gamma"){
        out <- predIntGammaSimultaneous(df$analysis_result, k = input$k,
                                        m = input$m, r = input$r,
                                        conf.level = conf.level)
      }
      if (input$int_type == "Non-parametric"){
        out <- predIntNparSimultaneous(df$analysis_result, k = input$k,
                                       m = input$m, r = input$r)  
        out["data.name"] <- paste(input$well_upl, input$analyte_upl, sep=" ")
      }
      out
    }
  })
  # End Prediction Limits

})