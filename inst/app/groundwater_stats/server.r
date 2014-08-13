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
             ".xls" = from_excel(input$manages_path$datapath))
    }      
  })

  # return a list of well names
  output$wells <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      well_names <- as.character(get_well_names(data))
      selectInput("well", "Monitoring Wells", well_names, multiple = TRUE,
                  selected = well_names[1])
    }
  })
  
  # return a list of constituents
  output$analytes <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      analyte_names <- as.character(get_analytes(data))
      selectInput("analyte", "Constituents", analyte_names, multiple = TRUE,
                  selected = analyte_names[1])
    }
  })
  
  # return start and end date of background data
  output$date_ranges <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      tagList(
        dateRangeInput("back_date_range", "Background Date Range", 
                       start = min(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE)),
        dateRangeInput("comp_date_range", "Compliance Date Range", 
                       start = max(data$sample_date, na.rm = TRUE),
                       end = max(data$sample_date, na.rm = TRUE))  
      )
    }
  })
  
  # Output a googleTable of the data
  output$well_table <- renderDataTable({
    if (!is.null(input$manages_path)){
      data <- get_data()
      return(data)
    }
  }, options = list(sScrollY = "100%", sScrollX = "100%", 
                    aLengthMenu = c(5, 10, 15, 25, 50, 100), 
                    iDisplayLength = 10)
  ) 
  
  # create boxplots 
  box_out <- reactive({
    if (!is.null(input$manages_path)){
      data <- get_data()
      data_selected <- subset(data, location_id %in% input$well & 
                                param_name %in% input$analyte)
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
      if(input$scale_plot){
        b1 <- b + geom_boxplot()  + facet_wrap(~name_units, scale="free")
      } else{
        b1 <- b + geom_boxplot() + facet_wrap(~name_units)
      }
      if(input$coord_flip){
        b1 <- b1 + coord_flip()
      }
      print(b1)
    }
  })
  
  # boxplot output to ui
  output$box_plot <- renderPlot({
    box_out()
  })
  
  # time series plot output
  ts_out <- reactive({
    if (!is.null(input$manages_path)){
      data <- get_data()
      
      data$non_detect <- ifelse(data$lt_measure == "<", 
                                "non-detect", "detected")
      #       df$param_name <- paste(df$short_name, " (", 
      #                              df$default_unit, ")", sep = "")
      
      data_selected <- subset(data, location_id %in% input$well & 
                                param_name %in% input$analyte)
      
      t <- ggplot(data_selected, aes(x=sample_date, y=analysis_result, 
                                     colour = param_name)) 
      
      if(input$scale_plot){
        # time series plot of analytes gridded by wells
        t1 <- t + geom_point(aes(colour=param_name, shape=factor(non_detect), 
                                 size=3)) + 
          geom_line() + 
          facet_wrap(~location_id, scales="free") + 
          theme_bw() + 
          xlab("Sample Date") + 
          scale_x_datetime(labels = scales::date_format("%Y")) +
          ylab("Analysis Result") +
          scale_colour_discrete(name = "Parameter") + 
          theme(axis.title.x = element_text(vjust=-0.3)) +
          theme(axis.text.x = element_text(angle=0)) +
          theme(axis.title.y = element_text(vjust=0.3)) +
          theme(legend.background = element_rect()) + 
          theme(plot.margin = grid::unit(c(0.75, 0.75, 0.75, 0.75), "in")) +
          guides(colour = guide_legend(override.aes = list(linetype = 0, 
                                                           fill = NA)), 
                 shape = guide_legend("Measure", 
                                      override.aes = list(linetype = 0)),
                 size = guide_legend("none"),
                 linetype = guide_legend("Limits")) +
          scale_shape_manual(values = c("non-detect" = 1, "detected" = 16))
      } else {
        t1 <- t + geom_point(aes(colour=param_name, shape=factor(non_detect), 
                                 size=3)) + geom_line() + 
          facet_wrap(~location_id) + 
          theme_bw() + 
          xlab("Sample Date") + 
          scale_x_datetime(labels = scales::date_format("%Y")) +
          ylab("Analysis Result") + 
          scale_colour_discrete(name = "Parameter") +
          theme(axis.title.x = element_text(vjust=-0.3)) +
          theme(axis.text.x = element_text(angle=0)) +
          theme(axis.title.y = element_text(vjust=0.3)) +
          theme(plot.margin = grid::unit(c(0.75, 0.75, 0.75, 0.75), "in")) +
          guides(colour = guide_legend(override.aes = list(linetype = 0, 
                                                           fill = NA)), 
                 shape = guide_legend("Measure", 
                                      override.aes = list(linetype = 0)),
                 size = guide_legend("none"),
                 linetype = guide_legend("Limits")) +
          scale_shape_manual(values = c("non-detect" = 1, "detected" = 16))
      }
      if(input$date_lines){
        # create separate data.frame for geom_rect data
        # change dates to POSIXct which is same as MANAGES database dates
        b1 <- min(as.POSIXct(input$back_date_range, format = "%Y-%m-%d"))
        c1 <- min(as.POSIXct(input$comp_date_range, format = "%Y-%m-%d"))
        b2 <- max(as.POSIXct(input$back_date_range, format = "%Y-%m-%d"))
        c2 <- max(as.POSIXct(input$comp_date_range, format = "%Y-%m-%d"))
        shaded_dates <- data.frame(xmin = c(b1, c1), xmax = c(b2, c2),
                                   ymin = c(-Inf, -Inf), ymax = c(Inf, Inf),
                                   Years = c("background", "compliance"))
        # draw shaded rectangle for background dates and compliance dates
        t1 <- t1 + geom_rect(data = shaded_dates, aes(xmin = xmin, ymin = ymin,
                                                      xmax = xmax, 
                                                      ymax = ymax, fill = Years),
                             alpha = 0.2, inherit.aes = FALSE) +
          scale_fill_manual(values=c("blue","green")) +
          guides(fill = guide_legend(override.aes = list(linetype = 0)))
      }
      print(t1)
    }
  })
  # output ts plot to ui
  output$time_plot <- renderPlot({
    ts_out()
  })
  
})