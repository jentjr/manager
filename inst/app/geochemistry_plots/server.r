# change options to handle large file size
options(shiny.maxRequestSize=-1)
library(groundwater)
library(ggplot2)
library(ggmap)
library(plyr)
library(lubridate)

# Define server
shinyServer(function(input, output) {
  # reactive function to upload data
  get_data <- reactive({
    if (!is.null(input$manages_path)){
        switch(input$file_type, 
               ".csv" = from_csv(input$manages_path$datapath),
               ".mdb" = connect_manages(input$manages_path$datapath),
               ".xls" = from_excel(input$manages_path$datapath))
      }      
  })
 
  # reactive function to upload spatial data   
  get_spatial_data <- reactive({
    if (!is.null(input$manages_path)){
      connect_manages_spatial(input$manages_path$datapath)
    }
  })
  
  # return a list of well names
  output$well_list <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
      well_names <- get_well_names(data)
      selectInput("well", "Monitoring Wells", well_names, multiple = TRUE,
                  selected = well_names[1])
    }
  })
  
  # return start and end dates of data
  output$date_ranges <- renderUI({
    if (!is.null(input$manages_path)){
      data <- get_data()
        dateRangeInput("date_range", "Date Range", 
                       start = min(data$sample_date,                                        
                                   na.rm = TRUE), 
                       end = max(data$sample_date, na.rm = TRUE))
    }
  })

  # Output a table of the data
  output$well_table <- renderDataTable({
    if (!is.null(input$manages_path)){
      data <- get_data()
      return(data)
    }
   }, options = list(sScrollY = "100%", sScrollX = "100%", 
                     aLengthMenu = c(5, 10, 15, 25, 50, 100), 
                     iDisplayLength = 15)
  )
  
  # Output of a ion data 
  output$ion_summary <- renderDataTable({
    if (!is.null(input$manages_path)){
      data <- get_data()
      ions <- get_major_ions(data, Mg=input$Mg, Ca=input$Ca, Na=input$Na, 
                             K=input$K, Cl=input$Cl, 
                             SO4=input$SO4, Alk=input$Alk, TDS=input$TDS)
      return(ions)
    }
  }, options = list(sScrollY = "100%", sScrollX = "100%",
                    aLengthMenu = c(5, 10, 15, 25, 50, 100),
                    iDisplayLength = 15)
  )
  
  output$piper_plot_data <- renderDataTable({
    if (!is.null(input$manages_path)){
      data <- get_data()
      # get the major cations/anions
      data_selected <- subset(data, location_id %in% input$well)      
      data_selected <- subset(data_selected, sample_date >= ymd(input$date_ranges[1]) &
                                sample_date <= ymd(input$date_ranges[2]))
      ions <- get_major_ions(data_selected, Mg=input$Mg, Ca=input$Ca,
                             Na=input$Na, K=input$K, Cl=input$Cl, 
                             SO4=input$SO4, Alk=input$Alk, TDS=input$TDS)
      piper_data <- transform_piper_data(ions, Mg=input$Mg, Ca=input$Ca,
                                         Na=input$Na, K=input$K, Cl=input$Cl, 
                                         SO4=input$SO4, Alk=input$Alk, 
                                         TDS=input$TDS)
      return(piper_data)
    }
   }, options = list(sScrollY = "100%", sScrollX = "100%",
                    aLengthMenu = c(5, 10, 15, 25, 50, 100), iDisplayLength = 15)
  )
  
  # stiff diagram
  output$stiff_plot_data <- renderDataTable({
    if (!is.null(input$manages_path)){
      data <- get_data()
      data_selected <- subset(data, location_id %in% input$well)
      ions <- get_major_ions(data_selected, Mg=input$Mg, Ca=input$Ca, Na=input$Na, K=input$K, Cl=input$Cl, 
                             SO4=input$SO4, Alk=input$Alk, TDS=input$TDS)
      plot_data <- convert_mgL_to_meqL(ions, Mg="Magnesium, dissolved", Ca="Calcium, dissolved",
                                       Na="Sodium, dissolved", K="Potassium, dissolved", 
                                       Cl="Chloride, total", SO4="Sulfate, total", 
                                       HCO3="Alkalinity, total (lab)")
      stiff_data <- transform_stiff_data(plot_data)
      return(stiff_data)
    }
   }, options = list(sScrollY = "100%", sScrollX = "100%",
                    aLengthMenu = c(5, 10, 15, 25, 50, 100), iDisplayLength = 15)
  )
  
    
 # piper plot
  output$piper_plot <- renderPlot({
    if (!is.null(input$manages_path)){
      data <- get_data()
      # get the major cations/anions
      data_selected <- subset(data, location_id %in% input$well)
      
      ions <- get_major_ions(data_selected, Mg=input$Mg, Ca=input$Ca, Na=input$Na, K=input$K, Cl=input$Cl, 
                             SO4=input$SO4, Alk=input$Alk, TDS=input$TDS)
      piper_data <- transform_piper_data(ions, Mg=input$Mg, Ca=input$Ca, Na=input$Na, K=input$K, Cl=input$Cl, 
                                         SO4=input$SO4, Alk=input$Alk, TDS=input$TDS)
      print(plot_piper(piper_data, TDS=input$TDS_plot))
    }
  })
  
#   # piper time plot
#   output$piper_time_plot <- renderUI({
#     if (!is.null(input$manages_path)){
#       data <- get_data()
#       # get the major cations/anions
#       data_selected <- subset(data, location_id %in% input$well)
#       
#       ions <- get_major_ions(data_selected, Mg=input$Mg, Ca=input$Ca, Na=input$Na, K=input$K, Cl=input$Cl, 
#                              SO4=input$SO4, Alk=input$Alk, TDS=input$TDS)
#       piper_data <- transform_piper_data(ions, Mg=input$Mg, Ca=input$Ca, Na=input$Na, K=input$K, Cl=input$Cl, 
#                                          SO4=input$SO4, Alk=input$Alk, TDS=input$TDS)
#       piper_time_html(piper_data)
#     }
#   })
  
 # stiff diagram
 output$stiff_diagram <- renderPlot({
   if (!is.null(input$manages_path)){
     data <- get_data()
     data_selected <- subset(data, location_id %in% input$well)
   }
 })
 
 # saptial plot of wells
  output$well_map <- renderPlot({
    if (!is.null(input$manages_path)){
      # read in spatial data and coerce long and lat to numeric
      sp_data <- get_spatial_data()
      sp_data <- na.omit(sp_data)
      sp_data$long_pos <- as.numeric(as.character(sp_data$long_pos))
      sp_data$lat_pos <- as.numeric(as.character(sp_data$lat_pos))
      
      well_map <- get_map(location = c(lon=mean(sp_data$long_pos), lat=mean(sp_data$lat_pos)), zoom=14)
      
      p1 <- ggmap(well_map, extent = "device", maptype = "terrain", color = "color")
      
      p2 <- p1 + geom_point(data = sp_data, aes(x = long_pos, y = lat_pos, colour = location_id), size = 2.25)
      
      print(p2)
    }
  })
})