library(shiny)
library(rCharts)

# Define server
shinyServer(function(input, output) {
  
 
  
  # Faciliy selection
  #TODO
  
  output$time_plot <- renderPlot({
    
    data_selected <- subset(data, location %in% input$well & analyte %in% input$analyte)
    
    t <- ggplot(data_selected, aes(x=date, y=result, colour=analyte)) 
    
    if(input$scale_plot){
      # time series plot of analytes gridded by wells
      t1 <- t + geom_point(aes(shape=analyte), size=3) + geom_line() + facet_wrap(~location, scales="free") + theme_bw()
    } else {
      t1 <- t + geom_point(aes(shape=analyte), size=3) + geom_line() + facet_wrap(~location) + theme_bw()
    }
      
    if(input$date_lines){
      t1 <- t1 + geom_vline(xintercept=c(min(as.numeric(input$back_date_range)), max(as.numeric(input$back_date_range))))
      t1 <- t1 + geom_vline(xintercept=c(min(as.numeric(input$comp_date_range)), max(as.numeric(input$comp_date_range))), linetype="longdash")
      # draw lines for background date range and compliance date range
#       t1 <- t1 + geom_rect(xmin=min(as.numeric(input$back_date_range)), xmax=max(as.numeric(input$back_date_range)), ymin=-Inf, ymax=Inf, alpha=1/50)
#       t1 <- t1 + geom_rect(xmin=min(as.numeric(input$comp_date_range)), xmax=max(as.numeric(input$comp_date_range)), ymin=-Inf, ymax=Inf, alpha=1/50, fill="red")
    }
    
    print(t1)
    
  })
  
  output$box_plot <- renderPlot({
    
    data_selected <- subset(data, location %in% input$well & analyte %in% input$analyte)
    
    # box plot of analyte
    b <- ggplot(data_selected, aes(location, y=result, colour=analyte)) + theme_bw()
    
    if(input$scale_plot){
      b1 <- b + geom_boxplot()  + facet_wrap(~analyte, scale="free")
    } else{
      b1 <- b + geom_boxplot() + facet_wrap(~analyte)
    }
    
    print(b1)
    
  })
  
  output$piper_plot <- renderPlot({
    
#     piper_data <- subset(data, location %in% well & analyte %in% c("Magnesium, Mg (dissolved)", "Calcium, Ca (dissolved)", "Chloride, Cl (total)", "Sulfate, SO4, (total)"))
    
    
    
    print(ggplot_piper())
    
  })
  
  # Show the first "n" observations
  output$well_table <- renderTable({  
    data_selected <- subset(data, location %in% input$well & analyte %in% input$analyte)
    # out <- subset(out_f, location %in% input$well & analyte %in% input$analyte)
    print(data_selected)
  })
  
})