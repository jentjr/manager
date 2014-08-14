library(ggvis)
library(shiny)
shinyUI(fluidPage(
  
  titlePanel("ggvis test"),
  
  sidebarLayout(
   sidebarPanel(
     uiOutput("wells"),
     uiOutput("analytes")
   ),
   mainPanel(
     tabsetPanel(
       tabPanel("ggvis ts", ggvisOutput("plot1"))
     )
   )   
  )  
))