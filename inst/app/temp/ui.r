library(ggvis)
library(shiny)
shinyUI(fluidPage(
  
  titlePanel("ggvis test"),
  
  sidebarLayout(
   sidebarPanel(
     fileInput(inputId = "manages_path", 
               label = "Browse to MANAGES Site.mdb file",
               accept = c(".mdb", ".csv", ".xls")),
     conditionalPanel(
       condition = "input.data == FALSE",
       radioButtons(inputId = "file_type", 
                    label = "File Extension", 
                    choices = c(".mdb", ".csv", ".xls"))),
     uiOutput("wells"),
     uiOutput("analytes")
   ),
   mainPanel(
     tabsetPanel(
       tabPanel("data", dataTableOutput("well_table")),
       tabPanel("ggvis ts", ggvisOutput("plot1"))
     )
   )   
  )  
))